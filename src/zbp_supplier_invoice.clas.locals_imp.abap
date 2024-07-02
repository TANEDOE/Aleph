CLASS lhc_supplierinvoice DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    CONSTANTS gc_message_id TYPE symsgid VALUE 'ZMSG_SUPPLIER_INV'.

    TYPES: tt_attachments TYPE STANDARD TABLE OF zta_attachment WITH DEFAULT KEY.

    METHODS validateinvoicedata FOR VALIDATE ON SAVE
      IMPORTING keys FOR supplierinvoice~validateinvoicedata.

    METHODS postsupplierinvoice FOR MODIFY
      IMPORTING keys FOR ACTION supplierinvoice~postsupplierinvoice RESULT result.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR supplierinvoice RESULT result.

    METHODS convert_to_odata_date_format
      IMPORTING iv_date          TYPE datum
      RETURNING VALUE(rv_result) TYPE string.

    METHODS get_error_message
      IMPORTING iv_longtext_url  TYPE string
      RETURNING VALUE(ro_result) TYPE REF TO if_abap_behv_message .

    METHODS is_po_gr_based
      IMPORTING iv_purchase_order      TYPE ebeln
                iv_purchase_order_item TYPE ebelp
      RETURNING VALUE(rv_result)       TYPE abap_bool.

    METHODS add_attachments
      IMPORTING iv_linked_sap_object_key TYPE string
                it_attachments           TYPE tt_attachments.

    METHODS get_journal_entry
      IMPORTING iv_invoice_reference TYPE xblnr1
      RETURNING VALUE(rv_result)     TYPE belnr_d.

    METHODS is_authorize
      IMPORTING iv_company_code  TYPE bukrs
      RETURNING VALUE(rv_result) TYPE abap_bool.
ENDCLASS.

CLASS lhc_supplierinvoice IMPLEMENTATION.


  METHOD validateinvoicedata.

    READ ENTITIES OF zi_supplier_invoice IN LOCAL MODE
      ENTITY supplierinvoice
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_supplier_invoices).

    LOOP AT lt_supplier_invoices ASSIGNING FIELD-SYMBOL(<fs_supplier_invoice>).
      " Clear state messages that might exist
      APPEND VALUE #(  %tky        = <fs_supplier_invoice>-%tky
                       %state_area = 'VALIDATE' ) TO reported-supplierinvoice.

    ENDLOOP.

  ENDMETHOD.

  METHOD postsupplierinvoice.

    DATA lv_has_error TYPE abap_bool.

    READ ENTITIES OF zi_supplier_invoice IN LOCAL MODE
        ENTITY supplierinvoice
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(lt_supplier_invoices).

    LOOP AT lt_supplier_invoices ASSIGNING FIELD-SYMBOL(<fs_supplier_invoice>).

      IF is_authorize( <fs_supplier_invoice>-companycode ) EQ abap_false.
        CONTINUE.
      ENDIF.

      DATA(lv_accounting_document) = get_journal_entry( to_upper( <fs_supplier_invoice>-reference ) ).

      IF lv_accounting_document IS NOT INITIAL.
        APPEND VALUE #( %tky = <fs_supplier_invoice>-%tky ) TO failed-supplierinvoice.

        APPEND VALUE #( %tky = <fs_supplier_invoice>-%tky
                        %msg = new_message(
                                 id       = gc_message_id
                                 number   = '002'
                                 v1       = lv_accounting_document
                                 severity = if_abap_behv_message=>severity-error ) ) TO reported-supplierinvoice.

        CONTINUE.
      ENDIF.

      TRY.
          DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                                       comm_scenario  = 'ZCS_SUPPLIER_INVOICE'
                                                       service_id     = 'ZOS_SUPPLIER_INVOICE_REST' ).

          DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_destination ).

          DATA(lo_request) = lo_http_client->get_http_request( ).

          lo_http_client->accept_cookies( abap_true ).

          lo_request->set_uri_path( |/A_SupplierInvoice| ).

          lo_request->set_content_type( 'application/json' ).

          lo_request->set_header_fields( VALUE #( ( name  = 'X-CSRF-Token'
                                                    value = 'fetch' ) ) ).

          DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>get ).

          DATA(lv_csrf_token) = lo_response->get_header_field( 'X-CSRF-Token' ).

          DATA(lv_session) = lo_response->get_header_field( 'set-cookie' ).

          DATA(lo_json_builder) = xco_cp_json=>data->builder( ).

          lo_json_builder->begin_object( ).
          lo_json_builder->add_member( 'CompanyCode' )->add_string( <fs_supplier_invoice>-companycode ).
          lo_json_builder->add_member( 'DocumentDate' )->add_string( convert_to_odata_date_format( <fs_supplier_invoice>-documentdate ) ).
          lo_json_builder->add_member( 'PostingDate' )->add_string( convert_to_odata_date_format( <fs_supplier_invoice>-postingdate ) ).
          lo_json_builder->add_member( 'InvoicingParty' )->add_string( <fs_supplier_invoice>-invoicingparty ).
          lo_json_builder->add_member( 'DocumentCurrency' )->add_string( <fs_supplier_invoice>-currencycode ).
          lo_json_builder->add_member( 'InvoiceGrossAmount' )->add_string( condense( CONV string( <fs_supplier_invoice>-grossinvoiceamount ) ) ).
          lo_json_builder->add_member( 'SupplierInvoiceIDByInvcgParty' )->add_string( <fs_supplier_invoice>-reference ).
          lo_json_builder->add_member( 'TaxDeterminationDate' )->add_string( convert_to_odata_date_format( <fs_supplier_invoice>-taxreportingdate ) ).
          lo_json_builder->add_member( 'TaxFulfillmentDate' )->add_string( convert_to_odata_date_format( <fs_supplier_invoice>-taxfulfillmentdate ) ).
          lo_json_builder->add_member( 'DocumentHeaderText' )->add_string( <fs_supplier_invoice>-documentheadertext ).
          lo_json_builder->add_member( 'TaxIsCalculatedAutomatically' )->add_boolean( abap_true ).
          lo_json_builder->add_member( 'SupplierInvoiceIsCreditMemo' )->add_string( SWITCH #( <fs_supplier_invoice>-transactiontype WHEN '2' THEN abap_true ELSE abap_false ) ).
          lo_json_builder->add_member( 'YY1_UploadedBy_MIH' )->add_string( <fs_supplier_invoice>-createdby ).

          READ ENTITIES OF zi_supplier_invoice IN LOCAL MODE
            ENTITY supplierinvoice BY \_poreference
            ALL FIELDS
            WITH VALUE #( ( %tky = <fs_supplier_invoice>-%tky ) )
            RESULT DATA(lt_po_references).

          lo_json_builder->add_member( 'to_SuplrInvcItemPurOrdRef' )->begin_array( ).

          DATA(lv_index) = 0.

          LOOP AT lt_po_references ASSIGNING FIELD-SYMBOL(<fs_po_reference>).
            lv_index += 1.

            lo_json_builder->begin_object( ).
            lo_json_builder->add_member( 'SupplierInvoiceItem' )->add_string( condense( CONV string( lv_index ) ) ).
            lo_json_builder->add_member( 'PurchaseOrder' )->add_string( <fs_po_reference>-purchaseorder ).
            lo_json_builder->add_member( 'PurchaseOrderItem' )->add_string( <fs_po_reference>-purchaseorderitem ).
            lo_json_builder->add_member( 'TaxCode' )->add_string( <fs_supplier_invoice>-taxcode ).
            lo_json_builder->add_member( 'DocumentCurrency' )->add_string( <fs_supplier_invoice>-currencycode ).
            lo_json_builder->add_member( 'SupplierInvoiceItemAmount' )->add_string( condense( CONV string( <fs_po_reference>-totalamount ) ) ).
            lo_json_builder->add_member( 'PurchaseOrderQuantityUnit' )->add_string( <fs_po_reference>-unitofmeasure ).
            lo_json_builder->add_member( 'QuantityInPurchaseOrderUnit' )->add_string( condense( CONV string(  <fs_po_reference>-quantity ) ) ).
            lo_json_builder->add_member( 'IN_HSNOrSACCode' )->add_string( <fs_po_reference>-hsnorsaccode ).

            IF is_po_gr_based( iv_purchase_order      = <fs_po_reference>-purchaseorder
                               iv_purchase_order_item = <fs_po_reference>-purchaseorderitem ).

              SELECT SINGLE FROM i_goodsmovementcube
                  FIELDS materialdocument,
                         materialdocumentitem,
                         fiscalyear
                  WHERE purchaseorder     = @<fs_po_reference>-purchaseorder
                    AND purchaseorderitem = @<fs_po_reference>-purchaseorderitem
                  INTO @DATA(ls_material_reference).
              IF sy-subrc = 0.
                lo_json_builder->add_member( 'ReferenceDocument' )->add_string( ls_material_reference-materialdocument ).
                lo_json_builder->add_member( 'ReferenceDocumentItem' )->add_string( ls_material_reference-materialdocumentitem ).
                lo_json_builder->add_member( 'ReferenceDocumentFiscalYear' )->add_string( ls_material_reference-fiscalyear ).
              ELSE.
                lv_has_error = abap_true.

                APPEND VALUE #( %tky = <fs_supplier_invoice>-%tky ) TO failed-supplierinvoice.

                APPEND VALUE #( %tky = <fs_supplier_invoice>-%tky
                                %msg = new_message(
                                         id       = gc_message_id
                                         number   = '004'
                                         v1       = <fs_po_reference>-purchaseorder
                                         severity = if_abap_behv_message=>severity-error ) ) TO reported-supplierinvoice.

              ENDIF.
            ENDIF.

            lo_json_builder->end_object( ).
          ENDLOOP.

          lo_json_builder->end_array( ).

          lo_json_builder->end_object( ).

          IF lv_has_error EQ abap_true.
            CONTINUE.
          ENDIF.

          DATA(lv_json_string) = lo_json_builder->get_data( )->to_string( ).

          lo_request->set_header_fields( VALUE #( ( name  = 'X-CSRF-Token'
                                                    value = lv_csrf_token ) ) ).

          lo_request->set_form_field( i_name  = 'Cookie'
                                      i_value = lv_session ) .

          lo_request->set_text( lv_json_string ).

          DATA(lo_post_response) = lo_http_client->execute( i_method = if_web_http_client=>post ).
        CATCH cx_http_dest_provider_error cx_web_http_client_error.
          APPEND VALUE #( %tky = <fs_supplier_invoice>-%tky ) TO failed-supplierinvoice.

          APPEND VALUE #( %tky = <fs_supplier_invoice>-%tky
                          %msg = new_message(
                                   id       = gc_message_id
                                   number   = '003'
                                   severity = if_abap_behv_message=>severity-error ) ) TO reported-supplierinvoice.

          CONTINUE.
      ENDTRY.

      " Get Response Payload from API
      DATA(lv_response) = lo_post_response->get_text( ).

      IF lo_post_response->get_status( )-code = '201'.
        <fs_supplier_invoice>-status        = 'S'.
        <fs_supplier_invoice>-invoicenumber = substring_before( val = substring_after( val = lv_response
                                                                                       sub = |<d:SupplierInvoice>| )
                                                                sub = |</d:SupplierInvoice>| ).

        MODIFY ENTITIES OF zi_supplier_invoice IN LOCAL MODE
          ENTITY supplierinvoice
          UPDATE FIELDS ( status invoicenumber )
          WITH CORRESPONDING #( lt_supplier_invoices ).

        " Fill the response table
        result = VALUE #( FOR ls_supplier_invoice IN lt_supplier_invoices
                            ( %tky   = ls_supplier_invoice-%tky
                              %param = ls_supplier_invoice ) ).

      ELSE.
        " Get Message ID, Number and parameters
        DATA(lv_longtext_url) =  substring_before( val = substring_after( val = lv_response
                                                                          sub = |<longtext_url>| )
                                                   sub = |</longtext_url>| ).

        APPEND VALUE #( %tky = <fs_supplier_invoice>-%tky ) TO failed-supplierinvoice.

        APPEND VALUE #( %tky = <fs_supplier_invoice>-%tky
                        %msg = get_error_message(  lv_longtext_url ) ) TO reported-supplierinvoice.

      ENDIF.

      IF <fs_supplier_invoice>-invoicenumber IS NOT INITIAL.
        READ ENTITIES OF zi_supplier_invoice IN LOCAL MODE
            ENTITY supplierinvoice BY \_attachment
            ALL FIELDS
            WITH VALUE #( ( %tky = <fs_supplier_invoice>-%tky ) )
            RESULT DATA(lt_attachments).

        add_attachments(
          iv_linked_sap_object_key = |{ <fs_supplier_invoice>-invoicenumber }{ <fs_supplier_invoice>-postingdate(4) }|
          it_attachments           = CORRESPONDING #( lt_attachments ) ).

      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD convert_to_odata_date_format.
    rv_result = |{  iv_date+0(4) }-{ iv_date+4(2) }-{ iv_date+6(2) }T00:00:00|.
  ENDMETHOD.

  METHOD get_error_message.
    DATA lv_pattern TYPE string VALUE `MSGID='([^\']*)',MSGNO='([^\']*)',MESSAGE_V1='([^\']*)',MESSAGE_V2='([^\']*)',MESSAGE_V3='([^\']*)',MESSAGE_V4='([^\']*)'`.

    FIND REGEX lv_pattern
            IN replace( val  = iv_longtext_url
                        pcre = |'''|
                        with = |'|
                        occ  = 0 )
    SUBMATCHES DATA(lv_msgid)
               DATA(lv_msgno)
               DATA(lv_message_v1)
               DATA(lv_message_v2)
               DATA(lv_message_v3)
               DATA(lv_message_v4).

    ro_result = new_message(
                    id       = CONV #( lv_msgid )
                    number   = CONV #( lv_msgno )
                    severity = if_abap_behv_message=>severity-error
                    v1       = lv_message_v1
                    v2       = lv_message_v2
                    v3       = lv_message_v3
                    v4       = lv_message_v4  ).

  ENDMETHOD.

  METHOD is_po_gr_based.
    SELECT SINGLE FROM i_purchaseorderitemapi01
        FIELDS invoiceisgoodsreceiptbased
        WHERE purchaseorder = @iv_purchase_order
          AND purchaseorderitem = @iv_purchase_order_item
        INTO @rv_result.

  ENDMETHOD.

  METHOD add_attachments.
    LOOP AT it_attachments ASSIGNING FIELD-SYMBOL(<fs_attachment>).
      TRY.
          DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                                       comm_scenario  = 'ZCS_SUPPLIER_INVOICE'
                                                       service_id     = 'ZOS_ATTACHMENTS_REST' ).

          DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_destination ).

          DATA(lo_request) = lo_http_client->get_http_request( ).

          lo_http_client->accept_cookies( abap_true ).

          DATA(lv_uri_path) = |/CreateUrlAsAttachment?BusinessObjectTypeName='BUS2081'&LinkedSAPObjectKey='{ iv_linked_sap_object_key }'&Url='{ <fs_attachment>-url }'&UrlDescription='{ <fs_attachment>-urldescription }'&MIMEType='text/url'|.

          lo_request->set_uri_path( lv_uri_path ).

          lo_request->set_content_type( 'application/json' ).

          lo_request->set_header_fields( VALUE #( ( name  = 'X-CSRF-Token'
                                                    value = 'fetch' ) ) ).

          DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>get ).

          DATA(lv_csrf_token) = lo_response->get_header_field( 'X-CSRF-Token' ).

          DATA(lv_session) = lo_response->get_header_field( 'set-cookie' ).

          lo_request->set_header_fields( VALUE #( ( name  = 'X-CSRF-Token'
                                                    value = lv_csrf_token ) ) ).

          lo_request->set_form_field( i_name  = 'Cookie'
                                      i_value = lv_session ) .

          DATA(lo_post_response) = lo_http_client->execute( i_method = if_web_http_client=>post ).

        CATCH cx_http_dest_provider_error cx_web_http_client_error.
      ENDTRY.

      " Get Response Payload from API
      DATA(lv_response) = lo_post_response->get_text( ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_journal_entry.

    SELECT SINGLE FROM i_supplierinvoiceapi01
        FIELDS concat( supplierinvoice, fiscalyear ) AS invoicereference
        WHERE supplierinvoiceidbyinvcgparty EQ @iv_invoice_reference
        INTO @DATA(lv_orig_reference_doc).
    IF sy-subrc = 0.
      SELECT SINGLE FROM i_journalentry
          FIELDS accountingdocument
          WHERE originalreferencedocument EQ @lv_orig_reference_doc
          INTO @rv_result.
    ENDIF.

  ENDMETHOD.

  METHOD is_authorize.
    AUTHORITY-CHECK
             OBJECT 'M_RECH_BUK'
                 ID 'ACTVT' FIELD '01'
                 ID 'BUKRS' FIELD iv_company_code.
    IF sy-subrc EQ 0.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_instance_authorizations.

  ENDMETHOD.

ENDCLASS.
