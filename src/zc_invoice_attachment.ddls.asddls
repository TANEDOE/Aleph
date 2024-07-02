@EndUserText.label: 'Invoice Attachment BO Projection View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity zc_invoice_attachment
  as projection on zi_invoice_attachment
{
  key SAP_UUID,
      SAP_Parent_UUID,
      URL,
      URLDescription,
      /* Associations */
      _SupplierInvoice : redirected to parent zc_supplier_invoice
}
