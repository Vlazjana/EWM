FUNCTION z_rf_emduri_step02_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EV_NEXT_STEP) TYPE  /SCWM/DE_STEP
*"     REFERENCE(ES_MSG) TYPE  BAPIRET2
*"  CHANGING
*"     REFERENCE(ZSRF_EMDURI) TYPE  ZSRF_EMDURI OPTIONAL
*"----------------------------------------------------------------------


  PERFORM fill_screen0300 CHANGING zsrf_emduri-inbound_list.

ENDFUNCTION.


& Form fill_screen0300
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_screen0300 CHANGING ct_inbound_list TYPE zsrf_emduri-inbound_list.

  DATA: lv_rowid TYPE i.
  DATA(lv_fcode) = /scwm/cl_rf_bll_srvc=>get_fcode( ).
  REFRESH: gt_inbound_list,gt_scr3.

  CASE lv_fcode.
    WHEN 'ENTER'.
      READ TABLE gt_supplier_list INTO DATA(ls_supplier)
            WITH KEY selno = gv_sel_scr2.

      IF sy-subrc <> 0.
        MESSAGE e003(z_msg_emduri).
        RETURN.
      ENDIF.

      LOOP AT ct_inbound_list ASSIGNING FIELD-SYMBOL(<fs_inbound_list>) WHERE supplier = ls_supplier-partyno.
        lv_rowid = lv_rowid + 10.
        <fs_inbound_list>-selno = lv_rowid.

        APPEND VALUE #( selno     = <fs_inbound_list>-selno
                        docno     = |{ <fs_inbound_list>-docno ALPHA = OUT }|
                        doccat    = <fs_inbound_list>-doccat
                        supplier  =  <fs_inbound_list>-supplier ) TO gt_scr3.
      ENDLOOP.
      CLEAR gv_sel_scr2.
      CLEAR line.
  ENDCASE.
ENDFORM.
