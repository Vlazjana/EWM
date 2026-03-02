FUNCTION z_rf_emduri_step03_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EV_NEXT_STEP) TYPE  /SCWM/DE_STEP
*"     REFERENCE(ES_MSG) TYPE  BAPIRET2
*"  CHANGING
*"     REFERENCE(ZSRF_EMDURI) TYPE  ZSRF_EMDURI OPTIONAL
*"----------------------------------------------------------------------


  DATA lv_docno TYPE /scdl/dl_docno.
  DATA(lv_fcode) = /scwm/cl_rf_bll_srvc=>get_fcode( ).

  CASE lv_fcode.
    WHEN 'ZSTEP4' OR 'ENTER'.

      PERFORM determine_source_docno USING    gv_sel_scr3
                                              gt_scr3
                                     CHANGING lv_docno.

      PERFORM build_product_list     USING    lv_docno.

      PERFORM finalize_screen400.

      zsrf_emduri-product_list[] = gt_product_list.
      CLEAR line.
  ENDCASE.


ENDFUNCTION.



    <-- LV_DOCNO
*&---------------------------------------------------------------------*
FORM determine_source_docno  USING    uv_sel_scr3 TYPE /scwm/de_rf_selno
                                      ut_scr3 TYPE zsrf_tt_emduri_scr3
                             CHANGING cv_docno TYPE /scdl/dl_docno_int.
  IF uv_sel_scr3 IS NOT INITIAL.
    READ TABLE ut_scr3 INTO gs_scr3
         WITH KEY selno = uv_sel_scr3.

    IF sy-subrc <> 0.
      MESSAGE e003(z_msg_emduri). " inbound not found in list
      RETURN.
    ENDIF.

    DATA(lv_docno_src) = gs_scr3-docno.

  ELSEIF gs_scr1-docno IS NOT INITIAL.
    lv_docno_src = gs_scr1-docno.
    gs_scr3-docno = gs_scr1-docno.
  ELSE.
    MESSAGE e001(z_msg_emduri).  " nessuna inbound indicata
    RETURN.
  ENDIF.

  cv_docno = |{ lv_docno_src ALPHA = IN }|.
ENDFORM.



 <--  p2        text
*&---------------------------------------------------------------------*
FORM build_product_list USING uv_docno TYPE /scdl/dl_docno.
  DATA:
    lv_rowid      TYPE i,
    lv_matid_read TYPE /scmb/mdl_matid,
    lv_matnr      TYPE matnr,
    lv_remain_qty type i.

  REFRESH: gt_product_list.
  DATA: gs_docno          TYPE /scwm/dlv_docno_itemno_str,
        gt_docno          TYPE /scwm/dlv_docno_itemno_tab,
        lo_dlv_management TYPE REF TO /scwm/cl_dlv_management_prd.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = uv_docno
    IMPORTING
      output = gs_docno-docno.
  APPEND gs_docno TO gt_docno.

  CREATE OBJECT lo_dlv_management.

  TRY.
      CALL METHOD lo_dlv_management->/scwm/if_dlv_manag_prd~query
        EXPORTING
          iv_whno    = 'DT20'
          it_docno   = gt_docno
        IMPORTING
          et_headers = gt_headers
          et_items   = gt_items.
    CATCH /scdl/cx_delivery.
  ENDTRY.

  LOOP AT gt_items ASSIGNING FIELD-SYMBOL(<fs_items>).
    IF sy-tabix = 1.
      lv_remain_qty = <fs_items>-qty-qty.
      DATA(lv_uom)        = <fs_items>-qty-uom.
      DATA(lv_product)    = <fs_items>-product-productno.
      DATA(lv_maktx)      = <fs_items>-product-product_text.
    ELSE.
      lv_remain_qty       = lv_remain_qty - <fs_items>-qty-qty.
    ENDIF.
  ENDLOOP.

  APPEND VALUE #( selno      = '10'
                  productno  = lv_product
                  maktx      = lv_maktx
                  qty        = lv_remain_qty
                  uom        = lv_uom  ) TO gt_product_list.

ENDFORM.
*&--------------------------------------


*&---------------------------------------------------------------------*
FORM finalize_screen400 .

  IF gt_product_list IS INITIAL.
    MESSAGE e006(z_msg_emduri).
    RETURN.
  ENDIF.

  gt_scr4                    = gt_product_list.
  fill1 = lines( gt_scr4 ).
ENDFORM.
