FUNCTION z_rf_emduri_step04_pai.
*"----------------------------------------------------------------------
*"*"LOCAL INTERFACE:
*"  EXPORTING
*"     REFERENCE(EV_NEXT_STEP) TYPE  /SCWM/DE_STEP
*"     REFERENCE(ES_MSG) TYPE  BAPIRET2
*"  CHANGING
*"     REFERENCE(ZSRF_EMDURI) TYPE  ZSRF_EMDURI OPTIONAL
*"     REFERENCE(GT_INBOUND_LIST) TYPE  ZSRF_TT_EMDURI_SCR3 OPTIONAL
*"----------------------------------------------------------------------

  perform finalize_screen500.

ENDFUNCTION.



*&---------------------------------------------------------------------*
FORM finalize_screen500 .
  READ TABLE gt_scr4 INTO gs_scr4 WITH KEY selno = gv_sel_scr4.
  IF sy-subrc IS INITIAL.
    gs_scr5-docno = gs_scr3-docno.
    MOVE-CORRESPONDING gs_scr4 TO gs_scr5.
    gs_scr5-sscc = '(10)0002602604(15)270730(37)0039'.
  ELSE.
    MESSAGE e003(z_msg_emduri).
    RETURN.
  ENDIF.
ENDFORM.
