
FUNCTION z_rf_emduri_step05_pai.
*"----------------------------------------------------------------------
*"*"Interfaccia locale:
*"  EXPORTING
*"     REFERENCE(EV_NEXT_STEP) TYPE  /SCWM/DE_STEP
*"     REFERENCE(ES_MSG) TYPE  BAPIRET2
*"  CHANGING
*"     REFERENCE(ZSRF_EMDURI) TYPE  ZSRF_EMDURI OPTIONAL
*"     REFERENCE(GT_INBOUND_LIST) TYPE  ZSRF_TT_EMDURI_SCR3 OPTIONAL
*"     REFERENCE(GT_SUPPLIER_LIST) TYPE  ZSRF_TT_EMDURI_SCR2 OPTIONAL
*"     REFERENCE(GT_PRODUCT_LIST) TYPE  ZSRF_TT_EMDURI_SCR4 OPTIONAL
*"     REFERENCE(GT_BARCODE_LIST) TYPE  ZSRF_TT_EMDURI_SCR5 OPTIONAL
*"     REFERENCE(GT_INTERNAL_BATCH) TYPE  ZSRF_TT_EMDURI_SCR6 OPTIONAL
*"----------------------------------------------------------------------

  DATA: lv_exp_int_batch  TYPE bapibatchkey-batch,
        lv_internal_batch TYPE bapibatchkey-batch.

  PERFORM decode_sscc.
  PERFORM get_umrez.
  PERFORM get_internal_batch     CHANGING lv_exp_int_batch.
  PERFORM bapi_batch_create      USING    lv_exp_int_batch
                                          'DT20'
                                 CHANGING lv_internal_batch.
  PERFORM finalize_screen600     USING lv_internal_batch.


ENDFUNCTION.


    <-- GS_SCR6
*&---------------------------------------------------------------------*
FORM decode_sscc.
  DATA: lv_sscc TYPE zsrf_emduri_scr5-sscc.
  lv_sscc = gs_scr5-sscc.
  IF lv_sscc IS INITIAL.
    MESSAGE e009(z_msg_emduri).    " "Inserire SSCC"
    RETURN.
  ENDIF.

  "AI 10 - Batch (10 char)
  FIND '(10)' IN lv_sscc MATCH OFFSET DATA(lv_off).
  IF sy-subrc = 0.
    lv_off = lv_off + 4.                  "salto '(10'
    DATA(lv_batch) = lv_sscc+lv_off(10).
  ENDIF.

  "AI 15 - Best before (YYMMDD)
  FIND '(15)' IN lv_sscc MATCH OFFSET lv_off.
  IF sy-subrc = 0.
    lv_off = lv_off + 4.                  "salto '(15'
    DATA(lv_bbd)  = |20{ lv_sscc+lv_off(6) }|.
  ENDIF.

  "AI 37 - Quantity (4 char)
  FIND '(37)' IN lv_sscc MATCH OFFSET lv_off.
  IF sy-subrc = 0.
    lv_off = lv_off + 4.                  "salto '(37'
    DATA(lv_qty)  = lv_sscc+lv_off(4).
  ENDIF.


  gs_scr6 = VALUE #(  vendor_batch  = lv_batch
                      expiry_date   = lv_bbd
                      coli          = lv_qty    ).

ENDFORM.




*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_umrez .
  " GET UMREZ

  SELECT SINGLE /sapapo/marm~umrez

    FROM /sapapo/matkey

    JOIN /sapapo/marm
    ON /sapapo/marm~matid = /sapapo/matkey~matid
    AND /sapapo/marm~meinh = 'SCM'

    INTO @DATA(lv_umrez)

    WHERE /sapapo/matkey~matnr = @gs_scr5-productno.

  IF sy-subrc = 0.
    gs_scr6-uvc = lv_umrez.
  ENDIF.
ENDFORM.


&---------------------------------------------------------------------*
*&      <-- LV_EXP_INT_BATCH
*&---------------------------------------------------------------------*
FORM get_internal_batch  CHANGING cv_exp_int_batch.
  DATA lv_batch_num      TYPE char10.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = '/SCWM/CHAR'
    IMPORTING
      number                  = lv_batch_num
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF sy-subrc <> 0.
    MESSAGE e015(z_msg_emduri).    " "ERRORE GENERAZIONE NUMERO BATCH"
    RETURN.
  ENDIF.
  cv_exp_int_batch = lv_batch_num.
ENDFORM.
*&------------------------------



->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM bapi_batch_create USING    uv_exp_int_batch  TYPE bapibatchkey-batch
                                uv_lgnum          TYPE bapibatchkey-plant
                       CHANGING cv_internal_batch TYPE bapibatchkey-batch.

  DATA: lv_matnr           TYPE bapibatchkey-material,
        ls_batchattributes TYPE bapibatchatt,
        lt_bapiret2        TYPE STANDARD TABLE OF bapiret2.

  lv_matnr = gs_scr5-productno.
  ls_batchattributes-vendrbatch = gs_scr6-vendor_batch.
  ls_batchattributes-expirydate = gs_scr6-expiry_date.
*  ls_batchattributes-PROD_DATE = gs_scr6-expiry_date - durata totale . "TODO

  CALL FUNCTION 'BAPI_BATCH_CREATE'
    EXPORTING
      material        = lv_matnr
      batch           = uv_exp_int_batch
      plant           = uv_lgnum
      batchattributes = ls_batchattributes
*     BATCHCONTROLFIELDS         =
*     BATCHSTORAGELOCATION       =
*     INTERNALNUMBERCOM          =
*     EXTENSION1      =
*     MATERIAL_EVG    =
*     MATERIAL_LONG   =
    IMPORTING
      batch           = cv_internal_batch
*     BATCHATTRIBUTES =
    TABLES
      return          = lt_bapiret2.

  " CONTROLLO ERRORI
  LOOP AT lt_bapiret2 INTO DATA(ls_bapiret2) WHERE type CA 'EA'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE e016(z_msg_emduri).    " "ERRORE CREAZIONE BATCH"
    RETURN.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
ENDFORM.


*& <--  p2        text
*&---------------------------------------------------------------------*
FORM finalize_screen600 USING uv_internal_batch TYPE bapibatchkey-batch.

  IF gs_scr6-coli IS INITIAL.
    gs_scr6-coli = 0.
  ELSE.
    gs_scr6-coli = |{ gs_scr6-coli ALPHA = OUT }|.
  ENDIF.

  IF gs_scr6-coli IS INITIAL.
    gs_scr6-coli = 0.
  ELSE.
    gs_scr6-coli = |{ gs_scr6-coli ALPHA = OUT }|.
  ENDIF.

  IF gs_scr6-zznumstr IS INITIAL.
    gs_scr6-zznumstr = 0.
  ENDIF.
  gs_scr6-zznumstr = |{ gs_scr6-zznumstr ALPHA = OUT }|.

  gs_scr6-vendor_product = gs_scr5-productno.    " LS_PROD-MATNR. " CODICE PRODOTTO FORNITORE
  gs_scr6-batchno_ui     = uv_internal_batch.    " LS_PROD-VSOLA. " BATCH CREATA
  gs_scr6-zfp = |{ gs_scr6-coli }X{ gs_scr6-zznumstr }X{ gs_scr6-uvc }|.
ENDFORM.
