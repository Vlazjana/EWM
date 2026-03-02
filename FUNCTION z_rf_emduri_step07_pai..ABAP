FUNCTION z_rf_emduri_step07_pai.
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
*"----------------------------------------------------------------------

  PERFORM validate_hu_creation_input.

  "GET STRORAGE BIN
**********************************************************************
  DATA: lv_gmbin TYPE /scwm/dl_gmbin.
  lv_gmbin = gt_items[ 1 ]-sapext-/scwm/gmbin.
**********************************************************************
  DATA        lv_matid TYPE /scmb/mdl_matid.
  PERFORM read_product_master  CHANGING lv_matid.
**********************************************************************
*
*  PERFORM create_hu
*    USING    'DT20'
*             lv_ritmid            " se non serve: space
*             lv_packmatid         " da cl_ui_stock_fields
*             lv_guidps            " GUID packspec (se lo hai, altrimenti space)
*             lv_huident      " UdM inserita
*             gs_scr7-nr_etichette
*    CHANGING lt_frhu.
*
*
*  .
  "HU
  DATA: lo_pack        TYPE REF TO /scwm/cl_wm_packing,
        ls_interval    TYPE nriv,
        lv_rangenr     TYPE inri-nrrangenr VALUE '01',
        lv_huobject    TYPE inri-object VALUE 'HU_VEKP',
        ls_huhdr_exist TYPE /scwm/s_huhdr_int,
        lv_huident     TYPE /scwm/huident.


  "CREAZIONE
  DATA: lv_lgnum       TYPE /scwm/lgnum,
        lv_delivery    TYPE /scdl/dl_docno,
        lv_hu          TYPE /scwm/de_huident,
        lv_matnr       TYPE matnr,
        lv_quantity    TYPE /scwm/de_quantity,
        lv_uom         TYPE meins,
        lv_batch       TYPE charg_d,
        lv_expiry_date TYPE /scwm/de_rf_sp_bbdat.
  DATA: lv_success  TYPE  abap_bool,
        lt_messages TYPE  bapiret2_t.

  CREATE OBJECT lo_pack.

  PERFORM check_existing_hu.

  DO gs_scr7-nr_etichette TIMES.

    CALL FUNCTION 'NUMBER_GET_INFO'
      EXPORTING
        nr_range_nr        = lv_rangenr
        object             = lv_huobject
      IMPORTING
        interval           = ls_interval
      EXCEPTIONS
        interval_not_found = 1
        object_not_found   = 2
        OTHERS             = 3.

    lv_huident = ls_interval-nrlevel.

    lo_pack->get_hu(
      EXPORTING
        iv_huident =  lv_huident      " Handling Unit Identification
        iv_lock    =  abap_true       " Single-Character Indicator
    IMPORTING
        es_huhdr   =  ls_huhdr_exist  " Internal Structure for Processing the HU Header
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2 ).

    IF sy-subrc = 0.
      MESSAGE 'HU already exsist' TYPE 'E'.
      RETURN.
    ENDIF.
**********************************************************************

    lv_lgnum        = 'DT20'.
    lv_delivery     = |{ gs_Scr5-docno ALPHA = IN }|. "'180003694'.
    lv_hu           = lv_huident. "'1000000062'.
    lv_matnr        = gs_Scr5-productno." lv_productno. "''.
    lv_quantity     = gs_Scr5-qty.
    lv_uom          = gs_Scr5-uom.
    lv_batch        = gs_scr6-batchno_ui.
    lv_expiry_date  = gs_scr6-expiry_date. "'30.07.2027'.





    CALL FUNCTION 'ZEWM_PACK_HU_INBOUND_DLV'
      EXPORTING
        iv_lgnum       = lv_lgnum
        iv_delivery    = lv_delivery
        iv_hu          = lv_hu
        iv_matnr       = lv_matnr
        iv_quantity    = lv_quantity
        iv_uom         = lv_uom
        iv_batch       = lv_batch
        iv_expiry_date = lv_expiry_date
      IMPORTING
        ev_success     = lv_success
        et_messages    = lt_messages.


  ENDDO.

ENDFUNCTION.


FORM validate_hu_creation_input .
  IF gs_scr7-nr_etichette <= 0.
    MESSAGE e021(z_msg_emduri).
    RETURN.
  ENDIF.
  IF gs_scr7-ps_id IS INITIAL.
    MESSAGE e022(z_msg_emduri).
    RETURN.
  ENDIF.
ENDFORM.



   --> GS_SCR5
*&      <-- LV_MATNR
*&---------------------------------------------------------------------*
FORM read_product_master CHANGING cv_pack_matid TYPE /scmb/mdl_matid.

  DATA: BEGIN OF ls_mat_read,
          matid          TYPE /sapapo/matid,
          matnr          TYPE matnr,
          maktx          TYPE maktx,
          ritmid         TYPE /scwm/de_itmid,
          shelf_life_dur TYPE  /sapapo/shelf_life_dur, "JQ
        END OF ls_mat_read,
        lv_matid TYPE /scmb/mdl_matid.
  CALL FUNCTION 'CONVERSION_EXIT_MDLPD_INPUT'
    EXPORTING
      input  = gs_scr5-productno
    IMPORTING
      output = cv_pack_matid.

  IF cv_pack_matid IS NOT INITIAL.
    TRY.
        CALL FUNCTION '/SCMB/MDL_PRODUCT_READ'
          EXPORTING
            iv_id   = cv_pack_matid
          IMPORTING
            es_data = ls_mat_read.


      CATCH /scmb/cx_mdl_interface
            /scmb/cx_mdl_result_empty
            /scmb/cx_mdl_result_toomany
            /scmb/cx_mdl_result.
        MESSAGE e010(z_msg_emduri).  " "ERRORE LETTURA PRODOTTO"
        RETURN.
    ENDTRY.
  ENDIF.

  IF ls_mat_read-matnr IS INITIAL.
    MESSAGE e020(z_msg_emduri).
    RETURN.
  ENDIF.


ENDFORM.


*&---------------------------------------------------------------------*
FORM check_existing_hu .

ENDFORM.


