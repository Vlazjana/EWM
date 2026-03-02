Z_RF_EMDURI_STEP01_PAI

FUNCTION z_rf_emduri_step01_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZSRF_EMDURI) TYPE  ZSRF_EMDURI OPTIONAL
*"----------------------------------------------------------------------
CLEAR ZSRF_EMDURI.

ENDFUNCTION.
FUNCTION z_rf_emduri_step01_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ZSRF_EMDURI) TYPE  ZSRF_EMDURI OPTIONAL
*"----------------------------------------------------------------------

  " Leggi i campi dalla struttura

  PERFORM check_input_values   USING    gs_scr1.

  IF gs_scr1-gmbin IS NOT INITIAL.
    PERFORM process_banchina   USING    gs_scr1-gmbin
                               CHANGING zsrf_emduri.
  ENDIF.
ENDFUNCTION.



&---------------------------------------------------------------------*
*& -->  P1        TEXT
*& <--  P2        TEXT
*&---------------------------------------------------------------------*
FORM check_input_values USING    us_scr1 TYPE zsrf_emduri_scr1.

  " NESSUN CAMPO COMPILATO
  IF us_scr1-gmbin IS INITIAL AND us_scr1-docno IS INITIAL.
    MESSAGE e000(z_msg_emduri).
  ENDIF.

  " ENTRAMBI I CAMPI COMPILATI
  IF us_scr1-gmbin IS NOT INITIAL AND us_scr1-docno IS NOT INITIAL.
    MESSAGE e001(z_msg_emduri).
  ENDIF.

  " CHECK BANCHINA
  IF us_scr1-gmbin IS NOT INITIAL.
    SELECT SINGLE @abap_true
      INTO @DATA(lv_check)
      FROM /scdl/db_proci_i
      WHERE /scwm/gmbin = @us_scr1-gmbin.

    IF lv_check IS INITIAL.
      MESSAGE e002(z_msg_emduri).
    ENDIF.
  ENDIF.

  " CHECK FORNITURA
  IF us_scr1-docno IS NOT INITIAL.
    SELECT SINGLE @abap_true
      INTO @DATA(lv_true)
      FROM /scdl/db_proci_i
      WHERE docno = @us_scr1-docno.

    IF sy-subrc <> 0.
      MESSAGE e003(z_msg_emduri).
    ENDIF.
  ENDIF.
ENDFORM.





FORM PROCESS_BANCHINA
*&---------------------------------------------------------------------*
*& TEXT
*&---------------------------------------------------------------------*
*& -->  P1        TEXT
*& <--  P2        TEXT
*&---------------------------------------------------------------------*
FORM process_banchina  USING     uv_banchina   TYPE /scwm/dl_gmbin
                       CHANGING  cs_srf_emduri TYPE zsrf_emduri.

  TYPES: BEGIN OF zsf_inbound_detail,
           docno         TYPE /scdl/dl_docno_int,
           gmbin         TYPE /scwm/dl_gmbin,
           supplier      TYPE bu_partner,        " BUSINESS PARTNER NUMBER
           supplier_name TYPE bu_name1tx,        " BUSINESS PARTNER NAME
         END OF zsf_inbound_detail.
  .

  DATA: ls_docid           TYPE /scwm/dlv_docid_item_str,
        lt_docid           TYPE /scwm/dlv_docid_item_tab,
        ls_include_data    TYPE /scwm/dlv_query_incl_str_prd,
        lt_whr_headers     TYPE /scwm/dlv_header_out_prd_tab,
        ls_whr_headers     TYPE /scwm/dlv_header_out_prd_str,
        ls_party_loc       TYPE /scdl/dl_partyloc_str,
        lt_whr_items       TYPE /scwm/dlv_item_out_prd_tab,
        lt_return          TYPE  bapirettab,
        lt_inbound_details TYPE TABLE OF zsf_inbound_detail,
        lv_docid_temp      TYPE /scwm/de_docid,
        ls_inbound_detail  TYPE zsf_inbound_detail,
        lv_rowid           TYPE i.

  REFRESH gt_inbound_list.

  SELECT DISTINCT i~docno,
         i~doccat,
         i~docid,
         i~itemid

    INTO CORRESPONDING FIELDS OF TABLE @gt_inbound_list
    FROM /scdl/db_proci_i AS i
    INNER JOIN /scdl/db_status AS s
       ON s~docid = i~docid
    WHERE s~status_type  = 'DGR'
      AND s~status_value  <> '9'
      AND i~/scwm/gmbin = @uv_banchina.

  IF sy-subrc <> 0 .
    MESSAGE e004(z_msg_emduri).
  ENDIF.

  REFRESH: lt_docid, lt_whr_headers, lt_whr_items, lt_return,gt_supplier_list.
  LOOP AT gt_inbound_list ASSIGNING FIELD-SYMBOL(<fs_inbound_list>).

    CLEAR:   ls_docid, ls_include_data, ls_whr_headers, ls_party_loc, lv_docid_temp.

    CALL FUNCTION '/SCWM/RF_DOCNO_TO_DOCID'
      EXPORTING
        iv_docno      = <fs_inbound_list>-docno
        iv_whr_doccat = <fs_inbound_list>-doccat
      IMPORTING
        ev_rdocid     = lv_docid_temp
      EXCEPTIONS
        not_found     = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
* IMPLEMENT SUITABLE ERROR HANDLING HERE
    ENDIF.

    ls_docid-doccat = 'PDI'.
    ls_docid-docid  = lv_docid_temp.
    APPEND ls_docid TO lt_docid.

    ls_include_data-head_partyloc = abap_true.

    CALL FUNCTION '/SCWM/WHR_QUERY'
      EXPORTING
        it_docid        = lt_docid
        is_include_data = ls_include_data
      IMPORTING
        et_whr_headers  = lt_whr_headers
        et_whr_items    = lt_whr_items
        et_return       = lt_return.

    READ TABLE lt_whr_headers INTO ls_whr_headers
      WITH KEY docid = lv_docid_temp doccat = 'PDI'.
    IF sy-subrc = 0.
      READ TABLE ls_whr_headers-partyloc INTO ls_party_loc
        WITH KEY party_role = 'SFPRT'.

      <fs_inbound_list>-supplier = ls_party_loc-partyno.

      ls_inbound_detail-docno = lv_docid_temp.
      ls_inbound_detail-supplier  = ls_party_loc-partyno.

      IF ls_party_loc IS NOT INITIAL.
        SELECT SINGLE name1
        FROM lfa1
        INTO ls_inbound_detail-supplier_name
        WHERE  lifnr = ls_party_loc-partyno.
      ENDIF.

      APPEND ls_inbound_detail TO lt_inbound_details.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_inbound_details ASSIGNING FIELD-SYMBOL(<fg_inbound_details>) GROUP BY <fg_inbound_details>-supplier.

    lv_rowid = lv_rowid + 10.
    APPEND VALUE #( selno   = lv_rowid
                    partyno = <fg_inbound_details>-supplier
                    name1   = <fg_inbound_details>-supplier_name ) TO gt_supplier_list.

  ENDLOOP.


  cs_srf_emduri-inbound_list  = gt_inbound_list.
  gt_scr2                     = gt_supplier_list.
  cs_srf_emduri-supplier_list = gt_supplier_list.

ENDFORM.



