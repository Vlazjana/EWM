FUNCTION z_ewm_rf_pick_pimtto_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_NESTED_HU) TYPE  /SCWM/TT_RF_NESTED_HU
*"     REFERENCE(WME_VERIF) TYPE  /SCWM/S_WME_VERIF
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(T_RF_PICK_HUS) TYPE  /SCWM/TT_RF_PICK_HUS
*"     REFERENCE(WHO) TYPE  /SCWM/S_WHO_INT
*"     REFERENCE(ZT_WHO_LIST) TYPE  /SCWM/TT_WHO_INT OPTIONAL
*"     REFERENCE(ZT_WHO_DISPLAY) TYPE  ZTT_RF_WHO_LIST OPTIONAL
*"     REFERENCE(ZS_WHO_DISPLAY) TYPE  ZSRF_WHO_LIST OPTIONAL
*"----------------------------------------------------------------------

  DATA: ls_mat_global       TYPE /scwm/s_material_global,
        lt_mat_uom          TYPE /scwm/tt_material_uom,
        ls_mat_uom          TYPE /scwm/s_material_uom,
        ls_ordim_confirm    TYPE /scwm/s_rf_ordim_confirm,
        ls_ordim_o          TYPE /scwm/ordim_o,
        ls_ordim_o_orig     TYPE /scwm/ordim_o,
        lv_dbatch_ind       TYPE /scwm/de_dbatchind,
        mtto_stock_fields   TYPE REF TO /scwm/cl_ui_stock_fields,
        lv_field            TYPE ddobjname,
        ls_dfies            TYPE dfies,
        lt_text             TYPE tdtab_c132,
        lt_valid_prf        TYPE /scwm/tt_valid_prf_ext,
        ls_valid_prf_pickhu TYPE /scwm/s_valid_prf_ext,
        ls_valid_prf_logpos TYPE /scwm/s_valid_prf_ext,
        lv_step             TYPE /scwm/de_step,
        lv_state            TYPE /scwm/de_state,
        lv_line             TYPE i,
        lv_fcode            TYPE /scwm/de_fcode,
        lv_pickhu_verif_req TYPE xfeld VALUE IS INITIAL,
        lv_pickhu_verif     TYPE xfeld VALUE IS INITIAL,
        lv_logpos_verif     TYPE xfeld VALUE IS INITIAL,
        ls_rf_pick_hus      TYPE /scwm/s_rf_pick_hus,
        lv_char40           TYPE /scwm/de_rf_text.
  DATA: lv_applic      TYPE /scwm/de_applic,
        lv_pres_prf    TYPE /scwm/de_pres_prf,
        lv_ltrans      TYPE /scwm/de_ltrans,
        lv_pickhu      TYPE /scwm/de_rf_pickhu,
        ls_huhdr_x     TYPE /scwm/huhdr,
        ls_who         TYPE /scwm/who,
        lt_ordim_o     TYPE /scwm/tt_ordim_o,
        lv_stock_docno TYPE /scwm/de_ui_stock_docno.

  DATA: lo_badi2 TYPE REF TO /scwm/ex_rf_pick_pickhu_det.
  DATA: cv_fcode  TYPE /scwm/de_fcode,
        lv_fcode1 TYPE /scwm/de_fcode.
  DATA: selection TYPE /scwm/s_rf_selection.

  DATA: ls_pickhu TYPE /scwm/s_huident,
        lt_pickhu TYPE /scwm/tt_huident,
        ls_t331   TYPE /scwm/t331.

  DATA: ls_mat_lgnum TYPE /scwm/s_material_lgnum,
        lv_pref_uom  TYPE /scwm/de_puom.

  DATA: lv_data_entry TYPE /scwm/de_data_entry,
        lv_stock      TYPE /scwm/de_ser_stock.

  DATA: ls_text        TYPE /scwm/s_rf_text.
  DATA: lv_is_combined TYPE boolean,
        lv_tanum       TYPE /scwm/de_rf_tanum.

  FIELD-SYMBOLS:
        <ordim_conf> TYPE /scwm/s_rf_ordim_confirm.

  DATA  lv_picker_drv_repl TYPE xfeld.

  DATA: lt_combined_lead TYPE /scwm/tt_rf_tanum,
        lv_lead_to       TYPE i.

  DATA: lv_batch        TYPE /scwm/de_charg,
        ls_selopt       TYPE  rsdsselopt,
        lr_lgpla        TYPE  rseloption,
        lr_stock_doccat TYPE  rseloption,
        lr_stock_docno  TYPE  rseloption,
        lr_stock_itmno  TYPE  rseloption,
        ls_huitm        TYPE /scwm/s_stock_select,
        lt_matid        TYPE /scwm/tt_matid,
        lt_huitm        TYPE /scwm/tt_stock_select.

  DATA: ls_disp          TYPE zsrf_who_list,
        lt_ordim_o_other TYPE /scwm/tt_ordim_o,
        ls_ordim_o_other TYPE /scwm/ordim_o,
        ls_mat_r         TYPE /scwm/s_material_global,
        lo_stock_f       TYPE REF TO /scwm/cl_ui_stock_fields,
        lv_partyno       TYPE /scdl/db_bploc-partyno,
        lv_name          TYPE but000-name_org1.

  BREAK-POINT ID /scwm/rf_picking.

  " Save all parameters globally for screen module
  gt_ordim_confirm  = tt_ordim_confirm.
  gs_ordim_confirm  = ordim_confirm.
  gs_resource_g     = resource.
  gt_t_rf_pick_hus  = t_rf_pick_hus.
  gs_wme_verif      = wme_verif.
  gt_tt_nested_hu   = tt_nested_hu.
  gt_zt_who_display = zt_who_display.
  gs_who_curr_g     = who.

  DATA lt_ordim_os TYPE /scwm/tt_ordim_os.
  CALL FUNCTION '/SCWM/TO_READ_SINGLE'
    EXPORTING
      iv_lgnum     = ordim_confirm-lgnum
      iv_tanum     = ordim_confirm-tanum
      iv_flglock   = ' '
    IMPORTING
      et_ordim_os  = lt_ordim_os
    EXCEPTIONS
      wrong_input  = 1
      not_found    = 2
      foreign_lock = 3
      error        = 4
      OTHERS       = 5.
  IF sy-subrc = 0 AND lt_ordim_os IS NOT INITIAL.
    gv_predef_sn = abap_true.
  ENDIF.

  /scwm/cl_rf_bll_srvc=>set_prmod(
                     /scwm/cl_rf_bll_srvc=>c_prmod_foreground ).

  gv_exec_step = wmegc_execstep_05.

  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).

  IF lv_data_entry = wmegc_data_entry_voice.
    gv_exec_step = wmegc_execstep_p3.
  ENDIF.

  /scwm/cl_rf_bll_srvc=>init_screen_param( ).
  /scwm/cl_rf_bll_srvc=>set_screen_param( 'TT_ORDIM_CONFIRM' ).


  gv_who_line = /scwm/cl_rf_bll_srvc=>get_line( ).
  IF gv_who_line = 0.
    gv_who_line = 1.
  ENDIF.


*  lv_fcode = /scwm/cl_rf_bll_srvc=>get_fcode( ).
*
*  lv_line = /scwm/cl_rf_bll_srvc=>get_line( ).
*  IF lv_line = 0.
*    lv_line = 1.
*    /scwm/cl_rf_bll_srvc=>set_line( lv_line ).
*  ENDIF.

   READ TABLE tt_ordim_confirm
    WITH KEY tanum = ordim_confirm-tanum
    TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    lv_line = sy-tabix.
  ELSE.
    lv_line = 1.
  ENDIF.

  READ TABLE tt_ordim_confirm INDEX lv_line INTO ordim_confirm.

  TRY.
      CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
        EXPORTING
          iv_matid      = ordim_confirm-matid
          iv_langu      = sy-langu
          iv_entitled   = ordim_confirm-entitled
          iv_lgnum      = ordim_confirm-lgnum
        IMPORTING
          es_mat_global = ls_mat_global
          es_mat_lgnum  = ls_mat_lgnum
          et_mat_uom    = lt_mat_uom.
    CATCH /scwm/cx_md_interface.
      MESSAGE e004 WITH ordim_confirm-matid.
    CATCH /scwm/cx_md_material_exist.
      MESSAGE e004 WITH ordim_confirm-matid.
    CATCH /scwm/cx_md_lgnum_locid.
      MESSAGE e004 WITH ordim_confirm-matid.
    CATCH /scwm/cx_md.
      MESSAGE e004 WITH ordim_confirm-matid.
  ENDTRY.

  IF NOT ls_mat_global-batch_req IS INITIAL.
    ordim_confirm-batch_req = gc_xfeld.
  ENDIF.

  IF ordim_confirm-flghuto = abap_false.
    lv_fcode1 = /scwm/cl_rf_comb_pick=>set_fcode_picpmt(
                         EXPORTING resource  = resource
                                   who       = who
                         CHANGING ordim_confirm    = ordim_confirm
                                  tt_ordim_confirm = tt_ordim_confirm ).
    IF lv_fcode1 IS NOT INITIAL AND ordim_confirm-uncomb = abap_false.
      /scwm/cl_rf_bll_srvc=>set_prmod(
                            /scwm/cl_rf_bll_srvc=>c_prmod_background ).
      /scwm/cl_rf_bll_srvc=>set_fcode( lv_fcode1 ).
      RETURN.
    ELSE.
      CALL METHOD /scwm/cl_rf_comb_pick=>get_is_combined
        IMPORTING
          ev_is_combined = lv_is_combined.
      IF lv_is_combined = abap_true AND
         ordim_confirm-uncomb = abap_false.
        CALL METHOD /scwm/cl_rf_comb_pick=>get_lead
          IMPORTING
            et_combined_lead = lt_combined_lead.
        DESCRIBE TABLE lt_combined_lead LINES lv_lead_to.
        READ TABLE lt_combined_lead INTO lv_tanum INDEX lv_lead_to.
        IF sy-subrc = 0.
          IF ordim_confirm-tanum = lv_tanum.
            /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on(
                                '/SCWM/S_RF_SCRELM-PGDN' ).
          ENDIF.
        ENDIF.
        CALL METHOD /scwm/cl_rf_comb_pick=>get_next_tanum
          EXPORTING
            ordim_confirm    = ordim_confirm
            tt_ordim_confirm = tt_ordim_confirm
            is_previous      = abap_false
          IMPORTING
            ev_next_tanum    = lv_tanum.
        IF lv_tanum <> ordim_confirm-tanum.
          READ TABLE tt_ordim_confirm INTO ls_ordim_confirm
            WITH KEY tanum = lv_tanum.
          IF sy-subrc = 0.
            IF ordim_confirm-vlpla = ls_ordim_confirm-vlpla.
              ordim_confirm-more = 'MTOs'.
            ELSE.
              ordim_confirm-more = ''.
            ENDIF.
          ENDIF.
        ELSE.
          ordim_confirm-more = ''.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF lv_data_entry = wmegc_data_entry_voice.
    IF ordim_confirm-cwrel = abap_true.
      MESSAGE e589.
    ENDIF.
    CALL FUNCTION '/SCWM/RF_SN_CHECK'
      EXPORTING
        iv_lgnum    = ordim_confirm-lgnum
        iv_matid    = ordim_confirm-matid
        iv_entitled = ordim_confirm-entitled
      IMPORTING
        ev_stock    = lv_stock.
    IF lv_stock = 'C'.
      MESSAGE e590.
    ENDIF.
  ENDIF.

  IF ordim_confirm-cwrel = abap_true.
    /scwm/cl_rf_bll_srvc=>set_screlm_required_off(
                        '/SCWM/S_RF_ORDIM_CONFIRM-NISTA_VERIF' ).
  ENDIF.

  CALL FUNCTION '/SCWM/TO_READ_SINGLE'
    EXPORTING
      iv_lgnum        = ordim_confirm-lgnum
      iv_tanum        = ordim_confirm-tanum
      iv_flglock      = ' '
      iv_read_from_db = 'X'
    IMPORTING
      es_ordim_o      = ls_ordim_o
    EXCEPTIONS
      wrong_input     = 1
      not_found       = 2
      foreign_lock    = 3
      error           = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    lv_dbatch_ind   = ls_ordim_o-dbind.
    ls_ordim_o_orig = ls_ordim_o.
    CLEAR ls_ordim_o.
  ENDIF.

  LOOP AT lt_mat_uom INTO ls_mat_uom
      WHERE meinh = ls_mat_global-meins.
    EXIT.
  ENDLOOP.

  ordim_confirm-p_brgew   = ls_mat_uom-brgew.
  ordim_confirm-p_gewei   = ls_mat_uom-gewei.
  ordim_confirm-p_volum   = ls_mat_uom-volum.
  ordim_confirm-p_voleh   = ls_mat_uom-voleh.
  ordim_confirm-p_laeng   = ls_mat_uom-laeng.
  ordim_confirm-p_breit   = ls_mat_uom-breit.
  ordim_confirm-p_hoehe   = ls_mat_uom-hoehe.
  ordim_confirm-p_meabm   = ls_mat_uom-meabm.
  ordim_confirm-maktx     = ls_mat_global-maktx.
  ordim_confirm-matnr     = ls_mat_global-matnr.
  ordim_confirm-vlpla_o   = ordim_confirm-vlpla.
  ordim_confirm-vlenr_o   = ordim_confirm-vlenr.
  ordim_confirm-nlpla_o   = ordim_confirm-nlpla.
  ordim_confirm-nlenr_o   = ls_ordim_o_orig-nlenr.
  ordim_confirm-srsrc_o   = ordim_confirm-srsrc.
  ordim_confirm-drsrc_o   = ordim_confirm-drsrc.
  ordim_confirm-kquan_chr = ordim_confirm-kquan.
*  ordim_confirm-vsola_chr = ordim_confirm-vsola.
  IF ordim_confirm-vltyp = 'CA01' AND gv_pallet_scanned > 0.
    ordim_confirm-vsola_chr = ordim_confirm-vsola - gv_pallet_scanned.
  ELSE.
    ordim_confirm-vsola_chr = ordim_confirm-vsola.
  ENDIF.
  ordim_confirm-cwunit    = ls_mat_global-cwunit.
  ordim_confirm-cwrel     = ls_mat_global-cwrel.

  CALL FUNCTION '/SCWM/RF_CW_IND_READ'
    EXPORTING
      iv_cwrel     = ordim_confirm-cwrel
    IMPORTING
      ev_cwrel_ind = ordim_confirm-cwrel_ind.

  IF ordim_confirm-kquan = 0.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_kquan_chr ).
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_kquan_verif ).
    /scwm/cl_rf_bll_srvc=>set_screlm_input_off( gc_scr_elmnt_kquan_verif ).
  ENDIF.

  IF ls_mat_global-batch_req IS INITIAL AND lv_dbatch_ind IS INITIAL.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_batch_vrf ).
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_rfbatch ).
    /scwm/cl_rf_bll_srvc=>set_screlm_input_off( gc_scr_elmnt_batch_vrf ).
  ELSE.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( gc_scr_elmnt_rfbatch ).
    IF ordim_confirm-batchid IS INITIAL.
      /scwm/cl_rf_bll_srvc=>set_screlm_input_on( gc_scr_elmnt_rfbatch ).
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_batch_vrf ).
      IF NOT lv_dbatch_ind IS INITIAL.
        ordim_confirm-dbind = lv_dbatch_ind.
      ENDIF.
    ENDIF.
    IF NOT ls_mat_global-batch_req IS INITIAL.
      IF NOT ordim_confirm-batchid IS INITIAL.
        IF ls_ordim_o_orig-batchid IS NOT INITIAL.
          /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( gc_scr_elmnt_batch_vrf ).
          /scwm/cl_rf_bll_srvc=>set_screlm_input_on( gc_scr_elmnt_batch_vrf ).
        ELSE.
          /scwm/cl_rf_bll_srvc=>set_screlm_input_off( gc_scr_elmnt_rfbatch ).
        ENDIF.
        ordim_confirm-batchid_o = ordim_confirm-batchid.
        IF mtto_stock_fields IS NOT BOUND.
          CREATE OBJECT mtto_stock_fields.
        ENDIF.
        mtto_stock_fields->get_batchno_by_id(
            EXPORTING iv_batchid = ordim_confirm-batchid
            RECEIVING ev_charg   = ordim_confirm-batch ).
        ordim_confirm-batch_o = ordim_confirm-batch.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ( ordim_confirm-pick_all = 2 ).
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_vsola_chr ).
  ENDIF.

  IF ls_ordim_o_orig-stock_doccat = 'SOS' AND
     ls_mat_global-batch_req IS NOT INITIAL AND
     ls_ordim_o_orig-batchid IS INITIAL.
    IF ordim_confirm-matid IS NOT INITIAL.
      APPEND ordim_confirm-matid TO lt_matid.
    ENDIF.
    ls_selopt-sign   = 'I'.
    ls_selopt-option = 'EQ'.
    IF ordim_confirm-vlpla IS NOT INITIAL.
      ls_selopt-low = ordim_confirm-vlpla.
      APPEND ls_selopt TO lr_lgpla.
    ENDIF.
    IF ordim_confirm-stock_doccat IS NOT INITIAL.
      ls_selopt-low = ordim_confirm-stock_doccat.
      APPEND ls_selopt TO lr_stock_doccat.
    ENDIF.
    IF ordim_confirm-stock_docno IS NOT INITIAL.
      ls_selopt-low = ordim_confirm-stock_docno.
      APPEND ls_selopt TO lr_stock_docno.
    ENDIF.
    IF ordim_confirm-stock_itmno IS NOT INITIAL.
      ls_selopt-low = ordim_confirm-stock_itmno.
      APPEND ls_selopt TO lr_stock_itmno.
    ENDIF.
    CALL FUNCTION '/SCWM/SELECT_STOCK'
      EXPORTING
        iv_lgnum        = ordim_confirm-lgnum
        it_matid        = lt_matid
        ir_stock_doccat = lr_stock_doccat
        ir_stock_docno  = lr_stock_docno
        ir_stock_itmno  = lr_stock_itmno
        ir_lgpla        = lr_lgpla
      IMPORTING
        et_huitm        = lt_huitm
      EXCEPTIONS
        error           = 1
        OTHERS          = 2.
    IF lines( lt_huitm ) = 1.
      READ TABLE lt_huitm INTO ls_huitm INDEX 1.
      IF mtto_stock_fields IS NOT BOUND.
        CREATE OBJECT mtto_stock_fields.
      ENDIF.
      mtto_stock_fields->get_batchno_by_id(
          EXPORTING iv_batchid = ls_huitm-batchid
          RECEIVING ev_charg   = ordim_confirm-batch ).
    ELSEIF lines( lt_huitm ) > 1.
      IF mtto_stock_fields IS NOT BOUND.
        CREATE OBJECT mtto_stock_fields.
      ENDIF.
      /scwm/cl_rf_bll_srvc=>init_listbox( '/SCWM/S_RF_ORDIM_CONFIRM-RFBATCH' ).
      LOOP AT lt_huitm INTO ls_huitm.
        mtto_stock_fields->get_batchno_by_id(
            EXPORTING iv_batchid = ls_huitm-batchid
            RECEIVING ev_charg   = lv_batch ).
        WRITE lv_batch TO lv_char40.
        /scwm/cl_rf_bll_srvc=>insert_listbox(
          iv_fieldname = '/SCWM/S_RF_ORDIM_CONFIRM-RFBATCH'
          iv_value     = lv_char40
          iv_text      = lv_char40 ).
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF ordim_confirm-batch IS NOT INITIAL.
    ordim_confirm-rfbatch = ordim_confirm-batch.
  ENDIF.

  CLEAR ordim_confirm-hazmat_ind.
  IF NOT ls_mat_global-hazmat IS INITIAL.
    CALL FUNCTION '/SCWM/RF_HAZMAT_IND_READ'
      EXPORTING
        iv_hazmat     = ls_mat_global-hazmat
      IMPORTING
        ev_hazmat_ind = ordim_confirm-hazmat_ind.
  ENDIF.

  CLEAR ordim_confirm-text_ind.
  CALL FUNCTION '/SCWM/RF_TEXT_GET_AND_SET'
    EXPORTING
      iv_lgnum        = ordim_confirm-lgnum
      iv_actty        = ordim_confirm-act_type
      iv_rdoccat      = ordim_confirm-rdoccat
      iv_rdocid       = ordim_confirm-rdocid
      iv_ritmid       = ordim_confirm-ritmid
      iv_matid        = ordim_confirm-matid
      iv_rtext        = ordim_confirm-rtext
    IMPORTING
      ev_text_ind     = ordim_confirm-text_ind
    EXCEPTIONS
      interface_error = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF ordim_confirm-text_ind IS NOT INITIAL.
      CLEAR ordim_confirm-text_scr.
      CALL METHOD /scwm/cl_rf_bll_srvc=>get_rf_text
        RECEIVING
          rt_rf_text = lt_text.
      LOOP AT lt_text INTO ls_text.
        FIND ls_text-text IN ordim_confirm-text_scr.
        IF sy-subrc = 0. CONTINUE. ENDIF.
        IF ls_text-text IS NOT INITIAL.
          IF ordim_confirm-text_scr IS INITIAL.
            ordim_confirm-text_scr = ls_text-text.
          ELSE.
            CONCATENATE ordim_confirm-text_scr ls_text-text
              INTO ordim_confirm-text_scr SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      CLEAR ordim_confirm-text_scr.
    ENDIF.
  ENDIF.

  CLEAR ordim_confirm-stock_doccat_ind.
  IF NOT ordim_confirm-stock_doccat IS INITIAL.
    CALL FUNCTION '/SCWM/RF_DOCCAT_TXT_READ'
      EXPORTING
        iv_stock_doccat      = ordim_confirm-stock_doccat
      IMPORTING
        ev_stock_doccat_text = ordim_confirm-stock_doccat_ind.
  ENDIF.

  CLEAR ordim_confirm-pick_all_ind.
  IF NOT ordim_confirm-pick_all IS INITIAL.
    MOVE '/SCWM/DE_RF_PICK_ALL_IND' TO lv_field.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = lv_field
        langu          = sy-langu
        all_types      = 'X'
      IMPORTING
        dfies_wa       = ls_dfies
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      MOVE ls_dfies-scrtext_s TO ordim_confirm-pick_all_ind.
    ENDIF.
  ENDIF.

  CALL FUNCTION '/SCWM/RF_SN_CHECK'
    EXPORTING
      iv_lgnum    = ordim_confirm-lgnum
      iv_matid    = ordim_confirm-matid
      iv_entitled = ordim_confirm-entitled
      iv_difty    = ' '
    IMPORTING
      ev_stock    = ordim_confirm-sn_type.

  MOVE-CORRESPONDING ordim_confirm TO ls_ordim_o.

  CLEAR lt_pickhu.
  LOOP AT t_rf_pick_hus INTO ls_rf_pick_hus.
    ls_pickhu-lgnum   = ordim_confirm-lgnum.
    ls_pickhu-huident = ls_rf_pick_hus-huident.
    APPEND ls_pickhu TO lt_pickhu.
  ENDLOOP.

  IF ordim_confirm-huent IS INITIAL OR lv_is_combined IS INITIAL.
    CALL FUNCTION '/SCWM/HUENT_DET'
      EXPORTING
        is_ordim_o   = ls_ordim_o
        iv_rsrc      = resource-rsrc
        it_pickhu    = lt_pickhu
        iv_desc_sort = 'X'
      IMPORTING
        ev_huent     = ordim_confirm-huent
        ev_nlenr     = ordim_confirm-pickhu
      EXCEPTIONS
        wrong_data   = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  PERFORM picker_driven_rpl_chk
    USING    ordim_confirm tt_ordim_confirm
    CHANGING lv_picker_drv_repl.

  IF lv_picker_drv_repl = 'X'.
    CLEAR: ordim_confirm-nlenr, ordim_confirm-pickhu.
    MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.
  ENDIF.

  TRY.
      GET BADI lo_badi2 FILTERS lgnum = who-lgnum.
      lv_applic   = /scwm/cl_rf_bll_srvc=>get_applic( ).
      lv_pres_prf = /scwm/cl_rf_bll_srvc=>get_pres_prf( ).
      lv_ltrans   = /scwm/cl_rf_bll_srvc=>get_ltrans( ).
      lv_step     = /scwm/cl_rf_bll_srvc=>get_step( ).
      lv_state    = /scwm/cl_rf_bll_srvc=>get_state( ).
      MOVE-CORRESPONDING who TO ls_who.
      CLEAR ls_ordim_o.
      REFRESH lt_ordim_o.
      LOOP AT tt_ordim_confirm ASSIGNING <ordim_conf>.
        MOVE-CORRESPONDING <ordim_conf> TO ls_ordim_o.
        INSERT ls_ordim_o INTO lt_ordim_o INDEX sy-tabix.
      ENDLOOP.
      lv_pickhu = ordim_confirm-pickhu.
      CALL BADI lo_badi2->propose
        EXPORTING
          iv_lgnum    = ls_who-lgnum
          iv_applic   = lv_applic
          iv_pres_prf = lv_pres_prf
          iv_ltrans   = lv_ltrans
          iv_step     = lv_step
          iv_fcode    = lv_fcode
          iv_state    = lv_state
          is_who      = ls_who
          it_ordim_o  = lt_ordim_o
          is_huhdr    = ls_huhdr_x
          it_pick_hus = t_rf_pick_hus
          iv_tanum    = ordim_confirm-tanum
          iv_pickhu   = lv_pickhu
        IMPORTING
          ev_pickhu   = lv_pickhu.
      ordim_confirm-pickhu = lv_pickhu.
    CATCH cx_badi.
  ENDTRY.

  ordim_confirm-processor = sy-uname.
  IF ordim_confirm-started_at IS INITIAL.
    GET TIME STAMP FIELD ordim_confirm-started_at.
  ENDIF.

  IF ordim_confirm-pickhu IS NOT INITIAL.
    IF NOT t_rf_pick_hus[] IS INITIAL.
      LOOP AT t_rf_pick_hus INTO ls_rf_pick_hus
        WHERE huident = ordim_confirm-pickhu.
        ordim_confirm-hupos = ls_rf_pick_hus-logpos.
        EXIT.
      ENDLOOP.
    ENDIF.
  ENDIF.

  PERFORM pbv_pimtto_pbo USING    ls_ordim_o_orig
                         CHANGING ordim_confirm.

  MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line.

  PERFORM pbv_check_for_commands_prd USING ordim_confirm
                                           tt_ordim_confirm
                                           lt_mat_uom
                                           ls_mat_global-meins.

  CALL FUNCTION '/SCWM/RF_FILL_WME_VERIF'
    EXPORTING
      iv_lgnum     = ordim_confirm-lgnum
      iv_procty    = ordim_confirm-procty
      iv_trart     = ordim_confirm-trart
      iv_act_type  = ordim_confirm-act_type
      iv_aarea     = ordim_confirm-aarea
    IMPORTING
      es_wme_verif = wme_verif.

  lv_step  = /scwm/cl_rf_bll_srvc=>get_step( ).
  lv_state = /scwm/cl_rf_bll_srvc=>get_state( ).

  CALL METHOD /scwm/cl_rf_bll_srvc=>get_valid_prf
    EXPORTING
      iv_step      = lv_step
      iv_state     = lv_state
    RECEIVING
      rt_valid_prf = lt_valid_prf.

  CLEAR ls_valid_prf_pickhu.
  READ TABLE lt_valid_prf INTO ls_valid_prf_pickhu
       WITH KEY param_name = 'TT_ORDIM_CONFIRM' valid_obj = 'PICKHU'.
  IF NOT ls_valid_prf_pickhu IS INITIAL AND
     NOT ls_valid_prf_pickhu-flg_verif IS INITIAL.
    lv_pickhu_verif = /scmb/cl_c=>boole_true.
  ENDIF.

  CLEAR ls_valid_prf_logpos.
  READ TABLE lt_valid_prf INTO ls_valid_prf_logpos
       WITH KEY param_name = 'TT_ORDIM_CONFIRM' valid_obj = 'LOGPOS'.
  IF NOT ls_valid_prf_logpos IS INITIAL AND
     NOT ls_valid_prf_logpos-flg_verif IS INITIAL.
    lv_logpos_verif = /scmb/cl_c=>boole_true.
  ENDIF.

  IF lv_logpos_verif = /scmb/cl_c=>boole_true AND
     lv_pickhu_verif = /scmb/cl_c=>boole_true.
    IF gv_postn_mngmnt IS INITIAL.
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_hupos_vrf ).
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_hupos ).
      IF ordim_confirm-pickhu_verif IS INITIAL.
        /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( gc_scr_elmnt_pickhu_vrf ).
        /scwm/cl_rf_bll_srvc=>set_screlm_input_on( gc_scr_elmnt_pickhu_vrf ).
      ENDIF.
      /scwm/cl_rf_bll_srvc=>set_fcode_off( fcode_vrfphu ).
      /scwm/cl_rf_bll_srvc=>set_fcode_on( fcode_vrflop ).
    ELSE.
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_pickhu_vrf ).
      IF ordim_confirm-hupos_verif IS INITIAL.
        /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( gc_scr_elmnt_hupos_vrf ).
        /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( gc_scr_elmnt_hupos ).
        /scwm/cl_rf_bll_srvc=>set_screlm_input_on( gc_scr_elmnt_hupos_vrf ).
      ENDIF.
      /scwm/cl_rf_bll_srvc=>set_fcode_off( fcode_vrflop ).
      /scwm/cl_rf_bll_srvc=>set_fcode_on( fcode_vrfphu ).
    ENDIF.
  ELSEIF lv_logpos_verif = /scmb/cl_c=>boole_true AND
         lv_pickhu_verif = /scmb/cl_c=>boole_false.
    IF gv_postn_mngmnt IS INITIAL.
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_hupos_vrf ).
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_hupos ).
      /scwm/cl_rf_bll_srvc=>set_fcode_off( fcode_vrfphu ).
      /scwm/cl_rf_bll_srvc=>set_fcode_on( fcode_vrflop ).
    ENDIF.
  ELSE.
    /scwm/cl_rf_bll_srvc=>set_fcode_off( fcode_vrfphu ).
    /scwm/cl_rf_bll_srvc=>set_fcode_off( fcode_vrflop ).
  ENDIF.

  CALL FUNCTION '/SCWM/RF_PICK_HU_DISPLAY_CHECK'
    IMPORTING
      es_t331       = ls_t331
    CHANGING
      ordim_confirm = ordim_confirm.

  lv_data_entry = /scwm/cl_rf_bll_srvc=>get_data_entry( ).

  IF gv_huobl = wmegc_huobl_forb.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_vlenr_vrf ).
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_vlenr ).
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_huent2 ).
  ELSEIF gv_huobl = wmegc_huobl_all OR gv_huobl = wmegc_huobl_obl.
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( gc_scr_elmnt_vlenr_vrf ).
    /scwm/cl_rf_bll_srvc=>set_screlm_input_on( gc_scr_elmnt_vlenr_vrf ).
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( gc_scr_elmnt_vlenr ).
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( gc_scr_elmnt_huent2 ).
  ENDIF.

  IF ( ( lv_data_entry = wmegc_data_entry_voice OR
         CAST /scwm/if_tm_global_info( /scwm/cl_tm_factory=>get_service(
           /scwm/cl_tm_factory=>sc_globals ) )->is_s4h_cloud( ) = abap_true )
    AND ( ordim_confirm-pickhu IS INITIAL AND
          ordim_confirm-hupos  IS INITIAL ) ).
    /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_scr_elmnt_pickhu_vrf ).
  ENDIF.

  gv_last_step = lv_step.

  /scwm/cl_rf_bll_srvc=>init_listbox( '/SCWM/S_RF_ORDIM_CONFIRM-PICKHU_VERIF' ).
  /scwm/cl_rf_bll_srvc=>init_listbox( '/SCWM/S_RF_ORDIM_CONFIRM-HUPOS_VERIF' ).

  LOOP AT t_rf_pick_hus INTO ls_rf_pick_hus.
    IF ( ( ls_rf_pick_hus-dstgrp = ordim_confirm-dstgrp OR
           ls_rf_pick_hus-dstgrp IS INITIAL ) AND
         ( ls_rf_pick_hus-huident IS NOT INITIAL ) ).
      WRITE ls_rf_pick_hus-huident TO lv_char40.
      /scwm/cl_rf_bll_srvc=>insert_listbox(
        iv_fieldname      = '/SCWM/S_RF_ORDIM_CONFIRM-PICKHU_VERIF'
        iv_value          = lv_char40
        iv_text           = lv_char40
        iv_add_dest_field = '/SCWM/S_RF_ORDIM_CONFIRM-PICKHU' ).
      IF ls_rf_pick_hus-logpos IS NOT INITIAL.
        CONCATENATE ls_rf_pick_hus-logpos lv_char40
          INTO lv_char40 SEPARATED BY space.
        /scwm/cl_rf_bll_srvc=>insert_listbox(
          iv_fieldname      = '/SCWM/S_RF_ORDIM_CONFIRM-HUPOS_VERIF'
          iv_value          = ls_rf_pick_hus-logpos
          iv_text           = lv_char40
          iv_add_dest_field = '/SCWM/S_RF_ORDIM_CONFIRM-HUPOS' ).
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM switch_fields_for_dd USING who lv_line t_rf_pick_hus
                               CHANGING tt_ordim_confirm ordim_confirm.

  /scwm/cl_rf_bll_srvc=>set_screlm_input_off( '/SCWM/S_RF_ORDIM_CONFIRM-ALTME' ).

  IF ls_mat_lgnum-puom_wh IS NOT INITIAL.
    lv_pref_uom = ls_mat_lgnum-puom_wh.
  ELSEIF ls_mat_global-puom IS NOT INITIAL.
    lv_pref_uom = ls_mat_global-puom.
  ENDIF.

  CALL FUNCTION '/SCWM/RF_SUOM_F8_LIST'
    EXPORTING
      iv_field      = '/SCWM/S_RF_ORDIM_CONFIRM-ALTME'
      iv_lgnum      = ordim_confirm-lgnum
      iv_matid      = ordim_confirm-matid
      iv_pref_uom   = lv_pref_uom
      iv_buom       = ls_mat_global-meins
      it_mat_uom    = lt_mat_uom
      iv_data_entry = lv_data_entry.



  /scwm/cl_rf_bll_srvc=>set_screen_param( 'ZT_WHO_DISPLAY' ).
  /scwm/cl_rf_bll_srvc=>set_screen_param( 'ZS_WHO_DISPLAY' ).

  CALL METHOD /scwm/cl_rf_bll_srvc=>set_scr_tabname
    EXPORTING
      iv_scr_tabname = 'ZTT_RF_WHO_LIST'.

  CALL METHOD /scwm/cl_rf_bll_srvc=>set_line
    EXPORTING
      iv_line = gv_who_index.

  LOOP AT zt_who_display ASSIGNING FIELD-SYMBOL(<ls_wl>).

    CLEAR: ls_disp, lt_ordim_o_other, ls_ordim_o_other,
           ls_mat_r, lv_partyno, lv_name.

    ls_disp-lgnum = <ls_wl>-lgnum.
    ls_disp-who   = <ls_wl>-who.

*   Read ALL WHOs from TO_READ_WHO — including current
    CLEAR: lt_ordim_o_other, ls_ordim_o_other.

    CALL FUNCTION '/SCWM/TO_READ_WHO'
      EXPORTING
        iv_lgnum   = <ls_wl>-lgnum
        iv_who     = <ls_wl>-who
        iv_flglock = ' '
      IMPORTING
        et_ordim_o = lt_ordim_o_other
      EXCEPTIONS
        OTHERS     = 0.

    READ TABLE lt_ordim_o_other INTO ls_ordim_o_other INDEX 1.

    IF sy-subrc = 0.
      ls_disp-vlpla     = ls_ordim_o_other-vlpla.
      ls_disp-vlenr     = ls_ordim_o_other-vlenr.
      ls_disp-nlpla     = ls_ordim_o_other-nlpla.
      ls_disp-vsola_chr = ls_ordim_o_other-vsola.
      ls_disp-altme     = ls_ordim_o_other-altme.
      ls_disp-pickhu    = ls_ordim_o_other-nlenr.

*     Batch
      IF ls_ordim_o_other-batchid IS NOT INITIAL.
        IF lo_stock_f IS NOT BOUND.
          CREATE OBJECT lo_stock_f.
        ENDIF.
        lo_stock_f->get_batchno_by_id(
          EXPORTING iv_batchid = ls_ordim_o_other-batchid
          RECEIVING ev_charg   = ls_disp-rfbatch ).
      ENDIF.

*     Material
      TRY.
          CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
            EXPORTING
              iv_matid      = ls_ordim_o_other-matid
              iv_langu      = sy-langu
              iv_lgnum      = <ls_wl>-lgnum
            IMPORTING
              es_mat_global = ls_mat_r.
          ls_disp-matnr = ls_mat_r-matnr.
          ls_disp-maktx = ls_mat_r-maktx.
          ls_disp-meins = ls_mat_r-meins.
        CATCH cx_root.
      ENDTRY.

*     Customer
      SELECT SINGLE b~partyno, c~name_org1
        FROM /scdl/db_proch_o AS a
        INNER JOIN /scdl/db_bploc AS b
          ON b~docid      = a~docid
         AND b~party_role = 'STPRT'
        INNER JOIN but000 AS c
          ON c~partner = b~partyno
        INTO ( @lv_partyno, @lv_name )
        WHERE a~docid = @ls_ordim_o_other-rdocid.

      ls_disp-partyno   = lv_partyno.
      ls_disp-name_org1 = lv_name.

*     Verif fields — only for current WHO from ordim_confirm
      IF <ls_wl>-who = who-who.
        ls_disp-vlenr_verif  = ordim_confirm-vlenr_verif.
        ls_disp-pickhu_verif = ordim_confirm-pickhu_verif.
        ls_disp-matid_verif  = ordim_confirm-matid_verif.

**       Decrement display for CA01
        IF ordim_confirm-vltyp = 'CA01' AND gv_pallet_scanned > 0.
          ls_disp-vsola_chr = ordim_confirm-vsola - gv_pallet_scanned.
          CLEAR ls_disp-vlenr_verif.
        ENDIF.
      ENDIF.
    ENDIF.


    MODIFY zt_who_display FROM ls_disp
      TRANSPORTING vlpla vlenr nlpla matnr maktx meins
                   vsola_chr altme pickhu rfbatch
                   vlenr_verif pickhu_verif matid_verif
                   partyno name_org1
      WHERE who = <ls_wl>-who.

  ENDLOOP.



* ordim_confirm-MATID_VERIF = ordim_confirm-MATNR.
*  MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line
*    TRANSPORTING MATID_VERIF.

  CASE ordim_confirm-vltyp.
    WHEN 'CA01'.

**     Show remaining pallets (decrement logic)
*      IF gv_pallet_scanned > 0.
*        ordim_confirm-vsola_chr = ordim_confirm-vsola - gv_pallet_scanned.
*        MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line
*          TRANSPORTING vsola_chr.
*      ENDIF.

*     Scenario 1 — Catasta
*     VLENR (UdMOr) — visible + editable (user scans SHU)
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_ON( gc_z_scr_elmnt_vlenr ).
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( gc_z_scr_elmnt_vlenr_vrf ).
      /scwm/cl_rf_bll_srvc=>set_screlm_input_on( gc_z_scr_elmnt_vlenr_vrf ).
*     PICKHU (UdMDes) — hidden
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_z_scr_elmnt_pickhu ).
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_z_scr_elmnt_pickhu_vrf ).
*     NLPLA — hidden
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_z_scr_elmnt_nlpla ).
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_z_scr_elmnt_matid_vrf ).

    WHEN 'SC01'.
*     Scenario 2 — Scorta
*     VLENR (UdMOr) — display only, verif hidden
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( gc_z_scr_elmnt_vlenr ).
      /scwm/cl_rf_bll_srvc=>set_screlm_input_off( gc_z_scr_elmnt_vlenr ).
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_z_scr_elmnt_vlenr_vrf ).
*     Auto-confirm VLENR — no scan needed
      ordim_confirm-vlenr_verif = ordim_confirm-vlenr.
      MODIFY tt_ordim_confirm FROM ordim_confirm INDEX lv_line
        TRANSPORTING vlenr_verif.
*     PICKHU (UdMDes) — visible + editable
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( gc_z_scr_elmnt_pickhu ).
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_off( gc_z_scr_elmnt_pickhu_vrf ).
      /scwm/cl_rf_bll_srvc=>set_screlm_input_on( gc_z_scr_elmnt_pickhu_vrf ).
*     NLPLA — hidden
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_z_scr_elmnt_nlpla ).
      /scwm/cl_rf_bll_srvc=>set_screlm_invisible_on( gc_z_scr_elmnt_matid_vrf ).
  ENDCASE.

* Sync back globals after processing
  gt_ordim_confirm  = tt_ordim_confirm.
  gs_ordim_confirm  = ordim_confirm.
  gt_t_rf_pick_hus  = t_rf_pick_hus.
  gs_wme_verif      = wme_verif.
  gt_zt_who_display = zt_who_display.

ENDFUNCTION.
