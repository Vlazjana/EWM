FUNCTION zewm_pack_hu_inbound_dlv.
*"----------------------------------------------------------------------
*"*"Interfaccia locale:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     REFERENCE(IV_DELIVERY) TYPE  /SCDL/DL_DOCNO
*"     REFERENCE(IV_HU) TYPE  /SCWM/DE_HUIDENT
*"     REFERENCE(IV_MATNR) TYPE  MATNR
*"     REFERENCE(IV_QUANTITY) TYPE  /SCWM/DE_QUANTITY
*"     REFERENCE(IV_UOM) TYPE  MEINS
*"     REFERENCE(IV_BATCH) TYPE  CHARG_D OPTIONAL
*"     REFERENCE(IV_EXPIRY_DATE) TYPE  /SCWM/DE_RF_SP_BBDAT OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_SUCCESS) TYPE  ABAP_BOOL
*"     REFERENCE(ET_MESSAGES) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------


  DEFINE m_sy_msg_and_return.
    APPEND VALUE #(
      type       = sy-msgty
      id         = sy-msgid
      number     = sy-msgno
      message_v1 = sy-msgv1
      message_v2 = sy-msgv2
      message_v3 = sy-msgv3
      message_v4 = sy-msgv4 ) TO et_messages.
    ev_success = abap_false.
    RETURN.
  END-OF-DEFINITION.

  DEFINE m_msg_and_return.

    APPEND VALUE #(
      type       = &1
      id         = &2
      number     = &3
      message_v1 = &4
      message_v2 = &5
      message_v3 = &6
      message_v4 = &7 ) TO et_messages.
    ev_success = abap_false.
    RETURN.
  END-OF-DEFINITION.


  DATA: cs_rehu      TYPE /scwm/s_rf_admin_rehu,
        cs_rehu_hu   TYPE /scwm/s_rf_rehu_hu,
        ct_rehu_hu   TYPE /scwm/tt_rf_rehu_hu,
        cs_rehu_prod TYPE /scwm/s_rf_rehu_prod,
        ct_rehu_prod TYPE /scwm/tt_rf_rehu_prod,
        cs_rehu_dlv  TYPE /scwm/s_rf_unlo_docid,
        ct_rehu_dlv  TYPE /scwm/tt_rf_unlo_docid.


  DATA: lt_dlv_sel      TYPE /scwm/dlv_selection_tab,
        ls_dlv_sel      TYPE /scwm/dlv_selection_str,
        ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
        ls_read_options TYPE /scwm/dlv_query_contr_str,
        lt_prd_hdr      TYPE /scwm/dlv_header_out_prd_tab,
        lt_items        TYPE /scwm/dlv_item_out_prd_tab,
        ls_status       TYPE /scdl/dl_status_str,
        lt_huhdr        TYPE /scwm/tt_huhdr_int,
        lt_huref        TYPE /scwm/tt_huref_int,
        lt_hu_items     TYPE /scwm/tt_huitm_int,
        ls_huref        TYPE /scwm/s_huref_int,
        lt_docid        TYPE /scwm/tt_docid,
        ls_docid        TYPE /scwm/s_docid,
        lv_docid        TYPE /scdl/dl_docid,
        ls_id           TYPE /scmb/mdl_prtid_str,
        lt_id           TYPE /scmb/mdl_prt_id_tab,
        ls_partner      TYPE /scmb/mdl_partner,
        lt_partner      TYPE STANDARD TABLE OF /scmb/mdl_partner,
        ls_partyloc     TYPE /scdl/dl_partyloc_str,
        ls_deliveries   TYPE /scwm/s_rf_unlo_docid.

  FIELD-SYMBOLS: <fs_prd_hdr>  TYPE /scwm/dlv_header_out_prd_str,
                 <fs_delivery> TYPE /scwm/s_rf_unlo_docid,
                 <fs_items>    TYPE /scwm/dlv_item_out_prd_str.

  DATA lo_dlv TYPE REF TO /scwm/cl_dlv_management_prd.


  DATA: ls_huhdr                   TYPE /scwm/s_huhdr_int,
        lt_huitm                   TYPE /scwm/tt_huitm_int,
        ls_hu_guids                TYPE /scwm/s_guid_hu,
        lt_hu_guids                TYPE /scwm/tt_guid_hu,
        lo_prd2hum                 TYPE REF TO /scwm/cl_dlv_prd2hum,
        lv_exist_in_other_delivery TYPE xfeld.


  DATA: lt_open_items           TYPE /scwm/dlv_hu_prd_tab,
        ls_items_step3          TYPE /scwm/dlv_hu_prd_str,
        lt_docid_step3          TYPE /scwm/dlv_docid_item_tab,
        ls_docid_step3          TYPE /scwm/dlv_docid_item_str,
        ls_rehu_dlv             TYPE /scwm/s_rf_unlo_docid,
        lt_dlvs                 TYPE /scwm/dlv_docid_item_tab,
        ls_dlvs                 TYPE /scwm/dlv_docid_item_str,
        ls_prd_hdr_s3           TYPE /scwm/dlv_header_out_prd_tab,
        lt_items_s3             TYPE /scwm/dlv_item_out_prd_tab,
        lv_failed               TYPE boole_d,
        lv_ean_gtin             TYPE xfeld,
        ls_mat_global           TYPE /scwm/s_material_global,
        ls_mat_lgnum            TYPE /scwm/s_material_lgnum,
        lt_mat_uom              TYPE /scwm/tt_material_uom,
        lt_mat_plant            TYPE /scwm/tt_material_base_plant,
        lt_batch_attr           TYPE /scwm/tt_batch_attr,
        lo_log                  TYPE REF TO /scwm/cl_log,
        lv_bbd_req              TYPE xfeld,
        lv_new_batch            TYPE xfeld,
        lv_batch_crea           TYPE /scwm/dl_batchcrea,
        lo_badi_crea            TYPE REF TO /scwm/ex_dlv_batch_crea,
        lo_item_step3           TYPE REF TO /scdl/cl_dl_item_write,
        lo_bo_step3             TYPE REF TO /scdl/if_bo,
        lo_bom_step3            TYPE REF TO /scdl/cl_bo_management,
        ls_product_step3        TYPE /scdl/dl_product_str,
        ls_sapext_step3         TYPE /scdl/dl_sap_dr_item_str,
        lv_badi_imp(30)         TYPE c,
        lv_entitled             TYPE /scwm/de_entitled,
        lo_md_access            TYPE REF TO /scwm/cl_dlv_md_access,
        ls_batch_md             TYPE /scwm/dlv_md_prod_batch_det,
        lv_batch_dl             TYPE /scdl/dl_batchno,
        lv_segment_managed_prod TYPE abap_bool VALUE abap_false,
        lv_sn_type              TYPE /scwm/de_rf_sn_type,
        lv_buom                 TYPE meins,
        lv_puom                 TYPE /scwm/de_puom,
        lv_quan_test            TYPE /scwm/de_quantity,
        lv_quantiy              TYPE /scwm/de_quantity,
        lf_hu_exist             TYPE boole.


  DATA: ls_packing_rule    TYPE /scwm/s_upb_pack_rule,
        ls_levels          TYPE /scwm/s_ps_level_int,
        ls_item_packaged   TYPE /scwm/s_ps_autopack,
        ls_top_hu_quan_upb TYPE /scwm/if_autopack_upb_util=>ys_huheader_result_quan,
        lt_huhdr_upb       TYPE /scwm/if_packing_upb=>yt_huheader_result,
        lt_huitm_ubp       TYPE /scwm/if_packing_upb=>yt_huitem_result,
        lt_huauxpmat_upb   TYPE /scwm/if_packing_upb=>yt_huauxpmat_result,
        lt_hutree_upb      TYPE /scwm/if_packing_upb=>yt_hutree_result.

  DATA: lv_msg_text TYPE string.


  ev_success = abap_true.
  CLEAR et_messages.


  CLEAR: gv_mass_hu_create, gv_multi_hu_upb.


  cs_rehu-lgnum = iv_lgnum.


  cs_rehu_hu-dlvno       = iv_delivery.
  cs_rehu_hu-vlenr_verif = iv_hu.


  cs_rehu_prod-matnr_verif = iv_matnr.
  cs_rehu_prod-nista_verif = iv_quantity.
  cs_rehu_prod-altme     = iv_uom.
  cs_rehu_prod-charg_verif = iv_batch.
  cs_rehu_prod-bbdat    = iv_expiry_date.


  cs_rehu-doccat    = wmegc_doccat_pdi.
  cs_rehu-j_vorgang = wmegc_btx_hul1.

  CLEAR: cs_rehu-deliveries,
         cs_rehu-hus,
         cs_rehu-huitms,
         ct_rehu_hu,
         cs_rehu-sumhu,
         cs_rehu-sumhu_proc.


  IF cs_rehu_hu-dlvno IS INITIAL.
    MESSAGE e013(/scwm/rf_de) WITH cs_rehu_hu-dlvno INTO lv_msg_text.
    m_sy_msg_and_return.
  ENDIF.


  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.


  ls_dlv_sel-fieldname = gc_dl_logfname_whno_i.
  ls_dlv_sel-option    = wmegc_option_eq.
  ls_dlv_sel-low       = cs_rehu-lgnum.
  CLEAR ls_dlv_sel-high.
  APPEND ls_dlv_sel TO lt_dlv_sel.

  ls_dlv_sel-fieldname = gc_dl_logfname_docno.
  ls_dlv_sel-option    = wmegc_option_eq.
  ls_dlv_sel-low       = cs_rehu_hu-dlvno.
  CLEAR ls_dlv_sel-high.
  APPEND ls_dlv_sel TO lt_dlv_sel.

  ls_dlv_sel-fieldname = /scdl/if_dl_logfname_c=>sc_status_value_dco_i.
  ls_dlv_sel-option    = wmegc_option_eq.
  ls_dlv_sel-low       = /scdl/if_dl_status_c=>sc_v_not_started.
  CLEAR ls_dlv_sel-high.
  APPEND ls_dlv_sel TO lt_dlv_sel.

  ls_dlv_sel-fieldname = gc_dl_logfname_value_dbc_h.
  ls_dlv_sel-option    = wmegc_option_eq.
  ls_dlv_sel-low       = gc_dl_status_value_unblocked.
  CLEAR ls_dlv_sel-high.
  APPEND ls_dlv_sel TO lt_dlv_sel.


  ls_read_options-data_retrival_only = 'X'.
  ls_read_options-mdctrl-text        = 'X'.
  ls_include_data-item_status        = 'X'.
  ls_include_data-item_partyloc      = 'X'.
  ls_include_data-head_status        = 'X'.
  ls_include_data-head_status_dyn    = 'X'.
  ls_include_data-head_partyloc      = 'X'.
  ls_include_data-head_transport     = 'X'.
  ls_include_data-head_date          = 'X'.
  APPEND /scdl/if_dl_status_c=>sc_t_completed TO ls_include_data-item_status_dyn_detail.

  DATA: ls_docno TYPE /scwm/dlv_docno_itemno_str,
        lt_docno TYPE /scwm/dlv_docno_itemno_tab.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = iv_delivery
    IMPORTING
      output = ls_docno-docno.
  APPEND ls_docno TO lt_docno.
  TRY.
      lo_dlv->query(
        EXPORTING
*    it_docid                    =
          it_docno                    = lt_docno
          iv_whno                     = iv_lgnum
*    it_selection                =
*    iv_doccat                   =
*    is_read_options             =
*    is_exclude_data             =
*    is_include_data             =
*    is_exclude_parameter        =
        IMPORTING
*    et_hu_top                   =
*    et_hu_prd                   =
*    et_hu_headers               =
*    et_hu_items                 =
*    et_hu_ref                   =
          et_headers                  = lt_prd_hdr
          et_items                    = lt_items
*    et_items_to                 =
*    eo_message                  =
*    et_od_headers               =
*    et_doccat_texts             =
*    et_doctype_texts            =
*    et_itemtype_texts           =
*    et_itemcat_texts            =
*    et_inco1_texts              =
*    et_srvlvl_details           =
*    et_refcat_details           =
*    et_party_role_details       =
*    ev_parallel_processing_done =
      ).
    CATCH /scdl/cx_delivery. " For New Exceptions Use /SCDL/CX_DELIVERY_T100
  ENDTRY.


****  TRY.
****      CALL METHOD lo_dlv->query
****        EXPORTING
****          it_selection    = lt_dlv_sel
****          iv_doccat       = cs_rehu-doccat
****          is_read_options = ls_read_options
****          is_include_data = ls_include_data
****        IMPORTING
****          et_headers      = lt_prd_hdr
****          et_items        = lt_items.
****    CATCH /scdl/cx_delivery.
****      m_sy_msg_and_return.
****  ENDTRY.
****  IF sy-subrc <> 0.
****    m_sy_msg_and_return.
****  ENDIF.

  IF lt_prd_hdr IS INITIAL.
    MESSAGE e424(/scwm/rf_en) WITH cs_rehu_hu-dlvno INTO lv_msg_text.
    m_sy_msg_and_return.
  ENDIF.

  " Delete deliveries locked by NCTS
  LOOP AT lt_prd_hdr ASSIGNING <fs_prd_hdr>.
    READ TABLE <fs_prd_hdr>-status
         WITH KEY status_type = /scwm/if_dl_status_c=>sc_t_ncts
         INTO ls_status.
    IF sy-subrc = 0
       AND ls_status-status_value <> /scwm/if_dl_status_c=>sc_v_ncts_not_relevant
       AND ls_status-status_value <> /scwm/if_dl_status_c=>sc_v_ncts_released.
      lv_docid = <fs_prd_hdr>-docid.
      DELETE lt_prd_hdr WHERE docid = lv_docid.
      DELETE lt_items    WHERE docid = lv_docid.
    ENDIF.
  ENDLOOP.

  IF lt_prd_hdr IS INITIAL AND lt_items IS INITIAL.
    MESSAGE e332(/scwm/rf_de) INTO lv_msg_text.
    m_sy_msg_and_return.
  ENDIF.


  LOOP AT lt_prd_hdr ASSIGNING <fs_prd_hdr>.
    ls_docid-docid = <fs_prd_hdr>-docid.
    APPEND ls_docid TO lt_docid.
  ENDLOOP.


  CALL FUNCTION '/SCWM/DLV_GET_HUS_FOR_DELIVERY'
    EXPORTING
      iv_doccat = /scdl/if_dl_doc_c=>sc_doccat_inb_prd
      it_docid  = lt_docid
    IMPORTING
      et_huref  = lt_huref
      et_huhdr  = lt_huhdr
      et_huitm  = lt_hu_items
    EXCEPTIONS
      error     = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
    m_sy_msg_and_return.
  ENDIF.


  CLEAR: ls_id, lt_id, ls_partner, lt_partner.
  LOOP AT lt_prd_hdr ASSIGNING <fs_prd_hdr>.
    READ TABLE <fs_prd_hdr>-partyloc INTO ls_partyloc
      WITH KEY party_role = 'SFPRT'.
    IF sy-subrc = 0.
      ls_id-partner_guid = ls_partyloc-partyid.
      APPEND ls_id TO lt_id.
    ENDIF.
  ENDLOOP.
  IF lt_id IS NOT INITIAL.
    SORT lt_id.
    DELETE ADJACENT DUPLICATES FROM lt_id.
    TRY.
        CALL FUNCTION '/SCMB/MDL_PARTNER_READ_MULTI'
          EXPORTING
            it_id   = lt_id
          IMPORTING
            et_data = lt_partner.
      CATCH /scmb/cx_mdl_interface
            /scmb/cx_mdl_result_empty
            /scmb/cx_mdl_result_toomany
            /scmb/cx_mdl_result
            /scmb/cx_mdl.                               "#EC NO_HANDLER
    ENDTRY.
  ENDIF.


  LOOP AT lt_prd_hdr ASSIGNING <fs_prd_hdr>.
    ls_deliveries-dlvno   = cs_rehu_hu-dlvno.
    ls_deliveries-docid   = <fs_prd_hdr>-docid.
    ls_deliveries-doccat  = <fs_prd_hdr>-doccat.
    ls_deliveries-doctype = <fs_prd_hdr>-doctype.


    READ TABLE <fs_prd_hdr>-transport TRANSPORTING NO FIELDS
      WITH KEY transpl_type = gc_transpl_type_advsr.
    IF sy-subrc = 0.
      ls_deliveries-advsr = gc_xfeld.
      READ TABLE <fs_prd_hdr>-partyloc INTO ls_partyloc WITH KEY party_role = 'UP'.
      IF sy-subrc = 0.
        ls_deliveries-unl_point = ls_partyloc-partyno.
      ENDIF.
    ENDIF.


    READ TABLE <fs_prd_hdr>-partyloc INTO ls_partyloc WITH KEY party_role = 'SFPRT'.
    IF sy-subrc = 0.
      READ TABLE lt_partner INTO ls_partner WITH KEY partner_guid = ls_partyloc-partyid.
      IF sy-subrc = 0.
        ls_deliveries-partyno   = ls_partner-partner.
        ls_deliveries-partytext = ls_partner-name.
      ENDIF.
    ENDIF.


    DATA: lv_tzone TYPE /scdl/dl_tzone,
          lv_date  TYPE /scwm/sp_credat,
          lv_subrc LIKE sy-subrc.
    READ TABLE <fs_prd_hdr>-dates INTO DATA(ls_planned_dlv_date)
      WITH KEY tsttype = 'TDELIVERY' tst_category = 'PLAN'.
    IF sy-subrc = 0.
      CONVERT TIME STAMP ls_planned_dlv_date-tstfr
              TIME ZONE ls_planned_dlv_date-tzone
              INTO DATE lv_date.
      lv_subrc = sy-subrc.
      IF lv_date < '99991231'.
        CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
          EXPORTING
            iv_lgnum = cs_rehu-lgnum
          IMPORTING
            ev_tzone = lv_tzone.
        CONVERT TIME STAMP ls_planned_dlv_date-tstfr
                TIME ZONE lv_tzone
                INTO DATE lv_date TIME DATA(lv_time).
        IF lv_subrc <> 0.
          MESSAGE e002(/scwm/delivery_ui) INTO lv_msg_text.
          m_sy_msg_and_return.
        ENDIF.
        ls_deliveries-dlv_date_plan = lv_date.
        ls_deliveries-dlv_time_plan = lv_time.
      ENDIF.
    ENDIF.

    APPEND ls_deliveries TO cs_rehu-deliveries.
    MODIFY cs_rehu-deliveries FROM ls_deliveries
      TRANSPORTING partyno partytext
      WHERE docid = <fs_prd_hdr>-docid.
  ENDLOOP.

  SORT cs_rehu-deliveries BY docid.
  DELETE ADJACENT DUPLICATES FROM cs_rehu-deliveries COMPARING docid.

  IF lines( lt_huhdr ) = 0 AND lines( lt_items ) = 0.
    MESSAGE e412(/scwm/rf_en) WITH cs_rehu_hu-dlvno INTO lv_msg_text.
    m_sy_msg_and_return.
  ENDIF.


  PERFORM prepare_hus
    USING    lt_huhdr
             lt_huref
    CHANGING cs_rehu
             cs_rehu_hu.

  SORT cs_rehu-hus BY huident.
  cs_rehu-huitms = lt_hu_items.
  cs_rehu-itms   = lt_items.

  PERFORM hu_admin_table_update
    USING    lt_prd_hdr
             lt_items
    CHANGING cs_rehu
             cs_rehu_hu
             ct_rehu_hu.


  LOOP AT lt_huref INTO ls_huref.
    READ TABLE cs_rehu-deliveries ASSIGNING <fs_delivery>
         WITH KEY docid  = ls_huref-docid
                  doccat = ls_huref-doccat.
    IF sy-subrc = 0.
      ADD 1 TO <fs_delivery>-sumhu.
      ADD 1 TO cs_rehu-sumhu.
    ENDIF.
  ENDLOOP.

***************************************************************************************
  cs_rehu_hu-vlenr_verif = iv_hu.


  CALL FUNCTION 'CONVERSION_EXIT_HUID_INPUT'
    EXPORTING
      input  = cs_rehu_hu-vlenr_verif
    IMPORTING
      output = cs_rehu_hu-huident.


  READ TABLE cs_rehu-hus WITH KEY huident = cs_rehu_hu-huident
                         TRANSPORTING NO FIELDS.

  IF sy-subrc = 0.

    READ TABLE cs_rehu-hus INTO cs_rehu_hu
                           WITH KEY huident = cs_rehu_hu-huident.
    IF sy-subrc <> 0.
      MESSAGE e415(/scwm/rf_en) INTO lv_msg_text.
      m_sy_msg_and_return.
    ENDIF.


    IF lo_prd2hum IS NOT BOUND.
      CREATE OBJECT lo_prd2hum.
    ENDIF.
    TRY.
        CALL METHOD lo_prd2hum->check_cross_hu
          EXPORTING
            iv_guid_hu  = cs_rehu_hu-guid_hu
          IMPORTING
            ev_cross_hu = cs_rehu_hu-cross_hu.
      CATCH /scdl/cx_delivery.                          "#EC NO_HANDLER
    ENDTRY.


    ls_hu_guids-guid_hu = cs_rehu_hu-guid_hu.
    APPEND ls_hu_guids TO lt_hu_guids.
    CALL FUNCTION '/SCWM/HUMAIN_REFRESH'
      EXPORTING
        it_hu_guids = lt_hu_guids.


    PERFORM hu_read_and_lock
      USING    cs_rehu_hu-huident
               cs_rehu-lgnum
               cs_rehu-deliveries
               abap_false
      CHANGING ls_huhdr
               lt_huitm
               cs_rehu_hu-hazmat_ind
               cs_rehu_hu-rdoccat
               cs_rehu_hu-rdocid.

    CLEAR: ct_rehu_prod, cs_rehu_prod.
    PERFORM fill_prod_data_from_huitem
      USING    cs_rehu-lgnum
               lt_huitm
               cs_rehu_hu-huident
      CHANGING cs_rehu_prod
               ct_rehu_prod.


    cs_rehu_prod-matnr_verif = iv_matnr.
    cs_rehu_prod-nista_verif = iv_quantity.
    cs_rehu_prod-altme       = iv_uom.
    cs_rehu_prod-charg_verif = iv_batch.
    cs_rehu_prod-bbdat       = iv_expiry_date.

  ELSE.
    "nuova HU da creare
    IF cs_rehu_hu-huident IS INITIAL.
      MESSAGE e415(/scwm/rf_en) INTO lv_msg_text.
      m_sy_msg_and_return.
    ENDIF.


    PERFORM check_for_other_delivery
      USING    cs_rehu_hu-huident
               cs_rehu-lgnum
      CHANGING lv_exist_in_other_delivery.

    IF lv_exist_in_other_delivery IS NOT INITIAL.
      MESSAGE e430(/scwm/rf_en) WITH cs_rehu_hu-huident INTO lv_msg_text.
      m_sy_msg_and_return.
    ENDIF.


    PERFORM hu_number_range_check
      USING cs_rehu_hu
            cs_rehu-lgnum.
  ENDIF.


  cs_rehu_prod-matnr = cs_rehu_prod-matnr_verif.

  CALL FUNCTION 'CONVERSION_EXIT_RFBA_INPUT'
    EXPORTING
      input  = cs_rehu_prod-charg_verif
    IMPORTING
      output = cs_rehu_prod-charg.

  cs_rehu_prod-nista = cs_rehu_prod-nista_verif.


  IF cs_rehu_prod-matnr IS INITIAL.
    MESSAGE e002(/scwm/md) INTO lv_msg_text.
    m_sy_msg_and_return.
  ENDIF.


  PERFORM matid_read_from_matnr
    USING    cs_rehu_prod-matnr
    CHANGING cs_rehu_prod-matid
             lv_ean_gtin.
  IF lv_ean_gtin IS NOT INITIAL.
    CLEAR cs_rehu_prod-altme.
  ENDIF.

  IF cs_rehu_prod-matid IS INITIAL.
    CLEAR cs_rehu_prod-matnr_verif.
    MESSAGE e002(/scwm/md) INTO lv_msg_text.
    m_sy_msg_and_return.
  ENDIF.


  READ TABLE cs_rehu-itms INTO DATA(ls_item_s3) INDEX 1.
  IF sy-subrc = 0.
    lv_entitled = ls_item_s3-sapext-entitled.
  ENDIF.

  TRY.
      CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
        EXPORTING
          iv_matid      = cs_rehu_prod-matid
          iv_lgnum      = cs_rehu-lgnum
          iv_entitled   = lv_entitled
        IMPORTING
          es_mat_global = ls_mat_global
          es_mat_lgnum  = ls_mat_lgnum
          et_mat_uom    = lt_mat_uom
          et_mat_plant  = lt_mat_plant.
    CATCH /scwm/cx_md.
      m_sy_msg_and_return.
  ENDTRY.

  cs_rehu_prod-batch_req = ls_mat_global-batch_req.


  IF ls_mat_lgnum-docbatch IS NOT INITIAL.
    DATA lv_dbatch_rel TYPE /scwm/dl_dbatch_rel.
    DATA lv_docu_batch TYPE xfeld.
    READ TABLE cs_rehu-itms INTO DATA(ls_item_bc_s3) INDEX 1.
    TRY.
        CALL METHOD /scwm/cl_dlv_batch=>check_item_dbatch
          EXPORTING
            iv_itemtype   = ls_item_bc_s3-itemtype
            iv_doccat     = ls_item_bc_s3-doccat
          RECEIVING
            rv_dbatch_rel = lv_dbatch_rel.
      CATCH /scwm/cx_dlv_conf.
        MESSAGE x049(/scwm/ui_packing) INTO lv_msg_text.
        m_sy_msg_and_return.
    ENDTRY.
    IF lv_dbatch_rel IS NOT INITIAL.
      lv_docu_batch = abap_true.
    ENDIF.
  ENDIF.


  IF cs_rehu_prod-batch_req IS NOT INITIAL.
    TRY.
        CALL METHOD /scwm/cl_batch_appl=>get_batch_attr_by_product
          EXPORTING
            iv_lgnum       = cs_rehu-lgnum
            iv_entitled    = lv_entitled
            iv_productid16 = cs_rehu_prod-matid
          IMPORTING
            et_batch_attr  = lt_batch_attr
            eo_log         = lo_log.
      CATCH /scwm/cx_batch_management.                  "#EC NO_HANDLER
    ENDTRY.
    READ TABLE lt_batch_attr
       WITH KEY characteristic = 'LOBM_VFDAT' TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      lv_bbd_req = 'X'.
    ENDIF.
  ENDIF.


  cs_rehu_prod-maktx = ls_mat_global-maktx.


  IF cs_rehu_prod-altme IS INITIAL.
    DATA ls_dlv_itms_s3 TYPE /scwm/dlv_item_out_prd_str.
    DATA ls_dlv_s3      TYPE /scwm/s_rf_unlo_docid.
    DATA ls_item_bc2    TYPE /scdl/dl_itype_detail_str.

    READ TABLE cs_rehu-itms INTO ls_dlv_itms_s3
      WITH KEY product-productid = ls_mat_global-matid.
    READ TABLE cs_rehu-deliveries INTO ls_dlv_s3
      WITH KEY docid = ls_dlv_itms_s3-docid.

    PERFORM get_item_bc_by_type
      USING    ls_dlv_itms_s3-itemtype
               ls_dlv_s3-doccat
      CHANGING ls_item_bc2.

    IF ls_item_bc2-use_alt_uom IS NOT INITIAL.
      cs_rehu_prod-altme = ls_mat_global-meins.
    ELSE.
      cs_rehu_prod-altme = ls_mat_global-meins.
    ENDIF.
  ENDIF.


  IF cs_rehu_prod-altme IS NOT INITIAL.
    READ TABLE lt_mat_uom
      WITH KEY meinh = cs_rehu_prod-altme
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      MESSAGE e700(/scwm/l1) WITH cs_rehu_prod-altme INTO lv_msg_text.
      m_sy_msg_and_return.
    ENDIF.
  ENDIF.


  lv_quan_test = cs_rehu_prod-nista.
  CALL FUNCTION '/SCWM/MATERIAL_SUOM_RELEVANT'
    EXPORTING
      iv_lgnum   = cs_rehu-lgnum
      iv_matid   = cs_rehu_prod-matid
    IMPORTING
      ev_buom    = lv_buom
      ev_puom    = lv_puom
      et_mat_uom = lt_mat_uom.
  IF lines( lt_mat_uom ) > 1 AND
     lv_buom <> cs_rehu_prod-altme.
    lv_quan_test = frac( lv_quan_test ).
    IF lv_quan_test IS NOT INITIAL.
      MESSAGE e604(/scwm/rf_en) INTO lv_msg_text.
      m_sy_msg_and_return.
    ENDIF.
  ENDIF.

  " batch obbligatorio
  IF cs_rehu_prod-batch_req IS NOT INITIAL OR
     lv_docu_batch IS NOT INITIAL.
    IF cs_rehu_prod-charg IS INITIAL.
      MESSAGE e395(/scwm/rf_en) WITH cs_rehu_prod-matnr INTO lv_msg_text.
      m_sy_msg_and_return.
    ELSEIF cs_rehu_prod-batch_req IS NOT INITIAL.

      lv_batch_dl = cs_rehu_prod-charg.
      IF lo_md_access IS NOT BOUND.
        lo_md_access = /scwm/cl_dlv_md_access=>get_instance( ).
      ENDIF.
      TRY.
          IF abap_true = lo_md_access->get_batch_existence(
                 iv_productid = cs_rehu_prod-matid
                 iv_batchno   = lv_batch_dl
                 iv_lgnum     = cs_rehu-lgnum
                 iv_entitled  = lv_entitled ).
            ls_batch_md = lo_md_access->get_batch_detail(
              iv_productno         = ls_mat_global-matnr
              iv_batchno           = lv_batch_dl
              iv_lgnum             = cs_rehu-lgnum
              iv_entitled          = lv_entitled
              iv_no_classification = abap_false ).
            IF ls_batch_md-sled_bbd IS NOT INITIAL.
              cs_rehu_prod-bbdat = ls_batch_md-sled_bbd.
            ENDIF.
          ELSE.
            " flag creazione nuovo batch
            lv_new_batch = 'X'.


            lo_bom_step3 = /scdl/cl_bo_management=>get_instance( ).
            DATA: lv_docid_new TYPE /scdl/dl_docid.
            READ TABLE cs_rehu-deliveries INTO DATA(ls_dlv_new) INDEX 1.
            lv_docid_new = ls_dlv_new-docid.
            lo_bo_step3  = lo_bom_step3->get_bo_by_id( iv_docid = lv_docid_new ).

            READ TABLE cs_rehu-itms INTO DATA(ls_itm_bc)
              WITH KEY product-productid = cs_rehu_prod-matid.
            IF sy-subrc = 0.
              lo_item_step3 ?= lo_bo_step3->get_item( iv_itemid = ls_itm_bc-itemid ).
              ls_product_step3 = lo_item_step3->get_product( ).
              ls_sapext_step3  = /scwm/cl_dlv_batch_internal=>item_get_sapext( lo_item_step3 ).
              GET BADI lo_badi_crea
                FILTERS
                  lgnum = cs_rehu-lgnum.
              CALL BADI lo_badi_crea->check_batch_create
                EXPORTING
                  iv_lgnum      = cs_rehu-lgnum
                  iv_doccat     = lo_item_step3->mv_doccat
                  iv_doc_type   = lo_item_step3->mv_doctype
                  iv_itemtype   = lo_item_step3->mv_itemtype
                  iv_itemcat    = lo_item_step3->mv_itemcat
                  iv_productid  = ls_product_step3-productid
                  iv_entitled   = ls_sapext_step3-entitled
                  iv_docid      = lo_item_step3->mv_docid
                  iv_itemid     = lo_item_step3->mv_itemid
                IMPORTING
                  ev_batch_crea = lv_batch_crea.
              IF lv_batch_crea = /scwm/if_dlv_batch_c=>sc_crea_no.
                lv_badi_imp = cl_abap_classdescr=>get_class_name( lo_badi_crea->imp ).
                IF lv_badi_imp CS '/SCWM/CL_EI_DLV_BATCH_DEF'.
                  MESSAGE e006(/scwm/dlv_batch) WITH lo_item_step3->mv_itemtype
                    lo_item_step3->mv_doctype cs_rehu-lgnum INTO lv_msg_text.
                ELSE.
                  MESSAGE e007(/scwm/dlv_batch) INTO lv_msg_text.
                ENDIF.
                m_sy_msg_and_return.
              ENDIF.
            ENDIF.
          ENDIF.
        CATCH /scwm/cx_dlv_batch.                       "#EC NO_HANDLER
      ENDTRY.
    ELSEIF lv_docu_batch IS NOT INITIAL.
      /scwm/cl_batch_appl=>convert_dbatchno(
        EXPORTING
          iv_dbatchno = cs_rehu_prod-charg
        IMPORTING
          ev_dbatchno = cs_rehu_prod-charg ).
    ENDIF.
  ENDIF.


  CALL FUNCTION '/SCWM/RF_QTY_POSITIVE_CHECK'
    EXPORTING
      iv_qty   = cs_rehu_prod-nista
    EXCEPTIONS
      negative = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    m_sy_msg_and_return.
  ENDIF.

  lv_quantiy = cs_rehu_prod-nista.
  IF lv_quantiy EQ 0.
    MESSAGE e404(/scwm/rf_en) INTO lv_msg_text.
    m_sy_msg_and_return.
  ENDIF.


  DATA ls_del_mark TYPE /scwm/s_rf_unlo_docid.
  CLEAR ls_del_mark-mark_sel.
  ls_del_mark-mark_sel = abap_false.
  MODIFY TABLE cs_rehu-deliveries FROM ls_del_mark TRANSPORTING mark_sel.


  cs_rehu_prod-matnr = ls_mat_global-matnr.


  DATA ls_docid_tmp TYPE /scwm/s_docid.
*  ls_docid_tmp-doccat = wmegc_doccat_pdi. "MC
  LOOP AT cs_rehu-deliveries ASSIGNING <fs_delivery>.
    ls_docid_step3-docid  = <fs_delivery>-docid.
    ls_docid_step3-doccat = wmegc_doccat_pdi.
    APPEND ls_docid_step3 TO lt_docid_step3.
  ENDLOOP.


  PERFORM check_for_open_quantity
    USING    cs_rehu-lgnum
             lv_segment_managed_prod
    CHANGING lt_docid_step3
             cs_rehu_prod
             lt_open_items.


  SORT lt_open_items BY prd_id doccat_prd item_id open_qty-qty.
  DELETE ADJACENT DUPLICATES FROM lt_open_items COMPARING prd_id doccat_prd.

* check HU exist
  READ TABLE cs_rehu-hus WITH KEY huident = cs_rehu_hu-huident
                         TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    lf_hu_exist = abap_true.
  ENDIF.


  CASE lines( lt_open_items ).

    WHEN 0.

      PERFORM delivery_table_fill
        USING    lt_docid_step3
                 'X'
        CHANGING ct_rehu_dlv.

      IF lines( lt_docid_step3 ) = 1.
        READ TABLE ct_rehu_dlv INTO ls_rehu_dlv INDEX 1.
        IF sy-subrc <> 0.
          m_sy_msg_and_return.
        ENDIF.

        IF lf_hu_exist = abap_true.
          MESSAGE e491(/scwm/rf_en) WITH ls_rehu_dlv-dlvno INTO lv_msg_text.
          m_sy_msg_and_return.
        ENDIF.


        READ TABLE cs_rehu-deliveries INTO DATA(ls_dlv_advsr)
          WITH KEY docid = ls_rehu_dlv-docid.
        IF sy-subrc = 0 AND ls_dlv_advsr-advsr = abap_true.
          MESSAGE e335(/scwm/dlv_egr) INTO lv_msg_text.
          m_sy_msg_and_return.
        ENDIF.


        DATA(lo_globals_s3) = CAST /scwm/if_tm_global_info(
            /scwm/cl_tm_factory=>get_service( /scwm/if_tm_global_info=>sc_my_tm_factory_name ) ).
        IF abap_false = lo_globals_s3->is_s4h_cloud( ).
          cs_rehu_hu-partyno = ls_rehu_dlv-partyno.
          PERFORM check_egr_exists_for_item
            USING    cs_rehu-lgnum
                     cs_rehu_prod
                     cs_rehu_hu
            CHANGING ls_prd_hdr_s3
                     lt_items_s3.
        ENDIF.
        IF lt_items_s3 IS INITIAL.
          MESSAGE e491(/scwm/rf_en) WITH ls_rehu_dlv-dlvno INTO lv_msg_text.
          m_sy_msg_and_return.
        ENDIF.
      ELSE.

        MESSAGE e491(/scwm/rf_en) WITH cs_rehu_hu-dlvno INTO lv_msg_text.
        m_sy_msg_and_return.
      ENDIF.

    WHEN 1.

      READ TABLE lt_open_items INTO ls_items_step3 INDEX 1.
      IF sy-subrc <> 0.
        m_sy_msg_and_return.
      ENDIF.

      CLEAR: lt_dlvs, ls_dlvs.
      ls_dlvs-docid  = ls_items_step3-prd_id.
      ls_dlvs-itemid = ls_items_step3-item_id.
      ls_dlvs-doccat = wmegc_doccat_pdi.
      APPEND ls_dlvs TO lt_dlvs.

      PERFORM delivery_table_fill
        USING    lt_dlvs
                 ' '
        CHANGING ct_rehu_dlv.

      READ TABLE ct_rehu_dlv INTO ls_rehu_dlv INDEX 1.

* check shelf life
      PERFORM check_min_rem_sl
        USING    cs_rehu_prod
                 ls_rehu_dlv
        CHANGING lv_failed.

      IF lv_failed IS NOT INITIAL.
*Errore o warning o skip?
        MESSAGE e511(/scwm/rf_en) INTO lv_msg_text.
        m_sy_msg_and_return.
      ENDIF.


      cs_rehu_hu-dlvno   = ls_items_step3-prd_no.
      cs_rehu_hu-docid   = ls_items_step3-prd_id.
      cs_rehu_hu-ritmid  = ls_items_step3-item_id.
      cs_rehu_hu-rdoccat = ls_items_step3-doccat_prd.


      IF lv_new_batch IS NOT INITIAL.
        lo_bom_step3 = /scdl/cl_bo_management=>get_instance( ).
        lo_bo_step3  = lo_bom_step3->get_bo_by_id( cs_rehu_hu-docid ).
        lo_item_step3 ?= lo_bo_step3->get_item( cs_rehu_hu-ritmid ).
        ls_product_step3 = lo_item_step3->get_product( ).
        ls_sapext_step3  = /scwm/cl_dlv_batch_internal=>item_get_sapext( lo_item_step3 ).
        GET BADI lo_badi_crea
          FILTERS
            lgnum = cs_rehu-lgnum.
        CALL BADI lo_badi_crea->check_batch_create
          EXPORTING
            iv_lgnum      = cs_rehu-lgnum
            iv_doccat     = lo_item_step3->mv_doccat
            iv_doc_type   = lo_item_step3->mv_doctype
            iv_itemtype   = lo_item_step3->mv_itemtype
            iv_itemcat    = lo_item_step3->mv_itemcat
            iv_productid  = ls_product_step3-productid
            iv_entitled   = ls_sapext_step3-entitled
            iv_docid      = lo_item_step3->mv_docid
            iv_itemid     = lo_item_step3->mv_itemid
          IMPORTING
            ev_batch_crea = lv_batch_crea.
        IF lv_batch_crea = /scwm/if_dlv_batch_c=>sc_crea_no.
          lv_badi_imp = cl_abap_classdescr=>get_class_name( lo_badi_crea->imp ).
          IF lv_badi_imp CS '/SCWM/CL_EI_DLV_BATCH_DEF'.
            MESSAGE e006(/scwm/dlv_batch) WITH lo_item_step3->mv_itemtype
              lo_item_step3->mv_doctype cs_rehu-lgnum INTO lv_msg_text.
          ELSE.
            MESSAGE e007(/scwm/dlv_batch) INTO lv_msg_text.
          ENDIF.
          m_sy_msg_and_return.
        ENDIF.
      ENDIF.

    WHEN OTHERS.

      MESSAGE e424(/scwm/rf_en) WITH cs_rehu_hu-dlvno INTO lv_msg_text.
      m_sy_msg_and_return.

  ENDCASE.

* pack spec
  PERFORM pack_spec_for_item
    USING    cs_rehu-deliveries
    CHANGING cs_rehu-lgnum
             cs_rehu_prod
             cs_rehu_hu
             ls_packing_rule
             ls_levels
             ls_item_packaged
             ls_top_hu_quan_upb
             lt_huhdr_upb
             lt_huitm_ubp
             lt_huauxpmat_upb
             lt_hutree_upb.


  PERFORM pack_item_to_delivery
    USING    cs_rehu-lgnum
             cs_rehu_hu-pmatid
             cs_rehu-procs
             cs_rehu-prr_id
             ls_packing_rule
             ls_levels
             ls_item_packaged
             lt_huhdr_upb
             lt_huitm_ubp
             lt_huauxpmat_upb
             lt_hutree_upb
    CHANGING cs_rehu_prod
             cs_rehu_hu
             ct_rehu_hu
             cs_rehu.


  IF lf_hu_exist = abap_false.
    cs_rehu-sumhu = cs_rehu-sumhu + 1.
  ENDIF.


  PERFORM hu_read_and_lock
    USING    cs_rehu_hu-huident
             cs_rehu-lgnum
             cs_rehu-deliveries
             abap_true
    CHANGING ls_huhdr
             lt_huitm
             cs_rehu_hu-hazmat_ind
             cs_rehu_hu-rdoccat
             cs_rehu_hu-rdocid.


  IF lf_hu_exist = abap_true.
    DATA(ls_rehu_prod_save) = cs_rehu_prod.
    CLEAR: cs_rehu_prod, ct_rehu_prod.
  ENDIF.

  PERFORM fill_prod_data_from_huitem
    USING    cs_rehu-lgnum
             lt_huitm
             cs_rehu_hu-huident
    CHANGING cs_rehu_prod
             ct_rehu_prod.


  IF lf_hu_exist = abap_false.
    APPEND cs_rehu_hu TO cs_rehu-hus.
    APPEND LINES OF lt_huitm TO cs_rehu-huitms.
  ELSE.

    DATA: lt_docid_curr TYPE /scwm/tt_docid,
          lt_hu_items2  TYPE /scwm/tt_huitm_int.
    lt_docid_curr = VALUE #( ( docid = cs_rehu_hu-docid ) ).
    CALL FUNCTION '/SCWM/DLV_GET_HUS_FOR_DELIVERY'
      EXPORTING
        iv_doccat = /scdl/if_dl_doc_c=>sc_doccat_inb_prd
        it_docid  = lt_docid_curr
      IMPORTING
        et_huitm  = lt_hu_items2
      EXCEPTIONS
        error     = 1
        OTHERS    = 2.
    IF sy-subrc = 0.
      cs_rehu-huitms = lt_hu_items2.
    ELSE.
      m_sy_msg_and_return.
    ENDIF.


    READ TABLE ct_rehu_prod INTO cs_rehu_prod
      WITH KEY matid   = ls_rehu_prod_save-matid
               ritmid  = cs_rehu_hu-ritmid
               charg   = ls_rehu_prod_save-charg
               bbdat   = ls_rehu_prod_save-bbdat.
    IF sy-subrc = 0.
      DELETE ct_rehu_prod WHERE guid_stock = cs_rehu_prod-guid_stock.
      INSERT cs_rehu_prod INTO ct_rehu_prod INDEX 1.
    ENDIF.
  ENDIF.

ENDFUNCTION.
