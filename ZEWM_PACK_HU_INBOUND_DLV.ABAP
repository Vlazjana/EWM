
┌─────────────────────────┐
│ 1. Check HU exists      │
└───────────┬─────────────┘
            ↓
┌─────────────────────────┐
│ 2. Create HU (if new)   │  → /scwm/if_pack_bas~create_hu
└───────────┬─────────────┘
            ↓
┌─────────────────────────┐
│ 3. Pack stock into HU   │  → /scwm/if_pack_bas~pack_stock
└───────────┬─────────────┘
            ↓
┌─────────────────────────┐
│ 4. Update BBD (optional)│  → /scwm/if_pack_bas~change_huitm
└───────────┬─────────────┘
            ↓
┌─────────────────────────┐
│ 5. Save & Commit        │  → /scwm/if_pack_bas~save
└─────────────────────────┘
HU Creation Part in the Function Module
The HU creation happens inside the FORM pack_item_to_delivery:

1. Check if HU Exists

" Check if HU already exists
CALL METHOD lo_pack->get_hu
  EXPORTING
    iv_huident = cs_rehu_hu-huident
    iv_lock    = abap_true
  IMPORTING
    es_huhdr   = ls_huhdr_exist
  EXCEPTIONS
    not_found  = 1
    OTHERS     = 2.

IF sy-subrc = 0.
  lf_hu_exist = abap_true.
ENDIF.

2. Create HU (if not exists)
DO lv_no_hu TIMES.

  IF lf_hu_exist = abap_true.
    " Use existing HU
    ls_huhdr = ls_huhdr_exist.
  ELSE.
    " Create new HU
    CALL METHOD lo_pack->/scwm/if_pack_bas~create_hu
      EXPORTING
        iv_pmat      = iv_hu_matid
        iv_huident   = lv_huident
        is_hu_create = ls_hu_create
      RECEIVING
        es_huhdr     = ls_huhdr
      EXCEPTIONS
        error        = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

3. Pack Stock into HU
  ls_material-qdoccat = cs_rehu_hu-rdoccat.
  ls_material-qdocid  = cs_rehu_hu-docid.
  ls_material-qitmid  = cs_rehu_hu-ritmid.
  ls_quantity-quan    = cs_rehu_prod-nista.
  ls_quantity-unit    = cs_rehu_prod-altme.

  CALL METHOD lo_pack->/scwm/if_pack_bas~pack_stock
    EXPORTING
      iv_dest_hu  = ls_huhdr-guid_hu
      is_material = ls_material
      is_quantity = ls_quantity
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


4. Update BBD (Best Before Date)
  IF cs_rehu_prod-bbdat IS NOT INITIAL.
    
    CALL METHOD lo_pack->/scwm/if_pack_bas~get_hu_item
      EXPORTING
        iv_guid_hu = ls_huhdr-guid_hu
      IMPORTING
        et_huitm   = lt_huitm.

    READ TABLE lt_huitm INTO ls_huitm INDEX 1.
    ls_huitm-vfdat = cs_rehu_prod-bbdat.

    CALL METHOD lo_pack->/scwm/if_pack_bas~change_huitm
      EXPORTING
        is_huitm = ls_huitm
      IMPORTING
        es_huitm = ls_huitm_out.
  ENDIF.

ENDDO.


5. Save HU
CALL METHOD lo_pack->/scwm/if_pack_bas~save
  EXPORTING
    iv_commit = ' '
    iv_wait   = ' '
  EXCEPTIONS
    error     = 1
    OTHERS    = 2.

COMMIT WORK AND WAIT.



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





----------------------------
----------------------------
----------------------------
----------------------------
  PERFORM prepare_hus
    USING    lt_huhdr
             lt_huref
    CHANGING cs_rehu
             cs_rehu_hu.

------------------------------------------------------------------*
*      -->P_LT_HUHDR  text
*      -->P_LT_HUREF  text
*      <--P_CS_REHU  text
*----------------------------------------------------------------------*
FORM prepare_hus  USING    it_huhdr   TYPE /scwm/tt_huhdr_int
                           it_huref   TYPE /scwm/tt_huref_int
                  CHANGING cs_rehu    TYPE /scwm/s_rf_admin_rehu
                           cs_rehu_hu TYPE /scwm/s_rf_rehu_hu.

  DATA: ls_huhdr      TYPE /scwm/s_huhdr_int,
        ls_huref      TYPE /scwm/s_huref_int,
        ls_hus        TYPE /scwm/s_rf_rehu_hu,
        ls_mat_global TYPE /scwm/s_material_global.

* create table of HUs
  LOOP AT it_huhdr INTO ls_huhdr WHERE top EQ 'X'.

    MOVE-CORRESPONDING ls_huhdr TO ls_hus.
    ls_hus-hutyp = ls_huhdr-letyp.

    READ TABLE it_huref INTO ls_huref WITH KEY guid_hu = ls_huhdr-guid_hu.
    ls_hus-docid   = ls_huref-docid.
    ls_hus-doccat  = ls_huref-doccat.
    ls_hus-shpmt   = cs_rehu_hu-shpmt.
    ls_hus-tu_num  = cs_rehu_hu-tu_num.
    ls_hus-door    = cs_rehu_hu-door.
    ls_hus-asn     = cs_rehu_hu-asn.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = ls_huhdr-pmat_guid
            iv_lgnum      = cs_rehu-lgnum
          IMPORTING
            es_mat_global = ls_mat_global.
      CATCH /scwm/cx_md.
        MESSAGE ID     sy-msgid
                TYPE   sy-msgty
                NUMBER sy-msgno
                WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.

    ls_hus-pmat = ls_mat_global-matnr.

    PERFORM hu_status_check
      USING    cs_rehu-lgnum
               cs_rehu
      CHANGING ls_hus.

    IF ls_hus-unloaded IS NOT INITIAL OR
       ls_hus-pgr IS NOT INITIAL.
      cs_rehu-sumhu_proc = cs_rehu-sumhu_proc + 1.
    ENDIF.

    APPEND ls_hus TO cs_rehu-hus.
    CLEAR: ls_hus, ls_huref.
  ENDLOOP.

ENDFORM.                    " prepare_hus

----------------------
----------------------
----------------------
----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_RECEIVING_HUSF16 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  hu_admin_table_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PRD_HDR  text
*      -->P_LT_ITEMS  text
*      <--P_CS_REHU_HU  text
*      <--P_CT_REHU_HU  text
*----------------------------------------------------------------------*
FORM hu_admin_table_update  USING    it_prd_hdr TYPE /scwm/dlv_header_out_prd_tab
                                     it_items  TYPE /scwm/dlv_item_out_prd_tab
                            CHANGING cs_rehu TYPE /scwm/s_rf_admin_rehu
                                     cs_rehu_hu TYPE /scwm/s_rf_rehu_hu
                                     ct_rehu_hu TYPE /scwm/tt_rf_rehu_hu.

  DATA: ls_items    TYPE /scwm/dlv_item_out_prd_str,
        ls_prd_hdr  TYPE /scwm/dlv_header_out_prd_str,
        ls_partyloc TYPE /scdl/dl_partyloc_str,
        ls_id       TYPE /scmb/mdl_prtid_str,
        lt_id       TYPE /scmb/mdl_prt_id_tab,
        ls_partner  TYPE /scmb/mdl_partner,
        lt_partner  TYPE STANDARD TABLE OF /scmb/mdl_partner,
        ls_unloadpt TYPE /scdl/dl_partyloc_str.

  FIELD-SYMBOLS: <fs_delivery> TYPE /scwm/s_rf_unlo_docid.

  DATA lv_door_dlv_date TYPE /scwm/de_rf_dlv_date.
* Keep the delivery date like by unloading
  lv_door_dlv_date = cs_rehu_hu-door_dlv_date.
  CLEAR cs_rehu_hu.
  cs_rehu_hu-door_dlv_date = lv_door_dlv_date.

* Vendor from Delivery Item
  CLEAR:ls_id,lt_id,ls_partner,lt_partner.
  LOOP AT it_prd_hdr INTO ls_prd_hdr.
    READ TABLE it_items INTO ls_items WITH KEY ls_prd_hdr-docid.
    IF sy-subrc EQ 0.
      READ TABLE ls_items-partyloc INTO ls_partyloc WITH KEY party_role = 'VENDOR'.
      IF sy-subrc = 0.
        ls_id-partner_guid = ls_partyloc-partyid.
        APPEND ls_id TO lt_id.
      ENDIF.
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
      CATCH
        /scmb/cx_mdl_interface
        /scmb/cx_mdl_result_empty
        /scmb/cx_mdl_result_toomany
        /scmb/cx_mdl_result
        /scmb/cx_mdl.                                   "#EC NO_HANDLER
    ENDTRY.
  ENDIF.

  LOOP AT it_prd_hdr INTO ls_prd_hdr.

    CLEAR:ls_partyloc,ls_partner,ls_unloadpt.

    READ TABLE it_items INTO ls_items WITH KEY ls_prd_hdr-docid.
    IF sy-subrc EQ 0.
      cs_rehu_hu-door      = ls_items-sapext-/scwm/door.
      READ TABLE ls_items-partyloc INTO ls_partyloc WITH KEY party_role = 'VENDOR'.
      IF sy-subrc EQ 0.
        READ TABLE lt_partner INTO ls_partner WITH KEY partner_guid = ls_partyloc-partyid.
        IF sy-subrc EQ 0.
          cs_rehu_hu-partyid   = ls_partyloc-partyid.
          cs_rehu_hu-partyno   = ls_partner-partner.
          cs_rehu_hu-partytext = ls_partner-name.
        ENDIF.
      ENDIF.

      READ TABLE ls_items-partyloc INTO ls_unloadpt WITH KEY party_role = 'UP'.
      IF sy-subrc EQ 0.
        cs_rehu_hu-id_lp_door = ls_unloadpt-locationno.
      ENDIF.
    ENDIF.

    LOOP AT cs_rehu-deliveries ASSIGNING <fs_delivery>
         WHERE docid = ls_prd_hdr-docid.

      READ TABLE ls_prd_hdr-partyloc INTO ls_partyloc
           WITH KEY party_role = /scdl/if_dl_partyloc_c=>sc_party_role_wh.
      IF sy-subrc = 0.
        MOVE ls_partyloc-locationid TO <fs_delivery>-wh_locationid.
        MOVE ls_partyloc-locationno TO <fs_delivery>-wh_locationno.
      ENDIF.

      IF cs_rehu_hu-partyno IS NOT INITIAL.
        <fs_delivery>-partyno   = cs_rehu_hu-partyno.
      ENDIF.
      IF ls_partner IS NOT INITIAL.
        <fs_delivery>-partytext = ls_partner-name.
      ENDIF.

      IF <fs_delivery>-door IS INITIAL.
        <fs_delivery>-door = ls_items-sapext-/scwm/door.
      ENDIF.

      IF <fs_delivery>-unl_point IS INITIAL.
        <fs_delivery>-unl_point = ls_unloadpt-locationno.
      ENDIF.
    ENDLOOP.

    MOVE-CORRESPONDING ls_prd_hdr TO cs_rehu_hu.
    cs_rehu_hu-dlvno     = ls_prd_hdr-docno.

    MODIFY cs_rehu-hus
     FROM  cs_rehu_hu
     TRANSPORTING partyid partyno partytext dlvno
          WHERE docid = cs_rehu_hu-docid.

    APPEND cs_rehu_hu TO ct_rehu_hu.
  ENDLOOP.

ENDFORM.                    " hu_admin_table_update

------------------
_______----------
-----------------


   Form  hu_read_and_lock
*&--------------------------------------------------------------------*
*       Read, Lock and Check HU
*---------------------------------------------------------------------*
*      -->IV_HUIDENT     HU Identification
*      -->IV_DOCID       Document number (Delivery GUID)
*      -->IV_LGNUM       Warehouse Number
*      -->IT_DELIVERIES  Selected Deliveries (GUIDs)
*      -->IV_LOCK        Lock HU
*      -->CS_HUHDR       HU Header
*      -->CV_HAZMAT_IND  Hazardous Material Indicator
*---------------------------------------------------------------------*
FORM hu_read_and_lock
  USING    iv_huident     TYPE /scwm/de_huident
*          iv_docid       TYPE /scwm/de_docid
           iv_lgnum       TYPE /scwm/lgnum
           it_deliveries  TYPE /scwm/tt_rf_unlo_docid
           iv_lock        TYPE boole_d
  CHANGING cs_huhdr       TYPE /scwm/s_huhdr_int
           ct_huitm       TYPE /scwm/tt_huitm_int
           cv_hazmat_ind  TYPE char4
           cv_doccat       TYPE /scwm/de_doccat
           cv_docid       TYPE /scwm/de_docid.

*----------------------------------------------------------------------*
* data declarations                                                    *
*----------------------------------------------------------------------*
  DATA: ls_deliveries TYPE /scwm/s_rf_unlo_docid.           "#EC NEEDED

  DATA: lt_huref TYPE /scwm/tt_huref_int,
        lv_exist TYPE xfeld.

  DATA: lo_packing TYPE REF TO /scwm/cl_wm_packing.

  FIELD-SYMBOLS <fs_huref> TYPE /scwm/s_huref_int.
*----------------------------------------------------------------------*
* program logic                                                        *
*----------------------------------------------------------------------*
  IF lo_packing IS NOT BOUND.
    CREATE OBJECT lo_packing.
  ENDIF.

* read all necessary data: HU status
  CALL METHOD lo_packing->get_hu
    EXPORTING
      iv_huident = iv_huident
      iv_lock    = iv_lock
    IMPORTING
      et_huitm   = ct_huitm
      es_huhdr   = cs_huhdr
      et_huref   = lt_huref
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

* does HU exist in given warehouse number and is HU top HU?
  IF  sy-subrc = 1
  OR cs_huhdr-top IS INITIAL
  OR cs_huhdr-lgnum <> iv_lgnum.
    MESSAGE e014(/scwm/rf_de) WITH iv_huident.
*   Handling Unit &1 ist not allowed
  ENDIF.

* is HU in predecessor document (delivery, door, TU, shipment)?
  SORT lt_huref BY guid_hu.

  CLEAR lv_exist.
  LOOP AT lt_huref ASSIGNING <fs_huref>
    WHERE  guid_hu = cs_huhdr-guid_hu.

    cv_doccat = <fs_huref>-doccat.
    cv_docid = <fs_huref>-docid.

    READ TABLE it_deliveries
         INTO ls_deliveries
         WITH KEY docid = <fs_huref>-docid.

    IF sy-subrc = 0.
      lv_exist = 'X'.
      EXIT.
    ENDIF.

  ENDLOOP.

  IF it_deliveries IS NOT INITIAL.
    IF lv_exist IS INITIAL.
      MESSAGE e014(/scwm/rf_de) WITH iv_huident.
*     Handling Unit &1 ist not allowed
    ELSE.
      MOVE: <fs_huref>-doccat TO cv_doccat,
            <fs_huref>-docid TO cv_docid.
    ENDIF.
  ENDIF.

* does HU contain hazardous material?
  CALL FUNCTION '/SCWM/RF_HAZMAT_IND_READ'
    EXPORTING
      iv_hazmat     = cs_huhdr-hzmt
    IMPORTING
      ev_hazmat_ind = cv_hazmat_ind.

ENDFORM.                    "hu_read_and_lock


-----------------
------------------
-----------------
INCLUDE /SCWM/LRF_RECEIVING_HUSF14 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  fill_prod_data_from_huitem
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_HUITM  text
*      <--CS_REHU_PROD  text
*      <--CT_REHU_PROD  text
*----------------------------------------------------------------------*
FORM fill_prod_data_from_huitem  USING    iv_lgnum      TYPE /scwm/lgnum
                                          it_huitm      TYPE /scwm/tt_huitm_int
                                          iv_huident    TYPE /scwm/de_rf_huident
                                 CHANGING cs_rehu_prod  TYPE /scwm/s_rf_rehu_prod
                                          ct_rehu_prod  TYPE /scwm/tt_rf_rehu_prod.

  DATA: lo_dlv TYPE REF TO /scwm/cl_dlv_management_prd,
        lo_msg TYPE REF TO /scwm/cl_dm_message_no.          "#EC NEEDED

  DATA: lt_items TYPE /scwm/dlv_item_out_prd_tab,
        lt_doc   TYPE /scwm/dlv_docid_item_tab.

  DATA: ls_items        TYPE /scwm/dlv_item_out_prd_str,
        ls_mat_global   TYPE  /scwm/s_material_global,
        ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
        ls_read_options TYPE /scwm/dlv_query_contr_str,
        ls_doc          TYPE /scwm/dlv_docid_item_str,
        ls_mat_lgnum    TYPE /scwm/s_material_lgnum.

  DATA: lo_packing TYPE REF TO /scwm/cl_wm_packing,
        lt_huhdr   TYPE        /scwm/tt_huhdr_int,
        ls_huhdr   TYPE        /scwm/s_huhdr_int.

  DATA: lv_sn_type      LIKE cs_rehu_prod-sn_type.

  FIELD-SYMBOLS: <fs_huitm> TYPE /scwm/s_huitm_int.


  IF lo_packing IS NOT BOUND.
    CREATE OBJECT lo_packing.
  ENDIF.

* read all necessary data: HU status
  CALL METHOD lo_packing->get_hu
    EXPORTING
      iv_huident = iv_huident
    IMPORTING
      et_huhdr   = lt_huhdr
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.

  ls_read_options-mix_in_object_instances = /scwm/if_dl_c=>sc_mix_in_load_instance.
  ls_read_options-data_retrival_only = 'X'.
* ensures the only needed data is read:
*  ls_include_data-item_status = 'X'.

  LOOP AT it_huitm ASSIGNING <fs_huitm>.
    ls_doc-doccat = <fs_huitm>-qdoccat.
    ls_doc-docid  = <fs_huitm>-qdocid.
    ls_doc-itemid = <fs_huitm>-qitmid.
    APPEND ls_doc TO lt_doc.
  ENDLOOP.

  TRY.
*   read delivery
      CALL METHOD lo_dlv->query
        EXPORTING
          it_docid        = lt_doc
          is_read_options = ls_read_options
          is_include_data = ls_include_data
        IMPORTING
          et_items        = lt_items.
    CATCH /scdl/cx_delivery .                           "#EC NO_HANDLER
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

  LOOP AT it_huitm ASSIGNING <fs_huitm>.

    READ TABLE lt_items INTO ls_items WITH KEY doccat = <fs_huitm>-qdoccat
                                               docid  = <fs_huitm>-qdocid
                                               itemid = <fs_huitm>-qitmid.
    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING <fs_huitm> TO cs_rehu_prod.

    cs_rehu_prod-procty = ls_items-sapext-/scwm/procty.
    cs_rehu_prod-gmbin  = ls_items-sapext-/scwm/gmbin.
    cs_rehu_prod-charg  = ls_items-product-batchno.
    cs_rehu_prod-matnr  = ls_items-product-productno.
    cs_rehu_prod-itemno = ls_items-itemno.

      cs_rehu_prod-vsola  = <fs_huitm>-quana.
    cs_rehu_prod-rdocid = ls_items-docid.
    cs_rehu_prod-rdoccat = <fs_huitm>-qdoccat.
    cs_rehu_prod-ritmid = ls_items-itemid.
    cs_rehu_prod-guid_stock = <fs_huitm>-guid_stock.
    cs_rehu_prod-dlvno = ls_items-docno.

    IF <fs_huitm>-entitled IS NOT INITIAL.
      CALL FUNCTION '/SCWM/RF_SN_CHECK'
        EXPORTING
          iv_lgnum    = iv_lgnum
          iv_matid    = cs_rehu_prod-matid
          iv_entitled = <fs_huitm>-entitled
        IMPORTING
          ev_stock    = lv_sn_type.
    ELSE.
      CLEAR lv_sn_type.
    ENDIF.
    cs_rehu_prod-sn_type = lv_sn_type.

      READ TABLE lt_huhdr INTO ls_huhdr
           WITH KEY guid_hu = <fs_huitm>-guid_parent.

      IF sy-subrc = 0.
        cs_rehu_prod-huident = ls_huhdr-huident.
        cs_rehu_prod-pmat    = ls_huhdr-pmat.
        cs_rehu_prod-hutyp = ls_huhdr-letyp.
      ELSE.
        cs_rehu_prod-huident = iv_huident.
      ENDIF.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = cs_rehu_prod-matid
            iv_lgnum      = iv_lgnum
            iv_entitled   = ls_items-sapext-entitled
          IMPORTING
            es_mat_global = ls_mat_global
            es_mat_lgnum  = ls_mat_lgnum.
      CATCH /scwm/cx_md_interface
      /scwm/cx_md_material_exist
      /scwm/cx_md_mat_lgnum_exist
      /scwm/cx_md_lgnum_locid
      /scwm/cx_md.                                      "#EC NO_HANDLER
        MESSAGE ID     sy-msgid
                TYPE   sy-msgty
                NUMBER sy-msgno
                WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDTRY.

    cs_rehu_prod-maktx  = ls_mat_global-maktx.

    BREAK-POINT ID /scwm/suom.

    IF ls_mat_lgnum-puom_wh IS NOT INITIAL.
      cs_rehu_prod-ruom = ls_mat_lgnum-puom_wh.
    ELSE.
      cs_rehu_prod-ruom = ls_mat_global-puom.
    ENDIF.

      APPEND cs_rehu_prod TO ct_rehu_prod.

  ENDLOOP.

ENDFORM.                    " fill_prod_data_from_huitem


------------------
_------------------
-------------------

--------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_RECEIVING_HUSF25 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  check_for_other_delivery
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->HUIDENT  text
*      <--CV_EXIST_IN_OTHER_DELIVERY  text
*----------------------------------------------------------------------*
FORM check_for_other_delivery  USING    iv_huident TYPE /scwm/de_rf_huident
                                        iv_lgnum   TYPE /scwm/lgnum
                               CHANGING cv_exist_in_other_delivery TYPE xfeld.


  DATA: ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
        ls_selection    TYPE /scwm/dlv_selection_str,
        lt_selection    TYPE /scwm/dlv_selection_tab,
        ls_read_options TYPE /scwm/dlv_query_contr_str,
        lt_prd_hdr      TYPE /scwm/dlv_header_out_prd_tab
        .

  DATA: ls_qdoccat_r TYPE  rsdsselopt,
        lt_qdoccat_r TYPE  rseloption,
        ls_huident_r TYPE  rsdsselopt,
        lt_huident_r TYPE  rseloption,
        lt_huitm     TYPE  /scwm/tt_huitm_int,
        ls_huitm     TYPE  /scwm/s_huitm_int.


  DATA: lo_dlv TYPE REF TO /scwm/cl_dlv_management_prd.

  CLEAR  cv_exist_in_other_delivery.

  ls_huident_r-sign   = wmegc_sign_inclusive.
  ls_huident_r-option = wmegc_option_eq.
  ls_huident_r-low    = iv_huident.
  APPEND ls_huident_r TO lt_huident_r.


  ls_qdoccat_r-sign   = wmegc_sign_inclusive.
  ls_qdoccat_r-option = wmegc_option_eq.
  ls_qdoccat_r-low    = /scdl/if_dl_doc_c=>sc_doccat_inb_prd.
  APPEND ls_qdoccat_r TO lt_qdoccat_r.
  CALL FUNCTION '/SCWM/HU_SELECT_GEN'
    EXPORTING
      iv_lgnum     = iv_lgnum
      ir_huident   = lt_huident_r
      ir_qdoccat   = lt_qdoccat_r
    IMPORTING
      et_huitm     = lt_huitm
*     et_huref     = lt_huref
    EXCEPTIONS
      wrong_input  = 1
      not_possible = 2
      OTHERS       = 3.

  IF sy-subrc <> 0.                                         "#EC *
  ENDIF.

  READ TABLE lt_huitm INTO ls_huitm INDEX 1.

  IF sy-subrc <> 0.                                         "#EC NEEDED
  ENDIF.


*XSEL_IBD I EQ  X
*HUNO I EQ  SZA01
*STATUS_VALUE_DGR_I I EQ  1
*STATUS_VALUE_DWA_I I EQ  1

  CLEAR ls_selection.
  ls_selection-fieldname =
    /scdl/if_dl_logfname_c=>sc_status_value_dgr_i.
  ls_selection-low    = /scdl/if_dl_status_c=>sc_v_not_started.
  ls_selection-sign   = wmegc_sign_inclusive.
  ls_selection-option = wmegc_option_eq.
  APPEND ls_selection TO lt_selection.
  CLEAR ls_selection.
  ls_selection-fieldname =
    /scdl/if_dl_logfname_c=>sc_status_value_dwa_i.
  ls_selection-low    = /scdl/if_dl_status_c=>sc_v_not_started.
  ls_selection-sign   = wmegc_sign_inclusive.
  ls_selection-option = wmegc_option_eq.
  APPEND ls_selection TO lt_selection.
  CLEAR ls_selection.
  ls_selection-fieldname =
    /scdl/if_dl_logfname_c=>sc_docid_h.
  ls_selection-low    = ls_huitm-qdocid.
  ls_selection-sign   = wmegc_sign_inclusive.
  ls_selection-option = wmegc_option_eq.
  APPEND ls_selection TO lt_selection.
  CLEAR ls_selection.
  ls_selection-fieldname = /scwm/if_dl_logfname_c=>sc_prodind_prdi_h.
  ls_selection-sign      = wmegc_sign_inclusive.
  ls_selection-option    = wmegc_option_eq.
  APPEND ls_selection TO lt_selection.


  ls_read_options-mix_in_object_instances =  /scwm/if_dl_c=>sc_mix_in_load_instance.
  ls_read_options-data_retrival_only = 'X'.
  ls_include_data-item_status = 'X'.
  ls_include_data-item_partyloc = 'X'.
  ls_include_data-head_status = 'X'.
  ls_include_data-head_status_dyn = 'X'.

* create delivery object
  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.

  TRY.
      CALL METHOD lo_dlv->query
        EXPORTING
          it_selection    = lt_selection
          iv_doccat       = wmegc_doccat_pdi
          is_read_options = ls_read_options
          is_include_data = ls_include_data
        IMPORTING
          et_headers      = lt_prd_hdr.
    CATCH /scdl/cx_delivery.                            "#EC NO_HANDLER
* ????
  ENDTRY.

  IF lt_prd_hdr IS NOT INITIAL.
    cv_exist_in_other_delivery = 'X'.
  ENDIF.


ENDFORM.                    " check_for_other_delivery


------------
------------
------------
----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_RECEIVING_HUSF34 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  hu_number_range_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CS_REHU_HU  text
*----------------------------------------------------------------------*
FORM hu_number_range_check  USING    cs_rehu_hu TYPE /scwm/s_rf_rehu_hu
                                     iv_lgnum TYPE /scwm/lgnum.


  DATA: lt_nriv TYPE TABLE OF inriv.
  DATA: lv_txt  TYPE string.
  DATA: ls_nriv TYPE inriv,
        lv_ok   TYPE xfeld VALUE abap_false.


* first check for SSCC
  CALL FUNCTION '/SCWM/SSCC_CHECK'
    EXPORTING
      iv_sscc         = cs_rehu_hu-huident
      iv_lgnum        = iv_lgnum
      iv_foreign_sscc = ' '
    EXCEPTIONS
      OTHERS          = 9.

* could be a foreign SSCC
  IF sy-subrc <> 0.
    CALL FUNCTION '/SCWM/SSCC_CHECK'
      EXPORTING
        iv_sscc                 = cs_rehu_hu-huident
        iv_lgnum                = iv_lgnum
        iv_foreign_sscc         = 'X'
      EXCEPTIONS
        iln_not_found           = 1
        invalid_call            = 2
        invalid_customizing     = 3
        invalid_iln             = 4
        invalid_no_out_of_range = 5
        invalid_sscc            = 6
        internal_error          = 7
        error_on_number_range   = 8
        OTHERS                  = 9.

*   not an SSCC number
    IF sy-subrc <> 0.

*...  read all intervalls
      CALL FUNCTION 'NUMBER_RANGE_INTERVAL_LIST'
        EXPORTING
          object              = '/SCWM/HUID'
          subobject           = iv_lgnum
          clear_local_memory  = abap_true
        TABLES
          interval            = lt_nriv
        EXCEPTIONS
          object_not_found    = 6
          subobject_not_found = 8
          OTHERS              = 99.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_txt.
      ENDIF.

      LOOP AT lt_nriv INTO ls_nriv.
        CHECK cs_rehu_hu-huident > ls_nriv-fromnumber AND
              cs_rehu_hu-huident < ls_nriv-tonumber.
        IF ls_nriv-externind IS INITIAL.  "internal intervall

          IF cs_rehu_hu-huident > ls_nriv-nrlevel.
            MESSAGE e043(/scwm/hu_wm) WITH cs_rehu_hu-huident .
          ENDIF.
          lv_ok = abap_true.
        ELSE.   "external intervall = OK
          lv_ok = abap_true.
        ENDIF.
        EXIT.
      ENDLOOP.
      CHECK lv_ok = abap_false.
* If there are no external interval maintained - all external numbers
* are allowed
      READ TABLE lt_nriv TRANSPORTING NO FIELDS
                     WITH KEY externind = 'X'.
      CHECK sy-subrc IS INITIAL.
      CLEAR cs_rehu_hu-vlenr_verif.
      MESSAGE e085(/scwm/hu_wm) WITH cs_rehu_hu-huident.
    ENDIF.
  ENDIF.




ENDFORM.                    " hu_number_range_check


----------------------
----------------------
-----------------------
----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_RECEIVING_HUSF21 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  matid_read_from_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->CS_REHU_PROD_MATNR  text
*      <--LV_MATID  text
*----------------------------------------------------------------------*
FORM matid_read_from_matnr  USING    iv_matnr TYPE /scwm/de_rf_matnr
                            CHANGING cv_matid TYPE /scwm/de_rf_matid
                                     cv_ean_gtin TYPE xfeld.

  DATA lv_matean TYPE /scwm/de_rf_ean11.

  CLEAR cv_matid.

  CALL FUNCTION '/SCWM/RF_PRODUCT_INPUT'
    EXPORTING
      input     = iv_matnr
    IMPORTING
      ev_matid  = cv_matid
      ev_matean = lv_matean.

  IF lv_matean IS NOT INITIAL.
    cv_ean_gtin = abap_true.
  ENDIF.
ENDFORM.                    " matid_read_from_matnr

----------------------
-----------------------
-----------------------


*&---------------------------------------------------------------------*
*&      Form  GET_ITEM_BC_BY_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM get_item_bc_by_type  USING    iv_itemtype TYPE /scdl/dl_itemtype
                                   iv_doccat   TYPE /scdl/dl_doccat
                          CHANGING es_item_bc
                                       TYPE /scdl/dl_itype_detail_str.

  DATA: lo_service     TYPE REF TO /scdl/cl_af_management,
        lo_bc_access   TYPE REF TO /scdl/if_af_business_conf,
        ls_item_extkey TYPE        /scdl/dl_itmtype_extkey_str.

  CLEAR es_item_bc.

  lo_service = /scdl/cl_af_management=>get_instance( ).
  IF lo_service IS NOT BOUND.
    RETURN.
  ENDIF.

  TRY.
      lo_bc_access ?= lo_service->get_service(
        /scdl/if_af_management_c=>sc_business_conf ).
    CATCH /scdl/cx_af_management.                       "#EC NO_HANDLER
      RETURN.
  ENDTRY.

* get business configuration
  ls_item_extkey-item_type = iv_itemtype.
  ls_item_extkey-category  = iv_doccat.

  TRY.
      lo_bc_access->get_item_bc_by_type(
        EXPORTING
          is_item_extkey = ls_item_extkey
        IMPORTING
          es_itype       = es_item_bc ).
    CATCH /scdl/cx_af_business_conf.                    "#EC NO_HANDLER
      RETURN.
  ENDTRY.

ENDFORM.                    " GET_ITEM_BC_BY_TYPE



-----------------------
---------------------
----------------------


----------------------------------------------------------------------*
***INCLUDE /SCWM/LRF_RECEIVING_HUSF20 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  check_for_open_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_CS_REHU  text
*      <--P_CS_REHU_PROD  text
*      <--P_CT_OPEN_ITEMS  text
*----------------------------------------------------------------------*
FORM check_for_open_quantity  USING    iv_lgnum       TYPE /scwm/lgnum
                                       ib_sgt_mnaged  TYPE boole_d
                              CHANGING ct_docid       TYPE /scwm/dlv_docid_item_tab
                                       cs_rehu_prod   TYPE  /scwm/s_rf_rehu_prod
                                       ct_open_items  TYPE /scwm/dlv_hu_prd_tab.


  DATA: lt_free        TYPE /scwm/dlv_hu_prd_tab,
        ls_free        TYPE /scwm/dlv_hu_prd_str,
        lt_docid       TYPE /scwm/dlv_docid_item_tab,
        ls_docid       TYPE /scwm/dlv_docid_item_str,
        ls_matid_charg TYPE /scwm/s_matid_charg,
        ls_dlv_sel     TYPE /scwm/dlv_selection_str,
        lt_dlv_sel     TYPE /scwm/dlv_selection_tab.

  DATA: ls_read_options TYPE /scwm/dlv_query_contr_str,
        ls_mat_global   TYPE  /scwm/s_material_global,
        ls_include_data TYPE /scwm/dlv_query_incl_str_prd.

  DATA: lo_pack     TYPE REF TO /scwm/cl_dlv_pack_ibdl,
        lo_dlv      TYPE REF TO /scwm/cl_dlv_management_prd,
        lt_prd_hdr  TYPE /scwm/dlv_header_out_prd_tab,      "#EC NEEDED
        lt_items    TYPE /scwm/dlv_item_out_prd_tab,
        ls_items    TYPE /scwm/dlv_item_out_prd_str,
        lv_entitled TYPE /scwm/de_entitled.

  DATA: lv_quan_conv  TYPE /scwm/de_quantity,
        lv_quan       TYPE /scwm/de_quantity,
        lv_idx        TYPE sy-tabix,
        lv_docu_batch TYPE xfeld.

  DATA  ls_addmeas_oq     TYPE /scdl/dl_addmeas_str.
  DATA  lv_quan_orig      TYPE /scwm/de_quantity.
  DATA  lv_max_qty        TYPE /scdl/dl_quantity.
  DATA  lv_round_qty      TYPE /scwm/de_quantity.
  DATA  ls_status         TYPE /scdl/dl_status_str.
  DATA  lv_dco_open       TYPE xfeld.
  DATA  lv_dsp_finished   TYPE xfeld.
  DATA  lv_quan_actu      TYPE /scwm/de_quantity.

  FIELD-SYMBOLS: <docid>    TYPE /scwm/dlv_docid_item_str,
                 <fs_items> TYPE /scwm/dlv_item_out_prd_str.
  FIELD-SYMBOLS: <fs_items_2> TYPE /scwm/dlv_item_out_prd_str,
                 <fs_free>    TYPE /scwm/dlv_hu_prd_str.

  CLEAR: ct_open_items.

* preselection to replace the method lo_pack->init with lo_pack->init_rf
* here we select only the relevant items by matid
  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.

  CLEAR ls_dlv_sel.
  LOOP AT ct_docid ASSIGNING <docid>.
    ls_dlv_sel-fieldname = gc_dl_logfname_docid.
    ls_dlv_sel-option    = wmegc_option_eq.
    ls_dlv_sel-low       = <docid>-docid.
    APPEND ls_dlv_sel TO lt_dlv_sel.
  ENDLOOP.

  ls_read_options-item_part_select = 'X'.
  ls_read_options-data_retrival_only = 'X'.
  ls_read_options-mix_in_object_instances = /scwm/if_dl_c=>sc_mix_in_load_instance.

  ls_include_data-item_hierarchy = abap_true.
  ls_include_data-item_addmeas   = abap_true.
* query has to be called with header selection only to include
* newly created data in the BO
* for memory usage reasons an empty include_data is passed
  TRY.
      CALL METHOD lo_dlv->query
        EXPORTING
          it_selection    = lt_dlv_sel
          is_read_options = ls_read_options
          is_include_data = ls_include_data
        IMPORTING
          et_items        = lt_items.
    CATCH /scdl/cx_delivery.
  ENDTRY.

* filter on product
  LOOP AT lt_items ASSIGNING <fs_items>
    WHERE product-productid = cs_rehu_prod-matid.

    ls_docid-docid = <fs_items>-docid.
    ls_docid-itemid = <fs_items>-itemid.
    APPEND ls_docid TO lt_docid.
  ENDLOOP.

* for mass HU creation, product does not exist in inbound delivery item is not supported.
  IF gv_mass_hu_create = abap_true AND lt_docid IS INITIAL.
    MESSAGE e823(/scwm/rf_en) .
  ENDIF.

  READ TABLE lt_items INTO ls_items INDEX 1.

* in an EWM Inbound Delivery, all items have always the same Entitled
  lv_entitled = ls_items-sapext-entitled.
* BBD rel?
  TRY.
      CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
        EXPORTING
          iv_matid      = cs_rehu_prod-matid
          iv_lgnum      = iv_lgnum
          iv_entitled   = lv_entitled
        IMPORTING
          es_mat_global = ls_mat_global.
    CATCH /scwm/cx_md.
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

  cs_rehu_prod-batch_req = ls_mat_global-batch_req.





  IF lt_docid IS NOT INITIAL.
    IF lo_pack IS NOT BOUND.
      CREATE OBJECT lo_pack.
    ENDIF.

    CALL METHOD lo_pack->init_rf
      EXPORTING
        iv_lgnum      = iv_lgnum
        it_docid      = lt_docid
        iv_doccat     = wmegc_doccat_pdi
      IMPORTING
        ev_docu_batch = lv_docu_batch.

    CALL METHOD lo_pack->get_free
      IMPORTING
        et_free = lt_free.

*   filter for the different material in the delivery
    DELETE lt_free WHERE stock-matid NE cs_rehu_prod-matid.
  ENDIF.

* Convert quantity
  MOVE cs_rehu_prod-nista TO lv_quan.

* Loop over the items and check if the UoM is the same
* if not convert and at the end compare the QtY.
  LOOP AT lt_free ASSIGNING <fs_free>.
    lv_idx = sy-tabix.

*   UoM conversion if the Delivery UoM <> selected UoM
    IF <fs_free>-open_qty-uom <> cs_rehu_prod-altme.

      TRY .
          CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
            EXPORTING
              iv_matid     = cs_rehu_prod-matid
              iv_quan      = lv_quan
              iv_unit_from = cs_rehu_prod-altme
              iv_unit_to   = <fs_free>-open_qty-uom
              iv_batchid   = <fs_free>-stock-batchid
            IMPORTING
              ev_quan      = lv_quan_conv.
        CATCH /scwm/cx_md_interface /scwm/cx_md_batch_required
              /scwm/cx_md_internal_error
              /scwm/cx_md_batch_not_required
              /scwm/cx_md_material_exist.
      ENDTRY.
    ELSE.
      lv_quan_conv = cs_rehu_prod-nista.
    ENDIF.

*   Round Quantity
    lv_round_qty = <fs_free>-open_qty-qty - lv_quan_conv.
    IF /qos/cl_qty_aux=>round_qty(  lv_round_qty  ) = 0.
      lv_quan_conv = <fs_free>-open_qty-qty.
    ENDIF.

*   if the requested quantity < than we need => delete
    IF <fs_free>-open_qty-qty < lv_quan_conv.

      READ TABLE lt_items ASSIGNING <fs_items>
        WITH KEY docid = <fs_free>-prd_id
                itemid = <fs_free>-item_id.

*     tolerance check should run for main item only.
      READ TABLE <fs_items>-hierarchy WITH KEY hierarchy_level = '1'
                                               hierarchy_type  = 'BSP' TRANSPORTING NO FIELDS.
      IF sy-subrc IS INITIAL.
        DELETE lt_free INDEX lv_idx.
        CONTINUE.
      ENDIF.

*     check if the delivery item has enough quantity with tolerance
      IF <fs_items>-delterm-tol_overunltd IS NOT INITIAL.
*       unlimited tolerance => receiving should be possible =>
      ELSEIF <fs_items>-delterm-tol_overpct IS NOT INITIAL.
*       get the original item qty
        READ TABLE <fs_items>-addmeas INTO ls_addmeas_oq
          WITH KEY qty_role     = /scdl/if_dl_addmeas_c=>sc_qtyrole_oq
                   qty_category = /scdl/if_dl_addmeas_c=>sc_qtycat_request.

        IF ls_addmeas_oq-uom <> <fs_free>-open_qty-uom.

          TRY.
*           Convert quantity
              MOVE ls_addmeas_oq-qty TO lv_quan.
            CATCH cx_sy_conversion_overflow. "cx_sy_arithmetic_overflow.
              MESSAGE e605(/scwm/rf_en) WITH cs_rehu_prod-nista.
          ENDTRY.

          TRY .
              CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
                EXPORTING
                  iv_matid     = cs_rehu_prod-matid
                  iv_quan      = lv_quan
                  iv_unit_from = ls_addmeas_oq-uom
                  iv_unit_to   = <fs_free>-open_qty-uom
                  iv_batchid   = <fs_free>-stock-batchid
                IMPORTING
                  ev_quan      = lv_quan_orig.
            CATCH /scwm/cx_md_interface /scwm/cx_md_batch_required
                  /scwm/cx_md_internal_error
                  /scwm/cx_md_batch_not_required
                  /scwm/cx_md_material_exist.
          ENDTRY.
        ELSE.
          lv_quan_orig = ls_addmeas_oq-qty.
        ENDIF.

        IF <fs_items>-qty-uom <> <fs_free>-open_qty-uom.
          TRY.
*           Convert quantity
              MOVE <fs_items>-qty-qty TO lv_quan.
            CATCH cx_sy_conversion_overflow. "cx_sy_arithmetic_overflow.
*             error handling
          ENDTRY.

          TRY .
              CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
                EXPORTING
                  iv_matid     = cs_rehu_prod-matid
                  iv_quan      = lv_quan
                  iv_unit_from = <fs_items>-qty-uom
                  iv_unit_to   = <fs_free>-open_qty-uom
                  iv_batchid   = <fs_free>-stock-batchid
                IMPORTING
                  ev_quan      = lv_quan_actu.
            CATCH /scwm/cx_md_interface /scwm/cx_md_batch_required
                  /scwm/cx_md_internal_error
                  /scwm/cx_md_batch_not_required
                  /scwm/cx_md_material_exist.
          ENDTRY.
        ELSE.
          lv_quan_actu = <fs_items>-qty-qty.
        ENDIF.

*       calculate the maximum possible quantity
*       original item qty + tolerance - packed qty
        lv_max_qty = lv_quan_orig +
                     ( lv_quan_orig * <fs_items>-delterm-tol_overpct / 100 ) -
                     lv_quan_actu +  " actual qty on main item
                     <fs_free>-open_qty-qty. " free qty on main item
        IF lv_max_qty GE lv_quan_conv.
*         item is suitalble for receiving =>
        ELSE.
*         no further options, look for EGR/PO
          DELETE lt_free INDEX lv_idx.
        ENDIF.
      ELSE.
*       no tolerance set, look for EGR/PO
        DELETE lt_free INDEX lv_idx.
      ENDIF.
    ENDIF.

  ENDLOOP.

*  Batch Requirement must be already filled here in the structure!
*  TRY.
*    CALL METHOD /scwm/cl_batch_appl=>batch_reqmt
*      EXPORTING
*        iv_matnr       = cs_rehu_prod-matnr
*      RECEIVING
*        rv_batch_reqmt = cs_rehu_prod-batch_req.
*  ENDTRY.



  IF cs_rehu_prod-batch_req IS NOT INITIAL OR
     lv_docu_batch IS NOT INITIAL.


    IF cs_rehu_prod-batch_req IS NOT INITIAL.
      ls_matid_charg-matid = cs_rehu_prod-matid.
      ls_matid_charg-charg = cs_rehu_prod-charg.
      TRY.
          CALL METHOD /scwm/cl_batch_appl=>get_batchid_by_no
            EXPORTING
              iv_entitled    = lv_entitled
              is_matid_charg = ls_matid_charg
            IMPORTING
              ev_batchid     = cs_rehu_prod-batchid.
        CATCH /scwm/cx_batch_precheck .                 "#EC NO_HANDLER
*       no problem if the batch does not exists. maybe it will be created later
      ENDTRY.

*     filter for the different batch in the delivery
      DELETE lt_free WHERE stock-batchid NE cs_rehu_prod-batchid.
    ENDIF.

*   check for inactive items from the delivery
    IF lt_free IS INITIAL.

      IF lo_dlv IS NOT BOUND.
        CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
          RECEIVING
            eo_instance = lo_dlv.
      ENDIF.

      CLEAR: ls_read_options, ls_include_data.
      ls_read_options-mix_in_object_instances =  /scwm/if_dl_c=>sc_mix_in_load_no_new_inst.
      ls_read_options-data_retrival_only = 'X'.
      ls_include_data-item_status = 'X'.
      ls_include_data-head_status = 'X'.
      ls_include_data-item_hierarchy = 'X'.
      ls_include_data-item_addmeas = 'X'.
      ls_include_data-item_refdoc = 'X'.

      TRY.
          CALL METHOD lo_dlv->query
            EXPORTING
              it_docid        = ct_docid
              is_read_options = ls_read_options
              is_include_data = ls_include_data
            IMPORTING
              et_headers      = lt_prd_hdr
              et_items        = lt_items.
        CATCH /scdl/cx_delivery.                        "#EC NO_HANDLER
          MESSAGE ID     sy-msgid
                  TYPE   sy-msgty
                  NUMBER sy-msgno
                  WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.


*     searching for inactive batch required items
*     the batch is not yet filled
      LOOP AT lt_items ASSIGNING <fs_items>
        WHERE product-productid = cs_rehu_prod-matid.

*       Check if the UoMs are the same => if not convert
        IF <fs_items>-qty-uom <> cs_rehu_prod-altme.
          TRY .
              CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
                EXPORTING
                  iv_matid     = cs_rehu_prod-matid
                  iv_quan      = lv_quan
                  iv_unit_from = cs_rehu_prod-altme
                  iv_unit_to   = <fs_items>-qty-uom
                  iv_batchid   = <fs_items>-batchid
                IMPORTING
                  ev_quan      = lv_quan_conv.
            CATCH /scwm/cx_md_interface /scwm/cx_md_batch_required
                  /scwm/cx_md_internal_error
                  /scwm/cx_md_batch_not_required
                  /scwm/cx_md_material_exist.
          ENDTRY.
        ELSE.
          lv_quan_conv = cs_rehu_prod-nista.
        ENDIF.

*       Round Quantity
        lv_round_qty = <fs_items>-qty-qty - lv_quan_conv.
        IF /qos/cl_qty_aux=>round_qty(  lv_round_qty  ) = 0.
          lv_quan_conv = <fs_items>-qty-qty.
        ENDIF.

*       If the Del. quantity in the same Measure is lower
*       continue => not relevant item!!
        IF <fs_items>-qty-qty < lv_quan_conv.

*         check if the delivery item has enough quantity with tolerance
          IF <fs_items>-delterm-tol_overunltd IS NOT INITIAL.
*           unlimited tolerance => receiving should be possible =>
          ELSEIF <fs_items>-delterm-tol_overpct IS NOT INITIAL.

*           get the original item qty
            READ TABLE <fs_items>-addmeas INTO ls_addmeas_oq
              WITH KEY qty_role     = /scdl/if_dl_addmeas_c=>sc_qtyrole_oq
                       qty_category = /scdl/if_dl_addmeas_c=>sc_qtycat_request.

            IF ls_addmeas_oq-uom <> <fs_items>-qty-uom.

              TRY.
*               Convert quantity
                  MOVE ls_addmeas_oq-qty TO lv_quan.
                CATCH cx_sy_conversion_overflow. "cx_sy_arithmetic_overflow.
                  MESSAGE e605(/scwm/rf_en) WITH cs_rehu_prod-nista.
              ENDTRY.

              TRY .
                  CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
                    EXPORTING
                      iv_matid     = cs_rehu_prod-matid
                      iv_quan      = lv_quan
                      iv_unit_from = ls_addmeas_oq-uom
                      iv_unit_to   = <fs_items>-qty-uom
                      iv_batchid   = <fs_items>-batchid
                    IMPORTING
                      ev_quan      = lv_quan_orig.
                CATCH /scwm/cx_md_interface /scwm/cx_md_batch_required
                      /scwm/cx_md_internal_error
                      /scwm/cx_md_batch_not_required
                      /scwm/cx_md_material_exist.
              ENDTRY.
            ELSE.
              lv_quan_orig = ls_addmeas_oq-qty.
            ENDIF.

*           calculate the maximum possible quantity
*           open quantity + tolerance
            lv_max_qty = <fs_items>-qty-qty +
                         ( lv_quan_orig * <fs_items>-delterm-tol_overpct / 100 ).

            IF lv_max_qty GE lv_quan_conv.
*             item is suitalble for receiving =>
            ELSE.
*             no further options, look for EGR/PO
              CONTINUE.
            ENDIF.
          ELSE.
*           no tolerance set, look for EGR/PO
            CONTINUE.
          ENDIF.
        ENDIF.

        READ TABLE <fs_items>-status WITH KEY status_type = 'DBD'
                                              status_value = 'X'
                                     TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          ls_free-prd_no     = <fs_items>-docno.
          ls_free-prd_id     = <fs_items>-docid.
          ls_free-doccat_prd = <fs_items>-doccat.
          ls_free-item_no    = <fs_items>-itemno.
          ls_free-item_id    = <fs_items>-itemid.
          ls_free-open_qty-qty = <fs_items>-qty-qty.
          ls_free-open_qty-uom = <fs_items>-qty-uom.
          APPEND ls_free TO lt_free.
        ENDIF.
      ENDLOOP.

*     searching for already batch splitted items
*     they are at top of the hierarchy
*     if there exist a batch splitted item the delivery is not inactive any more
      LOOP AT lt_items ASSIGNING <fs_items>
      WHERE product-productid = cs_rehu_prod-matid.
        CLEAR: ls_free,
               lv_dco_open.

        READ TABLE <fs_items>-hierarchy WITH KEY hierarchy_level = '0'
                                     TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          ls_free-prd_no     = <fs_items>-docno.
          ls_free-prd_id     = <fs_items>-docid.
          ls_free-doccat_prd = <fs_items>-doccat.
          ls_free-item_no    = <fs_items>-itemno.
          ls_free-item_id    = <fs_items>-itemid.
          ls_free-open_qty-qty = <fs_items>-qty-qty.
          ls_free-open_qty-uom = <fs_items>-qty-uom.

*         If batch split is finished and the closing indicator is communicated back to ERP, no more
*         split is allowed.
          READ TABLE <fs_items>-status WITH KEY status_type = 'DSP'
                                                status_value = /scdl/if_dl_status_c=>sc_v_finished
                                       TRANSPORTING NO FIELDS.
          IF sy-subrc IS INITIAL.
            lv_dsp_finished = abap_true.
          ENDIF.

*         search for the already existing batch subitemms
*         quantity reduction
          LOOP AT lt_items ASSIGNING <fs_items_2>
            WHERE product-productid = cs_rehu_prod-matid.

            READ TABLE <fs_items_2>-hierarchy WITH KEY parent_object = <fs_items>-itemid
                                     TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              ls_free-open_qty-qty = ls_free-open_qty-qty - <fs_items_2>-qty-qty.
            ELSE.
              CONTINUE. "other main items's subitem.
            ENDIF.

*           We can use the item only, if there is at least 1 BSP subitem with DCO status not 0 or 9.
            READ TABLE <fs_items_2>-status WITH KEY status_type = 'DCO' INTO ls_status.
            IF sy-subrc IS INITIAL AND
               ls_status <> /scdl/if_dl_status_c=>sc_v_not_relevant AND
               ls_status <> /scdl/if_dl_status_c=>sc_v_finished.
              lv_dco_open = abap_true.
            ENDIF.
          ENDLOOP.

*         cannot expand subitems
          IF lv_dsp_finished IS NOT INITIAL AND
             lv_dco_open IS INITIAL.
            CONTINUE.
          ENDIF.

*         Round Quantity
          lv_round_qty = ls_free-open_qty-qty - lv_quan_conv.
          IF /qos/cl_qty_aux=>round_qty(  lv_round_qty  ) = 0.
            lv_quan_conv = ls_free-open_qty-qty.
          ENDIF.

          IF ls_free-open_qty-qty < lv_quan_conv.
*           check if the delivery item has enough quantity with tolerance
            IF <fs_items>-delterm-tol_overunltd IS NOT INITIAL.
*             unlimited tolerance => receiving should be possible =>
            ELSEIF <fs_items>-delterm-tol_overpct IS NOT INITIAL.

*             get the original item qty
              READ TABLE <fs_items>-addmeas INTO ls_addmeas_oq
                WITH KEY qty_role     = /scdl/if_dl_addmeas_c=>sc_qtyrole_oq
                         qty_category = /scdl/if_dl_addmeas_c=>sc_qtycat_request.

              IF ls_addmeas_oq-uom <> ls_free-open_qty-uom.

                TRY.
*                 Convert quantity
                    MOVE ls_addmeas_oq-qty TO lv_quan.
                  CATCH cx_sy_conversion_overflow. "cx_sy_arithmetic_overflow.
                    MESSAGE e605(/scwm/rf_en) WITH cs_rehu_prod-nista.
                ENDTRY.

                TRY .
                    CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
                      EXPORTING
                        iv_matid     = cs_rehu_prod-matid
                        iv_quan      = lv_quan
                        iv_unit_from = ls_addmeas_oq-uom
                        iv_unit_to   = ls_free-open_qty-uom
                        iv_batchid   = ls_free-stock-batchid
                      IMPORTING
                        ev_quan      = lv_quan_orig.
                  CATCH /scwm/cx_md_interface /scwm/cx_md_batch_required
                        /scwm/cx_md_internal_error
                        /scwm/cx_md_batch_not_required
                        /scwm/cx_md_material_exist.
                ENDTRY.
              ELSE.
                lv_quan_orig = ls_addmeas_oq-qty.
              ENDIF.

*             calculate the maximum possible quantity
*             original item qty + tolerance - packed qty
              lv_max_qty = lv_quan_orig +
                           ( lv_quan_orig * <fs_items>-delterm-tol_overpct / 100 ) -
                           <fs_items>-qty-qty +  " actual qty on main item
                           ls_free-open_qty-qty. " free qty on main item

              IF lv_max_qty GE lv_quan_conv.
*               item is suitalble for receiving =>
              ELSE.
*               no further options, look for EGR/PO
                CONTINUE.
              ENDIF.
            ELSE.
*             no tolerance set, look for EGR/PO
              CONTINUE.
            ENDIF.
          ENDIF.
          APPEND ls_free TO lt_free.
        ENDIF.
      ENDLOOP.

    ENDIF.

    "---- PPO Segmentation integration --- begin ----"
    " For items refer to PPO, we have to ensure that the stock segment entered should be identical to item's
    IF ib_sgt_mnaged = abap_true.
      DATA: lb_stkseg_identical TYPE boole_d.
      PERFORM is_stkseg_identical TABLES lt_free
                                         lt_items
                                   USING cs_rehu_prod
                                CHANGING lb_stkseg_identical.
      IF lb_stkseg_identical = abap_false.
        MESSAGE e898(/scwm/rf_en) WITH cs_rehu_prod-stk_seg.
      ENDIF.
    ENDIF.
    "---- PPO Segmentation integration --- end ----"

    ct_open_items = lt_free.

  ELSE.
*   remaining product available to pack
    ct_open_items = lt_free.
  ENDIF.

ENDFORM.                    " check_for_open_quantity



-------------------
-------------------
------------------

-------------------------------------------------------------------*
*&      Form  deivery_table_fill
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DOCID  text
*      -->P_CT_REHU_DLV  text
*----------------------------------------------------------------------*
FORM delivery_table_fill  USING    it_docid TYPE /scwm/dlv_docid_item_tab
                                   lv_all TYPE xfeld
*                                  it_hus TYPE /scwm/tt_rf_rehu_hu
                         CHANGING ct_rehu_dlv TYPE /scwm/tt_rf_unlo_docid.


  DATA: ls_read_options TYPE /scwm/dlv_query_contr_str,
        lt_prd_hdr      TYPE /scwm/dlv_header_out_prd_tab,
        lt_id           TYPE /scmb/mdl_prt_id_tab,
        ls_id           TYPE /scmb/mdl_prtid_str,
        lt_items        TYPE /scwm/dlv_item_out_prd_tab,
        ls_items        TYPE /scwm/dlv_item_out_prd_str,
        ls_partyloc     TYPE /scdl/dl_partyloc_str,
        ls_rehu_dlv     TYPE /scwm/s_rf_unlo_docid,
        lt_partner      TYPE STANDARD TABLE OF /scmb/mdl_partner,
        ls_partner      TYPE /scmb/mdl_partner,
        ls_docid        TYPE /scwm/dlv_docid_item_str

        .

  DATA: lo_dlv TYPE REF TO /scwm/cl_dlv_management_prd.

  FIELD-SYMBOLS: <fs_dlv> TYPE /scwm/dlv_header_out_prd_str.

  CLEAR: ct_rehu_dlv.

  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.

  ls_read_options-mix_in_object_instances = /scwm/if_dl_c=>sc_mix_in_load_no_new_inst.
  ls_read_options-data_retrival_only = 'X'.

  TRY.
      CALL METHOD lo_dlv->query
        EXPORTING
          it_docid        = it_docid
          is_read_options = ls_read_options
        IMPORTING
          et_headers      = lt_prd_hdr
          et_items        = lt_items.
    CATCH /scdl/cx_delivery.                            "#EC NO_HANDLER
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

  READ TABLE lt_items INTO ls_items INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE ls_items-partyloc INTO ls_partyloc WITH KEY party_role = 'VENDOR'.
  IF sy-subrc EQ 0.
    ls_rehu_dlv-partyno   = ls_partyloc-partyno.
    ls_rehu_dlv-door      = ls_items-sapext-/scwm/door.

    ls_id-partner_guid = ls_partyloc-partyid.
    APPEND ls_id TO lt_id.


    TRY.
        CALL FUNCTION '/SCMB/MDL_PARTNER_READ_MULTI'
          EXPORTING
            it_id   = lt_id
          IMPORTING
            et_data = lt_partner.
      CATCH
        /scmb/cx_mdl_interface
        /scmb/cx_mdl_result_empty
        /scmb/cx_mdl_result_toomany
        /scmb/cx_mdl_result
        /scmb/cx_mdl.
        MESSAGE ID     sy-msgid
                TYPE   sy-msgty
                NUMBER sy-msgno
                WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.

    READ TABLE lt_partner INTO ls_partner INDEX 1.
    ls_rehu_dlv-partytext = ls_partner-name.
  ENDIF.

  LOOP AT lt_prd_hdr ASSIGNING <fs_dlv>.
    ls_rehu_dlv-dlvno   = <fs_dlv>-docno.
    ls_rehu_dlv-docid   = <fs_dlv>-docid.
    ls_rehu_dlv-doccat  = <fs_dlv>-doccat.
    MOVE-CORRESPONDING <fs_dlv> TO ls_rehu_dlv.
*   Store vendor in deliveries
    READ TABLE <fs_dlv>-partyloc INTO ls_partyloc
      WITH KEY party_role = 'SFPRT'.
    IF sy-subrc = 0.
      ls_rehu_dlv-partyno   = ls_partyloc-partyno.
      ls_rehu_dlv-partytext = ls_partyloc-party_text.
    ENDIF.
    APPEND ls_rehu_dlv TO ct_rehu_dlv.
  ENDLOOP.

  IF lv_all IS INITIAL.
    LOOP AT it_docid INTO ls_docid.
      CLEAR ls_rehu_dlv.
      ls_rehu_dlv-item_id = ls_docid-itemid.
      ls_rehu_dlv-docid  = ls_docid-docid.
      MODIFY ct_rehu_dlv FROM ls_rehu_dlv TRANSPORTING item_id WHERE docid = ls_rehu_dlv-docid.
    ENDLOOP.
  ENDIF.

  SORT ct_rehu_dlv BY docid.
  DELETE ADJACENT DUPLICATES FROM ct_rehu_dlv COMPARING docid.

ENDFORM.                    " deivery_table_fill



--------------------
-------------------
--------------------

    -->P_CS_REHU_LGNUM  text
*      -->P_CS_REHU_PROD  text
*      -->P_CS_REHU_HU  text
*----------------------------------------------------------------------*
FORM check_egr_exists_for_item  USING    iv_lgnum TYPE /scwm/lgnum
                                         cs_rehu_prod TYPE /scwm/s_rf_rehu_prod
                                         cs_rehu_hu TYPE /scwm/s_rf_rehu_hu
                                CHANGING  ct_prd_hdr TYPE /scwm/dlv_header_out_prd_tab
                                          ct_items TYPE /scwm/dlv_item_out_prd_tab.

  DATA: ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
        ls_selection    TYPE /scwm/dlv_selection_str,
        lt_selection    TYPE /scwm/dlv_selection_tab,
        ls_read_options TYPE /scwm/dlv_query_contr_str,
        ls_t300_md      TYPE /scwm/s_t300_md.

  DATA: lo_egr_manager TYPE REF TO /scwm/if_egr_manager,
        lt_prd_hdr     TYPE /scwm/dlv_header_out_prd_tab.


  FIELD-SYMBOLS <fs_prd_hdr> TYPE /scwm/dlv_header_out_prd_str.

  CALL FUNCTION '/SCWM/T300_MD_READ_SINGLE'
    EXPORTING
      iv_lgnum   = iv_lgnum
    IMPORTING
      es_t300_md = ls_t300_md
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR ls_selection.
  ls_selection-fieldname = /scwm/if_dl_logfname_c=>sc_prodind_prdi_h.
  ls_selection-sign      = wmegc_sign_inclusive.
  ls_selection-option    = wmegc_option_eq.
  APPEND ls_selection TO lt_selection.
  CLEAR ls_selection.
  ls_selection-fieldname = /scdl/if_dl_logfname_c=>sc_locationno_wh_h.
  ls_selection-sign      = wmegc_sign_inclusive.
  ls_selection-option    = wmegc_option_eq.
  ls_selection-low       = ls_t300_md-sc_unit.
  APPEND ls_selection TO lt_selection.
  CLEAR ls_selection.
  ls_selection-fieldname = /scdl/if_dl_logfname_c=>sc_productno_i.
  ls_selection-sign      = wmegc_sign_inclusive.
  ls_selection-option    = wmegc_option_eq.
  ls_selection-low       = cs_rehu_prod-matnr.
  APPEND ls_selection TO lt_selection.
  CLEAR ls_selection.
  ls_selection-fieldname = /scdl/if_dl_logfname_c=>sc_partyno_sfprt_h.
  ls_selection-sign      = wmegc_sign_inclusive.
  ls_selection-option    = wmegc_option_eq.
  ls_selection-low       = cs_rehu_hu-partyno.
  APPEND ls_selection TO lt_selection.

  ls_read_options-mix_in_object_instances =  /scwm/if_dl_c=>sc_mix_in_load_instance.
  ls_read_options-data_retrival_only = 'X'.
  ls_include_data-item_status = 'X'.
  ls_include_data-item_partyloc = 'X'.
  ls_include_data-head_status = 'X'.
  ls_include_data-head_status_dyn = 'X'.

* create EGR manager
  IF lo_egr_manager IS NOT BOUND.
    DATA(lo_saf) = /scdl/cl_af_management=>get_instance( ).
    lo_egr_manager ?= lo_saf->get_service( EXPORTING iv_service = /scwm/if_egr_manager=>sc_service ).
  ENDIF.

  TRY.
      CALL METHOD lo_egr_manager->query
        EXPORTING
          it_selection    = lt_selection
          is_read_options = ls_read_options
          is_include_data = ls_include_data
        IMPORTING
          et_headers      = ct_prd_hdr
          et_items        = ct_items.

    CATCH /scdl/cx_delivery.                            "#EC NO_HANDLER
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

* delete non relevant items
  DELETE ct_items WHERE product-productno NE cs_rehu_prod-matnr.

  DELETE ct_items WHERE NOT (
      product-batchno EQ cs_rehu_prod-charg OR
      product-batchno IS INITIAL ).

  LOOP AT ct_prd_hdr ASSIGNING <fs_prd_hdr>.

    READ TABLE ct_items
         WITH KEY docid = <fs_prd_hdr>-docid
         TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      APPEND <fs_prd_hdr> TO lt_prd_hdr.
    ENDIF.

  ENDLOOP.

  CLEAR ct_prd_hdr.
  ct_prd_hdr = lt_prd_hdr.

ENDFORM.                    " check_egr_exists_for_item

----------------
----------------
-----------------


---------------------------------------------------------------------*
*&      Form  deivery_table_fill
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DOCID  text
*      -->P_CT_REHU_DLV  text
*----------------------------------------------------------------------*
FORM delivery_table_fill  USING    it_docid TYPE /scwm/dlv_docid_item_tab
                                   lv_all TYPE xfeld
*                                  it_hus TYPE /scwm/tt_rf_rehu_hu
                         CHANGING ct_rehu_dlv TYPE /scwm/tt_rf_unlo_docid.


  DATA: ls_read_options TYPE /scwm/dlv_query_contr_str,
        lt_prd_hdr      TYPE /scwm/dlv_header_out_prd_tab,
        lt_id           TYPE /scmb/mdl_prt_id_tab,
        ls_id           TYPE /scmb/mdl_prtid_str,
        lt_items        TYPE /scwm/dlv_item_out_prd_tab,
        ls_items        TYPE /scwm/dlv_item_out_prd_str,
        ls_partyloc     TYPE /scdl/dl_partyloc_str,
        ls_rehu_dlv     TYPE /scwm/s_rf_unlo_docid,
        lt_partner      TYPE STANDARD TABLE OF /scmb/mdl_partner,
        ls_partner      TYPE /scmb/mdl_partner,
        ls_docid        TYPE /scwm/dlv_docid_item_str

        .

  DATA: lo_dlv TYPE REF TO /scwm/cl_dlv_management_prd.

  FIELD-SYMBOLS: <fs_dlv> TYPE /scwm/dlv_header_out_prd_str.

  CLEAR: ct_rehu_dlv.

  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.

  ls_read_options-mix_in_object_instances = /scwm/if_dl_c=>sc_mix_in_load_no_new_inst.
  ls_read_options-data_retrival_only = 'X'.

  TRY.
      CALL METHOD lo_dlv->query
        EXPORTING
          it_docid        = it_docid
          is_read_options = ls_read_options
        IMPORTING
          et_headers      = lt_prd_hdr
          et_items        = lt_items.
    CATCH /scdl/cx_delivery.                            "#EC NO_HANDLER
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

  READ TABLE lt_items INTO ls_items INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE ls_items-partyloc INTO ls_partyloc WITH KEY party_role = 'VENDOR'.
  IF sy-subrc EQ 0.
    ls_rehu_dlv-partyno   = ls_partyloc-partyno.
    ls_rehu_dlv-door      = ls_items-sapext-/scwm/door.

    ls_id-partner_guid = ls_partyloc-partyid.
    APPEND ls_id TO lt_id.


    TRY.
        CALL FUNCTION '/SCMB/MDL_PARTNER_READ_MULTI'
          EXPORTING
            it_id   = lt_id
          IMPORTING
            et_data = lt_partner.
      CATCH
        /scmb/cx_mdl_interface
        /scmb/cx_mdl_result_empty
        /scmb/cx_mdl_result_toomany
        /scmb/cx_mdl_result
        /scmb/cx_mdl.
        MESSAGE ID     sy-msgid
                TYPE   sy-msgty
                NUMBER sy-msgno
                WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.

    READ TABLE lt_partner INTO ls_partner INDEX 1.
    ls_rehu_dlv-partytext = ls_partner-name.
  ENDIF.

  LOOP AT lt_prd_hdr ASSIGNING <fs_dlv>.
    ls_rehu_dlv-dlvno   = <fs_dlv>-docno.
    ls_rehu_dlv-docid   = <fs_dlv>-docid.
    ls_rehu_dlv-doccat  = <fs_dlv>-doccat.
    MOVE-CORRESPONDING <fs_dlv> TO ls_rehu_dlv.
*   Store vendor in deliveries
    READ TABLE <fs_dlv>-partyloc INTO ls_partyloc
      WITH KEY party_role = 'SFPRT'.
    IF sy-subrc = 0.
      ls_rehu_dlv-partyno   = ls_partyloc-partyno.
      ls_rehu_dlv-partytext = ls_partyloc-party_text.
    ENDIF.
    APPEND ls_rehu_dlv TO ct_rehu_dlv.
  ENDLOOP.

  IF lv_all IS INITIAL.
    LOOP AT it_docid INTO ls_docid.
      CLEAR ls_rehu_dlv.
      ls_rehu_dlv-item_id = ls_docid-itemid.
      ls_rehu_dlv-docid  = ls_docid-docid.
      MODIFY ct_rehu_dlv FROM ls_rehu_dlv TRANSPORTING item_id WHERE docid = ls_rehu_dlv-docid.
    ENDLOOP.
  ENDIF.

  SORT ct_rehu_dlv BY docid.
  DELETE ADJACENT DUPLICATES FROM ct_rehu_dlv COMPARING docid.

ENDFORM.                    " deivery_table_fill



---------------
--------------
---------------


---------------------------------------------------------------------*
*      -->P_CS_REHU_PROD  text
*      -->P_CS_REHU_DLV  text
*      <--P_LV_FAILED  text
*----------------------------------------------------------------------*
FORM check_min_rem_sl  USING    cs_rehu_prod  TYPE /scwm/s_rf_rehu_prod
                                cs_rehu_dlv   TYPE /scwm/s_rf_unlo_docid
                       CHANGING ev_failed     TYPE boole_d.

  DATA: lt_docid        TYPE        /scwm/dlv_docid_item_tab,
        ls_docid        TYPE        /scwm/dlv_docid_item_str,
        lo_dlv          TYPE REF TO /scwm/cl_dlv_management_prd,
        ls_read_options TYPE        /scwm/dlv_query_contr_str,
        lt_prd_hdr      TYPE        /scwm/dlv_header_out_prd_tab,
        ls_prd_hdr      TYPE        /scwm/dlv_header_out_prd_str,
        lt_items        TYPE        /scwm/dlv_item_out_prd_tab,
        ls_item         TYPE        /scwm/dlv_item_out_prd_str,
        ls_date         TYPE        /scdl/dl_date_str,
        ls_status       TYPE        /scdl/dl_status_str,
        lv_batch        TYPE        /scdl/dl_batchno,
        ls_batch_md     TYPE        /scwm/dlv_md_prod_batch_det,
        lo_md_access    TYPE REF TO /scwm/cl_dlv_md_access,
        lv_vfdat        TYPE        /scwm/sled,
        lv_matnr        TYPE        /scwm/de_rf_matnr,
        ls_mat_global   TYPE  /scwm/s_material_global,
        lv_entitled     TYPE /scwm/de_entitled.

  DATA: lo_message      TYPE REF TO /scdl/cl_dm_message.

  CLEAR ev_failed.


  IF lo_dlv IS NOT BOUND.
    CALL METHOD /scwm/cl_dlv_management_prd=>get_instance
      RECEIVING
        eo_instance = lo_dlv.
  ENDIF.

  ls_read_options-mix_in_object_instances = /scwm/if_dl_c=>sc_mix_in_load_instance.
  ls_read_options-data_retrival_only = 'X'.

  ls_docid-docid  = cs_rehu_dlv-docid.
  ls_docid-itemid = cs_rehu_dlv-item_id.
  ls_docid-doccat = cs_rehu_dlv-doccat.

  APPEND ls_docid TO lt_docid.

  TRY.
      CALL METHOD lo_dlv->query
        EXPORTING
          it_docid        = lt_docid
          is_read_options = ls_read_options
        IMPORTING
          et_items        = lt_items.
    CATCH /scdl/cx_delivery.                            "#EC NO_HANDLER
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

  READ TABLE lt_items INTO ls_item INDEX 1.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.




* in an EWM Inbound Delivery, all items have always the same Entitled
  lv_entitled = ls_item-sapext-entitled.
* BBD rel?
  TRY.
      CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
        EXPORTING
          iv_matid      = cs_rehu_prod-matid
          iv_lgnum      = ls_item-sapext-/scwm/whno
          iv_entitled   = lv_entitled
        IMPORTING
          es_mat_global = ls_mat_global.
    CATCH /scwm/cx_md.
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.



  CHECK ls_mat_global-batch_req IS NOT INITIAL.



  IF ls_item-product-batchno IS NOT INITIAL.
    READ TABLE ls_item-status INTO ls_status
      WITH KEY status_type = 'DSL'.

    IF sy-subrc = 0.         "DSL status exists
      IF ls_status-status_value = /scdl/if_dl_status_c=>sc_v_checked_not_ok.

        ev_failed = 'X'.
      ENDIF.
    ENDIF.

    RETURN.
  ELSE.
*   search for a suitable subitem
    CLEAR lt_docid.
    CLEAR ls_docid.
    ls_docid-docid  = cs_rehu_dlv-docid.
    ls_docid-doccat = cs_rehu_dlv-doccat.

    APPEND ls_docid TO lt_docid.

    ls_read_options-mix_in_object_instances =  /scwm/if_dl_c=>sc_mix_in_load_no_new_inst.

    TRY.
        CALL METHOD lo_dlv->query
          EXPORTING
            it_docid        = lt_docid
            is_read_options = ls_read_options
          IMPORTING
            et_headers      = lt_prd_hdr
            et_items        = lt_items.
      CATCH /scdl/cx_delivery.                          "#EC NO_HANDLER
        MESSAGE ID     sy-msgid
                TYPE   sy-msgty
                NUMBER sy-msgno
                WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.

    LOOP AT lt_items INTO ls_item WHERE product-batchno = cs_rehu_prod-charg AND
               product-productid = cs_rehu_prod-matid.
      READ TABLE ls_item-hierarchy TRANSPORTING NO FIELDS
          WITH KEY hierarchy_type = 'BSP'
                   parent_object  = cs_rehu_dlv-item_id.
      IF sy-subrc = 0.
        READ TABLE ls_item-status INTO ls_status
          WITH KEY status_type = 'DSL'.

        IF sy-subrc = 0.         "DSL status exists
          IF ls_status-status_value = /scdl/if_dl_status_c=>sc_v_checked_not_ok.
            ev_failed = 'X'.
          ENDIF.
        ENDIF.

        RETURN.
      ENDIF.
    ENDLOOP.

*   subitem does not exists, do the check based on the customizing
    lv_batch =  cs_rehu_prod-charg.

    IF lo_md_access IS NOT BOUND.
      lo_md_access = /scwm/cl_dlv_md_access=>get_instance( ).
    ENDIF.

    TRY.
        IF abap_true = lo_md_access->get_batch_existence(
            iv_productid = cs_rehu_prod-matid
            iv_batchno   = lv_batch
            iv_lgnum     = ls_item-sapext-/scwm/whno
            iv_entitled  = ls_item-sapext-entitled ).

*         Conversion Exit for product
          CALL FUNCTION 'CONVERSION_EXIT_MDLP1_INPUT'
            EXPORTING
              input       = cs_rehu_prod-matnr
            IMPORTING
              output      = lv_matnr
            EXCEPTIONS
              input_error = 1
              OTHERS      = 2.

          ls_batch_md = lo_md_access->get_batch_detail(
            iv_productno         = lv_matnr
            iv_batchno           = lv_batch
            iv_lgnum             = ls_item-sapext-/scwm/whno
            iv_entitled          = ls_item-sapext-entitled
            iv_no_classification = abap_false ).

          lv_vfdat = ls_batch_md-sled_bbd.

        ELSE.
          lv_vfdat = cs_rehu_prod-bbdat.
        ENDIF.
      CATCH /scwm/cx_dlv_batch .

    ENDTRY.

    READ TABLE lt_prd_hdr INTO ls_prd_hdr INDEX 1.

    READ TABLE ls_prd_hdr-dates INTO ls_date
         WITH KEY tsttype = /scdl/if_dl_date_c=>sc_tsttype_delivery
             tst_category = /scdl/if_dl_date_c=>sc_tstcat_actual.

    IF sy-subrc <> 0.
      ls_date-tsttype = /scdl/if_dl_date_c=>sc_tsttype_delivery.
      ls_date-tst_category = /scdl/if_dl_date_c=>sc_tstcat_actual.
      GET TIME STAMP FIELD ls_date-tstfr.
      ls_date-tstto = ls_date-tstfr.

      CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
        EXPORTING
          iv_lgnum        = ls_item-sapext-/scwm/whno
        IMPORTING
          ev_tzone        = ls_date-tzone
        EXCEPTIONS
          interface_error = 1
          data_not_found  = 2
          OTHERS          = 3.

      APPEND ls_date TO ls_prd_hdr-dates.
    ENDIF.

    /scwm/cl_dlv_batch_internal=>item_val_sledbbd(
      EXPORTING
        iv_lgnum     = ls_item-sapext-/scwm/whno
        iv_docno     = ls_prd_hdr-docno
        iv_itemno    = ls_item-itemno
        iv_doccat    = ls_prd_hdr-doccat
        iv_doctype   = ls_prd_hdr-doctype
        iv_docid     = ls_prd_hdr-docid
        iv_itemcat   = ls_item-itemcat
        iv_itemtype  = ls_item-itemtype
        iv_itemid    = ls_item-itemid
        iv_action    = /scwm/if_dlv_batch_c=>sc_act_gr
        it_dates     = ls_prd_hdr-dates
        iv_productid = cs_rehu_prod-matid
        iv_vfdat     = lv_vfdat
        iv_entitled  = ls_item-sapext-entitled
      IMPORTING
        ev_failed      = ev_failed
        eo_message     = lo_message ).

  ENDIF.

ENDFORM.                    " CHECK_MIN_REM_SL



---------------
---------------
---------------

**INCLUDE /SCWM/LRF_RECEIVING_HUSF17 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  PACK_SPEC_FOR_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pack_spec_for_item USING    ct_deliveries TYPE /scwm/tt_rf_unlo_docid
                        CHANGING cs_lgnum TYPE /scwm/lgnum
                                 cs_rehu_prod TYPE /scwm/s_rf_rehu_prod
                                 cs_rehu_hu TYPE  /scwm/s_rf_rehu_hu
                                 cs_packing_rule TYPE /scwm/s_upb_pack_rule
                                 cs_levels TYPE /scwm/s_ps_level_int
                                 cs_item_packaged TYPE /scwm/s_ps_autopack
                                 cs_top_hu_quan_upb TYPE /scwm/if_autopack_upb_util=>ys_huheader_result_quan
                                 ct_huhdr_upb TYPE /scwm/if_packing_upb=>yt_huheader_result
                                 ct_huitm_ubp TYPE /scwm/if_packing_upb=>yt_huitem_result
                                 ct_huauxpmat_upb TYPE /scwm/if_packing_upb=>yt_huauxpmat_result
                                 ct_hutree_upb TYPE /scwm/if_packing_upb=>yt_hutree_result.

  DATA: lv_procedure TYPE /sapcnd/ctlist_name,
        lv_pspec     TYPE /scwm/de_matid,
        lv_doctype   TYPE /scdl/dl_doctype,
        lv_humatid   TYPE /scwm/de_matid.

  DATA: ls_fields         TYPE /scwm/pak_com_i,
        ls_ttype          TYPE /scdl/tdtype,
        ls_condition      TYPE /scwm/s_ps_cond,
        ls_levels         TYPE /scwm/s_ps_level_int,
        ls_packspec_level TYPE /scwm/s_ps_level_int,        "#EC NEEDED
        ls_data           TYPE /scwm/dlv_docid_item_str,
        ls_psp_hdr        TYPE /scwm/s_ps_header_int,       "#EC NEEDED
        lt_psp_content    TYPE /scwm/tt_packspec_nested,
        ls_psp_content    TYPE /scwm/s_packspec_nested,
        lt_packspec       TYPE /scwm/tt_guid_ps,
        lt_elementgroup   TYPE /scwm/tt_ps_elementgroup,    "#EC NEEDED
        ls_delivery       TYPE /scwm/s_rf_unlo_docid,
        ls_incl_prd_data  TYPE /scwm/dlv_query_incl_str_prd,
        ls_prd_read_opt   TYPE /scwm/dlv_query_contr_str,
        ls_prd_hdr        TYPE /scwm/dlv_header_out_prd_str,
        ls_partyloc       TYPE /scdl/dl_partyloc_str,
        ls_prd_itm        TYPE /scwm/dlv_item_out_prd_str,
        ls_item_ext_i     TYPE /scdl/dl_sap_prd_item_i_str,
        ls_transport      TYPE /scdl/dl_transport_str,
        lv_lgnum          TYPE /scwm/lgnum.

  DATA: lt_prd_itm        TYPE /scwm/dlv_item_out_prd_tab,
        lt_docid          TYPE /scwm/dlv_docid_item_tab,
        lt_prd_hdr        TYPE /scwm/dlv_header_out_prd_tab,
        lt_messages_query TYPE /scdl/dm_message_tab.

  FIELD-SYMBOLS:
        <lfs_messages>         TYPE /scdl/dm_message_str.

  DATA: lo_dlv_management_prd TYPE REF TO /scwm/cl_dlv_management_prd,
        lo_stockid_mapping    TYPE REF TO /scwm/if_stockid_mapping,
        lo_message            TYPE REF TO /scwm/cl_dm_message_no.



  READ TABLE ct_deliveries INTO ls_delivery
       WITH KEY docid = cs_rehu_hu-docid.

  ls_condition-quantity = cs_rehu_prod-nista.
  ls_condition-unit_q   = cs_rehu_prod-altme.

  IF ls_delivery-doctype IS INITIAL.
    lv_doctype = 'INB'.
  ELSE.
    lv_doctype = ls_delivery-doctype.
  ENDIF.

* Read Business Conf
  SELECT SINGLE * FROM /scdl/tdtype
    INTO ls_ttype
   WHERE type = lv_doctype AND
          category = wmegc_doccat_pdi.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
* Customer Connect: 280996 - Possibility to create one multi-level HU with UPB in RF
* Abort if no UPB profile assigned to doctype
  If ls_ttype-dlvap_pbp is initial and gv_multi_hu_upb is not initial.
   MESSAGE e005(/scwm/upb) WITH ls_ttype-type.
  endif.


* Collect determination field data
  ls_fields-pak_locid = ls_delivery-wh_locationid.

  ls_fields-pak_matid = cs_rehu_prod-matid.

* Call query (read-only)
  lo_dlv_management_prd = NEW #( ).

  lt_docid = VALUE #( ( docid = cs_rehu_hu-docid itemid = cs_rehu_hu-ritmid ) ).

  ls_incl_prd_data-head_transport = 'X'.
  ls_incl_prd_data-head_partyloc  = 'X'.
  ls_incl_prd_data-item_partyloc  = 'X'.

  ls_prd_read_opt-lock_result = abap_false.
  ls_prd_read_opt-data_retrival_only = abap_true.
  ls_prd_read_opt-mix_in_object_instances = /scwm/if_dl_c=>sc_mix_in_load_no_new_inst.

  TRY.
      CALL METHOD lo_dlv_management_prd->query
        EXPORTING
          it_docid        = lt_docid
          is_read_options = ls_prd_read_opt
          is_include_data = ls_incl_prd_data
        IMPORTING
          et_headers      = lt_prd_hdr
          et_items        = lt_prd_itm
          eo_message      = lo_message.

    CATCH /scdl/cx_delivery .
      ASSERT CONDITION 1 = 0.
  ENDTRY.
  IF lo_message IS BOUND.
    lt_messages_query = lo_message->get_messages( ).
    LOOP AT lt_messages_query ASSIGNING <lfs_messages> WHERE ( msgty = /scdl/cl_dm_message=>sc_msgty_abort AND consistency_message = abap_false )
                                                               OR ( msgty = /scdl/cl_dm_message=>sc_msgty_error AND consistency_message = abap_false ).
      MESSAGE ID <lfs_messages>-msgid TYPE <lfs_messages>-msgty NUMBER <lfs_messages>-msgno
              WITH <lfs_messages>-msgv1 <lfs_messages>-msgv2 <lfs_messages>-msgv3 <lfs_messages>-msgv4.
    ENDLOOP.
  ENDIF.

*Route ( Header )
  READ TABLE lt_prd_hdr INTO ls_prd_hdr INDEX 1.
  IF sy-subrc = 0.
    READ TABLE ls_prd_hdr-transport INTO ls_transport INDEX 1.
    IF sy-subrc = 0.
      ls_fields-pak_route = ls_transport-route_id.
    ENDIF.
  ELSE.
    RETURN.
  ENDIF.

* Item data
  READ TABLE lt_prd_itm INTO ls_prd_itm INDEX 1.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* warehouse process type
  ls_fields-pak_procty = ls_prd_itm-sapext-/scwm/procty.

* Production supply area
  ls_fields-pak_psa = ls_prd_itm-sapext-/scwm/psa.

* Country of Origin
  ls_fields-pak_cntry = ls_prd_itm-sapext-ocountry.

* Deconsolidation group
  ls_fields-pak_dist_group = ls_prd_itm-addin-dstgrp.

* ship-from
  READ TABLE ls_prd_itm-partyloc INTO ls_partyloc WITH KEY
       party_role = ls_ttype-pack_role .
  IF sy-subrc IS INITIAL.
    ls_fields-pak_ship_from = ls_partyloc-partyno.
  ELSE.
* Try to get ship- from from header instead
    READ TABLE ls_prd_hdr-partyloc INTO ls_partyloc WITH KEY
         party_role = ls_ttype-pack_role .
    IF sy-subrc IS INITIAL.
      ls_fields-pak_ship_from = ls_partyloc-partyno.
    ENDIF.
  ENDIF.
  CLEAR ls_partyloc.

* carrier
  READ TABLE ls_prd_itm-partyloc INTO ls_partyloc WITH KEY
       party_role = /scdl/if_dl_partyloc_c=>sc_party_role_carr.
  IF sy-subrc IS INITIAL.
    ls_fields-pak_carrier = ls_partyloc-partyno.
  ELSE.
* Try to get carrier from header instead
    READ TABLE ls_prd_hdr-partyloc INTO ls_partyloc WITH KEY
         party_role = /scdl/if_dl_partyloc_c=>sc_party_role_carr.
    IF sy-subrc IS INITIAL.
      ls_fields-pak_carrier = ls_partyloc-partyno.
    ENDIF.
  ENDIF.
  CLEAR ls_partyloc.

*Warehouse Number
  IF cs_lgnum IS INITIAL.
    lv_lgnum = ls_prd_itm-sapext-/scwm/whno.
  ELSE.
    lv_lgnum = cs_lgnum.
  ENDIF.


* Use the new field PAK_PLANT for determination of PS for this dtermination procedure of these processes
  CALL FUNCTION '/SCWM/GET_STOCKID_MAP_INSTANCE'
    IMPORTING
      eif_stockid_mapping = lo_stockid_mapping.
  IF lo_stockid_mapping IS NOT INITIAL.
    TRY.
        lo_stockid_mapping->get_plant_by_partnerno(
          EXPORTING
            iv_wme_partno = ls_prd_itm-sapext-entitled
          IMPORTING
            ev_erp_plant  = ls_fields-pak_plant
        ).
      CATCH /scwm/cx_stockid_map.
        CLEAR:ls_fields-pak_plant.
    ENDTRY.
  ENDIF.




  IF ls_ttype-dlvap_pbp IS NOT INITIAL.
    PERFORM determine_pack_rule_upb USING cs_lgnum
                                          ls_ttype
                                          cs_rehu_prod
                                          cs_rehu_hu
                                          ls_fields
                                    CHANGING
                                          cs_rehu_hu-pmatid
                                          cs_rehu_hu-pmat
                                          cs_rehu_hu-hutyp
                                          cs_packing_rule
                                          cs_levels
                                          cs_item_packaged
                                          cs_top_hu_quan_upb
                                          ct_huhdr_upb
                                          ct_huitm_ubp
                                          ct_huauxpmat_upb
                                          ct_hutree_upb.

    gs_levels_upb = cs_levels.
    gs_packing_rule_upb = cs_packing_rule.
    gs_item_packaged = cs_item_packaged.
    gs_top_hu_quan_upb = cs_top_hu_quan_upb.
    gt_huauxpmat_upb = ct_huauxpmat_upb.
    gt_huhdr_upb = ct_huhdr_upb.
    gt_huitm_ubp = ct_huitm_ubp.
    gt_hutree_upb = ct_hutree_upb.

* Customer Connect: 280996 - Possibility to create one HU with UPB in RF
* Abort if no packing rule was found
    IF gv_multi_hu_upb = abap_true AND ( ct_huhdr_upb IS INITIAL OR ct_huitm_ubp IS INITIAL ).
      MESSAGE e003(/scwm/upb) WITH ls_ttype-dlvap_pbp.
      RETURN.
    ENDIF.


  ELSE.
    lv_procedure = ls_ttype-dlvap.

    ls_data-docid  = cs_rehu_hu-docid.
    ls_data-itemid = cs_rehu_hu-ritmid.
    ls_data-doccat = cs_rehu_hu-rdoccat.

    CALL FUNCTION '/SCWM/PS_FIND_AND_EVALUATE'
      EXPORTING
        is_fields       = ls_fields
        iv_procedure    = lv_procedure
        is_condition    = ls_condition
        i_data          = ls_data
      IMPORTING
        et_packspec     = lt_packspec
      EXCEPTIONS
        determine_error = 1
        read_error      = 2
        no_record_found = 3
        OTHERS          = 4.

    IF lt_packspec IS INITIAL.
      RETURN.                   " no packspec exists.
    ENDIF.

    READ TABLE lt_packspec INTO lv_pspec INDEX 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    gv_pspec = lv_pspec.

    CALL FUNCTION '/SCWM/PS_PACKSPEC_GET'
      EXPORTING
        iv_guid_ps          = lv_pspec
        iv_read_elements    = 'X'
      IMPORTING
        es_packspec_header  = ls_psp_hdr
        et_packspec_content = lt_psp_content
        et_elementgroup     = lt_elementgroup
        es_packspec_level   = ls_packspec_level
      EXCEPTIONS
        error               = 1
        OTHERS              = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    READ TABLE lt_psp_content INTO ls_psp_content INDEX 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    READ TABLE ls_psp_content-levels INTO ls_levels
      WITH KEY hu_create = abap_true.
    IF sy-subrc IS INITIAL.
      cs_rehu_hu-pmatid = ls_levels-hu_matid.
      cs_rehu_hu-pmat = ls_levels-hu_mat.
      cs_rehu_hu-hutyp = ls_levels-hutyp.
    ENDIF.
  ENDIF.



ENDFORM.                    " PACK_SPEC_FOR_ITEM


 PERFORM determine_pack_rule_upb USING cs_lgnum
                                          ls_ttype
                                          cs_rehu_prod
                                          cs_rehu_hu
                                          ls_fields
                                    CHANGING
                                          cs_rehu_hu-pmatid
                                          cs_rehu_hu-pmat
                                          cs_rehu_hu-hutyp
                                          cs_packing_rule
                                          cs_levels
                                          cs_item_packaged
                                          cs_top_hu_quan_upb
                                          ct_huhdr_upb
                                          ct_huitm_ubp
                                          ct_huauxpmat_upb
                                          ct_hutree_upb.

    
FORM determine_pack_rule_upb USING    iv_lgnum        TYPE /scwm/lgnum
                                      is_ttype       TYPE /scdl/tdtype
                                      is_rehu_prod    TYPE /scwm/s_rf_rehu_prod
                                      is_rehu         TYPE /scwm/s_rf_rehu_hu
                                      is_fields       TYPE /scwm/pak_com_i
                             CHANGING cv_humatid      TYPE /scwm/de_matid
                                      cv_packmat      type /SCWM/DE_PMAT
                                      cv_hutyp        type /scwm/de_hutyp
                                      cs_packing_rule TYPE /scwm/s_upb_pack_rule
                                      cs_levels       TYPE /scwm/s_ps_level_int
                                      cs_item_packaged TYPE /scwm/s_ps_autopack
                                      cs_top_hu_quan_upb TYPE /scwm/if_autopack_upb_util=>ys_huheader_result_quan
                                      ct_huhdr_upb TYPE /scwm/if_packing_upb=>yt_huheader_result
                                      ct_huitm_ubp TYPE /scwm/if_packing_upb=>yt_huitem_result
                                      ct_huauxpmat_upb TYPE /scwm/if_packing_upb=>yt_huauxpmat_result
                                      ct_hutree_upb TYPE /scwm/if_packing_upb=>yt_hutree_result.

  IF iv_lgnum IS INITIAL.
    RETURN.
  ENDIF.

  MOVE-CORRESPONDING is_fields TO cs_item_packaged-det.

  cs_item_packaged-cond-quantity = is_rehu_prod-nista.
  cs_item_packaged-cond-unit_q = is_rehu_prod-altme.
  cs_item_packaged-stock-qdoccat = is_ttype-category.
  cs_item_packaged-stock-qdocid = is_rehu-docid.
  cs_item_packaged-stock-qitmid = is_rehu-ritmid.
  cs_item_packaged-stock-batchid = is_rehu_prod-batchid.
  cs_item_packaged-stock-matid = is_rehu_prod-matid.
* Customer Connect: 280996 - Possibility to create one multi-level HU with UPB in RF
  IF gv_multi_hu_upb = abap_true.
    /scwm/cl_autopack_upb_util=>get_instance( )->auto_pack_single(
      EXPORTING
        iv_lgnum         = iv_lgnum
        is_profile       = VALUE #( profile_id = is_ttype-dlvap_pbp doccat = is_ttype-category doctype = is_ttype-type )
        is_item_packaged = cs_item_packaged
      IMPORTING
        et_header        = ct_huhdr_upb
        et_item          = ct_huitm_ubp
        et_auxpmat       = ct_huauxpmat_upb
        et_tree          = ct_hutree_upb
        es_quan_top_hu   = cs_top_hu_quan_upb
        eo_message       = DATA(lo_message_upb)
    ).

    IF lo_message_upb IS BOUND AND lo_message_upb->check( ).
      lo_message_upb->get_messages(
        IMPORTING
          et_bapiret = DATA(lt_bapiret)
      ).
      LOOP AT lt_bapiret ASSIGNING FIELD-SYMBOL(<ls_bapiret>) WHERE type = /scwm/cl_dm_message_no=>sc_msgty_error.
        MESSAGE ID <ls_bapiret>-id TYPE <ls_bapiret>-type NUMBER <ls_bapiret>-number
           WITH <ls_bapiret>-message_v1 <ls_bapiret>-message_v2 <ls_bapiret>-message_v3 <ls_bapiret>-message_v4.
        RETURN.
      ENDLOOP.
    ENDIF.

    READ TABLE ct_huhdr_upb WITH KEY guid_hu = cs_top_hu_quan_upb-guid_hu ASSIGNING FIELD-SYMBOL(<ls_top_hu>).
    IF sy-subrc = 0.
      cv_humatid = <ls_top_hu>-material_key.
      cv_hutyp = <ls_top_hu>-hu_type.
      cv_packmat = <ls_top_hu>-material_number.
    ENDIF.

* Get only packaging material via UPB
  ELSE.

    /scwm/cl_autopack_upb_util=>get_instance( )->get_packaging_material(
      EXPORTING
        iv_lgnum         = iv_lgnum
        is_profile       =  VALUE #( profile_id = is_ttype-dlvap_pbp doccat = is_ttype-category doctype = is_ttype-type )
        is_item_packaged = cs_item_packaged
      IMPORTING
        es_levels        = cs_levels
        es_packing_rule  = cs_packing_rule
        ev_pack_mat      = cv_humatid ).

    cv_hutyp = cs_levels-hutyp.
    cv_packmat = cs_levels-hu_mat.

  ENDIF.

ENDFORM.

----------
*INCLUDE /SCWM/LRF_RECEIVING_HUSF13 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  pack_item_to_delivery
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_QUANTITY  text
*      <--P_LS_ITEMS  text
*      <--P_CS_REHU_HU  text
*----------------------------------------------------------------------*
FORM pack_item_to_delivery USING    iv_lgnum        TYPE /scwm/lgnum
                                    iv_hu_matid     TYPE /scwm/de_matid
                                    iv_procs        TYPE /scwm/de_procs
                                    iv_prr_id       TYPE /scmb/de_prr
                                    is_packing_rule TYPE /scwm/s_upb_pack_rule
                                    is_levels       TYPE /scwm/s_ps_level_int
                                    is_item_packaged TYPE /scwm/s_ps_autopack
                                    it_huhdr_upb TYPE /scwm/if_packing_upb=>yt_huheader_result
                                    it_huitm_ubp TYPE /scwm/if_packing_upb=>yt_huitem_result
                                    it_huauxpmat_upb TYPE /scwm/if_packing_upb=>yt_huauxpmat_result
                                    it_hutree_upb TYPE /scwm/if_packing_upb=>yt_hutree_result
                           CHANGING cs_rehu_prod TYPE /scwm/s_rf_rehu_prod
                                    cs_rehu_hu   TYPE  /scwm/s_rf_rehu_hu
                                    ct_rehu_hu   TYPE  /scwm/tt_rf_rehu_hu
                                    cs_rehu      TYPE  /scwm/s_rf_admin_rehu.

  DATA: lv_lock           TYPE xfeld  ,                     "#EC NEEDED
        lt_return_code    TYPE /scdl/t_sp_return_code,      "#EC NEEDED
        lv_bbdat          TYPE /scwm/de_rf_sp_bbdat,
        lv_timezone       TYPE tznzone,
        lv_batch_crea     TYPE /scwm/dl_batchcrea,
        lv_badi_imp(30)   TYPE  c,
        ev_rejected       TYPE  boole_d,                    "#EC NEEDED
        lv_proccode_added TYPE boolean.


  DATA: ls_docid         TYPE /scwm/s_docid,
        ls_material      TYPE /scwm/s_pack_stock,
        ls_mat_global    TYPE  /scwm/s_material_global,
        ls_quantity      TYPE /scwm/s_quan,
        ls_huitm         TYPE /scwm/s_huitm_int,
        ls_huitm_out     TYPE /scwm/s_huitm_int,            "#EC NEEDED
        ls_huhdr         TYPE /scwm/s_huhdr_int,
        ls_docid_query   TYPE /scwm/dlv_docid_item_str,
        ls_read_options  TYPE /scwm/dlv_query_contr_str,
        ls_inrecords_bbd TYPE /scdl/s_sp_a_item_sapext_prdi,
        ls_message       TYPE /scdl/dm_message_str,
        ls_items         TYPE /scwm/dlv_item_out_prd_str,
        ls_items2        TYPE /scwm/dlv_item_out_prd_str,
        lv_entitled      TYPE /scwm/de_entitled,
        ls_k_item        TYPE  /scdl/s_sp_k_item,
        ls_free          TYPE /scwm/dlv_hu_prd_str,
        ls_changed       TYPE /scwm/s_changed.

  DATA: lt_docid           TYPE /scwm/tt_docid,
        lt_huitm           TYPE /scwm/tt_huitm_int,
        lt_inrecords       TYPE /scdl/t_sp_a_item_product,
        lt_inrecords_bbd   TYPE /scdl/t_sp_a_item_sapext_prdi,
        lt_outrecords_bbd  TYPE /scdl/t_sp_a_item_sapext_prdi, "#EC NEEDED
        lt_outrecords_item TYPE /scdl/t_sp_a_item_product,  "#EC NEEDED
        lt_message         TYPE /scdl/dm_message_tab,
        lt_docid_query     TYPE /scwm/dlv_docid_item_tab,
        lt_docid_tmp       TYPE /scwm/dlv_docid_item_tab,
        lt_items           TYPE /scwm/dlv_item_out_prd_tab,
        lt_k_item          TYPE  /scdl/t_sp_k_item,
        lt_huhdr_changed   TYPE /scwm/tt_changed.

  DATA: lo_pack      TYPE REF TO /scwm/cl_dlv_pack_ibdl,
        lo_dlv       TYPE REF TO /scdl/cl_sp_prd_inb,
        lo_query     TYPE REF TO /scwm/cl_dlv_management_prd,
        lo_badi_crea TYPE REF TO /scwm/ex_dlv_batch_crea,
        lo_message   TYPE REF TO  /scdl/cl_sp_message_box.

  DATA ls_inrecords_qty   TYPE /scdl/s_sp_a_item_quantity.
  DATA lt_inrecords_qty   TYPE /scdl/t_sp_a_item_quantity.
  DATA lt_outrecords_qty  TYPE /scdl/t_sp_a_item_quantity.
  DATA ls_inrecords_prod  TYPE /scdl/s_sp_a_item_product.
  DATA lt_inrecords_prod  TYPE /scdl/t_sp_a_item_product.
  DATA lt_outrecords_prod TYPE /scdl/t_sp_a_item_product.
  DATA ls_action          TYPE /scdl/s_sp_act_action.
  DATA ls_context         TYPE /scdl/s_sp_act_item_split.
  DATA lt_item_key        TYPE /scdl/t_sp_k_item.
  DATA ls_item_key        TYPE /scdl/s_sp_k_item.
  DATA lt_outrecords      TYPE /scdl/t_sp_a_item.
  DATA ls_outrecords      TYPE /scdl/s_sp_a_item.
  DATA lv_new_item_id     TYPE /scdl/dl_itemid.

  DATA lo_batch   TYPE REF TO /scwm/cl_batch_appl.

  DATA lo_bo      TYPE REF TO /scdl/if_bo.
  DATA lo_item    TYPE REF TO /scdl/cl_dl_item_write.
  DATA lo_bom     TYPE REF TO /scdl/cl_bo_management.
  DATA ls_product TYPE        /scdl/dl_product_str.
  DATA ls_sapext  TYPE        /scdl/dl_sap_dr_item_str.

  DATA lv_quantity TYPE /lime/quantity.
  DATA lv_packed_qty TYPE /lime/quantity.
  DATA lv_round_qty  TYPE /scwm/de_quantity.

  DATA ls_item_qty_upd      TYPE /scdl/s_sp_a_item_quantity.
  DATA lt_item_qty_upd      TYPE /scdl/t_sp_a_item_quantity.
  DATA lt_item_qty_upd_out  TYPE /scdl/t_sp_a_item_quantity.
  DATA lv_update            TYPE xfeld.
  DATA ls_items_upd         TYPE /scwm/dlv_item_out_prd_str.
  DATA ls_item_parent       TYPE /scwm/dlv_item_out_prd_str.
  DATA lv_quan              TYPE /scdl/dl_quantity.
  DATA: lv_no_hu     TYPE i,
        ls_hu_create TYPE /scwm/s_huhdr_create_ext,
        lv_huident   TYPE /scwm/huident,
        lt_rehu_hu   TYPE /scwm/tt_rf_rehu_hu.

  DATA lo_ewl_manager      TYPE REF TO /scwm/if_api_lm_ewl_manager.
  DATA ls_addmeas    TYPE /scdl/dl_addmeas_str.
  DATA ls_delterm    TYPE /scdl/dl_delterm_str.
  DATA lv_max_qty    TYPE /scdl/dl_quantity.
  DATA lv_diff_qty   TYPE /scdl/dl_quantity.
  DATA ls_prcode     TYPE /scdl/s_sp_a_item_prcodes.
  DATA lt_prcode     TYPE /scdl/t_sp_a_item_prcodes.
  DATA lt_prc_out    TYPE /scdl/t_sp_a_item_prcodes.
  DATA ls_addmeas_oq        TYPE /scdl/dl_addmeas_str.
  DATA lt_free              TYPE /scwm/dlv_hu_prd_tab.
  DATA lv_quan_conv         TYPE /scwm/de_quantity.
  DATA lv_quan_orig         TYPE /scwm/de_quantity.
  DATA lo_item_pc           TYPE REF TO /scdl/cl_dl_item.
  DATA ls_item_extkey       TYPE /scdl/dl_itmtype_extkey_str.
  DATA lt_item_extkey       TYPE /scdl/dl_itmtype_extkey_tab.
  DATA ls_item_bc           TYPE /scdl/dl_itype_detail_str.
  DATA lt_item_bc           TYPE /scdl/dl_itype_detail_tab.
  DATA lt_profile           TYPE /scdl/t_k_profile.
  DATA ls_profile           TYPE /scdl/s_k_profile.
  DATA lt_prcode_bc         TYPE /scdl/t_prcode.
  DATA ls_prcode_bc         TYPE /scdl/s_prcode.
  DATA lo_saf               TYPE REF TO /scdl/cl_af_management.
  DATA lo_service_bc        TYPE REF TO /scdl/if_af_business_conf.
  DATA lo_service_pc        TYPE REF TO /scdl/if_af_prcode.
  DATA lo_message_pc        TYPE REF TO /scdl/cl_dm_message_extkey.
  DATA lt_message_ext       TYPE /scdl/dm_message_extkey_tab.
  DATA ls_message_ext       TYPE /scdl/dm_message_extkey_str.
  DATA ls_mat_lgnum         TYPE /scwm/s_material_lgnum.
  DATA lv_dbatch_rel        TYPE /scwm/dl_dbatch_rel.
  DATA lv_docu_batch        TYPE xfeld.
  DATA lt_keys_master       TYPE /scdl/t_sp_k_item.
  DATA ls_keys_master       TYPE /scdl/s_sp_k_item.
  DATA lt_item_core         TYPE /scdl/t_sp_a_item.

  DATA: ls_levels           TYPE /scwm/s_ps_level_int,
        ls_packspec_level   TYPE /scwm/s_ps_level_int,
        ls_data             TYPE /scwm/dlv_docid_item_str,
        ls_psp_hdr          TYPE /scwm/s_ps_header_int,
        lt_psp_content      TYPE /scwm/tt_packspec_nested,
        ls_psp_content      TYPE /scwm/s_packspec_nested,
        lt_packspec         TYPE /scwm/tt_guid_ps,
        lt_elementgroup     TYPE /scwm/tt_ps_elementgroup,
        lv_pspec            TYPE /scwm/de_matid,
        ls_packing_rule_upb TYPE /scwm/s_upb_pack_rule,
        ls_levels_upb       TYPE /scwm/s_ps_level_int,
        ls_item_packaged    TYPE /scwm/s_ps_autopack,
        lt_item_packaged    TYPE /scwm/tt_ps_autopack,
        lt_huhdr_upb_result TYPE /scwm/tt_huhdr_int,
        lt_huitm_upb_result TYPE /scwm/tt_huitm_int,
        lt_return_upb       TYPE bapirettab,
        lv_severity_upb     TYPE bapi_mtype,
        lt_huhdr_upb        TYPE /scwm/if_packing_upb=>yt_huheader_result,
        lt_huitm_ubp        TYPE /scwm/if_packing_upb=>yt_huitem_result,
        lt_huauxpmat_upb    TYPE /scwm/if_packing_upb=>yt_huauxpmat_result,
        lt_hutree_upb       TYPE /scwm/if_packing_upb=>yt_hutree_result.

  FIELD-SYMBOLS    <fs_item> TYPE /scwm/dlv_item_out_prd_str.
  FIELD-SYMBOLS    <fs_item_2> TYPE /scwm/dlv_item_out_prd_str.
  FIELD-SYMBOLS    <ls_parameter>   TYPE  any.
  FIELD-SYMBOLS    <fs_hierarchy> TYPE /scdl/dl_hierarchy_str.
  FIELD-SYMBOLS    <ls_free>        TYPE /scwm/dlv_hu_prd_str.

  CONSTANTS:
        wmelc_subitem_no TYPE i VALUE 1.                    "Number of subitems after item split

  DATA ls_wrk_itms  TYPE /scwm/dlv_item_out_prd_str.

* set data for printing
  CALL FUNCTION '/SCWM/RF_PRINT_GLOBAL_DATA'.

  /scwm/cl_api_factory=>get_service( IMPORTING eo_api = lo_ewl_manager ).

  ls_docid_query-docid = cs_rehu_hu-docid.
  ls_docid_query-itemid = cs_rehu_hu-ritmid.
  ls_docid_query-doccat = cs_rehu_hu-rdoccat.
  APPEND ls_docid_query TO lt_docid_query.
  ls_read_options-mix_in_object_instances =  /scwm/if_dl_c=>sc_mix_in_load_instance.

  IF lo_query IS NOT BOUND.
    CREATE OBJECT lo_query.
  ENDIF.

  TRY.
      CALL METHOD lo_query->query
        EXPORTING
          it_docid        = lt_docid_query
          iv_whno         = iv_lgnum
          is_read_options = ls_read_options
        IMPORTING
          et_items        = lt_items.
    CATCH /scdl/cx_delivery .                           "#EC NO_HANDLER
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

  READ TABLE lt_items INTO ls_items INDEX 1.

  lo_bom = /scdl/cl_bo_management=>get_instance( ).
*
  lo_bo = lo_bom->get_bo_by_id( cs_rehu_hu-docid ).

  IF lo_dlv IS NOT BOUND.
    CREATE OBJECT lo_dlv.
  ENDIF.

* in an EWM Inbound Delivery, all items have always the same Entitled
  lv_entitled = ls_items-sapext-entitled.
* BBD rel?
  TRY.
      CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
        EXPORTING
          iv_matid      = cs_rehu_prod-matid
          iv_lgnum      = iv_lgnum
          iv_entitled   = lv_entitled
        IMPORTING
          es_mat_global = ls_mat_global
          es_mat_lgnum  = ls_mat_lgnum.
    CATCH /scwm/cx_md.
      MESSAGE ID     sy-msgid
              TYPE   sy-msgty
              NUMBER sy-msgno
              WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

  cs_rehu_prod-batch_req = ls_mat_global-batch_req.

  IF ls_mat_lgnum-docbatch IS NOT INITIAL.
    TRY.
        CALL METHOD /scwm/cl_dlv_batch=>check_item_dbatch
          EXPORTING
            iv_itemtype   = ls_items-itemtype
            iv_doccat     = ls_items-doccat
          RECEIVING
            rv_dbatch_rel = lv_dbatch_rel.
      CATCH /scwm/cx_dlv_conf.
*    fatal error.
        MESSAGE x049(/scwm/ui_packing).
    ENDTRY.

    IF lv_dbatch_rel IS NOT INITIAL.
      lv_docu_batch = abap_true.
    ENDIF.
  ENDIF.


* check wether the batch is already exist
  IF cs_rehu_prod-batch_req IS NOT INITIAL.
** wrong place move to the begin!!
    DATA ls_batch_md            TYPE  /scwm/dlv_md_prod_batch_det.
    DATA lo_md_access TYPE REF TO /scwm/cl_dlv_md_access.
    DATA lv_batch TYPE /scdl/dl_batchno.
    DATA lv_new_batch TYPE xfeld.

    lv_batch =  cs_rehu_prod-charg.

    lo_md_access = /scwm/cl_dlv_md_access=>get_instance( ).
    TRY.
        ls_batch_md = lo_md_access->get_batch_detail(
          iv_productno         = ls_mat_global-matnr
          iv_batchno           = lv_batch
          iv_lgnum             = iv_lgnum
          iv_entitled          = ls_items-sapext-entitled
          iv_no_classification = abap_false ).
      CATCH /scwm/cx_dlv_batch .
*     batch is not yet existing -> possible to create
        lo_item ?= lo_bo->get_item( cs_rehu_hu-ritmid ).
        ls_sapext = /scwm/cl_dlv_batch_internal=>item_get_sapext( lo_item ).
        ls_product = lo_item->get_product( ).
        GET BADI lo_badi_crea
          FILTERS
            lgnum = iv_lgnum.

        CALL BADI lo_badi_crea->check_batch_create
          EXPORTING
            iv_lgnum      = iv_lgnum
            iv_doccat     = lo_item->mv_doccat
            iv_doc_type   = lo_item->mv_doctype
            iv_itemtype   = lo_item->mv_itemtype
            iv_itemcat    = lo_item->mv_itemcat
            iv_productid  = ls_product-productid
            iv_entitled   = ls_sapext-entitled
            iv_docid      = lo_item->mv_docid
            iv_itemid     = lo_item->mv_itemid
          IMPORTING
            ev_batch_crea = lv_batch_crea.

        IF lv_batch_crea = /scwm/if_dlv_batch_c=>sc_crea_no.
          lv_badi_imp = cl_abap_classdescr=>get_class_name( lo_badi_crea->imp ).
          IF lv_badi_imp CS '/SCWM/CL_EI_DLV_BATCH_DEF'.
*             SAP default implementation, table /SCWM/TDLVBATCH used
*             -> standard message
            MESSAGE e006(/scwm/dlv_batch) WITH lo_item->mv_itemtype lo_item->mv_doctype iv_lgnum.
          ELSE.
*            Customer implementation -> reason why creation id not allowed
*            is unknown -> generic message
            MESSAGE e007(/scwm/dlv_batch).
          ENDIF.
        ENDIF.

        lv_new_batch = 'X'.
        CALL FUNCTION '/SCWM/RF_REHU_CRBA'
          EXPORTING
            cv_matid    = cs_rehu_prod-matid
            cv_matnr    = cs_rehu_prod-matnr
            cv_batch    = cs_rehu_prod-charg
            iv_lgnum    = iv_lgnum
            iv_entitled = ls_items-sapext-entitled
          IMPORTING
            ev_batchid  = cs_rehu_prod-batchid
            eo_batch    = lo_batch.

        IF lo_batch IS NOT BOUND.
          MESSAGE e894(/scwm/rf_en) WITH cs_rehu_prod-charg cs_rehu_prod-matnr.
        ENDIF.
    ENDTRY.
  ENDIF.

* The PDI may be locked and batch creation triggered in the previous try, so after we retry the
* new item creation, the batch in the memory already.
  IF lv_new_batch IS INITIAL AND
     ls_batch_md-new_batch IS NOT INITIAL.
    lv_new_batch = 'X'.
    CALL FUNCTION '/SCWM/RF_REHU_CRBA'
      EXPORTING
        cv_matid    = cs_rehu_prod-matid
        cv_matnr    = cs_rehu_prod-matnr
        cv_batch    = cs_rehu_prod-charg
        iv_lgnum    = iv_lgnum
        iv_entitled = ls_items-sapext-entitled
      IMPORTING
        ev_batchid  = cs_rehu_prod-batchid
        eo_batch    = lo_batch.

    IF lo_batch IS NOT BOUND.
      MESSAGE e894(/scwm/rf_en) WITH cs_rehu_prod-charg cs_rehu_prod-matnr.
    ENDIF.
  ENDIF.

  IF gv_mass_hu_create = abap_true.
    lv_quantity = cs_rehu_hu-nohu * cs_rehu_prod-nista.
  ELSE.
    lv_quantity = cs_rehu_prod-nista.
  ENDIF.

  IF cs_rehu_prod-altme <> ls_items-qty-uom.
    PERFORM convert_quan
      USING
        cs_rehu_prod-matid
        cs_rehu_prod-altme
        ls_items-qty-uom
        cs_rehu_prod-batchid
      CHANGING
        lv_quantity.
  ENDIF.


* creating batch if batch does not exist in the delivery
  IF ( cs_rehu_prod-batchid   IS NOT INITIAL AND
       ls_items-product-batchno IS INITIAL ) OR
     lv_docu_batch = abap_true.

*   lock only the delivery item
    CLEAR lt_k_item.
    ls_k_item-docid = cs_rehu_hu-docid.
    ls_k_item-itemid = cs_rehu_hu-ritmid.
    APPEND ls_k_item TO lt_k_item.

    lo_dlv->lock(
      EXPORTING
        inkeys       = lt_k_item
        lockmode     = /scdl/if_sp1_locking=>sc_exclusive_lock
        aspect       = /scdl/if_sp_c=>sc_asp_item
      IMPORTING
        rejected     = ev_rejected
        return_codes = lt_return_code ).

    PERFORM raise_error_sp USING lo_dlv
                                 ev_rejected
                                 lt_return_code.

    lv_lock = 'X'.

*   we may have to add processcode to increase the main item to make room for the BSP item.
*   let's check it.
    CLEAR ls_docid_query.
    ls_docid_query-docid = cs_rehu_hu-docid.
    ls_docid_query-doccat = cs_rehu_hu-rdoccat.
    APPEND ls_docid_query TO lt_docid_tmp.
    ls_read_options-mix_in_object_instances =  /scwm/if_dl_c=>sc_mix_in_load_no_new_inst.
    ls_read_options-data_retrival_only = 'X'.

    TRY.
        CALL METHOD lo_query->query
          EXPORTING
            it_docid        = lt_docid_tmp
            iv_whno         = iv_lgnum
            is_read_options = ls_read_options
          IMPORTING
            et_items        = lt_items.
      CATCH /scdl/cx_delivery .                         "#EC NO_HANDLER
        MESSAGE ID     sy-msgid
                TYPE   sy-msgty
                NUMBER sy-msgno
                WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.

    DELETE lt_items WHERE product-productid <> cs_rehu_prod-matid.

*   calculate the open qty for the batch split.
*   check if the original item is a batch split main item
    READ TABLE ls_items-hierarchy WITH KEY hierarchy_level = '0'
                                 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
*     if so collect data from the original item
      ls_free-prd_no     = ls_items-docno.
      ls_free-prd_id     = ls_items-docid.
      ls_free-doccat_prd = ls_items-doccat.
      ls_free-item_no    = ls_items-itemno.
      ls_free-item_id    = ls_items-itemid.
      ls_free-open_qty-qty = ls_items-qty-qty.
      ls_free-open_qty-uom = ls_items-qty-uom.

*     and search for already existing batch subitemms
      LOOP AT lt_items ASSIGNING <fs_item_2>
        WHERE product-productid = cs_rehu_prod-matid.

        READ TABLE <fs_item_2>-hierarchy WITH KEY parent_object = ls_items-itemid
                                 TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
*         reduce the calculated open quantity with the quantity of the subitems
          ls_free-open_qty-qty = ls_free-open_qty-qty - <fs_item_2>-qty-qty.
          lv_packed_qty = lv_packed_qty + <fs_item_2>-qty-qty.
        ENDIF.
      ENDLOOP.
    ENDIF.

*   no split occured yet.
    IF ls_free IS INITIAL.
      ls_free-open_qty-qty = ls_items-qty-qty.
      ls_free-open_qty-uom = ls_items-qty-uom.
    ENDIF.

*   Round Quantity
    lv_round_qty = ls_free-open_qty-qty - lv_quantity.
    IF /qos/cl_qty_aux=>round_qty(  lv_round_qty  ) = 0.
      lv_quantity = ls_free-open_qty-qty.
    ENDIF.

*   The packing qty is larger than open qty, need to validate the tolerance.
    IF lv_quantity > ls_free-open_qty-qty.
      IF ls_items-delterm-tol_overunltd IS NOT INITIAL.
*       unlimited tolerance => receiving should be possible =>
*       calculate the necessary difference
        lv_diff_qty = lv_quantity - ls_free-open_qty-qty.
        lv_max_qty = 9999999999999.
      ELSEIF ls_items-delterm-tol_overpct IS NOT INITIAL.
*       get the original item qty
        READ TABLE ls_items-addmeas INTO ls_addmeas_oq
          WITH KEY qty_role     = /scdl/if_dl_addmeas_c=>sc_qtyrole_oq
                   qty_category = /scdl/if_dl_addmeas_c=>sc_qtycat_request.

        IF ls_addmeas_oq-uom <> ls_free-open_qty-uom.
          TRY.
*           Convert quantity
              MOVE ls_addmeas_oq-qty TO lv_quan.
            CATCH cx_sy_conversion_overflow. "cx_sy_arithmetic_overflow.
              MESSAGE e605(/scwm/rf_en) WITH cs_rehu_prod-nista.
          ENDTRY.

          TRY .
              CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
                EXPORTING
                  iv_matid     = cs_rehu_prod-matid
                  iv_quan      = lv_quan
                  iv_unit_from = ls_addmeas_oq-uom
                  iv_unit_to   = ls_free-open_qty-uom
                  iv_batchid   = ls_free-stock-batchid
                IMPORTING
                  ev_quan      = lv_quan_orig.
            CATCH /scwm/cx_md_interface /scwm/cx_md_batch_required
                  /scwm/cx_md_internal_error
                  /scwm/cx_md_batch_not_required
                  /scwm/cx_md_material_exist.
          ENDTRY.
        ELSE.
          lv_quan_orig = ls_addmeas_oq-qty.
        ENDIF.

*       calculate the maximum possible quantity
*       original item qty + tolerance - packed qty
        lv_max_qty = lv_quan_orig +
                     ( lv_quan_orig * ls_items-delterm-tol_overpct / 100 ) -
                     lv_packed_qty.

        IF lv_max_qty GE lv_quantity.
*         item is suitalble for receiving =>
*         calculate the necessary difference
          lv_diff_qty = lv_quantity - ls_free-open_qty-qty.
        ELSE.
*         no further options, look for EGR/PO
          MESSAGE e491(/scwm/rf_en).
        ENDIF.
      ELSE.
*       no tolerance and qty does not fit for the open qty.
        MESSAGE e491(/scwm/rf_en).
      ENDIF.

    ELSE.
      lv_max_qty = ls_items-qty-qty. " it does not matter, we just need to set a larger qty than lv_quantity.
    ENDIF.

*  IF lv_new_batch IS NOT INITIAL.
*   if the quantity equal to the whole amount from
*   the delivery item tehn no batch split is necessary.
*******************
* -> CHANGE!
*    Let's force the batch split, because otherwise we cannot add new BSP.

*    IF lv_quantity LT lv_max_qty.

    IF cs_rehu_prod-batch_req IS NOT INITIAL OR
       lv_docu_batch = abap_true.

      CLEAR ls_docid_query.
      ls_docid_query-docid = cs_rehu_hu-docid.
      ls_docid_query-doccat = cs_rehu_hu-rdoccat.
      APPEND ls_docid_query TO lt_docid_tmp.
      ls_read_options-mix_in_object_instances =  /scwm/if_dl_c=>sc_mix_in_load_no_new_inst.
      ls_read_options-data_retrival_only = 'X'.

      TRY.
          CALL METHOD lo_query->query
            EXPORTING
              it_docid        = lt_docid_tmp
              iv_whno         = iv_lgnum
              is_read_options = ls_read_options
            IMPORTING
              et_items        = lt_items.
        CATCH /scdl/cx_delivery .                       "#EC NO_HANDLER
          MESSAGE ID     sy-msgid
                  TYPE   sy-msgty
                  NUMBER sy-msgno
                  WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.

*       search for already batch splitted items
      LOOP AT lt_items INTO ls_items_upd WHERE product-batchno = cs_rehu_prod-charg AND
                 product-productid = cs_rehu_prod-matid AND
                 manual = 'X'.
        READ TABLE ls_items_upd-hierarchy ASSIGNING <fs_hierarchy> WITH KEY hierarchy_type = 'BSP'  parent_object = cs_rehu_hu-ritmid.
        IF sy-subrc = 0.
          lv_update = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

    ENDIF.

*******************************************************************************************************************
* The qty increase with a process code is tricky, we need to follow some rules. Especially if the dlv has tolerance
* 1. You cannot add process code to a match main item if the qty has reached
* NEW item
* -> we have qty on main item
*    add proccode to the main item and create new BSP with 0 qty and later you can increase the qty on the BSP item
* -> no open qty on main item
*    create the new BSP item with 0 qty and add process code to the new BSP item
*
* update existing BSP item
* -> we have qty on main item
*    add process code to the main item and later update the qty on the BSP item
* -> no open qty on the main item
*    add process code to the BSP item
*
* The diff qty contains that qty what we have to use for the process code, just pay attention to do the math
* correctly and add the proccode on the right level.
*******************************************************************************************************************


*     new BSP item will be created
    IF lv_update IS INITIAL.

*       increase mainitem qty before we update the BSP item
      IF lv_diff_qty IS NOT INITIAL AND ls_free-open_qty-qty IS NOT INITIAL.
        lv_lock = 'X'.
        PERFORM add_proc_code_2_increase_qty USING    lv_diff_qty
                                                      ls_free-open_qty-uom
                                                      ls_items
                                             CHANGING ev_rejected.
        lv_proccode_added = abap_true.
        CLEAR lv_diff_qty.
      ENDIF.

*       item key
      CLEAR lt_item_key.
      ls_item_key-docid  = cs_rehu_hu-docid.
      ls_item_key-itemid = cs_rehu_hu-ritmid.
      APPEND ls_item_key TO lt_item_key.

*       create a new batch subitem
      ls_context-hierarchy_type    = /scdl/if_dl_hierarchy_c=>sc_type_charge.
      ls_context-number_subitems   = wmelc_subitem_no.
      ls_action-action_code        = /scdl/if_bo_action_c=>sc_split_item.

      CREATE DATA ls_action-action_control TYPE ('/SCDL/S_SP_ACT_ITEM_SPLIT').
      ASSIGN ls_action-action_control->* TO <ls_parameter>.
      MOVE-CORRESPONDING ls_context TO <ls_parameter>.

      lo_dlv->execute(
        EXPORTING
          aspect       = /scdl/if_sp_c=>sc_asp_item
          inkeys       = lt_item_key
          inparam      = ls_action
          action       = /scdl/if_sp_c=>sc_act_execute_action
        IMPORTING
          outrecords   = lt_outrecords
          rejected     = ev_rejected
          return_codes = lt_return_code ).

      PERFORM raise_error_sp USING lo_dlv
                                   ev_rejected
                                   lt_return_code.

*       find the new subitem
      DELETE lt_outrecords WHERE itemid = ls_item_key-itemid.
      READ TABLE lt_outrecords INTO ls_outrecords WITH KEY docid = ls_items-docid.
      lv_new_item_id = ls_outrecords-itemid.
      cs_rehu_prod-ritmid = lv_new_item_id.
      cs_rehu_hu-ritmid   = lv_new_item_id.

*       update quantity
      ls_inrecords_qty-docid  = ls_items-docid.
      ls_inrecords_qty-itemid = lv_new_item_id.
*       increase main item qty before we update the BSP item
      IF ls_free-open_qty-qty IS NOT INITIAL OR
         lv_proccode_added IS NOT INITIAL.
        ls_inrecords_qty-qty    = lv_quantity.
      ELSE.
        ls_inrecords_qty-qty    = 0.
      ENDIF.
      ls_inrecords_qty-uom    =  ls_items-qty-uom.
      APPEND ls_inrecords_qty TO lt_inrecords_qty.

      CALL METHOD lo_dlv->/scdl/if_sp1_aspect~update
        EXPORTING
          aspect       = /scdl/if_sp_c=>sc_asp_item_quantity
          inrecords    = lt_inrecords_qty
        IMPORTING
          outrecords   = lt_outrecords_qty
          rejected     = ev_rejected
          return_codes = lt_return_code.

      PERFORM raise_error_sp USING lo_dlv
                                   ev_rejected
                                   lt_return_code.

*       increase subitem qty before we update the BSP item
      IF lv_diff_qty IS NOT INITIAL.
        lv_lock = 'X'.
        ls_items-itemid = lv_new_item_id.
        PERFORM add_proc_code_2_increase_qty USING    lv_diff_qty
                                                      ls_free-open_qty-uom
                                                      ls_items
                                             CHANGING ev_rejected.
      ENDIF.

      "--------------------------------------------------------------------------"
      "              PPO segmentation Integration                                "
      "--------------------------------------------------------------------------"
      " Update the bbd & ss of item for item which refers to Production Order, as the segmentation is validated while update the product
      " the segmentation validataion for PPO relevant ITEM is wrote on method UPD_ITEM_PRODUCT of class /SCDL/CL_SP
      TRY.
          DATA: lv_warehousezone TYPE tznzone.
          lo_item ?= lo_bo->get_item( lv_new_item_id ).
          " only update the property of sapext when the item refers to PPO
          IF cs_rehu_prod-bbdat IS NOT INITIAL AND lv_new_batch IS NOT INITIAL AND lo_item->get_refdoc( iv_refdoccat = 'PPO' ) IS NOT INITIAL.

            PERFORM get_warehouse_tzone USING iv_lgnum CHANGING lv_warehousezone.
            CONVERT DATE cs_rehu_prod-bbdat INTO TIME STAMP DATA(lv_bbd) TIME ZONE lv_warehousezone.

            PERFORM upd_item_sapext_property USING lo_item 'TZONEBB' lv_warehousezone.
            PERFORM upd_item_sapext_property USING lo_item 'TSTFRBB' lv_bbd.
            PERFORM upd_item_sapext_property USING lo_item 'TSTTOBB' lv_bbd.
            PERFORM upd_item_sapext_property USING lo_item 'STK_SEG_LONG' cs_rehu_prod-stk_seg.

            " update BATCH's characteristics here, therefore the next action doesn't raise exception
            TRY.
                IF lo_batch->mo_valuat_mng IS BOUND.
                  /scwm/cl_dlv_batch_internal=>item_batch_valuate(
                    iv_lgnum = iv_lgnum
                    io_item  = lo_item
                    io_batch = lo_batch ).
                ENDIF.
              CATCH /scwm/cx_dlv_batch
                    /scwm/cx_dlv_chval.
                MESSAGE e008(/scwm/batch).
            ENDTRY.

          ENDIF.
        CATCH cx_sy_move_cast_error .
          " ignore this error, which will be raised by next form raise_error_sp
      ENDTRY.
      "--------------------------------------------------------------------------"
      "              PPO segmentation Integration                                "
      "--------------------------------------------------------------------------"

*       update batch to delivery
      ls_inrecords_prod-docid          = ls_items-docid.
      ls_inrecords_prod-itemid         = cs_rehu_hu-ritmid.
      ls_inrecords_prod-productid      = ls_items-product-productid.
      ls_inrecords_prod-productno      = ls_items-product-productno.
      ls_inrecords_prod-productno_ext  = ls_items-product-productno_ext.
      ls_inrecords_prod-batchno        = cs_rehu_prod-charg.
      ls_inrecords_prod-productent     = ls_items-product-productent.
      ls_inrecords_prod-product_text   = ls_items-product-product_text.
      APPEND ls_inrecords_prod TO lt_inrecords_prod.

      CALL METHOD lo_dlv->/scdl/if_sp1_aspect~update
        EXPORTING
          aspect       = /scdl/if_sp_c=>sc_asp_item_product
          inrecords    = lt_inrecords_prod
        IMPORTING
          outrecords   = lt_outrecords_prod
          rejected     = ev_rejected
          return_codes = lt_return_code.

      PERFORM raise_error_sp USING lo_dlv
                                   ev_rejected
                                   lt_return_code.


*       redetermine main item and BSP, to get rid of the blocked status, because there was no BSP yet.
      CLEAR lt_item_key.
      ls_item_key-docid  = ls_items-docid.
      ls_item_key-itemid = ls_items-itemid.
      APPEND ls_item_key TO lt_item_key.

      ls_item_key-docid  = cs_rehu_hu-docid.
      ls_item_key-itemid = cs_rehu_hu-ritmid.
      APPEND ls_item_key TO lt_item_key.

      ls_action-action_code        = /scdl/if_bo_action_c=>sc_validate.
      CLEAR ls_action-action_control.

      lo_dlv->execute(
        EXPORTING
          aspect       = /scdl/if_sp_c=>sc_asp_item
          inkeys       = lt_item_key
          inparam      = ls_action
          action       = /scdl/if_sp_c=>sc_act_execute_action
        IMPORTING
          outrecords   = lt_outrecords
          rejected     = ev_rejected
          return_codes = lt_return_code ).


      PERFORM raise_error_sp USING lo_dlv
                                   ev_rejected
                                   lt_return_code.


      lo_item ?= lo_bo->get_item( lv_new_item_id ).
      ls_product = lo_item->get_product( ).

    ELSE.
*       update existing BSP item
      ls_items = ls_items_upd.

      IF lv_diff_qty IS NOT INITIAL.
        ls_items2 = ls_items.
        IF ls_free-open_qty-qty IS NOT INITIAL.
*           increase main item qty before we update the BSP item
          READ TABLE lt_items INTO ls_items2 WITH KEY docid  = ls_free-prd_id
                                                      itemid = ls_free-item_id.
        ENDIF.
        lv_lock = 'X'.
        PERFORM add_proc_code_2_increase_qty USING    lv_diff_qty
                                                      ls_free-open_qty-uom
                                                      ls_items2
                                             CHANGING ev_rejected.
        lv_proccode_added = abap_true.
      ENDIF.

      cs_rehu_prod-ritmid = ls_items-itemid.
      cs_rehu_hu-ritmid   = ls_items-itemid.

      IF ls_free-open_qty-qty IS NOT INITIAL.
        lv_quan = lv_quantity. "it is calculated earlier, must take into accoundt the mulHU!

        CLEAR: ls_item_qty_upd, lt_item_qty_upd.
        ls_item_qty_upd-docid = ls_items-docid.
        ls_item_qty_upd-itemid = ls_items-itemid.
        ls_item_qty_upd-qty = ls_items-qty-qty + lv_quan.
        ls_item_qty_upd-uom = ls_items-qty-uom.
        APPEND ls_item_qty_upd TO lt_item_qty_upd.

        CALL METHOD lo_dlv->/scdl/if_sp1_aspect~update
          EXPORTING
            aspect       = /scdl/if_sp_c=>sc_asp_item_quantity
            inrecords    = lt_item_qty_upd
          IMPORTING
            outrecords   = lt_item_qty_upd_out
            rejected     = ev_rejected
            return_codes = lt_return_code.

        PERFORM raise_error_sp USING lo_dlv
                                     ev_rejected
                                     lt_return_code.
      ENDIF.

      lo_item ?= lo_bo->get_item( ls_items-itemid ).
      ls_product = lo_item->get_product( ).

    ENDIF.

*    ELSE.
*
*      lo_item ?= lo_bo->get_item( cs_rehu_hu-ritmid  ).
*      ls_product = lo_item->get_product( ).
*
*    ENDIF.

    ls_sapext = /scwm/cl_dlv_batch_internal=>item_get_sapext( lo_item ).

* It is required to do the determinations not just with existing batch but for new batches too.
    CLEAR lt_item_key.
    ls_item_key-docid  = cs_rehu_hu-docid.
    ls_item_key-itemid = cs_rehu_hu-ritmid.
    APPEND ls_item_key TO lt_item_key.

    ls_action-action_code        = /scdl/if_bo_action_c=>sc_determine.
    CLEAR ls_action-action_control.

    lo_dlv->execute(
      EXPORTING
        aspect       = /scdl/if_sp_c=>sc_asp_item
        inkeys       = lt_item_key
        inparam      = ls_action
        action       = /scdl/if_sp_c=>sc_act_execute_action
      IMPORTING
        outrecords   = lt_outrecords
        rejected     = ev_rejected
        return_codes = lt_return_code ).

    PERFORM raise_error_sp USING lo_dlv
                                 ev_rejected
                                 lt_return_code.

*   update delivery for BBD if on the screen we added the BBD
    IF cs_rehu_prod-bbdat IS NOT INITIAL AND lv_new_batch IS NOT INITIAL.

      CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
        EXPORTING
          iv_lgnum        = iv_lgnum
        IMPORTING
          ev_tzone        = lv_timezone
        EXCEPTIONS
          interface_error = 1
          data_not_found  = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CONVERT TIME STAMP ls_items-sapext-tsttobb TIME ZONE lv_timezone INTO DATE lv_bbdat.

      IF cs_rehu_prod-bbdat NE ls_batch_md-sled_bbd AND
         ls_batch_md-sled_bbd IS NOT INITIAL.
        cs_rehu_prod-bbdat = ls_batch_md-sled_bbd.
      ENDIF.

      IF cs_rehu_prod-bbdat IS NOT INITIAL  AND
         lv_bbdat IS NOT INITIAL AND
         cs_rehu_prod-bbdat <> lv_bbdat.
        MESSAGE e436(/scwm/rf_en) WITH cs_rehu_prod-bbdat.
      ENDIF.

*     update the batch to the delivery in case of new batch
      IF cs_rehu_prod-batch_req IS NOT INITIAL.

        MOVE-CORRESPONDING ls_items-sapext TO  ls_inrecords_bbd.

        ls_inrecords_bbd-docid   = cs_rehu_hu-docid.
        ls_inrecords_bbd-itemid  = cs_rehu_hu-ritmid.
        CONVERT DATE cs_rehu_prod-bbdat
              INTO TIME STAMP ls_inrecords_bbd-tstfrbb
              TIME ZONE lv_timezone.
        ls_inrecords_bbd-tsttobb = ls_inrecords_bbd-tstfrbb.
        ls_inrecords_bbd-tzonebb = lv_timezone.
        " Stock Segment
        ls_inrecords_bbd-stk_seg_long = cs_rehu_prod-stk_seg.

        APPEND ls_inrecords_bbd TO lt_inrecords_bbd.

*     update the BBD to the delivery item
        CALL METHOD lo_dlv->/scdl/if_sp1_aspect~update
          EXPORTING
            aspect       = /scdl/if_sp_c=>sc_asp_item_sapext_prdi
            inrecords    = lt_inrecords_bbd
          IMPORTING
            outrecords   = lt_outrecords_bbd
            rejected     = ev_rejected
            return_codes = lt_return_code.

        PERFORM raise_error_sp USING lo_dlv
                                     ev_rejected
                                     lt_return_code.
      ENDIF.

    ENDIF.


    IF lv_new_batch IS NOT INITIAL.
*   update the valuation data
      TRY.
          IF lo_batch->mo_valuat_mng IS BOUND.
            /scwm/cl_dlv_batch_internal=>item_batch_valuate(
              iv_lgnum = iv_lgnum
              io_item  = lo_item
              io_batch = lo_batch ).
          ENDIF.
        CATCH /scwm/cx_dlv_batch
              /scwm/cx_dlv_chval.
          MESSAGE e008(/scwm/batch).
      ENDTRY.

*   save the batch
      TRY.
          lo_batch->before_save( ).

        CATCH /scwm/cx_batch_management.
          MESSAGE ID     sy-msgid
                  TYPE   sy-msgty
                  NUMBER sy-msgno
                  WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      ENDTRY.

      /scwm/cl_batch_appl=>save( ).
    ENDIF.

  ELSE.

    IF ls_mat_global-batch_req IS NOT INITIAL.

*   update delivery for BBD if on the screen we added the BBD
      IF cs_rehu_prod-bbdat  IS NOT INITIAL.

        CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
          EXPORTING
            iv_lgnum        = iv_lgnum
          IMPORTING
            ev_tzone        = lv_timezone
          EXCEPTIONS
            interface_error = 1
            data_not_found  = 2
            OTHERS          = 3.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        CONVERT TIME STAMP ls_items-sapext-tsttobb TIME ZONE lv_timezone INTO DATE lv_bbdat.

        IF cs_rehu_prod-bbdat IS NOT INITIAL  AND
           lv_bbdat IS NOT INITIAL AND
           cs_rehu_prod-bbdat <> lv_bbdat.
          MESSAGE e436(/scwm/rf_en) WITH cs_rehu_prod-bbdat.
        ENDIF.

*     lock only the delivery item
        CLEAR lt_k_item.
        ls_k_item-docid = cs_rehu_hu-docid.
        ls_k_item-itemid = cs_rehu_hu-ritmid.
        APPEND ls_k_item TO lt_k_item.

        lo_dlv->lock(
          EXPORTING
            inkeys       = lt_k_item
            lockmode     = /scdl/if_sp1_locking=>sc_exclusive_lock
            aspect       = /scdl/if_sp_c=>sc_asp_item
          IMPORTING
            rejected     = ev_rejected
            return_codes = lt_return_code ).

        PERFORM raise_error_sp USING lo_dlv
                                     ev_rejected
                                     lt_return_code.

        lv_lock = 'X'.

        MOVE-CORRESPONDING ls_items-sapext TO  ls_inrecords_bbd.

        ls_inrecords_bbd-docid   = cs_rehu_hu-docid.
        ls_inrecords_bbd-itemid  = cs_rehu_hu-ritmid.
        CONVERT DATE cs_rehu_prod-bbdat
              INTO TIME STAMP ls_inrecords_bbd-tstfrbb
              TIME ZONE lv_timezone.
        ls_inrecords_bbd-tsttobb = ls_inrecords_bbd-tstfrbb.
        APPEND ls_inrecords_bbd TO lt_inrecords_bbd.

*       update the BBD to the delivery item
        CALL METHOD lo_dlv->/scdl/if_sp1_aspect~update
          EXPORTING
            aspect       = /scdl/if_sp_c=>sc_asp_item_sapext_prdi
            inrecords    = lt_inrecords_bbd
          IMPORTING
            outrecords   = lt_outrecords_bbd
            rejected     = ev_rejected
            return_codes = lt_return_code.

        PERFORM raise_error_sp USING lo_dlv
                                     ev_rejected
                                     lt_return_code.
      ENDIF.

    ENDIF.

  ENDIF.

* to receive the quantity we need to increase item quantity
* to do it we need to know the actual open quantity
  IF lo_pack IS NOT BOUND.
    CREATE OBJECT lo_pack.
  ENDIF.
**************************START: Mixed HU*****************************
*******check if it is mixed hu case ( pack product into existing HU instead of create empty HU)
  DATA: ls_huhdr_exist TYPE /scwm/s_huhdr_int.
  CALL METHOD lo_pack->get_hu
    EXPORTING
      iv_huident = cs_rehu_hu-huident
      iv_lock    = abap_true
    IMPORTING
      es_huhdr   = ls_huhdr_exist
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.
  IF sy-subrc = 0.
    DATA(lf_hu_exist) = abap_true.
  ENDIF.
**************************END: Mixed HU*******************************
  CLEAR lt_docid_query.
  CLEAR ls_docid_query.
  ls_docid_query-docid = cs_rehu_hu-docid.
  ls_docid_query-itemid = cs_rehu_hu-ritmid.
  ls_docid_query-doccat = cs_rehu_hu-rdoccat.
  APPEND ls_docid_query TO lt_docid_query.

  CALL METHOD lo_pack->init_rf
    EXPORTING
      iv_lgnum  = iv_lgnum
      it_docid  = lt_docid_query
      iv_doccat = wmegc_doccat_pdi.

  CALL METHOD lo_pack->get_free
    IMPORTING
      et_free = lt_free.

* filter for the different material in the delivery
  DELETE lt_free WHERE stock-matid NE cs_rehu_prod-matid.
* filter for the different material in the delivery
  IF ls_mat_global-batch_req IS NOT INITIAL.
    DELETE lt_free WHERE stock-batchid NE cs_rehu_prod-batchid.
  ENDIF.
  READ TABLE lt_free ASSIGNING <ls_free> INDEX 1.

  IF gv_mass_hu_create = abap_true.
    lv_quantity = cs_rehu_hu-nohu * cs_rehu_prod-nista.
  ELSE.
    lv_quantity = cs_rehu_prod-nista.
  ENDIF.

* check if the field symbol is assigned to avoid short dump
* this is an unexpected case and we don't know the root cause yet (not reproducible)
  IF <ls_free> IS NOT ASSIGNED.
    MESSAGE e290(/scwm/rf_en).
  ENDIF.

* UoM conversion if the Delivery UoM <> selected UoM
  IF <ls_free>-open_qty-uom <> cs_rehu_prod-altme.

    TRY .
*     Convert quantity
        MOVE lv_quantity TO lv_quan.
      CATCH cx_sy_conversion_overflow. "cx_sy_arithmetic_overflow.
        MESSAGE e605(/scwm/rf_en) WITH cs_rehu_prod-nista.
    ENDTRY.

    TRY .
        CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
          EXPORTING
            iv_matid     = cs_rehu_prod-matid
            iv_quan      = lv_quan
            iv_unit_from = cs_rehu_prod-altme
            iv_unit_to   = <ls_free>-open_qty-uom
            iv_batchid   = <ls_free>-stock-batchid
          IMPORTING
            ev_quan      = lv_quan_conv.
      CATCH /scwm/cx_md_interface /scwm/cx_md_batch_required
            /scwm/cx_md_internal_error
            /scwm/cx_md_batch_not_required
            /scwm/cx_md_material_exist.
    ENDTRY.
  ELSE.
    lv_quan_conv = lv_quantity.
  ENDIF.

  CLEAR lv_diff_qty.

* Round Quantity
  lv_round_qty = <ls_free>-open_qty-qty - lv_quan_conv.
  IF /qos/cl_qty_aux=>round_qty(  lv_round_qty  ) = 0.
    lv_quan_conv = <ls_free>-open_qty-qty.
  ENDIF.

* check if the delivery item has enough quantity alone
  IF <ls_free>-open_qty-qty GE lv_quan_conv.
*   we have enough open quantity, continue the process

  ELSE.
*   check if the delivery item has enough quantity with tolerance
    IF ls_items-delterm-tol_overunltd IS NOT INITIAL.
*     unlimited tolerance => receiving should be possible =>
*     calculate the necessary difference
      lv_diff_qty = lv_quan_conv - <ls_free>-open_qty-qty.

    ELSEIF ls_items-delterm-tol_overpct IS NOT INITIAL.
*     get the original item qty
      READ TABLE ls_items-addmeas INTO ls_addmeas_oq
        WITH KEY qty_role     = /scdl/if_dl_addmeas_c=>sc_qtyrole_oq
                 qty_category = /scdl/if_dl_addmeas_c=>sc_qtycat_request.

      IF ls_addmeas_oq-uom <> <ls_free>-open_qty-uom.
        TRY.
*         Convert quantity
            MOVE ls_addmeas_oq-qty TO lv_quan.
          CATCH cx_sy_conversion_overflow. "cx_sy_arithmetic_overflow.
            MESSAGE e605(/scwm/rf_en) WITH cs_rehu_prod-nista.
        ENDTRY.

        TRY .
            CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
              EXPORTING
                iv_matid     = cs_rehu_prod-matid
                iv_quan      = lv_quan
                iv_unit_from = ls_addmeas_oq-uom
                iv_unit_to   = <ls_free>-open_qty-uom
                iv_batchid   = <ls_free>-stock-batchid
              IMPORTING
                ev_quan      = lv_quan_orig.
          CATCH /scwm/cx_md_interface /scwm/cx_md_batch_required
                /scwm/cx_md_internal_error
                /scwm/cx_md_batch_not_required
                /scwm/cx_md_material_exist.
        ENDTRY.
      ELSE.
        lv_quan_orig = ls_addmeas_oq-qty.
      ENDIF.

*     calculate the maximum possible quantity
*     original item qty + tolerance - packed qty
      lv_max_qty = lv_quan_orig +
                   ( lv_quan_orig * ls_items-delterm-tol_overpct / 100 ) -
                   <ls_free>-packed_qty-qty.

      IF lv_max_qty GE lv_quan_conv.
*       item is suitalble for receiving =>
*       calculate the necessary difference
        lv_diff_qty = lv_quan_conv - <ls_free>-open_qty-qty.
      ELSE.
*       no further options, look for EGR/PO
        RETURN.
      ENDIF.
    ENDIF.

*   add process code
    IF lv_diff_qty IS NOT INITIAL.
      lv_lock = 'X'.
      PERFORM add_proc_code_2_increase_qty USING    lv_diff_qty
                                                    <ls_free>-open_qty-uom
                                                    ls_items
                                           CHANGING ev_rejected.
    ENDIF.
  ENDIF.

* if new item was created, inbound delivery must be saved as well
  IF ls_items-objchg = /scdl/if_dl_c=>sc_objchg_create.
    lv_lock = 'X'.
  ENDIF.

* in case of any update in the delivery
  IF lo_dlv IS BOUND AND lv_lock IS NOT INITIAL.

    CLEAR: ev_rejected, lt_return_code.
*   necessary checks before saving
    CALL METHOD lo_dlv->/scdl/if_sp1_transaction~before_save
      IMPORTING
        rejected = ev_rejected.

    PERFORM raise_error_sp USING lo_dlv
                                 ev_rejected
                                 lt_return_code.

*   save the delivery
    CALL METHOD lo_dlv->/scdl/if_sp1_transaction~save
      IMPORTING
        rejected = ev_rejected.

    PERFORM raise_error_sp USING lo_dlv
                                 ev_rejected
                                 lt_return_code.

    COMMIT WORK AND WAIT.

    CALL METHOD /scwm/cl_tm=>cleanup( ).

  ENDIF.

* PACKING
  CALL METHOD /scwm/cl_tm=>cleanup( ).

  IF lo_pack IS NOT BOUND.
    CREATE OBJECT lo_pack.
  ENDIF.

  CLEAR lt_docid_query.
  CLEAR ls_docid_query.
  ls_docid_query-docid = cs_rehu_hu-docid.
  ls_docid_query-itemid = cs_rehu_hu-ritmid.
  ls_docid_query-doccat = cs_rehu_hu-rdoccat.
  APPEND ls_docid_query TO lt_docid_query.

* might happen on concurrent packing there is a lock on the same
* item. So let's try to lock a bit later.
  DO 5 TIMES.
    CALL METHOD lo_pack->init_rf
      EXPORTING
        iv_lgnum        = iv_lgnum
        it_docid        = lt_docid_query
        iv_doccat       = cs_rehu_hu-rdoccat
        iv_lock_dlv     = 'X'
      IMPORTING
        ev_foreign_lock = lv_lock.

*   Check the lock
    IF lv_lock IS INITIAL.
      EXIT.
    ENDIF.

    WAIT UP TO 1 SECONDS.
  ENDDO.

* create multipe hu in mass hu create case
  IF cs_rehu_hu-nohu IS NOT INITIAL.
    lv_no_hu = cs_rehu_hu-nohu.
    CLEAR lv_huident.
  ELSE.
    lv_no_hu = 1.
    lv_huident = cs_rehu_hu-huident.
  ENDIF.


  IF ( is_item_packaged IS NOT INITIAL AND it_huhdr_upb IS NOT INITIAL AND it_huitm_ubp IS NOT INITIAL ).
    ls_item_packaged = is_item_packaged.
    lt_huhdr_upb = it_huhdr_upb.
    lt_huitm_ubp = it_huitm_ubp.
    lt_hutree_upb = it_hutree_upb.
    lt_huauxpmat_upb = it_huauxpmat_upb.
  ELSEIF ( gs_item_packaged IS NOT INITIAL AND gt_huhdr_upb IS NOT INITIAL AND gt_huitm_ubp IS NOT INITIAL ).
    ls_item_packaged = gs_item_packaged.
    lt_huhdr_upb = gt_huhdr_upb.
    lt_huitm_ubp = gt_huitm_ubp.
    lt_hutree_upb = gt_hutree_upb.
    lt_huauxpmat_upb = gt_huauxpmat_upb.
  ENDIF.
*--------------------------------------------------------------------------------------------
* Customer Connect: 280996 - Possibility to create one HU with UPB in RF
* Special logic for creation of a single multi level HU with UPB
  IF gv_multi_hu_upb IS NOT INITIAL AND lt_huhdr_upb IS NOT INITIAL AND lt_huitm_ubp IS NOT INITIAL.

    "Update ItemID again in case a new batch subitem was created.
    "Update BatchID in case a new batch was created
    ls_item_packaged-stock-qitmid = cs_rehu_hu-ritmid.
    ls_item_packaged-stock-batchid = cs_rehu_prod-batchid.
    LOOP AT lt_huitm_ubp ASSIGNING FIELD-SYMBOL(<ls_hutitm_upb>).
      <ls_hutitm_upb>-batchid = cs_rehu_prod-batchid.
      <ls_hutitm_upb>-qitmid = cs_rehu_hu-ritmid.
    ENDLOOP.

    lo_pack->create_hu_pack_stock_upb(
      EXPORTING
        is_item_packaged = ls_item_packaged
        it_huhdr_upb     = lt_huhdr_upb
        it_huitm_ubp     = lt_huitm_ubp
        it_huauxpmat_upb = lt_huauxpmat_upb
        it_hutree_upb    = lt_hutree_upb
        it_huident_top   = VALUE #( ( cs_rehu_hu-huident ) )
     IMPORTING
        et_huhdr         = lt_huhdr_upb_result
        et_huitm         = lt_huitm_upb_result
        et_return        = lt_return_upb
        ev_severity      = lv_severity_upb
    ).
    IF lv_severity_upb CA wmegc_severity_eax.
      LOOP AT lt_return_upb ASSIGNING FIELD-SYMBOL(<ls_return_upb>)
      WHERE type CA wmegc_severity_eax.
        MESSAGE ID     <ls_return_upb>-id
                TYPE   <ls_return_upb>-type
                NUMBER <ls_return_upb>-number
                WITH   <ls_return_upb>-message_v1 <ls_return_upb>-message_v2
                       <ls_return_upb>-message_v3 <ls_return_upb>-message_v4.
        RETURN.
      ENDLOOP.
    ENDIF.

    READ TABLE lt_huhdr_upb_result WITH KEY top = abap_true INTO ls_huhdr.

* Fill Best Before Date
    IF cs_rehu_prod-bbdat IS NOT INITIAL.
      LOOP AT lt_huitm_upb_result INTO ls_huitm.

        ls_huitm-vfdat = cs_rehu_prod-bbdat.

        TRY.
            CALL METHOD lo_pack->/scwm/if_pack_bas~change_huitm
              EXPORTING
                is_huitm = ls_huitm
              IMPORTING
                es_huitm = ls_huitm_out.
          CATCH /scwm/cx_basics .
        ENDTRY.
      ENDLOOP.
    ENDIF.

    cs_rehu_hu-huident = ls_huhdr-huident.
    cs_rehu_hu-guid_hu = ls_huhdr-guid_hu.
    IF cs_rehu_hu-new_packmat IS NOT INITIAL.
      cs_rehu_hu-pmat = cs_rehu_hu-new_packmat.
    ENDIF.
    APPEND cs_rehu_hu TO lt_rehu_hu.

* End Customer Connect: 280996
*--------------------------------------------------------------------------------------------
  ELSE.
********************START: Mixed HU***************************
    "in case HU already exist, (pack into mixed HU case), no need to create HU
    " determine pack spec only when creating new HU
    IF lf_hu_exist = abap_false.
********************END: Mixed HU*****************************
      IF ( is_packing_rule IS NOT INITIAL AND is_levels IS NOT INITIAL ).
        ls_levels_upb = is_levels.
        ls_packing_rule_upb = is_packing_rule.
      ELSEIF ( gs_packing_rule_upb IS NOT INITIAL AND gs_levels_upb IS NOT INITIAL ).
        ls_levels_upb = gs_levels_upb.
        ls_packing_rule_upb = gs_packing_rule_upb.
      ENDIF.
      ls_hu_create-hutyp = cs_rehu_hu-hutyp.

      IF ( ls_levels_upb IS INITIAL AND ls_packing_rule_upb IS INITIAL ).
* Fill packaging specification relevant info in case it is present for material.
        IF gv_pspec IS NOT INITIAL.
          CALL FUNCTION '/SCWM/PS_PACKSPEC_GET'
            EXPORTING
              iv_guid_ps          = gv_pspec
              iv_read_elements    = 'X'
            IMPORTING
              es_packspec_header  = ls_psp_hdr
              et_packspec_content = lt_psp_content
              et_elementgroup     = lt_elementgroup
              es_packspec_level   = ls_packspec_level
            EXCEPTIONS
              error               = 1
              OTHERS              = 2.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          READ TABLE lt_psp_content INTO ls_psp_content INDEX 1.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          READ TABLE ls_psp_content-levels INTO ls_levels
            WITH KEY hu_create = abap_true.
          IF sy-subrc IS INITIAL.
            IF ls_levels-hu_create = abap_true.
              ls_hu_create-tare_var = ls_levels-tare_var.
              ls_hu_create-closed_package = ls_levels-closed_package.
              IF ls_levels-flag_weight = abap_true.
                ls_hu_create-unit_gw = ls_levels-unit_gw.
                ls_hu_create-t_weight = ls_levels-t_weight.
                ls_hu_create-unit_tw = ls_levels-unit_tw.
              ENDIF.
              IF ls_levels-flag_vol = abap_true.
                ls_hu_create-unit_gv = ls_levels-unit_gv.
                ls_hu_create-t_volume = ls_levels-t_volume.
                ls_hu_create-unit_tv = ls_levels-unit_tv.
              ENDIF.
              IF ls_levels-flag_dim = abap_true.
                ls_hu_create-length = ls_levels-length.
                ls_hu_create-width = ls_levels-width.
                ls_hu_create-height = ls_levels-height.
                ls_hu_create-unit_lwh = ls_levels-unit_lwh.
                ls_hu_create-max_length = ls_levels-max_length.
                ls_hu_create-max_width = ls_levels-max_width.
                ls_hu_create-max_height = ls_levels-max_height.
                ls_hu_create-unit_max_lwh = ls_levels-unit_max_lwh.
              ENDIF.
              IF ls_levels-flag_capa = abap_true.
                ls_hu_create-t_capa = ls_levels-t_capa.
                ls_hu_create-max_capa = ls_levels-max_capa.
                ls_hu_create-tolc = ls_levels-tolc.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING ls_levels_upb TO ls_hu_create.
      ENDIF.
    ENDIF.
********START:incident 2380084856****
    IF lf_hu_exist = abap_true.
      lv_no_hu = 1.
    ENDIF.
********END: incident 2380084856*****
    DO lv_no_hu TIMES.
* create empty HU withe the given huident
*****************START: Mixed HU case****************************
      "use existing HU in pack mixed hu case   -> New Scenario
      IF lf_hu_exist = abap_true.
        ls_huhdr = ls_huhdr_exist.
      ELSE.
        "otherwise, create new HU                -> Old Scenario
        CALL METHOD lo_pack->/scwm/if_pack_bas~create_hu
          EXPORTING
            iv_pmat      = iv_hu_matid
            iv_huident   = lv_huident
            is_hu_create = ls_hu_create
          RECEIVING
            es_huhdr     = ls_huhdr
          EXCEPTIONS
            error        = 1
            OTHERS       = 2.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
*****************END: Mixed HU case****************************
      ls_material-qdoccat = cs_rehu_hu-rdoccat.
      ls_material-qdocid  = cs_rehu_hu-docid.
      ls_material-qitmid  = cs_rehu_hu-ritmid.
*   Round Quantity
      lv_round_qty = <ls_free>-open_qty-qty - cs_rehu_prod-nista.
      IF /qos/cl_qty_aux=>round_qty(  lv_round_qty  ) = 0.
        ls_quantity-quan = <ls_free>-open_qty-qty.
      ELSE.
        ls_quantity-quan = cs_rehu_prod-nista.
      ENDIF.
      ls_quantity-unit = cs_rehu_prod-altme.

* pack the stock to the empty HU     (after adding Mixed HU case, ls_huhdr-guid_hu could be existing one: line1327)
      CALL METHOD lo_pack->/scwm/if_pack_bas~pack_stock
        EXPORTING
          iv_dest_hu  = ls_huhdr-guid_hu
          is_material = ls_material
          is_quantity = ls_quantity
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

* update the BBD to the HU
      IF cs_rehu_prod-bbdat IS NOT INITIAL.

        CLEAR lt_huitm.
        CALL METHOD lo_pack->/scwm/if_pack_bas~get_hu_item
          EXPORTING
            iv_guid_hu = ls_huhdr-guid_hu
          IMPORTING
            et_huitm   = lt_huitm
          EXCEPTIONS
            not_found  = 1
            OTHERS     = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        READ TABLE lt_huitm INTO ls_huitm INDEX 1.
        ls_huitm-vfdat = cs_rehu_prod-bbdat.

        TRY.
            CALL METHOD lo_pack->/scwm/if_pack_bas~change_huitm
              EXPORTING
                is_huitm = ls_huitm
              IMPORTING
                es_huitm = ls_huitm_out.
          CATCH /scwm/cx_basics .                       "#EC NO_HANDLER
            MESSAGE ID     sy-msgid
            TYPE   sy-msgty
            NUMBER sy-msgno
            WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

        ENDTRY.

      ENDIF.


      IF ( ls_packing_rule_upb IS NOT INITIAL AND ls_levels_upb IS NOT INITIAL ).

        CASE ls_packing_rule_upb-packing_engine.
          WHEN /scmb/if_pb_profile=>c_pb_engine-ps.

            lt_huhdr_changed = VALUE #( ( fieldname = 'PB_ENGINE'    value_c = ls_packing_rule_upb-packing_engine )
                                        ( fieldname = 'PS_GUID'      value_c = ls_packing_rule_upb-guid_ps )
                                        ( fieldname = 'PS_LEVEL_SEQ' value_c = ls_packing_rule_upb-ps_level_seq ) ).

          WHEN /scmb/if_pb_profile=>c_pb_engine-pi.
            lt_huhdr_changed = VALUE #( ( fieldname = 'PB_ENGINE' value_c = ls_packing_rule_upb-packing_engine )
                                        ( fieldname = 'PI_GUID'   value_c = ls_packing_rule_upb-guid_pi ) ).
          WHEN OTHERS.
            lt_huhdr_changed = VALUE #( ( fieldname = 'PB_ENGINE' value_c = ls_packing_rule_upb-packing_engine ) ).
        ENDCASE.

      ELSEIF gv_pspec IS NOT INITIAL.
        lt_huhdr_changed = VALUE #( ( fieldname = 'PS_GUID' value_c = gv_pspec )
                                    ( fieldname = 'PS_LEVEL_SEQ' value_c = ls_levels-level_seq ) ).
      ENDIF.

      IF lt_huhdr_changed IS NOT INITIAL.
        TRY.
            CALL FUNCTION '/SCWM/HUHDR_ATTR_CHANGE'
              EXPORTING
                iv_guid_hu = ls_huhdr-guid_hu
                it_changed = lt_huhdr_changed.
          CATCH /scwm/cx_basics.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDTRY.
      ENDIF.

      cs_rehu_hu-huident = ls_huhdr-huident.
      cs_rehu_hu-guid_hu = ls_huhdr-guid_hu.
      IF cs_rehu_hu-new_packmat IS NOT INITIAL.
        cs_rehu_hu-pmat = cs_rehu_hu-new_packmat.
      ENDIF.
      APPEND cs_rehu_hu TO lt_rehu_hu.
    ENDDO.

  ENDIF.

* set data for printing
  CALL FUNCTION '/SCWM/RF_PRINT_GLOBAL_DATA'.

  CALL METHOD lo_pack->/scwm/if_pack_bas~save
    EXPORTING
      iv_commit = ' '
      iv_wait   = ' '
    EXCEPTIONS
      error     = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.



  "Try to save open EWL
  IF lo_ewl_manager->is_standard_skipped( ) = abap_false AND
     iv_procs IS NOT INITIAL AND
     iv_prr_id IS NOT INITIAL.
    lo_ewl_manager->save( ).
  ENDIF.
  " ------

  COMMIT WORK AND WAIT.

  CALL METHOD /scwm/cl_tm=>cleanup( ).

  "EWL cleanup
  IF lo_ewl_manager->is_standard_skipped( ) = abap_false AND
     iv_procs IS NOT INITIAL AND
     iv_prr_id IS NOT INITIAL.
    lo_ewl_manager->cleanup( ).
  ENDIF.
  "----

  IF gv_mass_hu_create = abap_true.
    " DELETE ct_rehu_hu INDEX 1.
    CLEAR: ct_rehu_hu.
    APPEND LINES OF lt_rehu_hu TO ct_rehu_hu.
  ENDIF.

  IF lv_new_item_id IS NOT INITIAL.
    CLEAR lt_docid_query.
    ls_docid_query-docid  = cs_rehu_hu-docid.
    ls_docid_query-itemid = lv_new_item_id.
    ls_docid_query-doccat = cs_rehu_hu-rdoccat.
    APPEND ls_docid_query TO lt_docid_query.
    ls_read_options-mix_in_object_instances =  /scwm/if_dl_c=>sc_mix_in_load_instance.

    TRY.
        CALL METHOD lo_query->query
          EXPORTING
            it_docid        = lt_docid_query
            iv_whno         = iv_lgnum
            is_read_options = ls_read_options
          IMPORTING
            et_items        = lt_items.
      CATCH /scdl/cx_delivery .                         "#EC NO_HANDLER
        MESSAGE ID     sy-msgid
                TYPE   sy-msgty
                NUMBER sy-msgno
                WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.

    READ TABLE lt_items INTO ls_items INDEX 1.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING ls_items TO ls_wrk_itms.
      INSERT ls_wrk_itms INTO TABLE cs_rehu-itms.
    ENDIF.
  ENDIF.

  CLEAR gv_pspec.

ENDFORM.                    " pack_item_to_delivery


