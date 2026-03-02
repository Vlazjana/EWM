FUNCTION-POOL zfg_rf_emduri.

DATA: gs_scr1     TYPE zsrf_emduri_scr1,
      gt_scr2     TYPE STANDARD TABLE OF zsrf_emduri_scr2,
      gs_scr2     TYPE zsrf_emduri_scr2,
      gv_sel_scr2 TYPE /scwm/de_rf_selno,
      gt_scr3     TYPE STANDARD TABLE OF zsrf_emduri_scr3,
      gs_scr3     TYPE zsrf_emduri_scr3,
      gv_sel_scr3 TYPE /scwm/de_rf_selno,
      gt_scr4     TYPE STANDARD TABLE OF zsrf_emduri_scr4,
      gs_scr4     TYPE zsrf_emduri_scr4,
      gv_sel_scr4 TYPE /scwm/de_rf_selno,
      gs_scr5     TYPE zsrf_emduri_scr5,
      gv_sel_scr5 TYPE /scwm/de_rf_selno,
      gs_scr6     TYPE zsrf_emduri_scr6,
      gt_scr7     TYPE STANDARD TABLE OF zsrf_emduri_scr7,
      gs_scr7     TYPE zsrf_emduri_scr7,
      gt_scr8     TYPE STANDARD TABLE OF zsrf_emduri_scr8,
      gs_scr8     TYPE zsrf_emduri_scr8,
      gv_sel_scr8 TYPE /scwm/de_rf_selno.

DATA: gt_supplier_list   TYPE STANDARD TABLE OF zsrf_emduri_scr2,
      gs_supplier_list   TYPE zsrf_emduri_scr2,
      gt_inbound_list    TYPE STANDARD TABLE OF zsrf_emduri_scr3,
      gs_inbound_list    TYPE zsrf_emduri_scr3,
      gt_product_list    TYPE STANDARD TABLE OF zsrf_emduri_scr4,
      gs_product_list    TYPE zsrf_emduri_scr4,
      gt_barcode_list    TYPE STANDARD TABLE OF zsrf_emduri_scr5,
      gs_barcode_list    TYPE zsrf_emduri_scr5,
      gt_internal_batch  TYPE STANDARD TABLE OF zsrf_emduri_scr6,
      gs_internal_batch  TYPE zsrf_emduri_scr6,
      gt_packaging_matnr TYPE STANDARD TABLE OF zsrf_emduri_scr7,
      gs_packaging_matnr TYPE zsrf_emduri_scr7,
      gt_hu_list         TYPE STANDARD TABLE OF zsrf_emduri_scr8,
      gs_hu_list         TYPE zsrf_emduri_scr8.

DATA: idx   TYPE i,
      line  TYPE i,
      lines TYPE i,
      limit TYPE i,
      c     TYPE i.

DATA: save_ok TYPE sy-ucomm,
      fill    TYPE i,
      fill1   TYPE i,
      fill2   TYPE i.

DATA:gc_msgty_success TYPE symsgty VALUE 'S',
     gc_rf_en_msgid   TYPE symsgid VALUE '/SCWM/RF_EN'.

DATA: gt_headers TYPE /scwm/dlv_header_out_prd_tab,
      gt_items   TYPE /scwm/dlv_item_out_prd_tab.

TYPE-POOLS: wmegc, wmesr.


* Global Variable
DATA: gv_door_dd_fut    TYPE /scwm/de_rf_asgn_future_days,
      gv_door_dd_past   TYPE /scwm/de_rf_asgn_past_days,
      gv_tunum_int      TYPE /scwm/de_tu_num,
      gv_mass_hu_create TYPE /scwm/de_mass_hu_creation,
      gv_tu_sr_act_num  TYPE /scwm/de_tu_sr_act_num.

DATA: gv_pspec TYPE /scwm/de_matid.

DATA: gs_packing_rule_upb TYPE /scwm/s_upb_pack_rule,
      gs_levels_upb       TYPE /scwm/s_ps_level_int,
      gs_item_packaged type /scwm/s_ps_autopack,
      gs_top_hu_quan_upb type /scwm/if_autopack_upb_util=>ys_huheader_result_quan,
      gt_huhdr_upb type /scwm/if_packing_upb=>yt_huheader_result,
      gt_huitm_ubp type /scwm/if_packing_upb=>yt_huitem_result,
      gt_huauxpmat_upb type /scwm/if_packing_upb=>yt_huauxpmat_result,
      gt_hutree_upb type /scwm/if_packing_upb=>yt_hutree_result.

DATA: gv_is_advsr_qty_change TYPE abap_boolean.

DATA: gt_stocksegs TYPE /scwm/if_segmentation=>t_stk_segment.

* Customer Connect: 280996 - Possibility to create one multi-level HU with UPB in RF
DATA: gv_multi_hu_upb type boole_d.


* Time to Timestamp conversion
CONSTANTS:
  gc_time_low   TYPE uzeit  VALUE '000000',
  gc_time_high  TYPE uzeit  VALUE '235959',
  gc_day_in_sec TYPE i      VALUE '86400',
  gc_numcs      TYPE string VALUE '0123456789.,'.


* Status
CONSTANTS:
  gc_dl_status_type_dun         TYPE /scdl/dl_status_type
    VALUE /scdl/if_dl_status_c=>sc_t_unloading,
  gc_dl_status_value_dlun       TYPE /scdl/dl_status_value
    VALUE /scdl/if_dl_status_c=>sc_v_finished,
  gc_dl_status_type_dpt         TYPE /scdl/dl_status_type
    VALUE /scdl/if_dl_status_c=>sc_t_putaway,
  gc_dl_status_value_dptw       TYPE /scdl/dl_status_value
    VALUE /scdl/if_dl_status_c=>sc_v_finished,
  gc_dl_status_value_unblocked  TYPE /scdl/dl_status_value
    VALUE /scdl/if_dl_status_c=>sc_v_false,
  gc_dl_compl_unload            TYPE string
    VALUE /scwm/if_ui_shp_const=>sc_act_status_unload_end,
  gc_dl_status_type_dsp         TYPE /scdl/dl_status_type
    VALUE /scdl/if_dl_status_c=>sc_t_split,
  gc_dl_status_value_dsp_partly TYPE /scdl/dl_status_value
    VALUE /scdl/if_dl_status_c=>sc_v_partly_confirmed,

  gc_shortcut(60)               TYPE c VALUE '/SCWM/S_RF_SCRELM-SHORTCUT'.

* Delivery logical field names
CONSTANTS:
  gc_dl_logfname_docno           TYPE /scdl/dl_logfname
      VALUE /scdl/if_dl_logfname_c=>sc_docno_h,
  gc_dl_logfname_whno_i          TYPE /scdl/dl_logfname
     VALUE /scwm/if_dl_logfname_c=>sc_whno_i,
  gc_dl_logfname_docid           TYPE /scdl/dl_logfname
      VALUE /scdl/if_dl_logfname_c=>sc_docid_h,
  gc_dl_logfname_productid_i     TYPE /scdl/dl_logfname
       VALUE /scdl/if_dl_logfname_c=>sc_productid_i,
  gc_dl_logfname_door_i          TYPE /scdl/dl_logfname
     VALUE /scwm/if_dl_logfname_c=>sc_door_i,
  gc_dl_logfname_refdocno_bol_h  TYPE /scdl/dl_logfname
     VALUE /scdl/if_dl_logfname_c=>sc_refdocno_bol_h,
  gc_dl_logfname_refdocno_asn_h  TYPE /scdl/dl_logfname
     VALUE /scdl/if_dl_logfname_c=>sc_refdocno_asn_h,
  gc_dl_logfname_value_dun_i     TYPE /scdl/dl_logfname
     VALUE /scdl/if_dl_logfname_c=>sc_status_value_dun_i,
  gc_dl_logfname_value_dpt_i     TYPE /scdl/dl_logfname
     VALUE /scdl/if_dl_logfname_c=>sc_status_value_dpt_i,


  gc_dl_logfname_value_dbc_h     TYPE /scdl/dl_logfname
     VALUE /scdl/if_dl_logfname_c=>sc_status_value_dbc_h,
  gc_dl_logfname_value_dlvpldat  TYPE /scdl/dl_logfname
    VALUE /scdl/if_dl_logfname_c=>sc_tstfr_tdelivery_plan_h,
  gc_dl_logfname_value_dlvactdat TYPE /scdl/dl_logfname
   VALUE /scdl/if_dl_logfname_c=>sc_tstfr_tdelivery_actual_h,

  gc_dl_logfname_locationno_up_i TYPE /scdl/dl_logfname
   VALUE /scdl/if_dl_logfname_c=>sc_locationno_up_i,

* FieldNames in Structure
  gc_scr_elmnt_door_dd_past      TYPE /scwm/de_screlm_name VALUE
             '/SCWM/S_RF_REHU_HU-DOOR_DD_PAST',
  gc_scr_elmnt_door_dd_fut       TYPE /scwm/de_screlm_name VALUE
             '/SCWM/S_RF_REHU_HU-DOOR_DD_FUT',

  gc_param_cs_rehu_hu            TYPE /scwm/de_param_name VALUE
             'CS_REHU_HU' ,
  gc_param_cs_rehu               TYPE /scwm/de_param_name VALUE
             'CS_REHU' ,
  gc_param_ct_rehu_hu            TYPE /scwm/de_param_name VALUE
             'CT_REHU_HU' ,
  gc_param_cs_rehu_prod          TYPE /scwm/de_param_name VALUE
             'CS_REHU_PROD' ,
  gc_param_ct_rehu_prod          TYPE /scwm/de_param_name VALUE
             'CT_REHU_PROD' ,
  gc_param_cs_rehu_dlv           TYPE /scwm/de_param_name VALUE
              'CS_REHU_DLV' ,
  gc_param_ct_rehu_dlv           TYPE /scwm/de_param_name VALUE
              'CT_REHU_DLV' .

CONSTANTS:
  gc_fcode_hu     TYPE /scwm/de_fcode VALUE 'HU',
  gc_fcode_gtlp   TYPE /scwm/de_fcode VALUE 'GTLP',
  gc_fcode_gtdldt TYPE /scwm/de_fcode VALUE 'GTDLDT',
  gc_fcode_gtdldn TYPE /scwm/de_fcode VALUE 'GTDLDN',
  gc_fcode_gtnwhu TYPE /scwm/de_fcode VALUE 'GTNWHU',
  gc_fcode_hulist TYPE /scwm/de_fcode VALUE 'HULIST',
  gc_fcode_huwt   TYPE /scwm/de_fcode VALUE 'HUWT',
  gc_fcode_gtnmhu TYPE /scwm/de_fcode VALUE 'GTNMHU',
  gc_fcode_gtexhu TYPE /scwm/de_fcode VALUE 'GTEXHU',
  gc_fcode_gtegsl TYPE /scwm/de_fcode VALUE 'GTEGSL',
  gc_fcode_backf  TYPE /scwm/de_fcode VALUE 'BACKF',
  gc_fcode_enter  TYPE /scwm/de_fcode VALUE 'ENTER',
  gc_fcode_gtdlsl TYPE /scwm/de_fcode VALUE 'GTDLSL',
  gc_fcode_gtpmsl TYPE /scwm/de_fcode VALUE 'GTPMSL',
  gc_fcode_gthudt TYPE /scwm/de_fcode VALUE 'GTHUDT',
  gc_fcode_gtchpm TYPE /scwm/de_fcode VALUE 'GTCHPM',
  gc_fcode_gtchqy TYPE /scwm/de_fcode VALUE 'GTCHQY',
  gc_fcode_rhchqy TYPE /scwm/de_fcode VALUE 'RHCHQY',
  gc_fcode_gtsncl TYPE /scwm/de_fcode VALUE 'GTSNCL',
  gc_fcode_rhchpm TYPE /scwm/de_fcode VALUE 'RHCHPM',
  gc_fcode_gtchbd TYPE /scwm/de_fcode VALUE 'GTCHBD',
  gc_fcode_gthumi TYPE /scwm/de_fcode VALUE 'GTHUMI',
  gc_fcode_gthuad TYPE /scwm/de_fcode VALUE 'GTHUAD',
  gc_fcode_unload TYPE /scwm/de_fcode VALUE 'UNLOAD',
  gc_fcode_postgr TYPE /scwm/de_fcode VALUE 'POSTGR',
  gc_fcode_crwt   TYPE /scwm/de_fcode VALUE 'CRWT',
  gc_fcode_next   TYPE /scwm/de_fcode VALUE 'NEXT',
  gc_fcode_updbck TYPE /scwm/de_fcode VALUE 'UPDBCK',
  gc_fcode_yes    TYPE /scwm/de_fcode VALUE 'YES   ',
  gc_fcode_crwtdl TYPE /scwm/de_fcode VALUE 'CRWTDLV',
  gc_fcode_pgrhdr TYPE /scwm/de_fcode VALUE 'PGRHDR',
  gc_fcode_gtcolt TYPE /scwm/de_fcode VALUE 'GTCOLT',
  "Mixed HU, pack product into existing HU
  gc_fcode_packpd TYPE /scwm/de_fcode VALUE 'PACKPD',
  gc_fcode_gtfolt TYPE /scwm/de_fcode VALUE 'GTFOLT',
* Customer Connect: 280996 - Possibility to create one multi-level HU with UPB in RF
  gc_fcode_gtnshu  type /scwm/de_fcode VALUE 'GTNSHU'.

"logical transaction
CONSTANTS:
  gc_ltrans_rhdesi TYPE /scwm/de_ltrans VALUE 'RHDESI'. "Receiving HU by Delivery


CONSTANTS:
  gc_prmod_foregr TYPE /scwm/de_prmod VALUE '2',
  gc_prmod_backgr TYPE /scwm/de_prmod VALUE '1'.

CONSTANTS:
  gc_hu_laod_info_unlo  TYPE char1 VALUE 'A',
  gc_huident_length     TYPE i VALUE 20,
  gc_xfeld              TYPE xfeld VALUE 'X',
  gc_transpl_type_advsr TYPE /scwm/dl_transpl_type VALUE 'E'.
