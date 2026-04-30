*----------------------------------------------------------------------*
***INCLUDE LZEWMSCWM_RF_PICKINGO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module ZWHO_STATUS_HANDLE OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE zwho_status_handle OUTPUT.

  DATA:ls_who_curr   TYPE /scwm/s_who_int,
       lv_curr_idx   TYPE i,
       ls_rsrc       TYPE /scwm/rsrc,
       ls_s_rsrc     TYPE /scwm/s_rsrc,
       lt_who_unasg  TYPE /scwm/tt_who_int,
       ls_who_unasg  TYPE /scwm/s_who_int,
       ls_who_read   TYPE /scwm/s_who_int,
       ls_attributes TYPE /scwm/s_who_att.

  DATA(lv_fcode) = /scwm/cl_rf_bll_srvc=>get_fcode( ).

  CASE lv_fcode.
    WHEN 'PGUP' OR 'PGDN'.

      IF gt_who_list_gv IS INITIAL.
        RETURN.
      ENDIF.

      lv_curr_idx = /scwm/cl_rf_bll_srvc=>get_line( ).
      IF lv_curr_idx = 0.
        lv_curr_idx = 1.
      ENDIF.

      READ TABLE gt_who_list_gv INDEX lv_curr_idx
        INTO ls_who_curr.

      IF ls_who_curr IS INITIAL.
        RETURN.
      ENDIF.
      IF ls_who_curr-who = gv_who_active.
        RETURN.
      ENDIF.

      CALL FUNCTION '/SCWM/RSRC_RESOURCE_MEMORY'
        EXPORTING
          iv_uname = sy-uname
        CHANGING
          cs_rsrc  = ls_rsrc.
      MOVE-CORRESPONDING ls_rsrc TO ls_s_rsrc.

* Previous UNHOLD WHO- Open
* Read the previous WHO
      TRY.
          CALL FUNCTION '/SCWM/WHO_SELECT'
            EXPORTING
              iv_lgnum    = gv_who_lgnum
              iv_who      = gv_who_active
              iv_lock_who = 'X'
            IMPORTING
              es_who      = ls_who_read.
        CATCH /scwm/cx_core.
      ENDTRY.

      MOVE-CORRESPONDING ls_who_read TO ls_attributes.
      CLEAR:ls_attributes-started_at,  "this returns Open
            ls_attributes-rsrc,
            ls_attributes-processor,
            ls_attributes-start_bin.

      CALL FUNCTION '/SCWM/WHO_UPDATE'
        EXPORTING
          iv_lgnum      = gv_who_lgnum
          iv_db_update  = 'X'
          iv_who        = gv_who_active
          is_attributes = ls_attributes
          iv_synchron   = 'X'
        EXCEPTIONS
          OTHERS        = 0.

* HOLD NEW WHO  → D
* READ NEW WHO-n
      TRY.
          CALL FUNCTION '/SCWM/WHO_SELECT'
            EXPORTING
              iv_lgnum    = ls_who_curr-lgnum
              iv_who      = ls_who_curr-who
              iv_lock_who = 'X'
            IMPORTING
              es_who      = ls_who_read.
        CATCH /scwm/cx_core.
      ENDTRY.

      MOVE-CORRESPONDING ls_who_read TO ls_attributes.
      GET TIME STAMP FIELD ls_attributes-started_at.  " This sets D
      ls_attributes-rsrc = ls_rsrc-rsrc.

      CALL FUNCTION '/SCWM/WHO_UPDATE'
        EXPORTING
          iv_lgnum      = ls_who_curr-lgnum
          iv_db_update  = 'X'
          iv_who        = ls_who_curr-who
          is_attributes = ls_attributes
          iv_synchron   = 'X'
        EXCEPTIONS
          OTHERS        = 0.

      COMMIT WORK AND WAIT.

      DATA ls_resource_tmp TYPE /scwm/s_rsrc.
      DATA ls_who_tmp      TYPE /scwm/s_who_int.
      DATA lt_who_list_tmp TYPE /scwm/tt_who_int.
      DATA lv_last_tx_tmp  TYPE /scwm/de_ltrans.
      DATA lv_queue_tmp    TYPE /scwm/de_queue.
      DATA lv_additional   TYPE string.
      DATA ls_rsrc_tmp     TYPE /scwm/rsrc.

      MOVE-CORRESPONDING gs_resource_g TO ls_resource_tmp.
      MOVE-CORRESPONDING gs_resource_g TO ls_rsrc_tmp.
      ls_who_tmp = ls_who_curr.

* Build filter with only new WHO
      CLEAR lt_who_list_tmp.
      APPEND ls_who_curr TO lt_who_list_tmp.

      lv_last_tx_tmp = /scwm/cl_rf_bll_srvc=>get_ltrans( ).

* Call find_first_workable_who to get TT_ORDIM_CONFIRM
      PERFORM find_first_workable_who
        USING    ls_resource_tmp
                 ls_rsrc_tmp
                 lv_last_tx_tmp
                 lv_queue_tmp
                 lt_who_list_tmp
        CHANGING ls_resource_tmp
                 ls_who_tmp
                 gs_ordim_confirm
                 gt_ordim_confirm
                 gt_t_rf_pick_hus
                 lv_additional.

** Call PIMTTO_PBO with new WHO data
      IF gt_ordim_confirm IS NOT INITIAL.
        CALL FUNCTION 'Z_EWM_RF_PICK_PIMTTO_PBO'
          CHANGING
            ordim_confirm    = gs_ordim_confirm
            tt_ordim_confirm = gt_ordim_confirm
            tt_nested_hu     = gt_tt_nested_hu
            wme_verif        = gs_wme_verif
            resource         = gs_resource_g
            t_rf_pick_hus    = gt_t_rf_pick_hus
            who              = ls_who_tmp
            zt_who_list      = gt_who_list_gv
            zt_who_display   = gt_zt_who_display.
      ENDIF.

      gv_who_active = ls_who_curr-who.
      gv_who_lgnum  = ls_who_curr-lgnum.
      gv_who_index  = lv_curr_idx.
      /scwm/cl_rf_bll_srvc=>set_scr_tabname( 'ZTT_RF_WHO_LIST' ).
      /scwm/cl_rf_bll_srvc=>set_line( gv_who_index ).

    WHEN OTHERS.
      "     After scan — re-call PIMTTO_PBO to refresh screen
      IF gv_pallet_scanned > 0 AND gs_ordim_confirm-vltyp = 'CA01'.
        CALL FUNCTION 'Z_EWM_RF_PICK_PIMTTO_PBO'
          CHANGING
            ordim_confirm    = gs_ordim_confirm
            tt_ordim_confirm = gt_ordim_confirm
            tt_nested_hu     = gt_tt_nested_hu
            wme_verif        = gs_wme_verif
            resource         = gs_resource_g
            t_rf_pick_hus    = gt_t_rf_pick_hus
            who              = gs_who_curr_g
            zt_who_list      = gt_who_list_gv
            zt_who_display   = gt_zt_who_display.

        /scwm/cl_rf_bll_srvc=>set_scr_tabname( 'ZTT_RF_WHO_LIST' ).
        /scwm/cl_rf_bll_srvc=>set_line( gv_who_index ).
      ENDIF.


  ENDCASE.
ENDMODULE.



*&---------------------------------------------------------------------*
*&      Module  ZWHO_SYNC_ACTIVE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zwho_sync_active INPUT.

  DATA: ls_who_active TYPE /scwm/s_who_int.

  IF gt_who_list_gv IS INITIAL.
    RETURN.
  ENDIF.

  lv_curr_idx = /scwm/cl_rf_bll_srvc=>get_line( ).
  IF lv_curr_idx = 0.
    lv_curr_idx = 1.
  ENDIF.

  READ TABLE gt_who_list_gv INDEX lv_curr_idx INTO ls_who_active.
  IF sy-subrc = 0 AND ls_who_active-who IS NOT INITIAL.
    gs_who_curr_g = ls_who_active.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SCREEN_ADJUST OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE screen_adjust OUTPUT.


* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
*   LOOP AT SCREEN.
*    CASE gs_ordim_confirm-vltyp.
*      WHEN 'CA01'.
*        IF screen-name CS 'VLENR'.
*          screen-active = 0.
*          MODIFY SCREEN.
*        ENDIF.
*        IF screen-name CS 'VLENR_VERIF'.
*          screen-active = 1.
*          screen-input  = 1.
*          MODIFY SCREEN.
*        ENDIF.
*        IF screen-name CS 'PICKHU'.
*          screen-active = 0.
*          MODIFY SCREEN.
*        ENDIF.
*
*      WHEN 'SC01'.
*        IF screen-name CS 'VLENR'.
*          screen-active = 1.
*          MODIFY SCREEN.
*        ENDIF.
*        IF screen-name CS 'VLENR_VERIF'.
*          screen-active = 0.
*          MODIFY SCREEN.
*        ENDIF.
*        IF screen-name CS 'PICKHU'.
*          screen-active = 1.
*          screen-input  = 0.
*          MODIFY SCREEN.
*        ENDIF.
*    ENDCASE.
*  ENDLOOP.
ENDMODULE.
