SCREEN 200

-
PROCESS BEFORE OUTPUT.
  MODULE status_sscr_loop.
  MODULE status_sscr_loop_2.
* PASS TABLE ROWS TO THE STEP-LOOP
  LOOP AT gt_scr2 INTO gs_scr2 CURSOR c .
    MODULE transp_itab_out.
  ENDLOOP.

PROCESS AFTER INPUT.
  LOOP.
    MODULE transp_itab_in.
  ENDLOOP.

  MODULE user_command_scr200.

MODULE status_sscr_loop_2 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.

  fill = lines( gt_scr2 ).

ENDMODULE.

MODULE transp_itab_out OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.

  idx = sy-stepl + line.
  gs_scr2 = VALUE #( gt_scr2[ idx ] OPTIONAL ).
ENDMODULE.


*----------------------------------------------------------------------*
MODULE transp_itab_in INPUT.
  lines = sy-loopc.
  idx = sy-stepl + line.
  MODIFY gt_scr2 FROM gs_scr2 INDEX idx.
ENDMODULE.


--------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_scr200 INPUT.
  CASE sy-ucomm.
    WHEN 'PGDN'.
      line = line + 1.
      limit = fill - lines.
      IF line > limit.
        line = limit.
      ENDIF.
    WHEN 'PGUP'.
      line = line - 1.
      IF line < 0.
        line = 0.
      ENDIF.
    WHEN 'PB1' OR 'F7'.
      REFRESH: gt_scr2,
               gt_inbound_list,
               gt_supplier_list.
      CLEAR:   gs_scr1,gs_scr2,gv_sel_scr2.

  ENDCASE.
ENDMODULE.
*&--------------------------



SCREEN 300

PROCESS BEFORE OUTPUT.
* MODULE STATUS_0300.
  MODULE status_sscr_loop.
  MODULE status_sscr_loop_3.
* pass table rows to the step-loop
  LOOP AT gt_scr3 INTO gs_scr3 CURSOR c.

    MODULE transp_itab_out_inb.

  ENDLOOP.

PROCESS AFTER INPUT.
  LOOP.
    MODULE transp_itab_inb.
  ENDLOOP.

  MODULE user_command_scr300.
MODULE status_sscr_loop_3 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  fill = lines( gt_scr3 ).
ENDMODULE.


MODULE transp_itab_out_inb OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  idx = sy-stepl + line.
  gs_scr3 = VALUE #( gt_scr3[ idx ] OPTIONAL ).
ENDMODULE.

MODULE transp_itab_inb INPUT.
  lines = sy-loopc.
  idx = sy-stepl + line.
  MODIFY gt_scr3 FROM gs_scr3 INDEX idx.
ENDMODULE.
*&-----------


MODULE user_command_scr300 INPUT.
  CASE sy-ucomm.
    WHEN 'PGDN'.
      line = line + 1.
      limit = fill - lines.
      IF line > limit.
        line = limit.
      ENDIF.
    WHEN 'PGUP'.
      line = line - 1.
      IF line < 0.
        line = 0.
      ENDIF.
    WHEN 'PB1' OR 'F7'.
      CLEAR: gv_sel_scr3,line,lines.
  ENDCASE.
ENDMODULE.


SCREEN 400

PROCESS BEFORE OUTPUT.
* MODULE STATUS_0300.
  MODULE status_sscr_loop.
  MODULE status_sscr_loop_4.
* pass table rows to the step-loop
  LOOP AT gt_scr4 INTO gs_scr4 CURSOR c.

    MODULE transp_itab_out_4.

  ENDLOOP.

* set (disable/enable) PGUP/PGDN pushbuttons on the screen
*  MODULE loop_scrolling_set.
*
PROCESS AFTER INPUT.
  LOOP.
    MODULE transp_itab_in4.
*   save data of the step-loop elements
*    MODULE loop_input.
  ENDLOOP.

  MODULE user_command_scr400.

MODULE status_sscr_loop_4 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  fill2 = lines( gt_scr4 ).
ENDMODULE.
MODULE transp_itab_out_4 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  idx = sy-stepl + line.
  gs_scr4 = VALUE #( gt_scr4[ idx ] OPTIONAL ).
ENDMODULE.
*&----------
MODULE transp_itab_out_4 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  idx = sy-stepl + line.
  gs_scr4 = VALUE #( gt_scr4[ idx ] OPTIONAL ).
ENDMODULE.
*&----------
MODULE transp_itab_out_4 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  idx = sy-stepl + line.
  gs_scr4 = VALUE #( gt_scr4[ idx ] OPTIONAL ).
ENDMODULE.
*&----------


SCREEN 500
PROCESS BEFORE OUTPUT.

  MODULE status_sscr.

PROCESS AFTER INPUT.

  MODULE user_command_sscr.
  MODULE user_command_scr500.

MODULE user_command_scr500 INPUT.
  CASE sy-ucomm.
    WHEN 'PGDN'.
      line = line + 1.
      limit = fill - lines.
      IF line > limit.
        line = limit.
      ENDIF.
    WHEN 'PGUP'.
      line = line - 1.
      IF line < 0.
        line = 0.
      ENDIF.
    WHEN 'PB1' OR 'F7'.
  ENDCASE.
ENDMODULE.


SCREEN 700
PROCESS BEFORE OUTPUT.

  MODULE status_sscr.

PROCESS AFTER INPUT.

  MODULE user_command_sscr.
  MODULE user_command_scr700.

MODULE user_command_scr700 INPUT.
  CASE sy-ucomm.
    WHEN 'PGDN'.
      line = line + 1.
      limit = fill - lines.
      IF line > limit.
        line = limit.
      ENDIF.
    WHEN 'PGUP'.
      line = line - 1.
      IF line < 0.
        line = 0.
      ENDIF.
    WHEN 'PB1' OR 'F7'.
  ENDCASE.
ENDMODULE.


SCREEN 800
PROCESS BEFORE OUTPUT.
* MODULE STATUS_0300.
  MODULE status_sscr_loop.
  MODULE status_sscr_loop_8.
* pass table rows to the step-loop
  LOOP AT gt_scr8 INTO gs_scr8 CURSOR c.

    MODULE transp_itab_out_8.

  ENDLOOP.

* set (disable/enable) PGUP/PGDN pushbuttons on the screen
*  MODULE loop_scrolling_set.
*
PROCESS AFTER INPUT.
  LOOP.
    MODULE transp_itab_in8.
*   save data of the step-loop elements
*    MODULE loop_input.
  ENDLOOP.

  MODULE user_command_scr800.
MODULE status_sscr_loop_8 OUTPUT.
   fill = lines( gt_scr8 ).
ENDMODULE.

MODULE transp_itab_out_8 OUTPUT.
  idx = sy-stepl + line.
  gs_scr8 = VALUE #( gt_scr8[ idx ] OPTIONAL ).
ENDMODULE.

*----------------------------------------------------------------------*
MODULE transp_itab_in8 INPUT.
  lines = sy-loopc.
  idx = sy-stepl + line.
  MODIFY gt_scr8 FROM gs_scr8 INDEX idx.
ENDMODULE.

*----------------------------------------------------------------------*
MODULE user_command_scr800 INPUT.
  CASE sy-ucomm.
    WHEN 'PGDN'.
      line = line + 1.
      limit = fill - lines.
      IF line > limit.
        line = limit.
      ENDIF.
    WHEN 'PGUP'.
      line = line - 1.
      IF line < 0.
        line = 0.
      ENDIF.
    WHEN 'PB1' OR 'F7'.
      CLEAR: gv_sel_scr8,line,lines.
  ENDCASE.
ENDMODULE.
