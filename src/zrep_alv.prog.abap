*&---------------------------------------------------------------------*
*& Report ZREP_ALV
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrep_alv.

TABLES: kna1.

TYPES: BEGIN OF ty_alv,
         kunnr      TYPE kunnr,
         land1      TYPE land1_gp,
         name1      TYPE name1_gp,
         name2      TYPE name2_gp,
         "Add calculated column into the report
         calculated TYPE i,
       END OF ty_alv.

DATA: lt_alv TYPE STANDARD TABLE OF ty_alv,
      ls_alv LIKE LINE OF lt_alv.


DATA: go_alv TYPE REF TO cl_salv_table.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: so_kunnr FOR kna1-kunnr,
                so_land1 FOR kna1-land1.

SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
* Main processing
  PERFORM process_data.

  PERFORM display_alv.

*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.

  DATA: lv_counter TYPE i.

  SELECT
    kunnr
    land1
    name1
    name2
  UP TO 200 ROWS
  FROM
    kna1
  INTO TABLE
    lt_alv
  WHERE
    kunnr IN so_kunnr AND
    land1 IN so_land1.

  LOOP AT lt_alv INTO ls_alv.
    lv_counter = lv_counter + 1.
    ls_alv-calculated = lv_counter.
    MODIFY lt_alv FROM ls_alv TRANSPORTING calculated.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .

* Data declarations
  DATA: lv_msg            TYPE string,
        lo_msg            TYPE REF TO cx_salv_msg,
        lo_columns        TYPE REF TO cl_salv_columns_table,
        lo_column         TYPE REF TO cl_salv_column_table,
* ALV reference
*       lo_alv            TYPE REF TO cl_salv_table,
        lo_events         TYPE REF TO cl_salv_events_table,
        lo_functions_list TYPE REF TO cl_salv_functions_list,
        lo_salv_not_found TYPE REF TO cx_salv_not_found.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = lt_alv ).
    CATCH cx_salv_msg INTO lo_msg.
      MESSAGE lo_msg TYPE 'E' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.

  ENDTRY.

*BOC  E-ALELOPEZ  26.11.2019
  DATA: lr_aggregations TYPE REF TO cl_salv_aggregations.
  DATA: lr_groups TYPE REF TO cl_salv_sorts .
  DATA: toolbar TYPE REF TO cl_salv_functions_list .

  lr_aggregations = go_alv->get_aggregations( ).
  toolbar = go_alv->get_functions( ) .
  toolbar->set_all(
    value  = if_salv_c_bool_sap=>true ).

  lr_aggregations->clear( ).
  lr_groups = go_alv->get_sorts( ).
  lr_groups->clear( ).

* Set screen status with custom function
  go_alv->set_screen_status(
            pfstatus = 'SALV_TABLE_STANDARD'
            report = sy-repid
            set_functions = go_alv->c_functions_all ).

  lo_columns = go_alv->get_columns( ).
  lo_columns->set_optimize( ).

* Set Layout
  DATA: lr_layout TYPE REF TO cl_salv_layout.
  DATA: ls_key TYPE salv_s_layout_key.
  lr_layout = go_alv->get_layout( ).
  ls_key-report = sy-repid.
  lr_layout->set_key( ls_key ).
  lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

* Display ALV grid
  go_alv->display( ).

ENDFORM.
