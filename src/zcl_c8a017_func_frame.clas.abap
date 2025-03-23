CLASS zcl_c8a017_func_frame DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_trg_fieldname
      IMPORTING iv_rtti_param TYPE string
      RETURNING VALUE(rv)     TYPE fieldname.

    METHODS read_trg_value_as_str
      IMPORTING iv_fnm       TYPE fieldname
                is_cntx      TYPE any
      EXPORTING ev_val_str   TYPE string
                ev_not_exist TYPE abap_bool.


    METHODS fill_line_after_close
      IMPORTING it_templ_all      TYPE zif_c8a017_types=>tt_tline_str
                iv_from_line      TYPE syindex
                iv_operator_open  TYPE string
                iv_operator_close TYPE string
      CHANGING  cs_flow_control   TYPE zif_c8a017_types=>ts_flow_control.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_c8a017_func_frame IMPLEMENTATION.
  METHOD get_trg_fieldname.
    "IMPORTING iv_rtti_param TYPE string
    "RETURNING VALUE(rv)     TYPE fieldname.
    DATA lv_cell_nm TYPE string.

    DATA lv_strlen TYPE syindex.

    lv_cell_nm = iv_rtti_param+1.
    lv_strlen = strlen( lv_cell_nm ).
    lv_strlen = lv_strlen - 1.
    lv_cell_nm = lv_cell_nm(lv_strlen).

    "rv = to_upper( lv_cell_nm ).
    TRANSLATE lv_cell_nm TO UPPER CASE.
    rv = lv_cell_nm.
  ENDMETHOD.

  METHOD read_trg_value_as_str.
*      IMPORTING iv_fnm       TYPE fieldname
*                is_cntx      TYPE any
*      EXPORTING ev_val_str   TYPE string
*                ev_not_exist TYPE abap_bool.

    FIELD-SYMBOLS <fs_val> TYPE any.
    ASSIGN COMPONENT iv_fnm OF STRUCTURE is_cntx TO <fs_val>.
    IF sy-subrc EQ 0.
      IF <fs_val> IS INITIAL.
        CLEAR ev_val_str.
        RETURN.
        EXIT.
      ELSE.
        ev_val_str = <fs_val>.
      ENDIF.
      RETURN.
    ELSE.
      " for type pools vars
      ASSIGN (iv_fnm) TO <fs_val>.
      IF sy-subrc EQ 0.
        IF <fs_val> IS INITIAL.
          CLEAR ev_val_str.
          RETURN.
        ENDIF.
        ev_val_str = <fs_val>.
        RETURN.
      ENDIF.
    ENDIF.

    ev_not_exist = abap_true.
    CLEAR ev_val_str.
    RETURN.


  ENDMETHOD.

  METHOD fill_line_after_close.
*           EXPORTING it_templ_all = it_templ_all
*                     iv_from_line = iv_line_num
*                     iv_operator_open = is_func_params-func_name
*                     iv_operator_close = lv_close_operator
*           CHANGING cs_flow_control = cs_flow_control

    DATA lv_templ_current_sub TYPE sytabix.
    DATA lv_beg_inside TYPE syindex.
    FIELD-SYMBOLS <fs_line_str_sub> TYPE zif_c8a017_types=>ts_tline_str.

    LOOP AT it_templ_all ASSIGNING <fs_line_str_sub> FROM iv_from_line
        WHERE tdformat EQ '*'.
      lv_templ_current_sub = sy-tabix.
      IF ( strlen( <fs_line_str_sub>-tdline_str ) GE 3
            AND <fs_line_str_sub>-tdline_str(3) EQ '?/:' )

         OR ( <fs_line_str_sub>-tdline_str EQ iv_operator_open
               OR <fs_line_str_sub>-tdline_str EQ iv_operator_close )

            .

        IF <fs_line_str_sub>-tdline_str CS iv_operator_close.
          lv_beg_inside = lv_beg_inside - 1.
          IF lv_beg_inside EQ 0.
            cs_flow_control-next_line4proc = lv_templ_current_sub + 1.
            EXIT.
          ENDIF.
        ENDIF.

        IF <fs_line_str_sub>-tdline_str CS iv_operator_open.
          lv_beg_inside = lv_beg_inside + 1.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
