CLASS zcl_c8a017_func_tabfor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.

    INTERFACES zif_c8a017_func.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_func_frame TYPE REF TO zcl_c8a017_func_frame.
ENDCLASS.



CLASS zcl_c8a017_func_tabfor IMPLEMENTATION.
  METHOD constructor.
    mo_func_frame = NEW #(  ).
    " FOR_BEG($TAB$;)
  ENDMETHOD.

  METHOD zif_c8a017_func~do_conv.
    " do nothing
  ENDMETHOD.

  METHOD zif_c8a017_func~do_flow_control.
*    IMPORTING is_func_params  TYPE zif_c8a017_types=>ts_infunc_params
*              it_templ_all    TYPE zif_c8a017_types=>tt_tline_str
*              is_line_str     TYPE zif_c8a017_types=>ts_tline_str
*              iv_line_num     TYPE syindex
*              is_cntx         TYPE any
*    CHANGING  cs_flow_control TYPE zif_c8a017_types=>ts_flow_control.
    DATA lv_open_operator TYPE string.
    DATA lv_close_operator TYPE string.

    DATA lv_tab_name TYPE string.
    FIELD-SYMBOLS <fs_tab> TYPE table.
    FIELD-SYMBOLS <fs_line> TYPE any.

    DATA lv_do_function TYPE abap_bool.

    DATA ls_func_param TYPE zif_c8a017_types=>ts_func_param.


    IF is_func_params-func_name EQ 'FOR_BEG'.
      lv_open_operator  = is_func_params-func_name.
      lv_close_operator = 'FOR_END'.
      lv_do_function    = abap_true.

      lv_tab_name =
        mo_func_frame->get_trg_fieldname(
          iv_rtti_param =
          VALUE #( is_func_params-t_func_params[ 1 ]-p_val OPTIONAL )
          ).

    ENDIF.


    IF is_func_params-func_name EQ 'FIND_N_REP_TAB'.
      ls_func_param = VALUE #( is_func_params-t_func_params[ 1 ] OPTIONAL ).
      IF ls_func_param IS NOT INITIAL.
        lv_open_operator  = '$[' && ls_func_param-p_val && '$'.
        lv_close_operator = '$'  && ls_func_param-p_val && ']$'.

        lv_do_function = abap_true.

        lv_tab_name = ls_func_param-p_val.
      ENDIF.

    ENDIF.

    IF lv_tab_name IS INITIAL.
      RETURN.
    ENDIF.

    IF lv_do_function EQ abap_true.

      cs_flow_control-do_skip_this_line = abap_true.
      mo_func_frame->fill_line_after_close(
         EXPORTING it_templ_all      = it_templ_all
                   iv_from_line      = iv_line_num
                   iv_operator_open  = lv_open_operator
                   iv_operator_close = lv_close_operator
         CHANGING cs_flow_control    = cs_flow_control
      ).


      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      DATA lv_templ_from TYPE syindex.
      DATA lv_templ_upto TYPE syindex.
      DATA ls_html_res4line TYPE zif_c8a017_types=>ts_tline_str.
      DATA lt_templ4tab TYPE zif_c8a017_types=>tt_tline_str.
      FIELD-SYMBOLS <fs_templ_line> TYPE zif_c8a017_types=>ts_tline_str.

      data lt_res_lines  TYPE zif_c8a017_types=>tt_tline_str.

      DATA lo_templ_with_data TYPE REF TO zcl_c8a017_templ_with_data.
      CREATE OBJECT lo_templ_with_data.

      lv_templ_from = iv_line_num + 1.
      lv_templ_upto = cs_flow_control-next_line4proc - 2.

      CLEAR lt_templ4tab.
      LOOP AT it_templ_all ASSIGNING <fs_templ_line>
        FROM lv_templ_from TO lv_templ_upto.
        APPEND <fs_templ_line> TO lt_templ4tab.
      ENDLOOP.


      ASSIGN COMPONENT lv_tab_name OF STRUCTURE is_cntx TO <fs_tab>.
      IF sy-subrc EQ 0.
        LOOP AT <fs_tab> ASSIGNING <fs_line>.
          CLEAR ls_html_res4line.
          lo_templ_with_data->proc_templ(
            EXPORTING
              it_lines     = lt_templ4tab
              is_src       = <fs_line>
            IMPORTING
              ev_final_str = ls_html_res4line-tdline_str
              et_res_lines = lt_res_lines
          ).
        "  ls_html_res4line-tdformat = '*'.
        "  APPEND ls_html_res4line TO cs_flow_control-lines_before_next.
        APPEND LINES OF lt_res_lines to cs_flow_control-lines_before_next.
        ENDLOOP.
      ENDIF.




    ENDIF.

  ENDMETHOD.
ENDCLASS.
