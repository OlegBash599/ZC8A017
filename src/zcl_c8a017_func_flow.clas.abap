CLASS zcl_c8a017_func_flow DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.

    INTERFACES zif_c8a017_func.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ts_infunc_params TYPE zif_c8a017_types=>ts_infunc_params.

    DATA mo_func_frame TYPE REF TO zcl_c8a017_func_frame.

    METHODS _exe_cond_func
      IMPORTING is_func_params   TYPE ts_infunc_params
                is_cntx          TYPE any
      EXPORTING ev_cond_flow_res TYPE abap_bool.



ENDCLASS.



CLASS zcl_c8a017_func_flow IMPLEMENTATION.
  METHOD constructor.
    mo_func_frame = NEW #(  ).
  ENDMETHOD.

  METHOD zif_c8a017_func~do_flow_control.
*    IMPORTING is_func_params  TYPE zif_c8a017_types=>ts_infunc_params
*              it_templ_all    TYPE zif_c8a017_types=>tt_tline_str
*              is_line_str     TYPE zif_c8a017_types=>ts_tline_str
*              iv_line_num     TYPE syindex
*              is_cntx         TYPE any
*    CHANGING  cs_flow_control TYPE zif_c8a017_types=>ts_flow_control.

    DATA lv_cond_flow_res TYPE abap_bool.
    DATA lv_close_operator TYPE string.

*    IF cs_flow_control-block_func_cntx IS INITIAL
*       AND is_func_params-func_name IS NOT INITIAL.
*      cs_flow_control-block_func_cntx = is_func_params-func_name.
*    ENDIF.
*
*    IF is_func_params-func_name EQ 'IF_BEG'
*      AND cs_flow_control-block_func_cntx EQ 'IF_BEG'.
*      cs_flow_control-func_deep = cs_flow_control-func_deep + 1.
*    ENDIF.
*
*    IF is_func_params-func_name EQ 'IF_END'
*      AND cs_flow_control-block_func_cntx EQ 'IF_BEG'.
*      cs_flow_control-func_deep = cs_flow_control-func_deep - 1.
*    ENDIF.

    IF is_func_params-func_name EQ 'IF_BEG'.
      lv_close_operator = 'IF_END'.
      _exe_cond_func( EXPORTING is_func_params = is_func_params
                                is_cntx = is_cntx
                      IMPORTING ev_cond_flow_res = lv_cond_flow_res ).

      cs_flow_control-do_skip_this_line = abap_true.
      IF lv_cond_flow_res EQ abap_true.
        CLEAR cs_flow_control-next_line4proc.
        " next line is next line
      ELSE.
        " after IF_END
        mo_func_frame->fill_line_after_close(
           EXPORTING it_templ_all = it_templ_all
                     iv_from_line = iv_line_num
                     iv_operator_open = is_func_params-func_name
                     iv_operator_close = lv_close_operator
           CHANGING cs_flow_control = cs_flow_control
        ).
      ENDIF.
      "

    ENDIF.

*    IF is_func_params-func_name EQ 'IF_END'.
*      IF cs_flow_control-func_deep EQ 0.
*        CLEAR cs_flow_control-block_func_cntx.
*        CLEAR cs_flow_control-do_skip_this_line.
*      ENDIF.
*    ENDIF.



  ENDMETHOD.

  METHOD zif_c8a017_func~do_conv.
    " empty
  ENDMETHOD.

  METHOD _exe_cond_func.
    "IMPORTING is_func_params   TYPE ts_infunc_params
    "EXPORTING ev_cond_flow_res TYPE abap_bool.
    DATA lv_trg_fieldname TYPE fieldname.
    DATA lv_param_num TYPE syindex.
    DATA lv_val1_str TYPE string.
    DATA lv_val2_str TYPE string.

    FIELD-SYMBOLS <fs_func_params> TYPE zif_c8a017_types=>ts_func_param.
    FIELD-SYMBOLS <fs_src_in> TYPE any.
    FIELD-SYMBOLS <fs_val_str> TYPE string.

    CASE is_func_params-func_name.

      WHEN 'IF_BEG'.

        lv_param_num = 0.
        LOOP AT is_func_params-t_func_params ASSIGNING <fs_func_params>.

          lv_param_num = lv_param_num + 1.
          CASE lv_param_num.
            WHEN 1.
              ASSIGN lv_val1_str TO <fs_val_str>.
            WHEN 2.
              ASSIGN lv_val2_str TO <fs_val_str>.
            WHEN OTHERS.
              EXIT.
          ENDCASE.

          IF <fs_func_params>-p_val CP '$*$'.
            lv_trg_fieldname = mo_func_frame->get_trg_fieldname( iv_rtti_param = <fs_func_params>-p_val ).
            "            ASSIGN msr_src->* TO <fs_src_in>.
            ASSIGN is_cntx TO <fs_src_in>.
            IF sy-subrc EQ 0.
              mo_func_frame->read_trg_value_as_str( EXPORTING iv_fnm       = lv_trg_fieldname
                                                is_cntx      = <fs_src_in>
                                      IMPORTING ev_val_str   = <fs_val_str> ).
            ENDIF.

          ELSE.
            <fs_val_str> = <fs_func_params>-p_val.
          ENDIF.

        ENDLOOP.

        IF lv_val1_str EQ lv_val2_str.
          ev_cond_flow_res = abap_true.
        ENDIF.


      WHEN OTHERS.

    ENDCASE.


  ENDMETHOD.

ENDCLASS.
