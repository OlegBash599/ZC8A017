CLASS zcl_c8a017_func_conv DEFINITION
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



CLASS zcl_c8a017_func_conv IMPLEMENTATION.
  METHOD constructor.
    mo_func_frame = NEW #(  ).
  ENDMETHOD.

  METHOD zif_c8a017_func~do_conv.
*    IMPORTING is_func_params TYPE zif_c8a017_types=>ts_infunc_params
*              is_line_str    TYPE zif_c8a017_types=>ts_tline_str
*              is_cntx        TYPE any
*    CHANGING  cs_line_res    TYPE zif_c8a017_types=>ts_tline_str.

    DATA lv_doc_clear_line TYPE abap_bool.
    DATA lv_res_output TYPE string.

    DATA lv_trg_fieldname TYPE fieldname.
    DATA lv_field_not_exist TYPE abap_bool.
    DATA lv_val_as_str TYPE string.

    FIELD-SYMBOLS <fs_func_params> TYPE zif_c8a017_types=>ts_func_param.
    FIELD-SYMBOLS <fs_src_in> TYPE any.


    CASE is_func_params-func_name.
      WHEN 'VMESTE'.
        LOOP AT is_func_params-t_func_params ASSIGNING <fs_func_params>.
          IF <fs_func_params>-p_val CP '$*$'.

            lv_trg_fieldname = mo_func_frame->get_trg_fieldname( <fs_func_params>-p_val ).
            ASSIGN is_cntx TO <fs_src_in>.
            IF sy-subrc EQ 0.
              mo_func_frame->read_trg_value_as_str( EXPORTING iv_fnm       = lv_trg_fieldname
                                                is_cntx      = <fs_src_in>
                                      IMPORTING ev_val_str   = lv_val_as_str
                                                ev_not_exist = lv_field_not_exist ).

              IF lv_field_not_exist EQ abap_false
                AND lv_val_as_str IS NOT INITIAL.
                lv_res_output = lv_res_output && ` ` && lv_val_as_str.
              ELSE.
                lv_doc_clear_line = abap_true.
                EXIT.
              ENDIF.

            ENDIF.

          ELSE.

            lv_res_output = lv_res_output && ` ` && <fs_func_params>-p_val.

          ENDIF.
        ENDLOOP.

    ENDCASE.


    IF lv_doc_clear_line EQ abap_true.
      CLEAR cs_line_res.
      RETURN.
    ENDIF.

    cs_line_res-tdformat = is_line_str-tdformat.
    cs_line_res-tdline_str = lv_res_output.

  ENDMETHOD.

  METHOD zif_c8a017_func~do_flow_control.
    " nothing
  ENDMETHOD.

ENDCLASS.
