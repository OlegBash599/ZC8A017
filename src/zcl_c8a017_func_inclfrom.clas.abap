CLASS zcl_c8a017_func_inclfrom DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.

    INTERFACES zif_c8a017_func.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_func_frame TYPE REF TO zcl_c8a017_func_frame.

    CONSTANTS: BEGIN OF mc_params_nm
                   , src TYPE string VALUE 'SRC'
                   , srctype TYPE string VALUE 'SRCTY'
              , END OF mc_params_nm
              .

    TYPES: BEGIN OF ts_parsed_params
                , src TYPE string
                , srctype TYPE string
           , END OF ts_parsed_params
           .
    DATA ms_parsed_params TYPE ts_parsed_params.

    DATA ms_env_params   TYPE zif_c8a017_types=>ts_src_params.

    METHODS _parse_params
      IMPORTING is_func_params TYPE zif_c8a017_types=>ts_infunc_params.

    METHODS _read_lines_n_parse
      EXPORTING et_include_lines TYPE zif_c8a017_types=>tt_tline_str.

ENDCLASS.



CLASS zcl_c8a017_func_inclfrom IMPLEMENTATION.
  METHOD constructor.
    mo_func_frame = NEW #(  ).
    " ?/:include_from(src="incl/script_block.js.flstr";srcty="0") <!-- функция с параметрами -->
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
*              is_env_params   TYPE zif_c8a017_types=>ts_src_params
*    CHANGING  cs_flow_control TYPE zif_c8a017_types=>ts_flow_control.


    DATA lt_include_lines TYPE zif_c8a017_types=>tt_tline_str.
    FIELD-SYMBOLS <fs_tab> TYPE table.
    FIELD-SYMBOLS <fs_line> TYPE any.

    ms_env_params = is_env_params.

    cs_flow_control-do_skip_this_line = abap_true.

    _parse_params( is_func_params ).

    IF ms_parsed_params-src IS INITIAL.
      RETURN.
    ENDIF.

    _read_lines_n_parse( IMPORTING et_include_lines = lt_include_lines ).

    cs_flow_control-lines_before_next = lt_include_lines.

  ENDMETHOD.

  METHOD _parse_params.
    FIELD-SYMBOLS <fs_func_params> TYPE zif_c8a017_types=>ts_func_param.

    CLEAR ms_parsed_params.
    LOOP AT is_func_params-t_func_params ASSIGNING <fs_func_params>.
      CASE to_upper( <fs_func_params>-p_name ).
        WHEN mc_params_nm-src.
          ms_parsed_params-src = <fs_func_params>-p_val.
        WHEN mc_params_nm-srctype.
          ms_parsed_params-srctype = <fs_func_params>-p_val.

        WHEN OTHERS.

      ENDCASE.
      replace all OCCURRENCES OF '"' IN ms_parsed_params-src WITH ''.
    ENDLOOP.
  ENDMETHOD.

  METHOD _read_lines_n_parse.
    "EXPORTING et_include_lines TYPE zif_c8a017_types=>tt_tline_str.

    TYPES: BEGIN OF ts_dummy_src
                , any_field TYPE string
          , END OF ts_dummy_src
          .

    DATA ls_dummy_src TYPE ts_dummy_src.

    DATA lt_txt_lines TYPE zif_c8a017_types=>tt_tline_str.
    DATA lt_res_lines TYPE zif_c8a017_types=>tt_tline_str.
    DATA ls_params TYPE zif_c8a017_types=>ts_src_params.

    DATA ls_src_in TYPE zif_c8a017_types=>ts_src_info.

    DATA lo_src_lines TYPE REF TO zif_c8a017_src_lines.
    CREATE OBJECT lo_src_lines TYPE zcl_c8a017_file_client.

    " folder должно быть со / в конце
    ls_src_in-path2file = ms_env_params-file_prm-folder && ms_parsed_params-src.

    lo_src_lines->r_as_str_lines(
      EXPORTING
        is_src    = ls_src_in
      IMPORTING
        et        = lt_txt_lines
        es_params = ls_params
    ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF ls_params-file_prm-extensn EQ 'flstr'.
      NEW zcl_c8a017_templ_with_data(  )->proc_templ(
        EXPORTING
          it_lines      = lt_txt_lines
          is_src        = ls_dummy_src
          is_env_params = ls_params
        IMPORTING
*            ev_final_str  =
          et_res_lines  = lt_res_lines
      ).
    ELSE.
      lt_res_lines = lt_txt_lines.
    ENDIF.


    et_include_lines = lt_res_lines.

  ENDMETHOD.
ENDCLASS.
