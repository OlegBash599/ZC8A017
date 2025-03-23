CLASS zcl_c8a017_line_proctype DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.

    METHODS fill_func_params_by_line
      IMPORTING it_templ_all   TYPE zif_c8a017_types=>tt_tline_str
                is_templ_cur   TYPE zif_c8a017_types=>ts_tline_str
                iv_line_num    TYPE syindex
      EXPORTING es_func_params TYPE zif_c8a017_types=>ts_infunc_params.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: ts_infunc_params TYPE zif_c8a017_types=>ts_infunc_params.
    TYPES: ts_func_param TYPE zif_c8a017_types=>ts_func_param.
    TYPES: tt_func_param TYPE zif_c8a017_types=>tt_func_param.

    DATA mt_func_info TYPE zif_c8a017_types=>tt_func_info.

    METHODS _fill_func_list.

    METHODS _parse_infunc_data
      IMPORTING is_line_str      TYPE zif_c8a017_types=>ts_tline_str
      EXPORTING es_infunc_params TYPE ts_infunc_params.

    METHODS _if_tab_replace
      IMPORTING is_templ_cur   TYPE zif_c8a017_types=>ts_tline_str
      CHANGING  cs_func_params TYPE ts_infunc_params
      RETURNING VALUE(rc)      TYPE sysubrc.


ENDCLASS.



CLASS zcl_c8a017_line_proctype IMPLEMENTATION.
  METHOD constructor.
    _fill_func_list(  ).
  ENDMETHOD.

  METHOD fill_func_params_by_line.
*      IMPORTING it_templ_all   TYPE zif_c8a017_types=>tt_tline_str
*                is_templ_cur   TYPE zif_c8a017_types=>ts_tline_str
*                iv_line_num    TYPE syindex
*      EXPORTING es_func_params TYPE zif_c8a017_types=>ts_infunc_params.


    DATA ls_infunc_params TYPE ts_infunc_params.
    FIELD-SYMBOLS <fs_func_info> TYPE zif_c8a017_types=>ts_func_info.

    CLEAR es_func_params.

    es_func_params-proc_type = zif_c8a017_hardvals=>mc_proc_type-move_as_it_is.

    IF strlen( is_templ_cur-tdline_str ) GE 3.
      IF is_templ_cur-tdformat EQ '*'
          AND is_templ_cur-tdline_str(3) EQ '?/:'.
        _parse_infunc_data( EXPORTING is_line_str = is_templ_cur
                            IMPORTING es_infunc_params = ls_infunc_params ).

        READ TABLE mt_func_info ASSIGNING <fs_func_info>
            WITH KEY func_name = to_upper( ls_infunc_params-func_name ).
        IF sy-subrc EQ 0.
          es_func_params = ls_infunc_params.
          es_func_params-proc_type = <fs_func_info>-proc_type.
          es_func_params-func_processor = <fs_func_info>-func_processor.
          RETURN.
        ELSE.
          CLEAR es_func_params.
          es_func_params-proc_type = zif_c8a017_hardvals=>mc_proc_type-exclude_this_line.
          RETURN.
        ENDIF.

      ENDIF.
    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    DATA lv_regex_var TYPE string.
    DATA lt_results_match TYPE match_result_tab.
    DATA ls_func_param TYPE ts_func_param.
    DATA lv_length_var TYPE syindex.
    DATA lv_var_name TYPE string.
    FIELD-SYMBOLS <fs_result_match> TYPE match_result.
    lv_regex_var = zif_c8a017_hardvals=>mc_reg-regex_word_var.
    FIND ALL OCCURRENCES OF REGEX lv_regex_var IN is_templ_cur-tdline_str
         RESULTS lt_results_match.
    IF lines( lt_results_match ) > 0.
      es_func_params-proc_type = zif_c8a017_hardvals=>mc_proc_type-find_and_replace.
      es_func_params-func_name = 'FIND_N_REPLACE'.
      LOOP AT lt_results_match ASSIGNING <fs_result_match>.
        CLEAR ls_func_param.
        ls_func_param-p_name = is_templ_cur-tdline_str+<fs_result_match>-offset(<fs_result_match>-length).
        lv_var_name = ls_func_param-p_name+1.

        lv_length_var = strlen( lv_var_name ).
        lv_length_var = lv_length_var - 1.
        lv_var_name = lv_var_name(lv_length_var).
        ls_func_param-p_val = lv_var_name.
        APPEND ls_func_param TO es_func_params-t_func_params.
      ENDLOOP.
      RETURN.
    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    IF _if_tab_replace(
        EXPORTING is_templ_cur  = is_templ_cur
        CHANGING cs_func_params = es_func_params
        ) EQ 0.
      RETURN.
    ENDIF.


  ENDMETHOD.

  METHOD _if_tab_replace.
*    IMPORTING is_templ_cur   TYPE zif_c8a017_types=>ts_tline_str
*    CHANGING  cs_func_params TYPE ts_infunc_params
*    RETURNING VALUE(rc)      TYPE sysubrc.

    rc = 1.

    DATA lv_regex_var_tab TYPE string .

    DATA lt_results_match TYPE match_result_tab.
    DATA ls_func_param TYPE ts_func_param.

    DATA lv_length_var TYPE syindex.
    DATA lv_var_name TYPE string.

    FIELD-SYMBOLS <fs_result_match> TYPE match_result.
    FIELD-SYMBOLS <fs_func_info> TYPE zif_c8a017_types=>ts_func_info.

    lv_regex_var_tab = zif_c8a017_hardvals=>mc_reg-regex_tab_var_beg.

    FIND ALL OCCURRENCES OF REGEX lv_regex_var_tab IN is_templ_cur-tdline_str
         RESULTS lt_results_match.
    IF lines( lt_results_match ) > 0.

      cs_func_params-proc_type = zif_c8a017_hardvals=>mc_proc_type-flow_control.
      cs_func_params-func_name = 'FIND_N_REP_TAB'.
      cs_func_params-func_processor =
       VALUE #( mt_func_info[ func_name = cs_func_params-func_name ]-func_processor OPTIONAL ).

      LOOP AT lt_results_match ASSIGNING <fs_result_match>.
        CLEAR ls_func_param.
        ls_func_param-p_name = is_templ_cur-tdline_str+<fs_result_match>-offset(<fs_result_match>-length).

        lv_var_name     = ls_func_param-p_name+2.
        lv_length_var   = strlen( lv_var_name ).
        lv_length_var   = lv_length_var - 1.
        lv_var_name     = lv_var_name(lv_length_var).

        ls_func_param-p_val = lv_var_name.

        APPEND ls_func_param TO cs_func_params-t_func_params.

        rc = 0.

        " only one tab allowed in line
        EXIT.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD _parse_infunc_data.
    "IMPORTING is_line_str      TYPE zif_c8a017_types=>ts_tline_str
    "EXPORTING es_infunc_params TYPE ts_infunc_params.

    DATA lv_func_name TYPE string.
    DATA ls_func_param TYPE ts_func_param.
    DATA lt_func_param TYPE tt_func_param.
    DATA lt_func_params_str TYPE stringtab.

    DATA lv_str_beg_params TYPE char1 VALUE '('.
    DATA lv_str_end_params TYPE char1 VALUE ')'.
    DATA lv_str_sep_params TYPE char1 VALUE ';'.
    DATA lv_str_sep_eq TYPE char1 VALUE '='.
    DATA lt_split_res TYPE stringtab.

    DATA lv_func_params_part TYPE string.
    DATA lv_func_params_body TYPE string.
    DATA lv_func_after_params TYPE string.

    SPLIT is_line_str-tdline_str+3 AT lv_str_beg_params
        INTO lv_func_name lv_func_params_part.

    SPLIT lv_func_params_part AT lv_str_end_params
        INTO lv_func_params_body lv_func_after_params.

    SPLIT lv_func_params_body AT lv_str_sep_params
        INTO TABLE lt_func_params_str.

    FIELD-SYMBOLS <fs_func_params> TYPE string.
    FIELD-SYMBOLS <fs_func_info> TYPE zif_c8a017_types=>ts_func_info.


    lv_func_name = to_upper( lv_func_name ).

    READ TABLE mt_func_info ASSIGNING <fs_func_info>
        WITH KEY func_name = lv_func_name.
    IF sy-subrc EQ 0.
      IF <fs_func_info>-no_name_val_param EQ abap_true.
        LOOP AT lt_func_params_str ASSIGNING <fs_func_params>.
          CLEAR ls_func_param.
          ls_func_param-p_val = <fs_func_params>.


          IF ls_func_param IS NOT INITIAL.
            APPEND ls_func_param TO lt_func_param.
          ENDIF.

        ENDLOOP.

      ELSE.
        LOOP AT lt_func_params_str ASSIGNING <fs_func_params>.
          CLEAR ls_func_param.
          IF <fs_func_params> CS lv_str_sep_eq.
            SPLIT <fs_func_params> AT lv_str_sep_eq
                INTO ls_func_param-p_name ls_func_param-p_val.
          ELSE.
            ls_func_param-p_val = <fs_func_params>.
          ENDIF.
          IF ls_func_param IS NOT INITIAL.
            APPEND ls_func_param TO lt_func_param.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.



    es_infunc_params-func_name = lv_func_name.
    es_infunc_params-t_func_params = lt_func_param.

  ENDMETHOD.

  METHOD _fill_func_list.
    mt_func_info = VALUE #(

    (  func_name = 'VMESTE' func_processor = 'ZCL_C8A017_FUNC_CONV'
        proc_type = zif_c8a017_hardvals=>mc_proc_type-calc_conv_func
            no_name_val_param = abap_true )

    (  func_name = 'IF_BEG' func_processor = 'ZCL_C8A017_FUNC_FLOW'
        proc_type = zif_c8a017_hardvals=>mc_proc_type-flow_control )

    (  func_name = 'FOR_BEG' func_processor = 'ZCL_C8A017_FUNC_TABFOR'
        proc_type = zif_c8a017_hardvals=>mc_proc_type-flow_control )

    (  func_name = 'INCLUDE_FROM' func_processor = 'ZCL_C8A017_FUNC_INCLFROM'
        proc_type = zif_c8a017_hardvals=>mc_proc_type-flow_control )



    (  func_name = 'FIND_N_REPLACE' proc_type = zif_c8a017_hardvals=>mc_proc_type-find_and_replace )

    " = should be the same as FOR_BEG with no add params =
    (  func_name = 'FIND_N_REP_TAB' proc_type = zif_c8a017_hardvals=>mc_proc_type-flow_control
        func_processor = 'ZCL_C8A017_FUNC_TABFOR' )


    ).
  ENDMETHOD.

ENDCLASS.
