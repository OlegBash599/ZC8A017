CLASS zcl_c8a017_longtxt_templ DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA mv_templ TYPE string READ-ONLY .
    DATA mv_templ_with_data TYPE string .

    METHODS constructor
      IMPORTING
        !is_txt_h       TYPE thead
        !iv_remove_vars TYPE abap_bool DEFAULT abap_true.
    METHODS data2template
      IMPORTING !is            TYPE any
      EXPORTING et_unused_vars TYPE zif_c8a017_types=>tt_var_templ
      RETURNING VALUE(rv)      TYPE string .

    METHODS process_tmpl_with_data
      IMPORTING !is            TYPE any
      EXPORTING et_unused_vars TYPE zif_c8a017_types=>tt_var_templ
      RETURNING VALUE(rv)      TYPE string .


  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_txt_src TYPE thead.
    DATA mv_remove_unused_vars TYPE abap_bool.

    METHODS _remove_not_exist_vars
      EXPORTING et_unused_vars  TYPE zif_c8a017_types=>tt_var_templ
      CHANGING  cv_str_template TYPE string.


ENDCLASS.



CLASS zcl_c8a017_longtxt_templ IMPLEMENTATION.


  METHOD constructor.

    MOVE-CORRESPONDING is_txt_h TO ms_txt_src.

    IF ms_txt_src-tdspras IS INITIAL.
      ms_txt_src-tdspras = sy-langu.
    ENDIF.

    IF ms_txt_src-tdid IS INITIAL.
      ms_txt_src-tdid = 'ST'.
    ENDIF.

    IF ms_txt_src-tdobject IS INITIAL.
      ms_txt_src-tdobject = 'TEXT'.
    ENDIF.

    mv_remove_unused_vars = iv_remove_vars.

  ENDMETHOD.


  METHOD data2template.

    DATA lt_unused_vars TYPE zif_c8a017_types=>tt_var_templ.

    rv =
    NEW zcl_c8a017_sapscript_read( is_thead = ms_txt_src )->r_as_string( ).



    NEW zcl_c8a017_struct2templ( )->fill_vars_in_template(
      EXPORTING
        is_src          = is
      CHANGING
        cv_str_template = rv
    ).


    _remove_not_exist_vars( IMPORTING et_unused_vars = lt_unused_vars
                            CHANGING cv_str_template = rv ).

    et_unused_vars = lt_unused_vars.
  ENDMETHOD.

  METHOD process_tmpl_with_data.
*      IMPORTING !is            TYPE any
*      EXPORTING et_unused_vars TYPE zif_c8a017_types=>tt_var_templ
*      RETURNING VALUE(rv)      TYPE string .
    DATA lt_tline_str     TYPE zif_c8a017_types=>tt_tline_str.

    NEW zcl_c8a017_sapscript_read( is_thead = ms_txt_src )->r_as_str_lines( IMPORTING et = lt_tline_str ).


    new zcl_c8a017_templ_with_data(  )->proc_templ(
      EXPORTING
        it_lines     = lt_tline_str
        is_src       = is
      IMPORTING
        ev_final_str = rv
    ).

  ENDMETHOD.

  METHOD _remove_not_exist_vars.
    "IMPORTING et_var_list TYPE zif_c8a017_types=>tt_var_templ
    "changing cv_str_template TYPE string.

    DATA lt_results_match TYPE  match_result_tab.
    DATA lt_var_list TYPE zif_c8a017_types=>tt_var_templ.
    DATA ls_var_templ TYPE zif_c8a017_types=>ts_var_templ.
    FIELD-SYMBOLS <fs_result_match> TYPE match_result.

    FIND ALL OCCURRENCES OF REGEX zif_c8a017_hardvals=>mc_reg-regex_word_var IN cv_str_template
        RESULTS lt_results_match.

    LOOP AT lt_results_match ASSIGNING <fs_result_match>.
      CLEAR ls_var_templ.
      ls_var_templ-var_id = cv_str_template+<fs_result_match>-offset(<fs_result_match>-length).
      CLEAR ls_var_templ-var_val.
      APPEND ls_var_templ TO lt_var_list.
    ENDLOOP.


    IF mv_remove_unused_vars EQ abap_true.
      LOOP AT lt_var_list INTO  ls_var_templ.
        REPLACE ALL OCCURRENCES OF ls_var_templ-var_id IN cv_str_template WITH ls_var_templ-var_val.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
