*&---------------------------------------------------------------------*
*&  Include           ZREP_C8A017_FLORISTR_OUT_CLS08
*&---------------------------------------------------------------------*

CLASS lcl_florida_str DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_c8a017_html_handler.
    ALIASES on_sapevent_in_html FOR zif_c8a017_html_handler~on_sapevent_in_html.
    ALIASES on_closed_window FOR zif_c8a017_html_handler~on_closed_window.

    METHODS constructor
      IMPORTING is_scr TYPE lif_types=>ts_scr.
    METHODS fn
      RETURNING VALUE(rc) TYPE sysubrc.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA msr_scr TYPE REF TO lif_types=>ts_scr.

    DATA mt_file_lines TYPE zif_c8a017_types=>tt_tline_str.

    DATA mv_folder_path TYPE string.

    METHODS _read_file_by_lines
      EXPORTING ev_file_ext   TYPE string
                es_src_params TYPE zif_c8a017_types=>ts_src_params.

    METHODS _preproc_florida
      IMPORTING is_src_params TYPE zif_c8a017_types=>ts_src_params.

    METHODS _preproc_florida_no_vars
      IMPORTING is_src_params TYPE zif_c8a017_types=>ts_src_params.

    METHODS _preproc_florida_with_vars
      IMPORTING is_src_params TYPE zif_c8a017_types=>ts_src_params.

    METHODS _preproc_florida_with_loops
      IMPORTING is_src_params TYPE zif_c8a017_types=>ts_src_params.

    METHODS _preproc_florida_infunc
      IMPORTING is_src_params TYPE zif_c8a017_types=>ts_src_params.

    METHODS _preproc_html
      IMPORTING is_src_params    TYPE zif_c8a017_types=>ts_src_params
      EXPORTING et_load_tab_html TYPE zif_c8a017_types=>tt_load_tab_html.

    METHODS _get_func_params
      IMPORTING iv_cur_line TYPE string
      RETURNING VALUE(rv)   TYPE string.

    METHODS _prepare4browser
      EXPORTING ev_html_str      TYPE string
                et_load_tab      TYPE cl_abap_browser=>load_tab
      CHANGING  ct_load_tab_html TYPE zif_c8a017_types=>tt_load_tab_html.

ENDCLASS.

CLASS lcl_florida_str IMPLEMENTATION.
  METHOD constructor.
    msr_scr = REF #( is_scr ).
  ENDMETHOD.

  METHOD fn.

    DATA ls_src_params TYPE zif_c8a017_types=>ts_src_params.

    DATA lt_load_tab_html TYPE zif_c8a017_types=>tt_load_tab_html.
    DATA lt_load_tab_browser TYPE cl_abap_browser=>load_tab.

    DATA lv_html_base_final TYPE string.

    _read_file_by_lines( IMPORTING es_src_params = ls_src_params ).

    _preproc_florida( EXPORTING is_src_params = ls_src_params ).

    _preproc_html( EXPORTING is_src_params = ls_src_params
                   IMPORTING et_load_tab_html = lt_load_tab_html  ).

    _prepare4browser(
                      IMPORTING ev_html_str      = lv_html_base_final
                                et_load_tab      = lt_load_tab_browser
                     CHANGING ct_load_tab_html = lt_load_tab_html ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA lo_show_html TYPE REF TO zcl_c8a017_show_html.
    lo_show_html = NEW #( ).
    lo_show_html->show_html_str( EXPORTING iv_html_as_str = lv_html_base_final
                                           io_handler = me
                                           it_data_table = lt_load_tab_browser
                                           ).


  ENDMETHOD.


  METHOD _read_file_by_lines.

    DATA lo_reader_src TYPE REF TO zif_c8a017_src_lines.
    DATA ls_src TYPE zif_c8a017_types=>ts_src_info.
    DATA ls_src_params TYPE zif_c8a017_types=>ts_src_params.



    CREATE OBJECT lo_reader_src TYPE zcl_c8a017_file_client.

    ls_src-path2file = msr_scr->p_tmpl4.

    lo_reader_src->r_as_str_lines(
      EXPORTING
        is_src    = ls_src
      IMPORTING
        et        = mt_file_lines
        es_params = ls_src_params
    ).

    es_src_params = ls_src_params.

  ENDMETHOD.

  METHOD _preproc_florida.

    IF msr_scr->vars EQ abap_true.
      _preproc_florida_with_vars( is_src_params = is_src_params ).
      RETURN.
    ENDIF.

    IF msr_scr->loops EQ abap_true.
      _preproc_florida_with_loops( is_src_params = is_src_params ).
      RETURN.
    ENDIF.

    IF msr_scr->infunc EQ abap_true.
      _preproc_florida_infunc( is_src_params = is_src_params ).
      RETURN.
    ENDIF.

    _preproc_florida_no_vars( is_src_params = is_src_params ).
  ENDMETHOD.

  METHOD _preproc_florida_no_vars.
    TYPES: BEGIN OF ts_cntx
        , dummy_fld TYPE string
      , END OF  ts_cntx
      .

    DATA ls_cntx TYPE ts_cntx.

    DATA lv_final_str TYPE string.
    DATA lt_file_lines TYPE zif_c8a017_types=>tt_tline_str.

    BREAK-POINT.

    NEW zcl_c8a017_templ_with_data(  )->proc_templ(
      EXPORTING
        it_lines     = mt_file_lines
        is_src       = ls_cntx
        is_env_params = is_src_params
      IMPORTING
        ev_final_str = lv_final_str
        et_res_lines = lt_file_lines
    ).

    mt_file_lines = lt_file_lines.
  ENDMETHOD.

  METHOD _preproc_florida_with_vars.
    TYPES: BEGIN OF ts_cntx
        , but_left_descr TYPE string
        , but_right_descr TYPE string
      , END OF  ts_cntx
      .

    DATA ls_cntx TYPE ts_cntx.

    DATA lv_final_str TYPE string.
    DATA lt_file_lines TYPE zif_c8a017_types=>tt_tline_str.

    ls_cntx-but_left_descr  = 'Да, выполнить'.
    ls_cntx-but_right_descr = 'Нет, остановить'.

    NEW zcl_c8a017_templ_with_data(  )->proc_templ(
      EXPORTING
        it_lines     = mt_file_lines
        is_src       = ls_cntx
        is_env_params = is_src_params
      IMPORTING
        ev_final_str = lv_final_str
        et_res_lines = lt_file_lines
    ).

    mt_file_lines = lt_file_lines.
  ENDMETHOD.

  METHOD _preproc_florida_with_loops.
    TYPES: BEGIN OF ts_msg_info
                , short_msg TYPE string
                , long_msg TYPE string
                , msg_color TYPE string
           , END OF ts_msg_info
           , tt_msg_info TYPE STANDARD TABLE OF ts_msg_info WITH DEFAULT KEY
           .

    TYPES: BEGIN OF ts_cntx
    , but_left_descr TYPE string
    , but_right_descr TYPE string
    , t_msg TYPE tt_msg_info
  , END OF  ts_cntx
  .

    DATA ls_cntx TYPE ts_cntx.

    DATA lv_final_str TYPE string.
    DATA lt_file_lines TYPE zif_c8a017_types=>tt_tline_str.

    ls_cntx-but_left_descr  = 'Да, выполнить'.
    ls_cntx-but_right_descr = 'Нет, остановить'.
    ls_cntx-t_msg = VALUE #(
    ( short_msg = 'короткое сообщение' long_msg = 'расширенное сообщение, которое может быть совсем длинным' msg_color = 'green' )
    ( short_msg = '2короткое сообщение' long_msg = '2расширенное сообщение, которое может быть совсем длинным' msg_color = '' )
    ( short_msg = '3короткое сообщение' long_msg = '3расширенное сообщение, которое может быть совсем длинным' msg_color = 'yellow' )
    ( short_msg = '4короткое сообщение' long_msg = '4расширенное сообщение, которое может быть совсем длинным' msg_color = '' )
    ).

    NEW zcl_c8a017_templ_with_data(  )->proc_templ(
      EXPORTING
        it_lines     = mt_file_lines
        is_src       = ls_cntx
        is_env_params = is_src_params
      IMPORTING
        ev_final_str = lv_final_str
        et_res_lines = lt_file_lines
    ).

    mt_file_lines = lt_file_lines.
  ENDMETHOD.

  METHOD _preproc_florida_infunc.
    "IMPORTING is_src_params TYPE zif_c8a017_types=>ts_src_params.

    TYPES: BEGIN OF ts_cntx
    , but_one   TYPE string
    , but_two   TYPE string
    , but_three TYPE string
    , but_four  TYPE string
  , END OF  ts_cntx
  .

    DATA ls_cntx TYPE ts_cntx.

    DATA lv_final_str TYPE string.
    DATA lt_file_lines TYPE zif_c8a017_types=>tt_tline_str.

    ls_cntx-but_one = 'Кнопка_Один'.
    ls_cntx-but_two = 'Кнопка_Два'.
    "ls_cntx-but_three = 'Кнопка_Три'.
    ls_cntx-but_four = 'Кнопка_ЧТРЕ'.

    NEW zcl_c8a017_templ_with_data(  )->proc_templ(
      EXPORTING
        it_lines     = mt_file_lines
        is_src       = ls_cntx
        is_env_params = is_src_params
      IMPORTING
        ev_final_str = lv_final_str
        et_res_lines = lt_file_lines
    ).

    mt_file_lines = lt_file_lines.

  ENDMETHOD.

  METHOD _get_func_params.
    "IMPORTING iv_cur_line TYPE string
    "RETURNING VALUE(rv) TYPE string.
    DATA lv_match_1st_offset_beg TYPE syindex.
    DATA lv_match_1st_length_beg TYPE syindex.

    DATA lv_match_1st_offset_end TYPE syindex.
    DATA lv_match_1st_length_end TYPE syindex.

    DATA lv_beg_of_params TYPE syindex.
    DATA lv_length_of_params TYPE syindex.

    FIND FIRST OCCURRENCE OF '(' IN iv_cur_line
           MATCH OFFSET lv_match_1st_offset_beg
           MATCH LENGTH lv_match_1st_length_beg.

    FIND FIRST OCCURRENCE OF ')' IN iv_cur_line
           MATCH OFFSET lv_match_1st_offset_end
           MATCH LENGTH lv_match_1st_length_end.

    lv_beg_of_params = lv_match_1st_offset_beg + 1.
    lv_length_of_params = lv_match_1st_offset_end - lv_match_1st_offset_beg - 1.

    rv =
      iv_cur_line+lv_beg_of_params(lv_length_of_params).

  ENDMETHOD.

  METHOD _preproc_html.
*      IMPORTING is_src_params TYPE zif_c8a017_types=>ts_src_params
*      EXPORTING et_load_tab_html TYPE zif_c8a017_types=>tt_load_tab_html.

    DATA lo_html_preproc TYPE REF TO zcl_c8a017_html_preproc.

    CLEAR et_load_tab_html.

    CREATE OBJECT lo_html_preproc.
    lo_html_preproc->set_in(
      EXPORTING
        is_env_params    = is_src_params
      CHANGING
        ct_file_lines    = mt_file_lines
        ct_load_tab_html = et_load_tab_html
    )->fn( ).

  ENDMETHOD.

  METHOD _prepare4browser.
*      IMPORTING it_load_tab_html TYPE zif_c8a017_types=>tt_load_tab_html
*      EXPORTING ev_html_str      TYPE string
*                et_load_tab      TYPE cl_abap_browser=>load_tab.

    FIELD-SYMBOLS <fs_file_line> TYPE zif_c8a017_types=>ts_tline_str.

    CLEAR ev_html_str.

    LOOP AT mt_file_lines ASSIGNING <fs_file_line>.
      IF ev_html_str IS INITIAL.
        ev_html_str = <fs_file_line>-tdline_str.
      ELSE.
        ev_html_str = ev_html_str && cl_abap_char_utilities=>newline && <fs_file_line>-tdline_str.
      ENDIF.

    ENDLOOP.

    DATA ls_load_tab_line TYPE cl_abap_browser=>load_tab_line.
    FIELD-SYMBOLS <fs_load_tab_html> TYPE zif_c8a017_types=>ts_load_tab_html.

    CLEAR et_load_tab.
    LOOP AT ct_load_tab_html ASSIGNING <fs_load_tab_html>.
      ls_load_tab_line-name = <fs_load_tab_html>-name.
      ls_load_tab_line-type = <fs_load_tab_html>-type.
      ls_load_tab_line-dref = REF #( <fs_load_tab_html>-char_tab_cont ).

      INSERT ls_load_tab_line INTO TABLE et_load_tab.

    ENDLOOP.


  ENDMETHOD.

  METHOD on_sapevent_in_html .

  ENDMETHOD.

  METHOD on_closed_window .

  ENDMETHOD.

ENDCLASS.
