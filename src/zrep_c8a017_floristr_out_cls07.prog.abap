*&---------------------------------------------------------------------*
*&  Include           ZREP_C8A017_FLORISTR_OUT_CLS07
*&---------------------------------------------------------------------*

CLASS lcl_preprocess_tmpl DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_c8a017_html_handler.
    ALIASES on_sapevent_in_html FOR zif_c8a017_html_handler~on_sapevent_in_html.
    ALIASES on_closed_window FOR zif_c8a017_html_handler~on_closed_window.

    METHODS constructor
      IMPORTING is_scr TYPE lif_types=>ts_scr.
    METHODS fn
      RETURNING VALUE(rc) TYPE sysubrc.

  PROTECTED SECTION.
  PRIVATE   SECTION.


    TYPES: BEGIN OF ts_ext_file
            , fold TYPE string
            , sub_path TYPE string
            , full_path TYPE string
            , tab_cont TYPE zcl_c8a017_file_path=>tt_text_line
            , tab_cont_str TYPE stringtab
        , END OF ts_ext_file
        , tt_ext_file TYPE STANDARD TABLE OF ts_ext_file WITH DEFAULT KEY
        .



    DATA mt_file_lines TYPE stringtab.
    DATA mv_html_base_str TYPE string.
    DATA mv_folder_path TYPE string.

    DATA mt_load_tab TYPE cl_abap_browser=>load_tab.
    DATA mt_ext_file TYPE tt_ext_file.

    DATA msr_scr TYPE REF TO lif_types=>ts_scr.

    METHODS _read_file_by_lines.
    METHODS _preprocess.

    METHODS _tab_into_line
      EXPORTING ev_html_str TYPE string.

ENDCLASS.

CLASS lcl_preprocess_tmpl IMPLEMENTATION.
  METHOD constructor.
    "IMPORTING is_scr TYPE lif_types=>ts_scr.
    msr_scr = REF #( is_scr ).
  ENDMETHOD.

  METHOD fn.

    DATA lo_show_html TYPE REF TO zcl_c8a017_show_html.

    DATA lv_html_base_str TYPE string.

    _read_file_by_lines( ).

    _preprocess( ).

    _tab_into_line( IMPORTING ev_html_str = lv_html_base_str ).

    lo_show_html = NEW #( ).
    lo_show_html->show_html_str( EXPORTING iv_html_as_str = lv_html_base_str
                                           io_handler = me
                                           it_data_table = mt_load_tab
                                           ).

  ENDMETHOD.

  METHOD _read_file_by_lines.

    CLEAR mt_file_lines.
    CLEAR mv_folder_path.

    NEW zcl_c8a017_file_path( )->read_file_as_lines(
      EXPORTING
        iv_path2file  = msr_scr->p_tmpl3
      IMPORTING
        et_file_lines	= mt_file_lines
*        ev_file_name  =
        ev_folder_path = mv_folder_path
*        ev_mime_type  =
*        ev_file_ext   =
*        ev_file_size  =
    ).

  ENDMETHOD.

  METHOD _preprocess.

    DATA lv_href_ext_beg TYPE string VALUE 'href="'.
    DATA lv_href_ext_end TYPE string VALUE '"'.

    DATA lv_ext_js_beg TYPE string VALUE 'script src="'.
    DATA lv_ext_js_end TYPE string VALUE '"'.

    DATA lv_target_sub_beg TYPE syindex.
    DATA lv_target_sub_path TYPE string.

    DATA lt_result_match TYPE match_result_tab.

    DATA lv_match_1st_offset TYPE syindex.
    DATA lv_match_1st_length TYPE syindex.

    FIELD-SYMBOLS <fs_result_match> TYPE match_result.

    FIELD-SYMBOLS <fs_file_line> TYPE string.

    BREAK-POINT.

    """


    DATA ls_ext_file TYPE ts_ext_file.

    FIELD-SYMBOLS <fs_ext_file> TYPE ts_ext_file.



    LOOP AT mt_file_lines ASSIGNING <fs_file_line>.
      IF <fs_file_line> CS lv_href_ext_beg.

        IF <fs_file_line> CS 'SAPEVENT:'.
          CONTINUE.
        ENDIF.

        IF <fs_file_line> CS 'javascript:'.
          CONTINUE.
        ENDIF.

        CLEAR lt_result_match.

        FIND ALL OCCURRENCES OF lv_href_ext_beg IN <fs_file_line>
          RESULTS lt_result_match.

        LOOP AT lt_result_match ASSIGNING <fs_result_match>.
          lv_target_sub_beg = <fs_result_match>-offset + <fs_result_match>-length .
          lv_target_sub_path = <fs_file_line>+lv_target_sub_beg.

          FIND FIRST OCCURRENCE OF lv_href_ext_end IN lv_target_sub_path
           MATCH OFFSET lv_match_1st_offset
           MATCH LENGTH lv_match_1st_length
           .

          lv_target_sub_path = lv_target_sub_path(lv_match_1st_offset).

          CLEAR ls_ext_file.
          ls_ext_file-fold = mv_folder_path.
          ls_ext_file-sub_path = lv_target_sub_path.
          ls_ext_file-full_path = mv_folder_path && lv_target_sub_path.
          APPEND ls_ext_file TO mt_ext_file.


        ENDLOOP.

        CONTINUE.

      ENDIF.


      IF <fs_file_line> CS lv_ext_js_beg.

        CLEAR lt_result_match.

        FIND ALL OCCURRENCES OF lv_ext_js_beg IN <fs_file_line>
          RESULTS lt_result_match.

        LOOP AT lt_result_match ASSIGNING <fs_result_match>.
          lv_target_sub_beg = <fs_result_match>-offset + <fs_result_match>-length .
          lv_target_sub_path = <fs_file_line>+lv_target_sub_beg.

          FIND FIRST OCCURRENCE OF lv_ext_js_end IN lv_target_sub_path
           MATCH OFFSET lv_match_1st_offset
           MATCH LENGTH lv_match_1st_length
           .

          lv_target_sub_path = lv_target_sub_path(lv_match_1st_offset).

          CLEAR ls_ext_file.
          ls_ext_file-fold = mv_folder_path.
          ls_ext_file-sub_path = lv_target_sub_path.
          ls_ext_file-full_path = mv_folder_path && lv_target_sub_path.
          APPEND ls_ext_file TO mt_ext_file.


        ENDLOOP.

      ENDIF.

    ENDLOOP.


    """""
    DATA ls_load_tab_line TYPE cl_abap_browser=>load_tab_line.

    CLEAR mt_load_tab.

    CLEAR ls_ext_file.
    LOOP AT mt_ext_file ASSIGNING <fs_ext_file>.

      CLEAR ls_load_tab_line.

      NEW zcl_c8a017_file_path( )->read_file_as_lines(
        EXPORTING
          iv_path2file   = <fs_ext_file>-full_path
        IMPORTING
          et_file_lines  = <fs_ext_file>-tab_cont_str
          et_char_lines = <fs_ext_file>-tab_cont
*          ev_file_name   =
*          ev_folder_path =
*          ev_mime_type   =
*          ev_file_ext    =
*          ev_file_size   =
      ).

      ls_load_tab_line-name = <fs_ext_file>-sub_path.
      ls_load_tab_line-type = 'text'.
     " if ls_load_tab_line-name cs '.js'.
     "   ls_load_tab_line-type = 'javascript'.
    "  endif.

      ls_load_tab_line-dref = REF #( <fs_ext_file>-tab_cont ).

      INSERT ls_load_tab_line INTO TABLE mt_load_tab.

    ENDLOOP.


  ENDMETHOD.

  METHOD _tab_into_line.
    "EXPORTING ev_html_str TYPE string.

    FIELD-SYMBOLS <fs_file_line> TYPE string.

    CLEAR ev_html_str.

    LOOP AT mt_file_lines ASSIGNING <fs_file_line>.
      IF ev_html_str IS INITIAL.
        ev_html_str = <fs_file_line>.
      ELSE.
        ev_html_str = ev_html_str && cl_abap_char_utilities=>newline && <fs_file_line>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD on_sapevent_in_html .

    IF action EQ 'CLOSE'.
      cl_abap_browser=>close_browser( ).
    ENDIF.

  ENDMETHOD.

  METHOD on_closed_window .
    cl_abap_browser=>close_browser( ).
  ENDMETHOD.

ENDCLASS.
