CLASS zcl_c8a017_html_preproc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS set_in
      IMPORTING is_env_params TYPE zif_c8a017_types=>ts_src_params
      CHANGING  ct_file_lines TYPE zif_c8a017_types=>tt_tline_str
                ct_load_tab_html TYPE zif_c8a017_types=>tt_load_tab_html
      RETURNING VALUE(ro)     TYPE REF TO zcl_c8a017_html_preproc.

    METHODS fn
      RETURNING VALUE(rc) TYPE sysubrc.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ts_ext_file
            , fold TYPE string
            , sub_path TYPE string
            , full_path TYPE string
            , tab_cont TYPE zif_c8a017_types=>tt_text_line
            , tab_cont_str TYPE stringtab
        , END OF ts_ext_file
        , tt_ext_file TYPE STANDARD TABLE OF ts_ext_file WITH DEFAULT KEY
        .


    DATA msr_env_params TYPE REF TO zif_c8a017_types=>ts_src_params.
    DATA mtr_file_lines TYPE REF TO zif_c8a017_types=>tt_tline_str.

    data mtr_load_tab_html TYPE ref to zif_c8a017_types=>tt_load_tab_html.

ENDCLASS.



CLASS zcl_c8a017_html_preproc IMPLEMENTATION.
  METHOD set_in.
*      IMPORTING is_env_params TYPE zif_c8a017_types=>ts_src_params
*      CHANGING  ct_file_lines TYPE zif_c8a017_types=>tt_tline_str
*                ct_load_tab_html TYPE zif_c8a017_types=>tt_load_tab_html
*      RETURNING VALUE(ro)     TYPE REF TO zcl_c8a017_html_preproc.

    msr_env_params = REF #( is_env_params ).
    mtr_file_lines = REF #( ct_file_lines ).
    mtr_load_tab_html = ref #( ct_load_tab_html ).

    ro = me.
  ENDMETHOD.

  METHOD fn.
    "RETURNING VALUE(rc) TYPE sysubrc.

    DATA lv_href_ext_beg TYPE string VALUE 'href="'.
    DATA lv_href_ext_end TYPE string VALUE '"'.

    DATA lv_ext_js_beg TYPE string VALUE 'script src="'.
    DATA lv_ext_js_end TYPE string VALUE '"'.

    DATA lv_target_sub_beg TYPE syindex.
    DATA lv_target_sub_path TYPE string.

    DATA lt_result_match TYPE match_result_tab.

    DATA lv_match_1st_offset TYPE syindex.
    DATA lv_match_1st_length TYPE syindex.


    DATA mt_ext_file TYPE tt_ext_file.


    FIELD-SYMBOLS <fs_result_match> TYPE match_result.

    FIELD-SYMBOLS <fs_file_line> TYPE zif_c8a017_types=>ts_tline_str.

    BREAK-POINT.

    """


    DATA ls_ext_file TYPE ts_ext_file.

    FIELD-SYMBOLS <fs_ext_file> TYPE ts_ext_file.



    LOOP AT mtr_file_lines->* ASSIGNING <fs_file_line>.
      IF <fs_file_line>-tdline_str CS lv_href_ext_beg.

        IF <fs_file_line>-tdline_str CS 'SAPEVENT:'.
          CONTINUE.
        ENDIF.

        IF <fs_file_line>-tdline_str CS 'javascript:'.
          CONTINUE.
        ENDIF.

        CLEAR lt_result_match.

        FIND ALL OCCURRENCES OF lv_href_ext_beg IN <fs_file_line>-tdline_str
          RESULTS lt_result_match.

        LOOP AT lt_result_match ASSIGNING <fs_result_match>.
          lv_target_sub_beg = <fs_result_match>-offset + <fs_result_match>-length .
          lv_target_sub_path = <fs_file_line>-tdline_str+lv_target_sub_beg.

          FIND FIRST OCCURRENCE OF lv_href_ext_end IN lv_target_sub_path
           MATCH OFFSET lv_match_1st_offset
           MATCH LENGTH lv_match_1st_length
           .

          lv_target_sub_path = lv_target_sub_path(lv_match_1st_offset).

          CLEAR ls_ext_file.
          ls_ext_file-fold = msr_env_params->file_prm-folder.
          ls_ext_file-sub_path = lv_target_sub_path.
          ls_ext_file-full_path = ls_ext_file-fold && lv_target_sub_path.
          APPEND ls_ext_file TO mt_ext_file.


        ENDLOOP.

        CONTINUE.

      ENDIF.


      IF <fs_file_line>-tdline_str CS lv_ext_js_beg.

        CLEAR lt_result_match.

        FIND ALL OCCURRENCES OF lv_ext_js_beg IN <fs_file_line>-tdline_str
          RESULTS lt_result_match.

        LOOP AT lt_result_match ASSIGNING <fs_result_match>.
          lv_target_sub_beg = <fs_result_match>-offset + <fs_result_match>-length .
          lv_target_sub_path = <fs_file_line>-tdline_str+lv_target_sub_beg.

          FIND FIRST OCCURRENCE OF lv_ext_js_end IN lv_target_sub_path
           MATCH OFFSET lv_match_1st_offset
           MATCH LENGTH lv_match_1st_length
           .

          lv_target_sub_path = lv_target_sub_path(lv_match_1st_offset).

          CLEAR ls_ext_file.
          ls_ext_file-fold = msr_env_params->file_prm-folder.
          ls_ext_file-sub_path = lv_target_sub_path.
          ls_ext_file-full_path = ls_ext_file-fold && lv_target_sub_path.
          APPEND ls_ext_file TO mt_ext_file.


        ENDLOOP.

      ENDIF.

    ENDLOOP.


    """""
    "DATA ls_load_tab_line TYPE cl_abap_browser=>load_tab_line.

    DATA ls_load_tab_html TYPE zif_c8a017_types=>ts_load_tab_html.

    CLEAR ls_ext_file.
    LOOP AT mt_ext_file ASSIGNING <fs_ext_file>.

      CLEAR ls_load_tab_html.

      NEW zcl_c8a017_file_path( )->read_file_as_lines(
        EXPORTING
          iv_path2file   = <fs_ext_file>-full_path
        IMPORTING
*          et_file_lines  =
          et_char_lines  = ls_load_tab_html-char_tab_cont
*          ev_file_name   =
*          ev_folder_path =
*          ev_mime_type   =
*          ev_file_ext    =
*          ev_file_size   =
      ).


      ls_load_tab_html-name = <fs_ext_file>-sub_path.
      ls_load_tab_html-type = 'text'.
      " if ls_load_tab_line-name cs '.js'.
      "   ls_load_tab_line-type = 'javascript'.
      "  endif.

      "ls_load_tab_line-dref = REF #( <fs_ext_file>-tab_cont ).

      APPEND ls_load_tab_html TO mtr_load_tab_html->*.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
