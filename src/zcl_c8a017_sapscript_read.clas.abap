CLASS zcl_c8a017_sapscript_read DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA mv_long_text TYPE string READ-ONLY.

    METHODS constructor
      IMPORTING is_thead TYPE thead.

    METHODS r_as_string
      RETURNING VALUE(rv) TYPE string.

    METHODS r_as_str_lines
      EXPORTING et TYPE zif_c8a017_types=>tt_tline_str.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ts_var_templ TYPE zif_c8a017_types=>ts_var_templ.
    TYPES: tt_var_templ TYPE zif_c8a017_types=>tt_var_templ.

    DATA ms_thead TYPE thead.

    METHODS _read_text_lines
      IMPORTING is_txt_head  TYPE thead
      EXPORTING es_txt_head  TYPE thead
                et_txt_lines TYPE tline_tab.

    METHODS _replace_include_n_controls
      CHANGING cs_txt_head  TYPE thead
               ct_txt_lines TYPE tline_tab.

    METHODS _r_as_tlines
      EXPORTING et TYPE tline_tab.

    METHODS _concat_text
      IMPORTING it_txt_lines TYPE tline_tab
      RETURNING VALUE(rv)    TYPE string.

ENDCLASS.



CLASS zcl_c8a017_sapscript_read IMPLEMENTATION.
  METHOD constructor.
    ms_thead = is_thead.
  ENDMETHOD.

  METHOD r_as_string.
    "  RETURNING VALUE(rv) TYPE string.

    DATA lt_txt_lines     TYPE tline_tab.

    CLEAR mv_long_text.
    _r_as_tlines( IMPORTING  et = lt_txt_lines ).

    mv_long_text = _concat_text( lt_txt_lines ).

    rv = mv_long_text.

  ENDMETHOD.

  METHOD _r_as_tlines.
    "EXPORTING et TYPE tline_tab.
    DATA ls_txt_head_out  TYPE thead.
    DATA lt_txt_lines     TYPE tline_tab.



    _read_text_lines( EXPORTING is_txt_head = ms_thead
                      IMPORTING es_txt_head = ls_txt_head_out
                                et_txt_lines = lt_txt_lines ).

    _replace_include_n_controls( CHANGING cs_txt_head = ls_txt_head_out
                                      ct_txt_lines = lt_txt_lines ).



    et = lt_txt_lines.
  ENDMETHOD.

  METHOD r_as_str_lines.
    "EXPORTING et TYPE zif_c8a017_types=>tt_tline_str.
    DATA lt_txt_lines     TYPE tline_tab.

    DATA lt_tline_str TYPE zif_c8a017_types=>tt_tline_str.
    DATA ls_tline_str TYPE zif_c8a017_types=>ts_tline_str.

    FIELD-SYMBOLS <fs_tline> TYPE tline.

    CLEAR mv_long_text.
    _r_as_tlines( IMPORTING  et = lt_txt_lines ).

    CLEAR ls_tline_str.
    LOOP AT lt_txt_lines ASSIGNING <fs_tline>.

      CASE <fs_tline>-tdformat.

        WHEN '='.
          ls_tline_str-tdline_str =
          ls_tline_str-tdline_str && <fs_tline>-tdline.
        WHEN OTHERS.
          ls_tline_str-tdformat = <fs_tline>-tdformat.
          ls_tline_str-tdline_str = <fs_tline>-tdline.
          APPEND ls_tline_str TO  lt_tline_str.
          clear ls_tline_str.

      ENDCASE.

    ENDLOOP.

    et = lt_tline_str.

  ENDMETHOD.

  METHOD _read_text_lines.
    "IMPORTING is_txt_head  TYPE thead
    "EXPORTING es_txt_head  TYPE thead
    "          et_txt_lines TYPE tline_tab.

    DATA ls_txt_head_out  TYPE thead.
    DATA lt_txt_lines     TYPE tline_tab.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = is_txt_head-tdid     " Text ID of text to be read
        language                = is_txt_head-tdspras  " Language of text to be read
        name                    = is_txt_head-tdname   " Name of text to be read
        object                  = is_txt_head-tdobject " Object of text to be read
*       archive_handle          = 0                " Archive Handle
*       local_cat               = space            " Text catalog local
      IMPORTING
        header                  = ls_txt_head_out
*       old_line_counter        =                  " Original Number of Text Lines
      TABLES
        lines                   = lt_txt_lines
      EXCEPTIONS
        id                      = 1                " Text ID invalid
        language                = 2                " Invalid language
        name                    = 3                " Invalid text name
        not_found               = 4                " Text not found
        object                  = 5                " Invalid text object
        reference_check         = 6                " Reference chain interrupted
        wrong_access_to_archive = 7                " Archive handle invalid for access
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      RETURN.
    ENDIF.

    es_txt_head = ls_txt_head_out.
    et_txt_lines = lt_txt_lines.

  ENDMETHOD.


  METHOD _replace_include_n_controls.
    "CHANGING cs_txt_head  TYPE thead
    "         ct_txt_lines TYPE tline_tab.

    DATA lv_was_changed_replace TYPE abap_bool.
    DATA lv_was_changed_control TYPE abap_bool.
    DATA lv_was_changed_symbol TYPE abap_bool.

    IF ct_txt_lines IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TEXT_INCLUDE_REPLACE'
      EXPORTING
*       all_level = 'X'              " Expand nested INCLUDES
*       endline   = 99999            " End line for INCLUDE replacement
        header    = cs_txt_head                " Text header
*       startline = 1                " Start line for INCLUDE replacement
*       program   = space            " Program name for symbol replacement
      IMPORTING
        changed   = lv_was_changed_replace                 " Indicator if text was changed
*       error_type =                  " Error type
        newheader = cs_txt_head                 " Text header (new)
      TABLES
        lines     = ct_txt_lines.                 " Text lines


    CALL FUNCTION 'TEXT_CONTROL_REPLACE'
      EXPORTING
        header    = cs_txt_head                 " Text header
"       program   = mv_sycprog            " Program name for program symbol replacement
*       replace_comment = 'X'              " Remove comment lines
      IMPORTING
        changed   = lv_was_changed_control                 " Replace control structures
        newheader = cs_txt_head                 " Text header (new)
      TABLES
        lines     = ct_txt_lines.                 " Text lines


  ENDMETHOD.


  METHOD _concat_text.
    "IMPORTING it_txt_lines TYPE tline_tab
    "RETURNING VALUE(rv) TYPE string.
    " concat to string
    DATA lt_var_templ TYPE tt_var_templ.
    FIELD-SYMBOLS <fs_txt_line> TYPE tline.

    CLEAR rv.

    LOOP AT it_txt_lines ASSIGNING <fs_txt_line>.
      CASE <fs_txt_line>-tdformat.
        WHEN '*'.
          rv = rv && ` ` && <fs_txt_line>-tdline.
        WHEN OTHERS.
          rv = rv && <fs_txt_line>-tdline.

      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
