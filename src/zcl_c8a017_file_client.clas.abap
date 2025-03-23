CLASS zcl_c8a017_file_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_c8a017_src_lines.
    ALIASES r_as_string FOR zif_c8a017_src_lines~r_as_string.
    ALIASES r_as_str_lines FOR zif_c8a017_src_lines~r_as_str_lines.

    TYPES:
      BEGIN OF ts_text_line
                , line TYPE text4096
            , END OF ts_text_line .
    TYPES:
      tt_text_line TYPE STANDARD TABLE OF ts_text_line WITH DEFAULT KEY .

    METHODS read_file_as_str
      IMPORTING
        !iv_path2file TYPE string
      EXPORTING
        !ev_file_str  TYPE string
        !ev_file_name TYPE string
        !ev_mime_type TYPE string
        !ev_file_ext  TYPE string
        !ev_file_size TYPE string .
    METHODS read_file_as_lines
      IMPORTING
        !iv_path2file   TYPE string
      EXPORTING
        !et_file_lines  TYPE stringtab
        !et_char_lines  TYPE tt_text_line
        !ev_file_name   TYPE string
        !ev_folder_path TYPE string
        !ev_mime_type   TYPE string
        !ev_file_ext    TYPE string
        !ev_file_size   TYPE string
      .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS _get_file_name_only
      IMPORTING
        !iv_path2file TYPE string
      RETURNING
        VALUE(rv_val) TYPE string .
    METHODS _get_file_mime_type
      IMPORTING
        !iv_path2file TYPE string
      RETURNING
        VALUE(rv_val) TYPE string .
    METHODS _get_file_extension
      IMPORTING
        !iv_path2file TYPE string
      RETURNING
        VALUE(rv_val) TYPE string .
    METHODS _get_folder_path
      IMPORTING
        !iv_path2file TYPE string
      RETURNING
        VALUE(rv_val) TYPE string .
ENDCLASS.



CLASS zcl_c8a017_file_client IMPLEMENTATION.



  METHOD read_file_as_lines.
*      IMPORTING
*        !iv_path2file   TYPE string
*      EXPORTING
*        !et_file_lines  TYPE stringtab
*        !et_char_lines  TYPE tt_text_line
*        !ev_file_name   TYPE string
*        !ev_folder_path TYPE string
*        !ev_mime_type   TYPE string
*        !ev_file_ext    TYPE string
*        !ev_file_size   TYPE string

    DATA lv_file_name TYPE string.
    DATA lv_file_length TYPE syindex.
    DATA lv_file_length_out TYPE syindex.
    DATA lvx_header TYPE xstring.
    DATA lt_file_bin_tab TYPE soli_tab.

    """

    FIELD-SYMBOLS <fs_tab_text> TYPE ts_text_line.

    ""

    lv_file_name = iv_path2file.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = lv_file_name
        filetype                = 'BIN'
*        has_field_separator     = space
*        header_length           = 0
*        read_by_line            = 'X'
        "dat_mode                = abap_false
        "codepage                = '4110'
"        codepage                = '65001'
*        ignore_cerr             = abap_true
*        replacement             = '#'
*        virus_scan_profile      =
      IMPORTING
        filelength              = lv_file_length
        header                  = lvx_header
      CHANGING
        data_tab                = lt_file_bin_tab
*        isscanperformed         = space
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      RETURN.
    ENDIF.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF lt_file_bin_tab IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_TEXT'
      EXPORTING
        input_length  = lv_file_length
*       first_line    = 0
*       last_line     = 0
*       append_to_table = SPACE
*       mimetype      = SPACE
*       wrap_lines    = SPACE    " Screens, display user entry
*       encoding      =
      IMPORTING
        output_length = lv_file_length_out
      TABLES
        binary_tab    = lt_file_bin_tab
        text_tab      = et_char_lines
      EXCEPTIONS
        failed        = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS <fs_file_line> TYPE string.
    LOOP AT et_char_lines ASSIGNING <fs_tab_text>.
      APPEND INITIAL LINE TO et_file_lines ASSIGNING <fs_file_line>.
      <fs_file_line> = <fs_tab_text>-line.
    ENDLOOP.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ev_file_size = lv_file_length. " xstrlen( xfile )
    ev_file_name = _get_file_name_only( lv_file_name ).
    ev_folder_path = _get_folder_path( lv_file_name ).
    ev_mime_type = _get_file_mime_type( lv_file_name ).
    ev_file_ext = _get_file_extension( ev_file_name ).

  ENDMETHOD.

  METHOD r_as_string.

  ENDMETHOD.

  METHOD r_as_str_lines .

    DATA lt_file_lines  TYPE stringtab.
    DATA lt_char_lines  TYPE tt_text_line.
    DATA lv_file_name   TYPE string.
    DATA lv_folder_path TYPE string.
    DATA lv_mime_type   TYPE string.
    DATA lv_file_ext    TYPE string.
    DATA lv_file_size   TYPE string.

    DATA lv_src_line_counter TYPE syindex.

    read_file_as_lines(
      EXPORTING
        iv_path2file   = is_src-path2file
      IMPORTING
        et_file_lines  = lt_file_lines
        et_char_lines  = lt_char_lines
        ev_file_name   = lv_file_name
        ev_folder_path = lv_folder_path
        ev_mime_type   = lv_mime_type
        ev_file_ext    = lv_file_ext
        ev_file_size   = lv_file_size
    ).



    FIELD-SYMBOLS <fs_char_lines> TYPE ts_text_line.
    FIELD-SYMBOLS <fs_file_line> TYPE string.

    FIELD-SYMBOLS <fs_tline_str> TYPE zif_c8a017_types=>ts_tline_str.

    " if 1st symb is =
    " it is concat with previous
    lv_src_line_counter = 0.
    LOOP AT lt_char_lines ASSIGNING <fs_char_lines>.
      lv_src_line_counter = lv_src_line_counter + 1.

      IF <fs_char_lines> IS INITIAL.
        APPEND INITIAL LINE TO et ASSIGNING <fs_tline_str>.
        <fs_tline_str>-tdline_str   = <fs_char_lines>-line.
        <fs_tline_str>-line_num_src = lv_src_line_counter.
        <fs_tline_str>-tdformat     = '*'.
        CONTINUE.
      ENDIF.

      IF <fs_char_lines>(1) eq '='.
        IF <fs_tline_str> IS NOT ASSIGNED.
          APPEND INITIAL LINE TO et ASSIGNING <fs_tline_str>.
          <fs_tline_str>-tdline_str   = <fs_char_lines>-line.
          <fs_tline_str>-line_num_src = lv_src_line_counter.
          <fs_tline_str>-tdformat     = '*'.
        ELSE.
          <fs_tline_str>-tdline_str =
            <fs_tline_str>-tdline_str && <fs_char_lines>-line.
        ENDIF.



      ELSE.
        APPEND INITIAL LINE TO et ASSIGNING <fs_tline_str>.
        <fs_tline_str>-tdformat = '*'.
        <fs_tline_str>-tdline_str = <fs_char_lines>-line.
        <fs_tline_str>-line_num_src = lv_src_line_counter.
      ENDIF.

    ENDLOOP.

    es_params-file_prm-size_length = lv_file_size.
    es_params-file_prm-folder      = lv_folder_path.
    es_params-file_prm-mime_type   = lv_mime_type.
    es_params-file_prm-extensn     = lv_file_ext.

  ENDMETHOD.

  METHOD read_file_as_str.
*      IMPORTING iv_path2file TYPE string
*      EXPORTING ev_file_str  TYPE string
*                ev_file_name TYPE string
*                ev_mime_type TYPE string
*                ev_file_ext  TYPE string
*                ev_file_size TYPE string.

    DATA lv_file_name TYPE string.
    DATA lv_file_length TYPE syindex.
    DATA lv_file_length_out TYPE syindex.
    DATA lvx_header TYPE xstring.
    DATA lt_file_bin_tab TYPE soli_tab.

    lv_file_name = iv_path2file.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = lv_file_name
        filetype                = 'BIN'
*        has_field_separator     = space
*        header_length           = 0
*        read_by_line            = 'X'
*        dat_mode                = space
*        codepage                = '4110'
*        ignore_cerr             = abap_true
*        replacement             = '#'
*        virus_scan_profile      =
      IMPORTING
        filelength              = lv_file_length
        header                  = lvx_header
      CHANGING
        data_tab                = lt_file_bin_tab
*        isscanperformed         = space
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      RETURN.
    ENDIF.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF lt_file_bin_tab IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING
        input_length  = lv_file_length
*       first_line    = 0
*       last_line     = 0
*       mimetype      = SPACE
*       encoding      =
      IMPORTING
        text_buffer   = ev_file_str
        output_length = lv_file_length_out
      TABLES
        binary_tab    = lt_file_bin_tab
      EXCEPTIONS
        failed        = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      RETURN.
    ENDIF.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ev_file_size = lv_file_length. " xstrlen( xfile )
    ev_file_name = _get_file_name_only( lv_file_name ).
    ev_mime_type = _get_file_mime_type( lv_file_name ).
    ev_file_ext = _get_file_extension( ev_file_name ).

  ENDMETHOD.


  METHOD _get_file_extension.
*      IMPORTING !iv_path2file TYPE string
*      returning value(rv_val) TYPE string .
    DATA lc_dot_separator TYPE char1 VALUE '.'.
    DATA lt_file_dot_splitted TYPE stringtab.

    SPLIT iv_path2file AT lc_dot_separator INTO TABLE lt_file_dot_splitted.
    rv_val = VALUE #( lt_file_dot_splitted[ lines( lt_file_dot_splitted ) ] OPTIONAL ).

  ENDMETHOD.


  METHOD _get_file_mime_type.
*      IMPORTING !iv_path2file TYPE string
*      RETURNING VALUE(rv_val) TYPE string .

    DATA lv_mimetype TYPE  skwf_mime.

    CALL FUNCTION 'SKWF_MIMETYPE_OF_FILE_GET'
      EXPORTING
        filename = CONV skwf_filnm( iv_path2file )
*       x_use_local_registry =
      IMPORTING
        mimetype = lv_mimetype.

    rv_val = lv_mimetype.

  ENDMETHOD.


  METHOD _get_file_name_only.
*      IMPORTING !iv_path2file TYPE string
*      RETURNING VALUE(rv_val) TYPE string .

    DATA lt_file_path_splitted TYPE stringtab.
    DATA lv_file_separator TYPE char1.

    cl_gui_frontend_services=>get_file_separator(
      CHANGING
        file_separator       = lv_file_separator
      EXCEPTIONS
        not_supported_by_gui = 1
        error_no_gui         = 2
        cntl_error           = 3
        OTHERS               = 4
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      RETURN.
    ENDIF.

    SPLIT iv_path2file AT lv_file_separator INTO TABLE lt_file_path_splitted.
    rv_val = VALUE #( lt_file_path_splitted[ lines( lt_file_path_splitted ) ] OPTIONAL ).


  ENDMETHOD.


  METHOD _get_folder_path.
*      IMPORTING !iv_path2file TYPE string
*      RETURNING VALUE(rv_val) TYPE string .

    DATA lt_file_path_splitted TYPE stringtab.
    DATA lv_file_separator TYPE char1.

    FIELD-SYMBOLS <fs_file_path> TYPE string.

    cl_gui_frontend_services=>get_file_separator(
      CHANGING
        file_separator       = lv_file_separator
      EXCEPTIONS
        not_supported_by_gui = 1
        error_no_gui         = 2
        cntl_error           = 3
        OTHERS               = 4
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      RETURN.
    ENDIF.

    SPLIT iv_path2file AT lv_file_separator INTO TABLE lt_file_path_splitted.
    DELETE lt_file_path_splitted INDEX lines( lt_file_path_splitted ).

    CLEAR rv_val.
    LOOP AT lt_file_path_splitted ASSIGNING <fs_file_path>.
      IF rv_val IS INITIAL.
        rv_val = <fs_file_path> && lv_file_separator.
      ELSE.
        rv_val = rv_val && <fs_file_path> && lv_file_separator.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
