CLASS zcl_c8a017_file_path DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_text_line
                , line TYPE text4096
            , END OF ts_text_line .
    TYPES:
      tt_text_line TYPE STANDARD TABLE OF ts_text_line WITH DEFAULT KEY .

    METHODS dialog_on_pc
      IMPORTING
        !iv_file_type TYPE int1 DEFAULT 0
      EXPORTING
        !ev_path2file TYPE string .
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



CLASS ZCL_C8A017_FILE_PATH IMPLEMENTATION.


  METHOD dialog_on_pc.

    DATA lv_window_title TYPE string VALUE 'Выбор файла'.

    DATA lv_file_filter TYPE string.
    DATA lt_file_filter_txt TYPE STANDARD TABLE OF string.
    DATA lt_file_filter_florida TYPE STANDARD TABLE OF string.
    DATA lt_file_filter_all TYPE STANDARD TABLE OF string.

    DATA lt_file_table  TYPE filetable.
    DATA lv_rc  TYPE i.


    lt_file_filter_txt = VALUE #(
    ( `HTML/Txt/Json/XML/CSV files (*.HTML, *.HTM, *.txt, *.json, *.xml, *.csv )|*.HTML;*.HTM;*.TXT;*.JSON;*.XML;*.CSV|` )
    ).

    lt_file_filter_florida = VALUE #(
    ( `FloridaString files (*.flstr)|*.flstr|` )
    ).

    lt_file_filter_all = VALUE #(
    ( `All Files (*.*)|*.*` )
    ).

    CASE iv_file_type.
      WHEN 1.
        lv_file_filter = concat_lines_of( table = lt_file_filter_txt
                                            sep = '|' ).

      WHEN 2.
        lv_file_filter = concat_lines_of( table = lt_file_filter_florida
                                            sep = '|' ).

      WHEN OTHERS. " 0 - all
        lv_file_filter = concat_lines_of( table = lt_file_filter_all
                                            sep = '|' ).
    ENDCASE.


    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = lv_window_title    " Title Of File Open Dialog
*      default_extension       =     " Default Extension
*      default_filename        =     " Default File Name
      file_filter             =  lv_file_filter   " File Extension Filter String
*      with_encoding           =     " File Encoding
*      initial_directory       =     " Initial Directory
*      multiselection          =     " Multiple selections poss.
      CHANGING
        file_table              = lt_file_table    " Table Holding Selected Files
        rc                      = lv_rc    " Return Code, Number of Files or -1 If Error Occurred
*      user_action             =     " User Action (See Class Constants ACTION_OK, ACTION_CANCEL)
*      file_encoding           =
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      CLEAR ev_path2file.
      RETURN.
    ENDIF.

    IF lv_rc < 0. " some error
      CLEAR ev_path2file.
      RETURN.
    ENDIF.

    ev_path2file = VALUE #( lt_file_table[ 1 ]-filename OPTIONAL ).

  ENDMETHOD.


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
