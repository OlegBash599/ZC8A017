*&---------------------------------------------------------------------*
*&  Include           ZREP_C8A017_FLORISTR_OUT_CLS06
*&---------------------------------------------------------------------*

CLASS lcl_popup_yes_no DEFINITION.

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
    DATA mv_file_str TYPE string.

    DATA msr_scr TYPE REF TO lif_types=>ts_scr.

    METHODS _read_file_as_str.

ENDCLASS.


CLASS lcl_popup_yes_no IMPLEMENTATION.

  METHOD constructor.
    "IMPORTING is_scr TYPE lif_types=>ts_scr.
    msr_scr = REF #( is_scr ).
  ENDMETHOD.

  METHOD fn.

    DATA lo_show_html TYPE REF TO zcl_c8a017_show_html.


    _read_file_as_str( ).

    DATA lv_html_as_str TYPE string.

    lo_show_html = NEW #( ).
    lo_show_html->show_html_str( EXPORTING iv_html_as_str = mv_file_str
                                           io_handler = me ).

  ENDMETHOD.

  METHOD on_sapevent_in_html.
    "ACTION
    "QUERY_TABLE

    CASE action.
      WHEN 'CLOSE'.
        cl_abap_browser=>close_browser( ).

      when 'yes' or 'no'.

        MESSAGE s000(cl) WITH action ' _ was clicked'.

        cl_abap_browser=>close_browser( ).
      WHEN OTHERS.
    ENDCASE.

    BREAK-POINT.
  ENDMETHOD.

  METHOD on_closed_window.
    BREAK-POINT.
  ENDMETHOD.

  METHOD _read_file_as_str.

    NEW zcl_c8a017_file_path( )->read_file_as_str(
      EXPORTING
        iv_path2file = msr_scr->p_tmpl2
      IMPORTING
        ev_file_str  = mv_file_str
*        ev_file_name =
*        ev_mime_type =
*        ev_file_ext  =
*        ev_file_size =
    ).

  ENDMETHOD.

ENDCLASS.
