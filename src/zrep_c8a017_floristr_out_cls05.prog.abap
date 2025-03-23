*&---------------------------------------------------------------------*
*&  Include           ZREP_C8A017_FLORISTR_OUT_CLS05
*&---------------------------------------------------------------------*

CLASS lcl_html2browser_a1 DEFINITION.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING is_scr TYPE lif_types=>ts_scr.
    METHODS fn.

  PROTECTED SECTION.
  PRIVATE   SECTION.
    DATA msr_scr TYPE REF TO lif_types=>ts_scr.

    DATA mv_file_str TYPE string.

    METHODS _valid_in
      RETURNING VALUE(rc) TYPE sysubrc.

    METHODS _read_file_as_str.

    METHODS _show_html.

ENDCLASS.


CLASS lcl_html2browser_a1 IMPLEMENTATION.
  METHOD constructor.
    msr_scr = REF #( is_scr ).
  ENDMETHOD.

  METHOD fn.

    IF _valid_in( ) NE 0.
      RETURN.
    ENDIF.

    _read_file_as_str( ).

    _show_html( ).


  ENDMETHOD.

  METHOD _valid_in.
    rc = 0.
  ENDMETHOD.

  METHOD _read_file_as_str.

    NEW zcl_c8a017_file_path( )->read_file_as_str(
      EXPORTING
        iv_path2file = msr_scr->path2templ
     IMPORTING
        ev_file_str  = mv_file_str
*       ev_file_name =
*       ev_mime_type =
*       ev_file_ext  =
*       ev_file_size =
    ).

  ENDMETHOD.

  METHOD _show_html.

    cl_abap_browser=>show_html(
      EXPORTING
*        html         =     " HTML Table, Line Width 255 Characters
*        title        =     " Window Title
*        size         = CL_ABAP_BROWSER=>MEDIUM    " Size (S,M.L,XL)
*        modal        = ABAP_TRUE    " Dialog box is modal (else modeless)
        html_string  = mv_file_str    " HTML String
*        printing     = ABAP_FALSE    " Key for printing
*        buttons      = NAVIGATE_OFF    " Navigation Keys navigate_...
*        format       = CL_ABAP_BROWSER=>LANDSCAPE    " Landscape/portrait format
*        position     = CL_ABAP_BROWSER=>TOPLEFT    " Position
*        data_table   =     " External data
*        anchor       =     " Goto Point
*        context_menu = ABAP_FALSE    " Display context menu in browser
*        html_xstring =     " HTML Binary String
*        check_html   = ABAP_TRUE    " Test of HTML File
*        container    =     " Container for display
*        dialog       = ABAP_TRUE    " Display in dialog box
*      IMPORTING
*        html_errors  =     " Error List from Test
    ).

  ENDMETHOD.

ENDCLASS.
