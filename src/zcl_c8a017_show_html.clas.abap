class ZCL_C8A017_SHOW_HTML definition
  public
  final
  create public .

public section.

  methods SHOW_HTML_STR
    importing
      !IV_HTML_AS_STR type STRING
      !IO_HANDLER type ref to ZIF_C8A017_HTML_HANDLER optional
      !IT_DATA_TABLE type CL_ABAP_BROWSER=>LOAD_TAB optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_C8A017_SHOW_HTML IMPLEMENTATION.


  METHOD show_html_str.

    IF io_handler IS BOUND.
      SET HANDLER io_handler->on_sapevent_in_html. " static event - no for
    ENDIF.

    cl_abap_browser=>show_html(
  EXPORTING
*        html         =     " HTML Table, Line Width 255 Characters
*        title        =     " Window Title
*        size         = CL_ABAP_BROWSER=>MEDIUM    " Size (S,M.L,XL)
*        modal        = ABAP_TRUE    " Dialog box is modal (else modeless)
    html_string  = iv_html_as_str    " HTML String
        printing     = ABAP_true    " Key for printing
   "    buttons      = NAVIGATE_OFF    " Navigation Keys navigate_...
*        format       = CL_ABAP_BROWSER=>LANDSCAPE    " Landscape/portrait format
*        position     = CL_ABAP_BROWSER=>TOPLEFT    " Position
        data_table   =  it_data_table   " External data
*        anchor       =     " Goto Point
        context_menu = ABAP_true    " Display context menu in browser
*        html_xstring =     " HTML Binary String
*        check_html   = ABAP_TRUE    " Test of HTML File
*        container    =     " Container for display
*        dialog       = ABAP_TRUE    " Display in dialog box
*      IMPORTING
*        html_errors  =     " Error List from Test
).

  ENDMETHOD.
ENDCLASS.
