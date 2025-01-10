*&---------------------------------------------------------------------*
*& Report ZREP_C8A017_TEMPLATES_LONGTXT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrep_c8a017_templates_longtxt.

PARAMETERS: p_txtnm TYPE tdobname DEFAULT 'ZC8A017_TEMPL_ORD1' OBLIGATORY.

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.
    METHODS main.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_tab_line
            , matnr TYPE char18
            , matnr_txt TYPE text80
            , quan TYPE p LENGTH 10 DECIMALS 3
            , meins TYPE meins
            , ship_date TYPE sydatum
        , END OF ts_tab_line
        , tt_tab_line TYPE STANDARD TABLE OF ts_tab_line WITH DEFAULT KEY
        .

    TYPES: BEGIN OF ts_deliv_addr
              , city TYPE string
              , street TYPE string
              , house TYPE string
           , END OF ts_deliv_addr
           .

    TYPES: BEGIN OF ts_out_doc
              , kunnr TYPE kunnr
              , kunnr_txt TYPE text80
              , order_num TYPE vbeln
              , customer_order TYPE text40
              , order_lines TYPE tt_tab_line
              , dlvaddr TYPE ts_deliv_addr
              , kunnr_empty TYPE kunnr
          , END OF ts_out_doc
          .



    METHODS _fill_doc
      EXPORTING es_out_doc TYPE ts_out_doc.

    METHODS _show_html_out
      IMPORTING iv_html TYPE string.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD main.

    DATA ls_out_doc TYPE ts_out_doc.
    DATA lv_html_out TYPE string.

    DATA lv_txt_name TYPE tdobname VALUE 'ZC8A017_TEMPL_ORD1'.

    lv_txt_name = p_txtnm.

    _fill_doc( IMPORTING es_out_doc = ls_out_doc ).


    DATA lo_so10_template TYPE REF TO zcl_c8a017_longtxt_templ.
    lo_so10_template = NEW #( is_txt_h = VALUE #( tdname = lv_txt_name ) ).
    lv_html_out =
   " lo_so10_template->data2template( ls_out_doc ).
    lo_so10_template->process_tmpl_with_data(
      EXPORTING
        is             = ls_out_doc
    ).

    _show_html_out( lv_html_out ).

  ENDMETHOD.

  METHOD _fill_doc.
    "EXPORTING es_out_doc TYPE ts_out_doc.
    CLEAR es_out_doc .

    es_out_doc-kunnr = '1001'.
    CLEAR es_out_doc-kunnr_empty. " example of empty field
    es_out_doc-kunnr_txt = 'Customer1001 Company Name'.
    es_out_doc-order_num = '3100012345'.
    es_out_doc-customer_order = 'EXT_CUST_123'.

    es_out_doc-order_lines = VALUE #(
    ( matnr = '25001' matnr_txt = 'Material25001 description' quan = '25.1' meins = 'ST' ship_date = sy-datum + 1 )
    ( matnr = '26001' matnr_txt = 'Material26001 description' quan = '26.1' meins = 'ST' ship_date = sy-datum + 2 )
    ( matnr = '27001' matnr_txt = 'Material27001 description' quan = '27'   meins = 'ST' ship_date = sy-datum + 1 )
    ).

    es_out_doc-dlvaddr-city = 'Tula'.
    es_out_doc-dlvaddr-street = 'Mendeleevskaya'.
    es_out_doc-dlvaddr-house = 'Bld.8'.

  ENDMETHOD.

  METHOD _show_html_out.
    "IMPORTING iv_html TYPE string.
    IF iv_html IS NOT INITIAL.
      cl_abap_browser=>show_html(
        EXPORTING
          html_string  = iv_html
    ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.





START-OF-SELECTION.
  NEW lcl_app( )->main( ).

end-of-SELECTION.
