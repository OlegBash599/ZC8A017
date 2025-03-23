*"* use this source file for your ABAP unit test classes
CLASS ltc_terun DEFINITION DEFERRED.
CLASS zcl_c8a017_templ_with_data DEFINITION LOCAL FRIENDS ltc_terun.

CLASS ltc_terun DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT INHERITING FROM cl_aunit_assert
   .

  PUBLIC SECTION.
    METHODS ut_run_01 FOR TESTING.
    METHODS ut_run_var FOR TESTING.
    METHODS ut_run_as_tab_var FOR TESTING.

    METHODS ut_run_vmeste_func FOR TESTING.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES: BEGIN OF ts_item
            , matnr TYPE string
            , matnr_txt TYPE string
         , END OF ts_item
         , tt_item TYPE STANDARD TABLE OF ts_item WITH DEFAULT KEY
         .

    TYPES: BEGIN OF ts_src
            , kunnr TYPE string
            , kunnr_txt TYPE string
            , t_items TYPE tt_item
         , END OF ts_src
         .

    DATA mo_cut TYPE REF TO zcl_c8a017_templ_with_data.
    " data mo_mock_data type ref to ZCL_C8A017_TEMPL_WITH_DATA_mock_data.

    DATA ms_src TYPE ts_src.

    METHODS setup.
    METHODS teardown.

    METHODS _fill_src.

ENDCLASS.


CLASS ltc_terun IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #(  ).
    "mo_mock_data = new #( ).

    _fill_src(  ).

  ENDMETHOD.

  METHOD teardown.
    CLEAR mo_cut .
  ENDMETHOD.

  METHOD _fill_src.
    CLEAR ms_src.
    ms_src-kunnr = '1002003'.
    ms_src-kunnr_txt = 'Customer name for003'.

    ms_src-t_items = VALUE #(
    ( matnr = '123' matnr_txt = 'text_matnr123' )
    ( matnr = '123' matnr_txt = 'text124_description' )
    ).

  ENDMETHOD.

  METHOD ut_run_01.

  ENDMETHOD.


  METHOD ut_run_var.

    DATA lv_html_out TYPE string.
    DATA lt_template_lines     TYPE zif_c8a017_types=>tt_tline_str.
    lt_template_lines = VALUE #(
    ( tdformat = '*' tdline_str = 'simple line as it is' )
    ( tdformat = '*' tdline_str = 'var in line $KUNNR$ and after' )
    ( tdformat = '*' tdline_str = '?/:IF_BEG($KUNNR$;20)' )
    ( tdformat = '*' tdline_str = 'should not output' )
    ( tdformat = '*' tdline_str = '?/:IF_END' )
    ( tdformat = '*' tdline_str = '?/:IF_BEG($KUNNR$;1002003)' )
    ( tdformat = '*' tdline_str = 'if is true - should be putputed' )
    ( tdformat = '*' tdline_str = '?/:IF_END' )
    ( tdformat = '*' tdline_str = '?/:FOR_BEG($T_ITEMS$)' )
    ( tdformat = '*' tdline_str = 'material: $MATNR$' )
    ( tdformat = '*' tdline_str = 'mat_txt: $MATNR_TXT$' )
    ( tdformat = '*' tdline_str = '?/:FOR_END' )
    ).

    mo_cut->_proc_templ_v2(
      EXPORTING
        it_lines     = lt_template_lines
        is_src       = ms_src
      IMPORTING
        ev_final_str = lv_html_out
    ).

  ENDMETHOD.

  METHOD ut_run_as_tab_var.
    DATA lv_html_out TYPE string.
    DATA lt_template_lines     TYPE zif_c8a017_types=>tt_tline_str.
    lt_template_lines = VALUE #(
    ( tdformat = '*' tdline_str = 'simple line as it is' )
    ( tdformat = '*' tdline_str = 'var in line $KUNNR$ and after' )
    ( tdformat = '*' tdline_str = '?/:IF_BEG($KUNNR$;20)' )
    ( tdformat = '*' tdline_str = 'should not output' )
    ( tdformat = '*' tdline_str = '?/:IF_END' )
    ( tdformat = '*' tdline_str = '?/:IF_BEG($KUNNR$;1002003)' )
    ( tdformat = '*' tdline_str = 'if is true - should be putputed' )
    ( tdformat = '*' tdline_str = '?/:IF_END' )
    ( tdformat = '*' tdline_str = '$[T_ITEMS$' )
    ( tdformat = '*' tdline_str = 'material: $MATNR$' )
    ( tdformat = '*' tdline_str = 'mat_txt: $MATNR_TXT$' )
    ( tdformat = '*' tdline_str = '$T_ITEMS]$' )
    ).

    mo_cut->_proc_templ_v2(
      EXPORTING
        it_lines     = lt_template_lines
        is_src       = ms_src
      IMPORTING
        ev_final_str = lv_html_out
    ).
  ENDMETHOD.

  METHOD ut_run_vmeste_func.

    DATA lv_html_out TYPE string.
    DATA lt_template_lines     TYPE zif_c8a017_types=>tt_tline_str.
    lt_template_lines = VALUE #(
    ( tdformat = '*' tdline_str = 'simple line as it is' )
    ( tdformat = '*' tdline_str = 'var in line $KUNNR$ and after' )
    ( tdformat = '*' tdline_str = '?/:IF_BEG($KUNNR$;20)' )
    ( tdformat = '*' tdline_str = 'should not output' )
    ( tdformat = '*' tdline_str = '?/:IF_END' )
    ( tdformat = '*' tdline_str = '?/:IF_BEG($KUNNR$;1002003)' )
    ( tdformat = '*' tdline_str = 'if is true - should be putputed' )
    ( tdformat = '*' tdline_str = '?/:IF_END' )
    ( tdformat = '*' tdline_str = '?/:VMESTE(<B>Customer ID:</B> ;$KUNNR2$; <BR/>)' )
    ( tdformat = '*' tdline_str = '?/:VMESTE(<B>Customer ID:</B> ;$KUNNR_EMPTY$; <BR/>)' )
    ).

    mo_cut->_proc_templ_v2(
      EXPORTING
        it_lines     = lt_template_lines
        is_src       = ms_src
      IMPORTING
        ev_final_str = lv_html_out
    ).

  ENDMETHOD.

ENDCLASS.
