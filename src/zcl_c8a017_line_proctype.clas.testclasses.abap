*"* use this source file for your ABAP unit test classes
CLASS ltc_terun DEFINITION DEFERRED.
CLASS zcl_c8a017_line_proctype DEFINITION LOCAL FRIENDS ltc_terun.

CLASS ltc_terun DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT INHERITING FROM cl_aunit_assert
.

  PUBLIC SECTION.
    METHODS ut_run_01 FOR TESTING.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_c8a017_line_proctype.
    " DATA mo_mock_data TYPE REF TO zcl_wf004_mock_data.

    METHODS setup.
    METHODS teardown.

ENDCLASS.


CLASS ltc_terun IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #(  ).
    "  mo_mock_data = NEW #( ).
  ENDMETHOD.

  METHOD teardown.
    CLEAR mo_cut .
  ENDMETHOD.

  METHOD ut_run_01.

    DATA ls_templ_cur TYPE zif_c8a017_types=>ts_tline_str.

    DATA ls_infunc_params TYPE zif_c8a017_types=>ts_infunc_params.

    ls_templ_cur-tdline_str = '$[T_MSG$'.

    mo_cut->_if_tab_replace(
      EXPORTING
        is_templ_cur   = ls_templ_cur
      CHANGING
        cs_func_params = ls_infunc_params
    ).

    assert_not_initial(
      EXPORTING
        act              = ls_infunc_params-func_processor
        msg              = 'no processor'
*        level            = if_aunit_constants=>severity-medium
*        quit             = if_aunit_constants=>quit-test
*      RECEIVING
*        assertion_failed =
    ).

  ENDMETHOD.

ENDCLASS.
