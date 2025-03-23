*"* use this source file for your ABAP unit test classes
CLASS ltc_terun DEFINITION DEFERRED.
CLASS zcl_c8a017_func_flow DEFINITION LOCAL FRIENDS ltc_terun.

CLASS ltc_terun DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT INHERITING FROM cl_aunit_assert
.

  PUBLIC SECTION.
    METHODS ut_run_01 FOR TESTING.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_c8a017_func_flow.
    "data mo_mock_data type ref to zcl_wf004_mock_data.

    METHODS setup.
    METHODS teardown.

ENDCLASS.


CLASS ltc_terun IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #(  ).
    " mo_mock_data = new #( ).
  ENDMETHOD.

  METHOD teardown.
    CLEAR mo_cut .
  ENDMETHOD.

  METHOD ut_run_01.
    TYPES: BEGIN OF ts_data_context
              , show_f1_link TYPE abap_bool
            , END OF ts_data_context
            .

    DATA ls_infunc_params TYPE zif_c8a017_types=>ts_infunc_params.

    DATA ls_data_context TYPE ts_data_context.
    DATA lv_cond_flow_res TYPE abap_bool.

    ls_data_context-show_f1_link = abap_true.

    ls_infunc_params-func_name = 'IF_BEG'.
    ls_infunc_params-t_func_params = value #(
    ( p_val = '$SHOW_F1_LINK$' )
    ( p_val = '$ABAP_FALSE$' )
    ( p_val = 'NE' )
    ).

    mo_cut->_exe_cond_func(
      EXPORTING
        is_func_params   = ls_infunc_params
        is_cntx          = ls_data_context
      IMPORTING
        ev_cond_flow_res = lv_cond_flow_res
    ).

    assert_equals(
      EXPORTING
        exp                  = abap_true
        act                  = lv_cond_flow_res
        msg                  = 'wrong comparison result'
*        level                = if_aunit_constants=>severity-medium
*        tol                  =
*        quit                 = if_aunit_constants=>quit-test
*        ignore_hash_sequence = abap_false
*      RECEIVING
*        assertion_failed     =
    ).

  ENDMETHOD.

ENDCLASS.
