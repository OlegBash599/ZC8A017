*"* use this source file for your ABAP unit test classes
CLASS ltc_terun DEFINITION DEFERRED.
CLASS zcl_c8a017_num2txt DEFINITION LOCAL FRIENDS ltc_terun.

CLASS ltc_terun DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT INHERITING FROM cl_aunit_assert
.

  PUBLIC SECTION.
    METHODS ut_run_01 FOR TESTING.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_c8a017_num2txt.


    METHODS setup.
    METHODS teardown.

ENDCLASS.


CLASS ltc_terun IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #(  ).

  ENDMETHOD.

  METHOD teardown.
    CLEAR mo_cut .
  ENDMETHOD.

  METHOD ut_run_01.
    TYPES: BEGIN OF ts_num_txt
                , num TYPE syindex
                , num_ord_exp TYPE string
                , num_ord_act TYPE string
         , END OF ts_num_txt
         , tt_num_txt TYPE STANDARD TABLE OF ts_num_txt WITH DEFAULT KEY
         .

    FIELD-SYMBOLS <fs_num_txt> TYPE ts_num_txt.

    DATA lt_num_txt TYPE tt_num_txt.


    lt_num_txt = VALUE #(
    ( num = 1 num_ord_exp = '1st')
    ( num = 21 num_ord_exp = '21st')
    ( num = 3 num_ord_exp = '3rd')
    ( num = 13 num_ord_exp = '13th')
    ( num = 2 num_ord_exp = '2nd')
    ( num = 4 num_ord_exp = '4th')
    ( num = 5 num_ord_exp = '5th')
    ( num = 6 num_ord_exp = '6th')
    ( num = 7 num_ord_exp = '7th')
    ( num = 8 num_ord_exp = '8th')
    ( num = 9 num_ord_exp = '9th')
    ).

    LOOP AT lt_num_txt ASSIGNING <fs_num_txt>.
      mo_cut->ordinal_txt_en(
        EXPORTING
          iv_num = <fs_num_txt>-num
        RECEIVING
          rv     = <fs_num_txt>-num_ord_act
        ).

      assert_equals(
        EXPORTING
          exp                  = <fs_num_txt>-num_ord_exp
          act                  = <fs_num_txt>-num_ord_act
          msg                  = |wrong for { <fs_num_txt>-num }|
*            level                = if_aunit_constants=>severity-medium
*            tol                  =
*            quit                 = if_aunit_constants=>quit-test
*            ignore_hash_sequence = abap_false
*          RECEIVING
*            assertion_failed     =
      ).

    ENDLOOP.


  ENDMETHOD.

ENDCLASS.
