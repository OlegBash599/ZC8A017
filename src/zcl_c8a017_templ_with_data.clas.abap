CLASS zcl_c8a017_templ_with_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.

    METHODS proc_templ
      IMPORTING it_lines     TYPE zif_c8a017_types=>tt_tline_str
                is_src       TYPE any
      EXPORTING ev_final_str TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: ts_flow_control  TYPE zif_c8a017_types=>ts_flow_control.
    TYPES: ts_infunc_params TYPE zif_c8a017_types=>ts_infunc_params.

    DATA msr_src TYPE REF TO data.

    METHODS _proc_templ_v2
      IMPORTING it_lines     TYPE zif_c8a017_types=>tt_tline_str
                is_src       TYPE any
      EXPORTING ev_final_str TYPE string.

    METHODS _exe_conv_func
      IMPORTING is_func_params TYPE ts_infunc_params
                is_line_str    TYPE zif_c8a017_types=>ts_tline_str
      CHANGING  cs_line_res    TYPE zif_c8a017_types=>ts_tline_str.

    METHODS _do_flow_control
      IMPORTING is_func_params  TYPE ts_infunc_params
                it_templ_all    TYPE zif_c8a017_types=>tt_tline_str
                is_line_str     TYPE zif_c8a017_types=>ts_tline_str
                iv_line_num     TYPE syindex
                is_src_cntx     TYPE any
      CHANGING  cs_flow_control TYPE ts_flow_control.

    METHODS _find_and_replace
      IMPORTING is_func_params TYPE ts_infunc_params
                is_line_str    TYPE zif_c8a017_types=>ts_tline_str
                is_src_cntx    TYPE any
      CHANGING  cs_line_res    TYPE zif_c8a017_types=>ts_tline_str.
ENDCLASS.



CLASS ZCL_C8A017_TEMPL_WITH_DATA IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_C8A017_TEMPL_WITH_DATA->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_C8A017_TEMPL_WITH_DATA->PROC_TEMPL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_LINES                       TYPE        ZIF_C8A017_TYPES=>TT_TLINE_STR
* | [--->] IS_SRC                         TYPE        ANY
* | [<---] EV_FINAL_STR                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD proc_templ.
*      IMPORTING it_lines     TYPE tline_tab
*                is_src       TYPE any
*      EXPORTING ev_final_str TYPE string.

    _proc_templ_v2(
      EXPORTING
        it_lines     = it_lines
        is_src       = is_src
      IMPORTING
        ev_final_str = ev_final_str
    ).


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_C8A017_TEMPL_WITH_DATA->_DO_FLOW_CONTROL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_FUNC_PARAMS                 TYPE        TS_INFUNC_PARAMS
* | [--->] IT_TEMPL_ALL                   TYPE        ZIF_C8A017_TYPES=>TT_TLINE_STR
* | [--->] IS_LINE_STR                    TYPE        ZIF_C8A017_TYPES=>TS_TLINE_STR
* | [--->] IV_LINE_NUM                    TYPE        SYINDEX
* | [--->] IS_SRC_CNTX                    TYPE        ANY
* | [<-->] CS_FLOW_CONTROL                TYPE        TS_FLOW_CONTROL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _do_flow_control.
*      IMPORTING is_func_params  TYPE ts_infunc_params
*                it_templ_all    TYPE zif_c8a017_types=>tt_tline_str
*                is_line_str     TYPE zif_c8a017_types=>ts_tline_str
*                iv_line_num     TYPE syindex
*                is_src_cntx     TYPE any
*      CHANGING  cs_flow_control TYPE ts_flow_control.

    DATA lx_root TYPE REF TO cx_root.
    DATA lo_func TYPE REF TO zif_c8a017_func.

    IF is_func_params-func_processor IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        CREATE OBJECT lo_func TYPE (is_func_params-func_processor).
        lo_func->do_flow_control( EXPORTING is_func_params = is_func_params
                                    it_templ_all   = it_templ_all
                                    is_line_str    = is_line_str
                                    iv_line_num    = iv_line_num
                                    is_cntx        = is_src_cntx
                           CHANGING cs_flow_control    = cs_flow_control ).
      CATCH cx_root INTO lx_root.
        RETURN.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_C8A017_TEMPL_WITH_DATA->_EXE_CONV_FUNC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_FUNC_PARAMS                 TYPE        TS_INFUNC_PARAMS
* | [--->] IS_LINE_STR                    TYPE        ZIF_C8A017_TYPES=>TS_TLINE_STR
* | [<-->] CS_LINE_RES                    TYPE        ZIF_C8A017_TYPES=>TS_TLINE_STR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _exe_conv_func.
*      IMPORTING is_func_params TYPE ts_infunc_params
*                is_line_str    TYPE zif_c8a017_types=>ts_tline_str
*      CHANGING  cs_line_res    TYPE zif_c8a017_types=>ts_tline_str.
    DATA lx_root TYPE REF TO cx_root.
    DATA lo_func TYPE REF TO zif_c8a017_func.

    FIELD-SYMBOLS <fs_src_in> TYPE any.

    IF is_func_params-func_processor IS INITIAL.
      CLEAR cs_line_res.
      RETURN.
    ENDIF.

    ASSIGN msr_src->* TO <fs_src_in>.
    IF sy-subrc NE 0.
      CLEAR cs_line_res.
      RETURN.
    ENDIF.

    TRY.
        CREATE OBJECT lo_func TYPE (is_func_params-func_processor).
        lo_func->do_conv( EXPORTING is_func_params = is_func_params
                                    is_line_str    = is_line_str
                                    is_cntx        = <fs_src_in>
                           CHANGING cs_line_res    = cs_line_res ).
      CATCH cx_root INTO lx_root.
        CLEAR cs_line_res.
        RETURN.
    ENDTRY.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_C8A017_TEMPL_WITH_DATA->_FIND_AND_REPLACE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_FUNC_PARAMS                 TYPE        TS_INFUNC_PARAMS
* | [--->] IS_LINE_STR                    TYPE        ZIF_C8A017_TYPES=>TS_TLINE_STR
* | [--->] IS_SRC_CNTX                    TYPE        ANY
* | [<-->] CS_LINE_RES                    TYPE        ZIF_C8A017_TYPES=>TS_TLINE_STR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _find_and_replace.
*      IMPORTING is_func_params TYPE ts_infunc_params
*                is_line_str    TYPE zif_c8a017_types=>ts_tline_str
*                is_src_cntx    TYPE any
*      CHANGING  cs_line_res    TYPE zif_c8a017_types=>ts_tline_str.
*    FIELD-SYMBOLS <fs_func_params> TYPE zif_c8a017_types=>ts_func_param.
*    FIELD-SYMBOLS <fs_src_val> TYPE any.
*
*    cs_line_res-tdformat = is_line_str-tdformat.
*    cs_line_res-tdline_str = is_line_str-tdline_str.
*
*    LOOP AT is_func_params-t_func_params ASSIGNING <fs_func_params>.
*      ASSIGN COMPONENT <fs_func_params>-p_val OF STRUCTURE is_src_cntx TO <fs_src_val>.
*      IF sy-subrc EQ 0.
*        REPLACE ALL OCCURRENCES OF <fs_func_params>-p_name IN cs_line_res-tdline_str
*             WITH <fs_src_val>.
*      ENDIF.
*    ENDLOOP.

  "" https://github.com/OlegBash599/ZC8A017/issues/2
    DATA lo_type_descr TYPE REF TO cl_abap_typedescr.
    DATA lv_str_cell TYPE string.

    FIELD-SYMBOLS <fs_func_params> TYPE zif_c8a017_types=>ts_func_param.
    FIELD-SYMBOLS <fs_src_val> TYPE any.

    cs_line_res-tdformat = is_line_str-tdformat.
    cs_line_res-tdline_str = is_line_str-tdline_str.

    LOOP AT is_func_params-t_func_params ASSIGNING <fs_func_params>.
      ASSIGN COMPONENT <fs_func_params>-p_val OF STRUCTURE is_src_cntx TO <fs_src_val>.
      IF sy-subrc EQ 0.

        "DESCRIBE FIELD <fs_src_val>
        lo_type_descr ?= cl_abap_typedescr=>describe_by_data( <fs_src_val> ).

        CASE lo_type_descr->type_kind.
          WHEN cl_abap_typedescr=>typekind_packed.
            lv_str_cell = <fs_src_val>.
            CONDENSE lv_str_cell.

            REPLACE ALL OCCURRENCES OF <fs_func_params>-p_name IN cs_line_res-tdline_str
                 WITH lv_str_cell.

          WHEN OTHERS.

            REPLACE ALL OCCURRENCES OF <fs_func_params>-p_name IN cs_line_res-tdline_str
                 WITH <fs_src_val>.

        ENDCASE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_C8A017_TEMPL_WITH_DATA->_PROC_TEMPL_V2
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_LINES                       TYPE        ZIF_C8A017_TYPES=>TT_TLINE_STR
* | [--->] IS_SRC                         TYPE        ANY
* | [<---] EV_FINAL_STR                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _proc_templ_v2.
*      IMPORTING it_lines     TYPE zif_c8a017_types=>tt_tline_str
*                is_src       TYPE any
*      EXPORTING ev_final_str TYPE string.
    DATA lo_proc_type TYPE REF TO zcl_c8a017_line_proctype.
    lo_proc_type = NEW #(  ).


    DATA ls_infunc_params TYPE zif_c8a017_types=>ts_infunc_params.
    DATA lt_infunc_params TYPE zif_c8a017_types=>tt_infunc_params.
    DATA lv_templ_current TYPE syindex.

    DATA lt_lines_res     TYPE zif_c8a017_types=>tt_tline_str.
    DATA ls_line_res      TYPE zif_c8a017_types=>ts_tline_str.



    DATA ls_flow_control TYPE ts_flow_control.
    DATA ls_func_params TYPE ts_infunc_params.

    DATA lt_res_after_tmpl TYPE zif_c8a017_types=>tt_tline_str.

    FIELD-SYMBOLS <fs_line_str> TYPE zif_c8a017_types=>ts_tline_str.
    FIELD-SYMBOLS <fs_line_str_sub> TYPE zif_c8a017_types=>ts_tline_str.

    FIELD-SYMBOLS <fs_src_in> TYPE any.

    msr_src = REF #( is_src ).

    ASSIGN msr_src->* TO <fs_src_in>.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    lv_templ_current = 0.
    LOOP AT it_lines ASSIGNING <fs_line_str>.
      lv_templ_current = lv_templ_current + 1.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF ls_flow_control-next_line4proc IS NOT INITIAL.
        IF lv_templ_current = ls_flow_control-next_line4proc .
          CLEAR ls_flow_control-next_line4proc .
        ENDIF.
        IF lv_templ_current LT ls_flow_control-next_line4proc.
          CONTINUE.
        ENDIF.
      ENDIF.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      CLEAR ls_line_res.

      lo_proc_type->fill_func_params_by_line(
        EXPORTING it_templ_all   = it_lines
                  is_templ_cur   = <fs_line_str>
                  iv_line_num    = lv_templ_current
        IMPORTING es_func_params = ls_func_params   ).


      CASE ls_func_params-proc_type.

        WHEN zif_c8a017_hardvals=>mc_proc_type-calc_conv_func.
          _exe_conv_func( EXPORTING is_func_params = ls_func_params
                                    is_line_str = <fs_line_str>
                           CHANGING cs_line_res = ls_line_res ).
          IF ls_line_res IS NOT INITIAL.
            APPEND ls_line_res TO lt_lines_res.
          ENDIF.

        WHEN zif_c8a017_hardvals=>mc_proc_type-flow_control.

          _do_flow_control( EXPORTING is_func_params = ls_func_params
                                      it_templ_all   = it_lines
                                      is_line_str = <fs_line_str>
                                      iv_line_num    = lv_templ_current
                                      is_src_cntx = <fs_src_in>
                             CHANGING cs_flow_control = ls_flow_control ).

          IF lines( ls_flow_control-lines_before_next ) > 0.
            APPEND LINES OF ls_flow_control-lines_before_next
                TO lt_lines_res.
            clear ls_flow_control-lines_before_next.
          ENDIF.

          IF ls_flow_control-do_skip_this_line EQ abap_true.
            clear ls_flow_control-do_skip_this_line.
            CONTINUE.
          ENDIF.

        WHEN zif_c8a017_hardvals=>mc_proc_type-find_and_replace.
          _find_and_replace( EXPORTING is_func_params = ls_func_params
                                    is_line_str = <fs_line_str>
                                    is_src_cntx = <fs_src_in>
                           CHANGING cs_line_res = ls_line_res ).
          IF ls_line_res IS NOT INITIAL.
            APPEND ls_line_res TO lt_lines_res.
          ENDIF.
        WHEN zif_c8a017_hardvals=>mc_proc_type-exclude_this_line.
          CONTINUE.
        WHEN OTHERS.
          APPEND <fs_line_str> TO lt_lines_res.
      ENDCASE.

    ENDLOOP.


    LOOP AT lt_lines_res ASSIGNING <fs_line_str>.
      IF ev_final_str IS INITIAL.
        ev_final_str = <fs_line_str>-tdline_str.
      ELSE.
        ev_final_str = ev_final_str && ` ` && <fs_line_str>-tdline_str.
      ENDIF.
    ENDLOOP.



  ENDMETHOD.
ENDCLASS.
