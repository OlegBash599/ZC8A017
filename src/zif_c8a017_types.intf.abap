INTERFACE zif_c8a017_types
  PUBLIC .
  TYPES: BEGIN OF ts_var_templ
        , var_id TYPE string
        , var_val TYPE string
        , contxt_block TYPE string
        , contxt_block_type TYPE string
   , END OF ts_var_templ
   , tt_var_templ TYPE STANDARD TABLE OF ts_var_templ WITH DEFAULT KEY
   .

  TYPES: BEGIN OF ts_tline_str
           , tdformat TYPE tdformat
           , tdline_str TYPE string
        , END OF ts_tline_str
        , tt_tline_str TYPE STANDARD TABLE OF ts_tline_str WITH DEFAULT KEY
        .

  TYPES: BEGIN OF ts_flow_control
              , do_skip_this_line TYPE abap_bool
              , block_func_cntx   TYPE string
              , func_deep         TYPE int4
              , next_line4proc    TYPE int4
              , lines_before_next TYPE tt_tline_str
         , END OF ts_flow_control
         .

  TYPES: BEGIN OF ts_func_param
              , p_name TYPE string
              , p_val TYPE string
         , END OF ts_func_param
         , tt_func_param TYPE STANDARD TABLE OF ts_func_param WITH DEFAULT KEY
         .

  TYPES: BEGIN OF ts_infunc_params
             , proc_type TYPE int1
             , func_name TYPE string
             , func_processor TYPE seoclsname
             , t_func_params TYPE tt_func_param
             , v_beg_line_templ TYPE syindex
             , v_end_line_templ TYPE syindex
        , END OF ts_infunc_params
        , tt_infunc_params TYPE STANDARD TABLE OF ts_infunc_params WITH DEFAULT KEY
        .

  TYPES: BEGIN OF ts_func_info
             , func_name TYPE string
             , proc_type TYPE int1
             , func_processor TYPE seoclsname
           , END OF ts_func_info
           , tt_func_info TYPE STANDARD TABLE OF ts_func_info WITH DEFAULT KEY
           .

ENDINTERFACE.
