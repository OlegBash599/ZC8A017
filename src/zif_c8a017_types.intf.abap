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

  TYPES: BEGIN OF ts_src_info
            , src_type  TYPE string
            , path2file TYPE string

            , tdobject  TYPE thead-tdobject
            , tdname    TYPE thead-tdname
            , tdid      TYPE thead-tdid
            , tdspras   TYPE spras

       , END OF ts_src_info
       .

  TYPES: BEGIN OF ts_file_params
             , size_length TYPE syindex
             , folder TYPE string
             , mime_type TYPE string
             , extensn TYPE string
         , END OF ts_file_params
         .

  TYPES: BEGIN OF ts_src_params
          , file_prm TYPE ts_file_params
          , sapscript_prm TYPE ts_file_params
        , END OF ts_src_params
        .


  TYPES: BEGIN OF ts_tline_str
           , line_num_src TYPE syindex
           , line_num_wrk TYPE syindex
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
             , no_name_val_param TYPE abap_bool
           , END OF ts_func_info
           , tt_func_info TYPE STANDARD TABLE OF ts_func_info WITH DEFAULT KEY
           .

    TYPES:
      BEGIN OF ts_text_line
                , line TYPE text4096
            , END OF ts_text_line .
    TYPES:
      tt_text_line TYPE STANDARD TABLE OF ts_text_line WITH DEFAULT KEY .

  TYPES: BEGIN OF ts_load_tab_html
            , name          TYPE string
            , type          TYPE string
            , char_tab_cont TYPE tt_text_line
       , END OF ts_load_tab_html
       , tt_load_tab_html TYPE STANDARD TABLE OF ts_load_tab_html WITH DEFAULT KEY
       .

ENDINTERFACE.
