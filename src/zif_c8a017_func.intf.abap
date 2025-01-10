INTERFACE zif_c8a017_func
  PUBLIC .

  METHODS do_conv
    IMPORTING is_func_params TYPE zif_c8a017_types=>ts_infunc_params
              is_line_str    TYPE zif_c8a017_types=>ts_tline_str
              is_cntx        TYPE any
    CHANGING  cs_line_res    TYPE zif_c8a017_types=>ts_tline_str.

  METHODS do_flow_control
    IMPORTING is_func_params  TYPE zif_c8a017_types=>ts_infunc_params
              it_templ_all    TYPE zif_c8a017_types=>tt_tline_str
              is_line_str     TYPE zif_c8a017_types=>ts_tline_str
              iv_line_num     TYPE syindex
              is_cntx         TYPE any
    CHANGING  cs_flow_control TYPE zif_c8a017_types=>ts_flow_control.


ENDINTERFACE.
