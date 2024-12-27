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
ENDINTERFACE.
