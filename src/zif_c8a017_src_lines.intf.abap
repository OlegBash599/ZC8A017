INTERFACE zif_c8a017_src_lines
  PUBLIC .

  METHODS r_as_string
    IMPORTING is_src    TYPE zif_c8a017_types=>ts_src_info
    RETURNING VALUE(rv) TYPE string.

  METHODS r_as_str_lines
    IMPORTING is_src    TYPE zif_c8a017_types=>ts_src_info
    EXPORTING et        TYPE zif_c8a017_types=>tt_tline_str
              es_params TYPE zif_c8a017_types=>ts_src_params.

ENDINTERFACE.
