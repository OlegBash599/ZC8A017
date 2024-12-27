CLASS zcl_c8a017_longtxt_templ DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA mv_templ TYPE string READ-ONLY .
    DATA mv_templ_with_data TYPE string .

    METHODS constructor
      IMPORTING
        !is_txt_h TYPE thead .
    METHODS data2template
      IMPORTING
        !is       TYPE any
      RETURNING
        VALUE(rv) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_txt_src TYPE thead.


ENDCLASS.



CLASS zcl_c8a017_longtxt_templ IMPLEMENTATION.


  METHOD constructor.

    MOVE-CORRESPONDING is_txt_h TO ms_txt_src.

    IF ms_txt_src-tdspras IS INITIAL.
      ms_txt_src-tdspras = sy-langu.
    ENDIF.

    IF ms_txt_src-tdid IS INITIAL.
      ms_txt_src-tdid = 'ST'.
    ENDIF.

    IF ms_txt_src-tdobject IS INITIAL.
      ms_txt_src-tdobject = 'TEXT'.
    ENDIF.


  ENDMETHOD.


  METHOD data2template.


    rv =
    new zcl_c8a017_sapscript_read( is_thead = ms_txt_src )->r_as_string( ).



    new zcl_c8a017_struct2templ( )->fill_vars_in_template(
      EXPORTING
        is_src          = is
      CHANGING
        cv_str_template = rv
    ).


  ENDMETHOD.


ENDCLASS.
