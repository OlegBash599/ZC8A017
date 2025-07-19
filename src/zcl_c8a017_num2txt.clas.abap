CLASS ZCL_C8A017_NUM2TXT DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS ordinal_txt_en
      IMPORTING iv_num    TYPE syindex
      RETURNING VALUE(rv) TYPE string.

    METHODS month_year_en
      IMPORTING iv_date   TYPE sydatum
                iv_format TYPE int1 DEFAULT 1
      RETURNING VALUE(rv) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_C8A017_NUM2TXT IMPLEMENTATION.


  METHOD month_year_en.
    "IMPORTING iv_date TYPE sydatum
    "RETURNING VALUE(rv) TYPE string.
    DATA lv_month TYPE numc2.
    DATA lv_month_str TYPE string.
    DATA lv_year TYPE numc4.

    IF iv_date IS INITIAL.
      CLEAR rv.
      RETURN.
    ENDIF.

    lv_month = iv_date+4.
    lv_year  = iv_date.

    CASE lv_month.
      WHEN '01'. lv_month_str = 'January'.
      WHEN '02'. lv_month_str = 'Fedruary'.
      WHEN '03'. lv_month_str = 'March'.
      WHEN '04'. lv_month_str = 'April'.
      WHEN '05'. lv_month_str = 'May'.
      WHEN '06'. lv_month_str = 'June'.
      WHEN '07'. lv_month_str = 'July'.
      WHEN '08'. lv_month_str = 'August'.
      WHEN '09'. lv_month_str = 'September'.
      WHEN '10'. lv_month_str = 'October'.
      WHEN '11'. lv_month_str = 'November'.
      WHEN '12'. lv_month_str = 'December'.
    ENDCASE.

    rv = |{ lv_month_str }/{ lv_year }|.

  ENDMETHOD.


  METHOD ordinal_txt_en.
    "IMPORTING iv_num TYPE syindex
    "RETURNING VALUE(rv) TYPE string.

    DATA lv_last_digit TYPE syindex.

    IF iv_num GE 10
        AND iv_num LE 20.
      rv = |{ iv_num }th|.
      RETURN.
    ENDIF.

    lv_last_digit = iv_num MOD 10.

    CASE lv_last_digit.
      WHEN 1.
        rv = |{ iv_num }st|.
      WHEN 2.
        rv = |{ iv_num }nd|.

      WHEN 3.
        rv = |{ iv_num }rd|.

      WHEN OTHERS.
        rv = |{ iv_num }th|.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
