CLASS zcl_c8a017_struct2templ DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS fill_vars_in_template
      IMPORTING is_src          TYPE any
      CHANGING  cv_str_template TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: ts_var_templ TYPE zif_c8a017_types=>ts_var_templ.
    TYPES: tt_var_templ TYPE zif_c8a017_types=>tt_var_templ.

    METHODS _validate_src
      IMPORTING is_src    TYPE any
      RETURNING VALUE(rc) TYPE sysubrc.

    METHODS _struct2templ
      IMPORTING is_src           TYPE any
                iv_struct_nm     TYPE string OPTIONAL
      CHANGING  cv_templ_context TYPE string.

    METHODS _read_struct_components
      IMPORTING is_src      TYPE any
      EXPORTING et_comp_tab TYPE abap_component_tab.

    METHODS _get_template_for_tab
      IMPORTING iv_txt_tmpl TYPE string
                iv_beg_from TYPE string
                iv_end_upto TYPE string
      EXPORTING ev_tab_part TYPE string.

    METHODS _replace_in_templ_context
      IMPORTING iv_beg_from      TYPE string
                iv_end_upto      TYPE string
                iv_new_var4tab   TYPE string
      CHANGING  cv_templ_context TYPE string.

    METHODS _fill_templ_by_struct
      IMPORTING is_comp          TYPE any
                iv_comp_nm       TYPE string
      CHANGING  cv_templ_context TYPE string.

    METHODS _fill_templ_by_itab
      IMPORTING it_comp          TYPE ANY TABLE
                iv_comp_nm       TYPE string
      CHANGING  cv_templ_context TYPE string.

    METHODS _fill_var4simple
      IMPORTING iv_comp_val  TYPE simple
                iv_comp_nm   TYPE string
                iv_struct_nm TYPE string OPTIONAL
      CHANGING  ct_var_templ TYPE tt_var_templ.


    METHODS _fill_var4date
      IMPORTING iv_comp_val  TYPE sydatum
                iv_comp_nm   TYPE string
                iv_struct_nm TYPE string OPTIONAL
      CHANGING  ct_var_templ TYPE tt_var_templ.

    METHODS _vars2str
      IMPORTING it_var_templ TYPE tt_var_templ
      CHANGING  cv_templ     TYPE string.

ENDCLASS.



CLASS zcl_c8a017_struct2templ IMPLEMENTATION.
  METHOD fill_vars_in_template.
    "IMPORTING is_src          TYPE any
    "CHANGING  cv_str_template type string.

    " should be structure (deep structure is allowed) - not any other type
    IF _validate_src( is_src ) NE 0.
      CLEAR cv_str_template.
      RETURN.
    ENDIF.

    _struct2templ(  EXPORTING  is_src = is_src
                    CHANGING   cv_templ_context = cv_str_template  ).


*    FIELD-SYMBOLS <fs_var_templ> TYPE ts_var_templ.
*    LOOP AT lt_var_templ ASSIGNING <fs_var_templ>.
*      REPLACE ALL OCCURRENCES OF <fs_var_templ>-var_id
*        IN mv_templ WITH  <fs_var_templ>-var_val.
*    ENDLOOP.

  ENDMETHOD.

  METHOD _validate_src.
    "IMPORTING is_src    TYPE any
    "RETURNING VALUE(rc) TYPE sysubrc.
    DATA lv_type_kind TYPE abap_typekind.
    rc = 4.
    TRY.
        lv_type_kind =
        cl_abap_datadescr=>get_data_type_kind( p_data = is_src ).

        IF lv_type_kind EQ cl_abap_typedescr=>typekind_struct1
            OR lv_type_kind EQ cl_abap_typedescr=>typekind_struct2.
          rc = 0.
        ENDIF.

      CATCH cx_root.

        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD _struct2templ.
*      IMPORTING is_src           TYPE any
*                iv_struct_nm     TYPE string OPTIONAL
*      CHANGING  cv_templ_context TYPE string.

    DATA lv_type_kind TYPE abap_typekind.
    DATA lt_comp_tab TYPE abap_component_tab.

    DATA lt_var_templ TYPE tt_var_templ.


    FIELD-SYMBOLS <fs_comp_tab> TYPE abap_componentdescr.
    FIELD-SYMBOLS <fs_field_comp> TYPE any.

    _read_struct_components( EXPORTING is_src = is_src
                             IMPORTING et_comp_tab = lt_comp_tab  ).


    LOOP AT lt_comp_tab ASSIGNING <fs_comp_tab>.

      ASSIGN COMPONENT <fs_comp_tab>-name OF STRUCTURE is_src TO <fs_field_comp>.
      IF sy-subrc EQ 0.
        lv_type_kind = cl_abap_datadescr=>get_data_type_kind( p_data = <fs_field_comp> ).
        CASE lv_type_kind.

          WHEN cl_abap_typedescr=>typekind_struct1
            OR cl_abap_typedescr=>typekind_struct2.

            _fill_templ_by_struct(  EXPORTING is_comp = <fs_field_comp>
                                              iv_comp_nm = <fs_comp_tab>-name
                                    CHANGING cv_templ_context =  cv_templ_context ).

          WHEN cl_abap_typedescr=>typekind_table.
            _fill_templ_by_itab( EXPORTING it_comp = <fs_field_comp>
                                           iv_comp_nm = <fs_comp_tab>-name
                                 CHANGING cv_templ_context =  cv_templ_context ).


          WHEN cl_abap_typedescr=>typekind_char
            OR cl_abap_typedescr=>typekind_string
            OR cl_abap_typedescr=>typekind_int
            OR cl_abap_typedescr=>typekind_int1
            OR cl_abap_typedescr=>typekind_int8
            OR cl_abap_typedescr=>typekind_int2
            OR cl_abap_typedescr=>typekind_num
            OR cl_abap_typedescr=>typekind_numeric
            OR cl_abap_typedescr=>typekind_packed
            .

            _fill_var4simple( EXPORTING iv_comp_val = <fs_field_comp>
                                        iv_comp_nm = <fs_comp_tab>-name
                                        iv_struct_nm = iv_struct_nm
                               CHANGING ct_var_templ = lt_var_templ ).

          WHEN cl_abap_typedescr=>typekind_date.

            _fill_var4date( EXPORTING iv_comp_val = <fs_field_comp>
                                      iv_comp_nm = <fs_comp_tab>-name
                                      iv_struct_nm = iv_struct_nm
                             CHANGING ct_var_templ = lt_var_templ ).

          WHEN OTHERS.
            " do nothing for now
        ENDCASE.
      ENDIF.

    ENDLOOP.

    _vars2str( EXPORTING it_var_templ = lt_var_templ
                CHANGING cv_templ     = cv_templ_context  ).

  ENDMETHOD.

  METHOD _read_struct_components.
    "IMPORTING  is_src TYPE any
    "EXCEPTIONS et_comp_tab type abap_component_tab.

    DATA lo_struct  TYPE REF TO cl_abap_structdescr.
    TRY.

        lo_struct ?= cl_abap_datadescr=>describe_by_data( p_data = is_src ).
        et_comp_tab = lo_struct->get_components( ).
      CATCH cx_sy_unknown_type.
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD _get_template_for_tab.
*      IMPORTING iv_txt_tmpl TYPE string
*                iv_beg_from TYPE string
*                iv_end_upto TYPE string
*      EXPORTING ev_tab_part TYPE string.

    DATA lv_beg_offset TYPE syindex.
    DATA lv_end_offset TYPE syindex.
    DATA lv_content_length TYPE syindex.

    FIND FIRST OCCURRENCE OF iv_beg_from IN iv_txt_tmpl MATCH OFFSET lv_beg_offset.
    FIND FIRST OCCURRENCE OF iv_end_upto IN iv_txt_tmpl MATCH OFFSET lv_end_offset.

    lv_beg_offset = lv_beg_offset + strlen( iv_beg_from ).

    IF lv_beg_offset < lv_end_offset
        AND lv_end_offset LE strlen( iv_txt_tmpl ).
      lv_content_length = lv_end_offset - lv_beg_offset.

      ev_tab_part = iv_txt_tmpl+lv_beg_offset(lv_content_length).
    ENDIF.


  ENDMETHOD.

  METHOD _replace_in_templ_context.
*      IMPORTING iv_beg_from      TYPE string
*                iv_end_upto      TYPE string
*                iv_new_var4tab   TYPE string
*      CHANGING  cv_templ_context TYPE string.

    DATA lv_beg_offset TYPE syindex.
    DATA lv_end_offset TYPE syindex.
    DATA lv_content_length TYPE syindex.

    FIND FIRST OCCURRENCE OF iv_beg_from IN cv_templ_context MATCH OFFSET lv_beg_offset.
    FIND FIRST OCCURRENCE OF iv_end_upto IN cv_templ_context MATCH OFFSET lv_end_offset.

    lv_end_offset += strlen( iv_end_upto ).

    IF lv_beg_offset < lv_end_offset
        AND lv_end_offset LE strlen( cv_templ_context ).
      lv_content_length = lv_end_offset - lv_beg_offset.

      cv_templ_context = cv_templ_context(lv_beg_offset) && iv_new_var4tab && cv_templ_context+lv_end_offset.
    ENDIF.
  ENDMETHOD.

  METHOD _fill_templ_by_struct.
*      IMPORTING is_comp          TYPE any
*                iv_comp_nm       TYPE string
*      CHANGING  cv_templ_context TYPE string.
    DATA lt_var_templ_struct TYPE tt_var_templ.
    FIELD-SYMBOLS <fs_var_templ_struct> TYPE ts_var_templ.

    _struct2templ( EXPORTING  is_src           = is_comp
                              iv_struct_nm = iv_comp_nm
                    CHANGING  cv_templ_context = cv_templ_context  ).

  ENDMETHOD.

  METHOD _fill_templ_by_itab.
*      IMPORTING it_comp          TYPE ANY TABLE
*                iv_comp_nm       TYPE string
*      CHANGING  cv_templ_context TYPE string.
    DATA lv_tab_beg_from TYPE string.
    DATA lv_tab_end_upto TYPE string.

    DATA lv_templ4tab TYPE string.

    DATA lv_new_var4tab TYPE string.


    lv_tab_beg_from = '$[' && iv_comp_nm && '$'.
    lv_tab_end_upto = '$' && iv_comp_nm && ']$'.

    lv_new_var4tab = '$[' && iv_comp_nm && ']$'.


    _get_template_for_tab( EXPORTING iv_txt_tmpl = cv_templ_context
                                     iv_beg_from = lv_tab_beg_from
                                     iv_end_upto = lv_tab_end_upto
                           IMPORTING ev_tab_part = lv_templ4tab ).

    _replace_in_templ_context( EXPORTING iv_beg_from = lv_tab_beg_from
                                         iv_end_upto = lv_tab_end_upto
                                         iv_new_var4tab = lv_new_var4tab
                               CHANGING cv_templ_context = cv_templ_context ).

    DATA lt_var_templ_tab_line TYPE tt_var_templ.
    FIELD-SYMBOLS <fs_var_templ> TYPE ts_var_templ.

    DATA lv_tab_line_step TYPE string.
    DATA lv_tab_all TYPE string.



    CLEAR lv_tab_all.
    LOOP AT it_comp ASSIGNING FIELD-SYMBOL(<fs_tab_line>).
      CLEAR lv_tab_line_step.
      lv_tab_line_step = lv_templ4tab.

      _struct2templ( EXPORTING is_src = <fs_tab_line>
                     CHANGING  cv_templ_context = lv_tab_line_step ).

      IF lv_tab_all IS INITIAL.
        lv_tab_all = lv_tab_line_step.
      ELSE.
        lv_tab_all = lv_tab_all && lv_tab_line_step.
      ENDIF.

    ENDLOOP.


    REPLACE ALL OCCURRENCES OF lv_new_var4tab
                  IN cv_templ_context WITH  lv_tab_all.
  ENDMETHOD.

  METHOD _fill_var4simple.
*      IMPORTING iv_comp_val  TYPE simple
*                iv_comp_nm   TYPE string
*                iv_struct_nm TYPE string OPTIONAL
*      CHANGING  ct_var_templ TYPE tt_var_templ.
    DATA ls_var_templ TYPE ts_var_templ.

    ls_var_templ-var_val = iv_comp_val.

    IF iv_struct_nm IS INITIAL.
      ls_var_templ-var_id = '$' && iv_comp_nm && '$'.
    ELSE.
      ls_var_templ-var_id = '$' && iv_struct_nm && '-$' && iv_comp_nm && '$'.
    ENDIF.
    ls_var_templ-var_id = to_upper( ls_var_templ-var_id ).
    APPEND ls_var_templ TO ct_var_templ.

  ENDMETHOD.

  METHOD _fill_var4date.
*      IMPORTING iv_comp_val  TYPE simple
*                iv_comp_nm   TYPE string
*      CHANGING  ct_var_templ TYPE tt_var_templ.

    DATA lv_date10 TYPE char10.
    DATA ls_var_templ TYPE ts_var_templ.

    ls_var_templ-var_id = '$' && iv_comp_nm && '$'.
    WRITE iv_comp_val TO lv_date10.
    ls_var_templ-var_val = lv_date10.

    APPEND ls_var_templ TO ct_var_templ.

  ENDMETHOD.

  METHOD _vars2str.
    "IMPORTING it_var_templ TYPE tt_var_templ
    "CHANGING  cv_templ     TYPE string.
    FIELD-SYMBOLS <fs_var_templ> TYPE ts_var_templ.

    LOOP AT it_var_templ ASSIGNING <fs_var_templ>.
      REPLACE ALL OCCURRENCES OF <fs_var_templ>-var_id
          IN cv_templ WITH  <fs_var_templ>-var_val.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
