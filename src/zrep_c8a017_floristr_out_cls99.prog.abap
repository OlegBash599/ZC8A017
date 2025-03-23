*&---------------------------------------------------------------------*
*&  Include           ZREP_C8A017_FLORISTR_OUT_CLS99
*&---------------------------------------------------------------------*

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS init.
    METHODS start_of_sel.
    METHODS end_of_sel.

    METHODS at_sel_screen.
    METHODS at_sel_screen_out.

    METHODS val_request
      IMPORTING iv_fnam  TYPE fieldname
      CHANGING  cv_templ TYPE any.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA ms_scr TYPE lif_types=>ts_scr.

    DATA mo_file_path TYPE REF TO zcl_c8a017_file_path.

    METHODS _fill_screen.

    METHODS _hide_show_elements.

ENDCLASS.


CLASS lcl_app IMPLEMENTATION.

  METHOD constructor.

    mo_file_path = NEW #( ).

  ENDMETHOD.

  METHOD init.

    DATA lv_listbox_shcase TYPE vrm_id VALUE 'P_SHCASE'.
    DATA lt_listbox_vals TYPE vrm_values.

    lt_listbox_vals = VALUE #(
    ( key = 'A1' text = 'Вывод в HTML напрямую из текста (без процессора)' )
    ( key = 'A2' text = 'Вывод в HTML из base-include' )
    ( key = 'A3' text = 'With external file' )
    ( key = 'A4' text = 'Florida String (*.flstr): Template File' )
    ).

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = lv_listbox_shcase    " Name of Value Set
        values          = lt_listbox_vals
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD start_of_sel.

    _fill_screen( ).

    CASE p_shcase.
      WHEN 'A1'.

        NEW lcl_html2browser_a1( is_scr = ms_scr )->fn( ).

      WHEN 'A2'.

        NEW lcl_popup_yes_no( is_scr = ms_scr )->fn( ).

      when 'A3'.

        NEW lcl_preprocess_tmpl( is_scr = ms_scr )->fn( ).

      when 'A4'.

        NEW lcl_florida_str( is_scr = ms_scr )->fn( ).

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD end_of_sel.

  ENDMETHOD.

  METHOD at_sel_screen.


  ENDMETHOD.

  METHOD _hide_show_elements.

    DATA lv_group_display TYPE char3.

    LOOP AT SCREEN.
      IF screen-group1 EQ 'M99'
          OR screen-group1 IS INITIAL.
        CONTINUE.
      ENDIF.

      lv_group_display = 'M' && ms_scr-shcase.

      IF screen-group1 EQ lv_group_display.
        screen-active = 1.
        screen-invisible = 0.
        screen-output = 1.
        MODIFY SCREEN.
      ELSE.
        screen-active = 0.
        screen-invisible = 1.
        screen-output = 0.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD at_sel_screen_out.
    _fill_screen( ).

    _hide_show_elements( ).
  ENDMETHOD.

  METHOD val_request.
    "IMPORTING iv_fnam  TYPE fieldname
    "CHANGING  cv_templ TYPE any.

    DATA lv_fnam      TYPE fieldname.
    DATA lv_path2file TYPE string.

    lv_fnam = to_upper( iv_fnam ).

    CASE lv_fnam .
      WHEN 'P_TEMPL' or 'P_TMPL2' or 'P_TMPL3' .

        mo_file_path->dialog_on_pc( EXPORTING iv_file_type = 1
                                    IMPORTING ev_path2file = lv_path2file ).

        IF lv_path2file IS NOT INITIAL.
          cv_templ = lv_path2file.
        ENDIF.

      when 'P_TMPL4'.

        mo_file_path->dialog_on_pc( EXPORTING iv_file_type = 2
                                    IMPORTING ev_path2file = lv_path2file ).

        IF lv_path2file IS NOT INITIAL.
          cv_templ = lv_path2file.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD _fill_screen.

    ms_scr-shcase = p_shcase.
    ms_scr-path2templ   = p_templ.
    ms_scr-p_tmpl2      = p_tmpl2.
    ms_scr-p_tmpl3      = p_tmpl3.
    ms_scr-p_tmpl4      = p_tmpl4.

    ms_scr-incl     = incl.
    ms_scr-vars     = vars.
    ms_scr-loops    = loops.
    ms_scr-infunc   = infunc.
    ms_scr-condit   = condit.
    ms_scr-comps    = comps.

  ENDMETHOD.

ENDCLASS.


DATA lo_app TYPE REF TO lcl_app.
