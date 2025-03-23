*&---------------------------------------------------------------------*
*&  Include           ZREP_C8A017_FLORISTR_OUT_EVNTS
*&---------------------------------------------------------------------*



INITIALIZATION.
  lo_app = NEW #( ).
  lo_app->init( ).

AT SELECTION-SCREEN.
  lo_app->at_sel_screen( ).

AT SELECTION-SCREEN OUTPUT.
  lo_app->at_sel_screen_out( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_templ.
  lo_app->val_request( EXPORTING iv_fnam = 'P_TEMPL'
                       CHANGING cv_templ = p_templ ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tmpl2.
  lo_app->val_request( EXPORTING iv_fnam = 'P_TMPL2'
                       CHANGING cv_templ = p_tmpl2 ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tmpl3.
  lo_app->val_request( EXPORTING iv_fnam = 'P_TMPL3'
                       CHANGING cv_templ = p_tmpl3 ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tmpl4.
  lo_app->val_request( EXPORTING iv_fnam = 'P_TMPL4'
                       CHANGING cv_templ = p_tmpl4 ).

START-OF-SELECTION.
  lo_app->start_of_sel( ).

END-OF-SELECTION.
  lo_app->end_of_sel( ).
