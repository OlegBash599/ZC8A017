*&---------------------------------------------------------------------*
*&  Include           ZREP_C8A017_FLORISTR_OUT_SCRN
*&---------------------------------------------------------------------*


PARAMETERS: p_shcase TYPE char2 DEFAULT 'A1' AS LISTBOX VISIBLE LENGTH 70
                                USER-COMMAND lb1
                                OBLIGATORY
                                MODIF ID m99.


SELECTION-SCREEN BEGIN OF BLOCK ba1 WITH FRAME TITLE TEXT-ba1.
  PARAMETERS: p_templ TYPE string LOWER CASE MODIF ID ma1.
SELECTION-SCREEN END OF BLOCK ba1.

SELECTION-SCREEN BEGIN OF BLOCK ba2 WITH FRAME TITLE TEXT-ba2.
  PARAMETERS: p_tmpl2 TYPE string LOWER CASE MODIF ID ma2.
SELECTION-SCREEN END OF BLOCK ba2.

SELECTION-SCREEN BEGIN OF BLOCK ba3 WITH FRAME TITLE TEXT-ba3.
  PARAMETERS: p_tmpl3 TYPE string LOWER CASE MODIF ID ma3.
SELECTION-SCREEN END OF BLOCK ba3.

SELECTION-SCREEN BEGIN OF BLOCK ba4 WITH FRAME TITLE TEXT-ba4.
  PARAMETERS: p_tmpl4 TYPE string LOWER CASE MODIF ID ma4.
  PARAMETERS:   incl RADIOBUTTON GROUP rg4 MODIF ID ma4 DEFAULT 'X'
              , vars RADIOBUTTON GROUP rg4 MODIF ID ma4
              , loops RADIOBUTTON GROUP rg4 MODIF ID ma4
              , infunc RADIOBUTTON GROUP rg4 MODIF ID ma4
              , condit RADIOBUTTON GROUP rg4 MODIF ID ma4
              , comps RADIOBUTTON GROUP rg4 MODIF ID ma4
              .
SELECTION-SCREEN END OF BLOCK ba4.
