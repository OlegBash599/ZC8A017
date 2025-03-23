*&---------------------------------------------------------------------*
*&  Include           ZREP_C8A017_FLORISTR_OUT_CLS01
*&---------------------------------------------------------------------*

INTERFACE lif_types.

  types: BEGIN OF ts_scr
            , shcase TYPE char2
            , path2templ TYPE string

            , p_tmpl2 TYPE string

            , p_tmpl3 TYPE string
            , p_tmpl4 TYPE string

            , incl   type char1
            , vars   type char1
            , loops  type char1
            , infunc type char1
            , condit type char1
            , comps  type char1

        , END OF ts_scr
        .

ENDINTERFACE.
