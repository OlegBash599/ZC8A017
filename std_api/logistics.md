# STD API to be wrapped firstly

## Availability Check
```abap

    TYPES: BEGIN OF ts_bapi_in
            , v_plant TYPE bapimatvp-werks
            , v_MATERIAL TYPE  matnr18
            , v_unit TYPE bapiadmm-unit
            , v_CHECK_RULE TYPE bapit441v-prreg

            , v_ENDLEADTME TYPE bapicm61m-wzter
            , v_AV_QTY_PLT TYPE bapicm61v-wkbst
            , v_DIALOGFLAG TYPE bapicm61v-diafl
            , s_RETURN TYPE bapireturn

            , t_WMDVSX TYPE STANDARD TABLE OF bapiwmdvs WITH DEFAULT KEY
            , t_WMDVEX TYPE STANDARD TABLE OF bapiwmdve WITH DEFAULT KEY
         , END OF ts_bapi_in
         .

        ls_bapi_in-v_plant = is_resb-werks.
        ls_bapi_in-v_MATERIAL = is_resb-matnr.
        ls_bapi_in-v_unit = is_resb-erfme.
        ls_bapi_in-v_CHECK_RULE = 'PM'.

        CALL FUNCTION 'BAPI_MATERIAL_AVAILABILITY'
          EXPORTING
            plant      = ls_bapi_in-v_plant
            material   = ls_bapi_in-v_MATERIAL
            unit       = ls_bapi_in-v_unit
            check_rule = ls_bapi_in-v_CHECK_RULE
*           stge_loc   =
*           batch      =
*           customer   =
*           doc_number =
*           itm_number =
*           wbs_elem   =
*           stock_ind  =
*           dec_for_rounding   =
*           dec_for_rounding_x =
*           read_atp_lock      =
*           read_atp_lock_x    =
*           material_evg       =
*           sgt_rcat   =
*           material_long      =
*           req_seg_long       =
          IMPORTING
            endleadtme = ls_bapi_in-v_endleadtme
            av_qty_plt = ls_bapi_in-v_av_qty_plt
            dialogflag = ls_bapi_in-v_dialogflag
            return     = ls_bapi_in-s_return
          TABLES
            wmdvsx     = ls_bapi_in-t_wmdvsx
            wmdvex     = ls_bapi_in-t_wmdvex.

```
