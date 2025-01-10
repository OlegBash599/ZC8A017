INTERFACE zif_c8a017_hardvals
  PUBLIC .

  CONSTANTS: BEGIN OF mc_reg
                 , regex_word_var TYPE string VALUE '\$\w+\$'
             , END OF mc_reg
             .

    CONSTANTS: BEGIN OF mc_proc_type
                    , move_as_it_is TYPE int1 VALUE 0
                    , calc_conv_func TYPE int1 VALUE 1
                    , flow_control TYPE int1 VALUE 2
                    , exclude_this_line TYPE int1 VALUE 3
                    , find_and_replace TYPE int1 VALUE 4
                    , find_and_replace_tab TYPE int1 VALUE 5
              , END OF mc_proc_type
              .
ENDINTERFACE.
