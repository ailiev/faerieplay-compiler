-- GenHelper_C.hs:
-- generate a helper C++ file for the main C++ file, which mainly parses the parameters and
-- prints out the results.

module Faerieplay.GenHelper_C where


import qualified Faerieplay.Intermediate        as Im

import Faerieplay.SashoLib                      (DocAble(..))

import Control.Exception                        (assert)
import Text.Printf                              (printf)
import Text.PrettyPrint




genHelper :: String -> Im.Prog -> String
genHelper templ prog =
        let (Im.Func name _ rettype args _) = Im.getMain prog
            (names, ts)             = unzip args
            -- eg. ["*x", "*y"]
            star_name_ds            = map (\nm -> char '*' <> text nm) names
            rettype_d               = doc rettype
            sfdlmain_arg_decls_d    = hsep $
                                      punctuate comma
                                                (map doc args)
            parse_outargs_decl_d    = sep $
                                      punctuate comma [t <+> nm
                                                           | t  <- map doc ts
                                                           | nm <- star_name_ds
                                                      ]
            num_main_args           = int $ length args + 1
            arg_parses_d            = vcat $ zipWith gen_arg_parser args [1..]
            main_param_var_decls_d  = vcat $ punctuate semi $ map doc args
            main_param_var_addrs_d  = sep $ punctuate comma $
                                      map (\nm -> char '&' <> text nm) names
            main_params_d           = sep $ punctuate comma $ map text names
        in  printf templ (render rettype_d)
                         (render sfdlmain_arg_decls_d)
                         (render parse_outargs_decl_d)
                         (render num_main_args)
                         (render arg_parses_d)
                         (render main_param_var_decls_d)
                         (render main_param_var_addrs_d)
                         (render main_params_d)


gen_arg_parser (name, Im.IntT _) arg_idx =
    hcat [char '*', text name,
          equals,
          text "atoi", parens (text "argv" <> brackets (int arg_idx)),
          semi]
