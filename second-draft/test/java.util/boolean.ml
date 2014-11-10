open Definitions
open Well_formed
open Test_utils

let m_sig_and = Method( Instance("Boolean", empty_varmap)
                      , "and"
                      , [Arg(Instance("Boolean", empty_varmap), "that")])
let m_sig_not = Method( Instance("Boolean", empty_varmap)
                      , "not"
                      , [])

let i_boolean = Interface ("Boolean", [], [], []
                           , [ (NoCond, m_sig_and)
                             ; (NoCond, m_sig_not)])

let c_true = Class ( "True", [], []
                     , [(NoCond, i_boolean)]
                     , []
                     , [ (NoCond, (m_sig_and, Return Null))
                       ; (NoCond, (m_sig_not, Return Null))])

let c_false = Class ( "False", [], []
                      , [(NoCond, i_boolean)]
                      , []
                      , [ (NoCond, (m_sig_and, Return Null))
                        ; (NoCond, (m_sig_not, Return Null))])

let ctx =
  let cc =
    StringMap.add (name_of_inter_t i_boolean) (I i_boolean)
                  (StringMap.add (name_of_class_t c_true) (C c_true)
                                 (StringMap.add (name_of_class_t c_false) (C c_false)
                                                StringMap.empty))
  in
  let sc = StringMap.empty in
  let vm   = empty_varmap in
  context_init cc sc vm

let () =
  let () = typecheck (interface_ok ctx i_boolean) in
  let () = Format.printf "%s\n" (Pretty_print.pinter_t ctx i_boolean) in
  let () = typecheck (class_ok ctx c_true) in
  let () = Format.printf "%s\n" (Pretty_print.pclass_t ctx c_true) in
  let () = typecheck (class_ok ctx c_false) in
  let () = Format.printf "%s\n" (Pretty_print.pclass_t ctx c_false) in
  ()
