open Definitions
open Well_formed
open Test_utils


let i_boolean =
  Interface ("Boolean", [], [], []
             , [ (NoCond, Method( Instance("Boolean", empty_varmap)
                                , "and"
                                , [Arg(Instance("Boolean", empty_varmap), "that")]))
               ; (NoCond, Method( Instance("Boolean", empty_varmap)
                                , "not"
                                , []))
               ])
let ctx =
  let cc =
    StringMap.add (name_of_inter_t i_boolean) (I i_boolean)
      StringMap.empty
  in
  let sc = StringMap.empty in
  let vm   = empty_varmap in
  context_init cc sc vm

let () =
  let () = typecheck (interface_ok ctx i_boolean) in
  let () = Format.printf "%s\n" (Pretty_print.pinter_t ctx i_boolean) in
  ()
