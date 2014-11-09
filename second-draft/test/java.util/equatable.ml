open Definitions
open Well_formed
open Test_utils

let s_equatable =
  Shape ( "Equatable"
        , []
        , [(NoCond, Method( Instance("Boolean", empty_varmap)
                          , "equals"
                          , [Arg(TVar "THIS", "that")]))])

let () =
  let ctx = Boolean.ctx in
  let () = assert (shape_ok ctx s_equatable) in
  let () = Format.printf "%s\n" (Pretty_print.pshape_t ctx s_equatable) in
  ()
