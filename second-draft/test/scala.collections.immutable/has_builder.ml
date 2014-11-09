open Definitions
open Test_utils
open Well_formed

(* aka Cloneable *)
let s_has_builder =
  Shape ( "HasBuilder"
        , []
        , [(NoCond, Method( TVar("THIS")
                          , "newBuilder"
                          , []))]
        )

let () =
  let ctx = context_init StringMap.empty StringMap.empty empty_varmap in
  let () = typecheck (shape_ok ctx s_has_builder) in
  let () = Format.printf "%s\n" (Pretty_print.pshape_t ctx s_has_builder) in
  ()
