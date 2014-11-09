open Definitions
open Well_formed
open Test_utils

let s_serializable =
  Shape ( "Serializable"
            , []
            , []
            )

let () =
  let ctx =
    let cc = StringMap.empty
    and sc = StringMap.empty
    and vm = empty_varmap in
    context_init cc sc vm
  in
  let () = typecheck (shape_ok ctx s_serializable) in
  let () = Format.printf "%s\n" (Pretty_print.pshape_t ctx s_serializable) in
  ()
