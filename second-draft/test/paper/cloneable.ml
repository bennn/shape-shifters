open Definitions
open Well_formed
open Test_utils

let s_cloneable =
  Shape ( "Cloneable"
        , []
        , [(NoCond, Method( TVar "THIS"
                          , "clone"
                          , []))
        ])

let () =
  let ctx = Context.init [] [] [] in
  let () = assert_true (shape_ok ctx s_cloneable) in
  let () = Format.printf "%s\n" (Pretty_print.string_of_shape_t ctx s_cloneable) in
  ()
