open Definitions
open Well_formed
open Test_utils

let s_addable =
  Shape ( "Addable"
        , []
        , [(NoCond, Method( TVar "THIS"
                          , "plus"
                          , [Arg(TVar "THIS", "that")] ))
        ])

let () =
  let ctx = Context.init [] [] [] in
  let () = assert_true (shape_ok ctx s_addable) in
  let () = Format.printf "%s\n" (Pretty_print.string_of_shape_t ctx s_addable) in
  ()
