open Definitions
open Well_formed
open Test_utils

let s_comparable =
  Shape ( "Comparable"
        , [(NoCond, Equatable.s_equatable)]
        , [(NoCond, Method( Instance("Boolean", empty_varmap)
                          , "leq"
                          , [Arg(TVar "THIS", "that")]))])

let () =
  let ctx = Boolean.ctx in
  let () = typecheck (shape_ok ctx s_comparable) in
  let () = Format.printf "%s\n" (Pretty_print.pshape_t ctx s_comparable) in
  ()
