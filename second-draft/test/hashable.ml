open Definitions
open Well_formed
open Test_utils

let s_hashable =
  Shape ( "Hashable"
        , [(NoCond, Equatable.s_equatable)]
        , [(NoCond, Method( Instance("Integer", [])
                          , "hash"
                          , []))
        ])

let () =
  let ctx =
    let cc = ClassContext.of_list [I Number.i_number; I Number.i_integer; I Boolean.i_boolean] in
    Context.init cc [] []
  in
  let () = assert_true (shape_ok ctx s_hashable) in
  let () = Format.printf "%s\n" (Pretty_print.string_of_shape_t ctx s_hashable) in
  ()
