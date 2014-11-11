open Definitions
open Well_formed
open Test_utils

let i_number =
  Interface ( "Number"
            , [] (* type parameters *)
            , [] (* extends interfaces *)
            , [ (NoCond, Comparable.s_comparable)
              ; (NoCond, Addable.s_addable)]
            , [ (NoCond, Method( Instance("Number", [])
                               , "max3"
                               , [ Arg(Instance("Number", []), "num1")
                                 ; Arg(Instance("Number", []), "num2")]))
              ; (NoCond, Method( Instance("Integer", [])
                               , "intValue"
                               , []))
              ; (NoCond, Method( Instance("Long", [])
                               , "longValue"
                               , []))
              ; (NoCond, Method( Instance("Boolean", [])
                               , "isZero"
                               , []))
            ])

let i_integer =
  Interface ( "Integer"
            , []
            , [ (NoCond, i_number)]
            , [ (NoCond, Comparable.s_comparable)
              ; (NoCond, Addable.s_addable)]
            , [])

let i_long =
  Interface ( "Long"
            , []
            , [ (NoCond, i_number)]
            , [ (NoCond, Comparable.s_comparable)
              ; (NoCond, Addable.s_addable)]
            , [])

let () =
  let ctx =
    let cc = ClassContext.of_list [I Boolean.i_boolean; I i_number
                                   ; I i_integer; I i_long] in
    Context.init cc [] []
  in
  let () = typecheck ctx (I i_number) in
  let () = Format.printf "%s\n" (Pretty_print.string_of_sig_t ctx (I i_number)) in
  let () = typecheck ctx (I i_integer) in
  let () = Format.printf "%s\n" (Pretty_print.string_of_sig_t ctx (I i_integer)) in
  let () = typecheck ctx (I i_long) in
  let () = Format.printf "%s\n" (Pretty_print.string_of_sig_t ctx (I i_long)) in
  ()
