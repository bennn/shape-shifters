open Definitions
open Well_formed
open Test_utils

let i_boolean =
  Interface ("Boolean", [], [], []
             , [ (NoCond, Method( Instance("Boolean", [])
                                , "and"
                                , [Arg(Instance("Boolean", []), "that")]))
               ; (NoCond, Method( Instance("Boolean", [])
                                , "not"
                                , []))])

let () =
  let ctx =
    let cc = ClassContext.of_list [I i_boolean] in
    Context.init cc [] []
  in
  let () = typecheck ctx (I i_boolean) in
  let () = Format.printf "%s\n" (Pretty_print.string_of_sig_t ctx (I i_boolean)) in
  ()
