open Definitions
open Well_formed
open Test_utils

let param = "CONTAINER_PARAM"
let i_container =
  Interface ( "Container"
            , [param]
            , [] (* extends interfaces *)
            , [] (* satisifies shapes *)
            , [ (NoCond, Method( Instance("Boolean", [])
                               , "contains"
                               , [Arg(TVar param, "elem")] ))
              ])


let () =
  let ctx =
    let cc = ClassContext.of_list [I Boolean.i_boolean; I i_container] in
    let tc = TypeContext.of_list  [(param, Bot, Bot)] in
    Context.init cc [] tc
  in
  let () = typecheck ctx (I i_container) [] in
  let () = Format.printf "%s\n" (Pretty_print.string_of_sig_t ctx (I i_container)) in
  ()
