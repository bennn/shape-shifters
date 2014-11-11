open Definitions
open Well_formed
open Test_utils

let param = "ITERATOR_PARAM"
let i_iterator =
  Interface ( "Iterator"
            , [param]
            , [] (* extends interfaces *)
            , [] (* satisifies shapes *)
            , [ (NoCond, Method( TVar param
                               , "next"
                               , []))
              ])


let () =
  let ctx =
    let cc = ClassContext.of_list [I i_iterator] in
    let tc = TypeContext.of_list  [(param, Bot, Bot)] in
    Context.init cc [] tc
  in
  let () = typecheck ctx (I i_iterator) in
  let () = Format.printf "%s\n" (Pretty_print.string_of_sig_t ctx (I i_iterator)) in
  ()
