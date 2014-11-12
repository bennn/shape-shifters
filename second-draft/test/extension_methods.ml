open Definitions
open Well_formed
open Test_utils

(* Testing extension method functionality *)

(* Array<A> ++ List<B>, where B is addable but A is not, should get type List<B> *)

let class_b =
  Class ( "B"
        , [] (* params *)
        , [] (* extends *)
        , [] (* implements *)
        , [(NoCond, Addable.s_addable)] (* satisfies *)
        , [(NoCond, (Method(Instance("B", [])
                           , "plus"
                           , [Arg(Instance("B", []), "that")])
                    , Return (New("B", [])) ))
          ])

let class_a =
  Class ( "A"
        , [] (* params *)
        , [(NoCond, class_b)] (* extends *)
        , [] (* implements *)
        , [] (* satisfies *)
        , [] (* methods *)
        )

let test_ab_well_formed () =
  let ctx =
    let cc = ClassContext.of_list [C class_a; C class_b] in
    Context.init cc [] []
  in
  let () = typecheck ctx (C class_a) [] in
  let () = typecheck ctx (C class_b) [] in
  ()

let test_add_array_list () =
  ()
  (* let ctx = *)
  (*   let cc = ClassContext.of_list [I Number.i_integer; I Number.i_number; *)
  (*                                  I Number.i_long; I Iterator.i_iterator; *)
  (*                                  I Boolean.i_boolean; I Iterable.i_iterable; *)
  (*                                  I Indexed.i_indexed; I My_list.i_my_list; *)
  (*                                  C My_array.c_my_array] *)
  (*   and sc = [] *)
  (*   and tc =  *)
  (*   in *)


let () = (* main *)
  let () = test_ab_well_formed () in
  let () = test_add_array_list () in
  ()
