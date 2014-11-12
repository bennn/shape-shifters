open Definitions
open Well_formed
open Test_utils
open Extension_methods

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
  let cc = ClassContext.of_list [I Number.i_integer; I Number.i_number;
                                 I Number.i_long; I Iterator.i_iterator;
                                 I Boolean.i_boolean; I Iterable.i_iterable;
                                 I Indexed.i_indexed; I My_list.i_my_list;
                                 C class_a; C class_b;
                                 C My_array.c_my_array] in
  let arrActx =
    Context.init cc []
                 (TypeContext.of_list  [(Iterator.param,  Bot, TVar My_array.param);
                                        (Iterable.param,  Bot, TVar My_array.param);
                                        (Indexed.param,   Bot, TVar My_array.param);
                                        (Container.param, TVar My_array.param, Bot);
                                        (My_list.param,   Bot, TVar My_array.param);
                                        (My_array.param,  Bot, Instance("A", []))])
  in
  let listBctx =
    Context.init cc []
                 (TypeContext.of_list  [(Iterator.param,  Bot, TVar My_list.param);
                                        (Iterable.param,  Bot, TVar My_list.param);
                                        (Indexed.param,   Bot, TVar My_list.param);
                                        (Container.param, TVar My_list.param, Bot);
                                        (My_list.param,  Bot, Instance("B", []))])
  in
  let s_opt = call_extension (arrActx, C My_array.c_my_array)
                         (listBctx, I My_list.i_my_list)
                         "plus"
  in
  begin match s_opt with
  | None -> Format.printf "nothing\n"
  | Some s -> Format.printf "GOT IT '%s'\n" s
  end


let () = (* main *)
  let () = test_ab_well_formed () in
  let () = test_add_array_list () in
  ()
