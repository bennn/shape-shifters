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
                                 C class_a; C class_b; I Container.i_container;
                                 C My_array.c_my_array] in
  let arr_tc = TypeContext.of_list [(Iterator.param,  Bot, TVar My_array.param);
                                        (Iterable.param,  Bot, TVar My_array.param);
                                        (Indexed.param,   Bot, TVar My_array.param);
                                        (Container.param, TVar My_array.param, Bot);
                                        (My_list.param,   Bot, TVar My_array.param)]
  and list_tc = TypeContext.of_list [(Iterator.param,  Bot, TVar My_list.param);
                                        (Iterable.param,  Bot, TVar My_list.param);
                                        (Indexed.param,   Bot, TVar My_list.param);
                                        (Container.param, TVar My_list.param, Bot)]
  in
  let () =
    let arr_ctx = Context.add_var (Context.init cc [] arr_tc) My_array.param (Instance("A", []), Instance("A", [])) in
    let list_ctx = Context.add_var (Context.init cc [] list_tc) My_list.param (Bot, Instance("B", [])) in
    let s_opt = call_extension (arr_ctx, C My_array.c_my_array)
                               (list_ctx, I My_list.i_my_list)
                               "plus"
    in begin match s_opt with
       | None -> failwith "extension method MyArray<A>:(MyList<B>) FAILED"
       | Some tt -> let ctx' = Context.init cc [] list_tc in
          let () = assert_true (Subtype.subtype ctx' (Instance("MyList", [(My_list.param, Bot, Instance("B", []))])) tt) in
          let () = assert_true (Subtype.subtype ctx' tt (Instance("MyList", [(My_list.param, Bot, Instance("B", []))])) ) in
          let () = Format.printf "/* MyArray<A>:add(MyList<B>) = %s */\n" (Pretty_print.string_of_type_t_shallow ctx' tt) in
          ()
       end
  in
  (* (\* MyList<Int>:plus(MyArray<Long>) *\) *)
  let () =
    let arr_ctx = Context.add_var (Context.init cc [] arr_tc) My_array.param (Instance("A", []), Instance("Long", [])) in
    let list_ctx = Context.add_var (Context.init cc [] list_tc) My_list.param (Bot, Instance("Integer", [])) in
    let s_opt = call_extension (list_ctx, I My_list.i_my_list)
                               (arr_ctx, C My_array.c_my_array)
                               "plus"
    in begin match s_opt with
       | None -> failwith "extension method MyList<Integer>:plus(MyArray<Long>) FAILED"
       | Some tt -> let ctx' = Context.init cc [] list_tc in
          let () = assert_true (Subtype.subtype ctx' (Instance("MyList", [(My_list.param, Bot, Instance("Number", []))])) tt) in
          let () = assert_true (Subtype.subtype ctx' tt (Instance("MyList", [(My_list.param, Bot, Instance("Number", []))])) ) in
          let () = assert_false (Subtype.subtype ctx' tt (Instance("MyList", [(My_list.param, Bot, Instance("Integer", []))]))) in
          let () = assert_false (Subtype.subtype ctx' tt (Instance("MyList", [(My_list.param, Bot, Instance("Long", []))]))) in
          let () = Format.printf "/* MyList<Int>:plus(MyArray<Long>) = %s */\n" (Pretty_print.string_of_type_t_shallow ctx' tt) in
          ()
       end
  in
  ()

let () = (* main *)
  let () = test_ab_well_formed () in
  let () = test_add_array_list () in
  ()
