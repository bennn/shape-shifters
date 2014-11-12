open Definitions
open Well_formed
open Test_utils

(* Shape Shifter: finite function *)

(* First, a class for functions *)
let paramIn = "A"
let paramOut = "B"
let c_function =
  Class ( "Function"
        , [paramIn; paramOut] (* type parameters *)
        , [] (* extends ... *)
        , [] (* implements ... *)
        , [] (* satisfies .. *)
        , [(NoCond, (Method( TVar paramOut
                          , "apply"
                          , [Arg(TVar paramIn, "value")])
                    , Return Null))
        ])

let () =
  (* check well-formed *)
  let ctx =
    let tc = TypeContext.of_list [(paramIn, Bot, Bot);
                                  (paramOut, Bot, Bot)]
    in
    Context.init [C c_function] [] tc
  in
  let () = typecheck ctx (C c_function) [] in
  (* Assert methods *)
  let () = check_method_names ctx ~expected:["apply"]
                              ~observed:(Instance("Function", [])) in
  ()

(* Shape shifter *)
let w_finite_function =
  Shifter ( "FiniteFunction"
          , [] (* type parameters *)
          , [Equatable.s_equatable] (* shapes it links to *)
          )

let () = (* Make Array<BlackBox> with and without RefEqual *)
  let fun_bot = Instance("Function", [(paramIn, Bot, Bot);
                                      (paramOut, Bot, Bot)])
  in
  let cc = ClassContext.of_list [I Number.i_integer; I Number.i_number;
                          I Number.i_long; I Iterator.i_iterator;
                          I Boolean.i_boolean; I Iterable.i_iterable;
                          I Indexed.i_indexed; I My_list.i_my_list;
                          C My_array.c_my_array; C c_function]
  in
  let tc = TypeContext.of_list [(Iterator.param, Bot, TVar My_array.param);
                                (Iterable.param, Bot, TVar My_array.param);
                                (Indexed.param, Bot, TVar My_array.param);
                                (My_list.param, Bot, TVar My_array.param);
                                (Container.param, TVar My_array.param, Top);
                                (My_array.param, fun_bot, fun_bot)]
  in
  let ctx_without = Context.init cc [] tc in
  let ctx_with    = Context.init cc [("Function", w_finite_function)] tc in
  (* 'contains' present / absent *)
  let () = check_method_names ctx_without ~expected:["addItem"; "get"; "getIterator"; "getLength"]
                              ~observed:(Instance("MyArray", [])) in
  let () = check_method_names ctx_with ~expected:["addItem"; "get"; "getIterator"; "getLength"; "contains"; "equals"]
                              ~observed:(Instance("MyArray", [])) in
  ()
