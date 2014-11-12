open Definitions
open Well_formed
open Test_utils

(* Shape Shifter: reference equality *)

(* Opaque class, not equatable by default *)
let c_black_box =
  Class ( "BlackBox"
        , [] (* type parameters *)
        , [] (* extends ... *)
        , [] (* implements ... *)
        , [] (* satisfies .. *)
        , [(NoCond, (Method( Bot
                          , "doesNothing"
                          , [])
                    , Return Null))
        ])

let () = (* quick test, assert that black box instances do not have a .equals *)
  (* Check well-formed *)
  let ctx = Context.init [C c_black_box] [] [] in
  let () = typecheck ctx (C c_black_box) [] in
  (* Assert methods *)
  let () = check_method_names ctx ~expected:["doesNothing"]
                              ~observed:(Instance("BlackBox", [])) in
  ()

(* Shape shifter *)
let w_ref_equal =
  Shifter ( "RefEqual"
          , [] (* type parameters *)
          , [Equatable.s_equatable] (* shapes it links to *)
          )

let () = (* Make Iterable<BlackBox> with and without RefEqual *)
  let ctx_without, ctx_with =
    let cc = [C c_black_box; I Iterable.i_iterable; I Boolean.i_boolean] in
    let sc = [("BlackBox", w_ref_equal)] in
    let tc = [(Iterable.param, Bot, Instance("BlackBox", []))] in
    ( Context.init cc [] tc
    , Context.init cc sc tc)
  in
  (* 'contains' present / absent *)
  let () = check_method_names ctx_without ~expected:["getIterator"; "getLength"]
                              ~observed:(Instance("Iterable", [])) in
  let () = check_method_names ctx_with ~expected:["getIterator"; "getLength"; "contains"]
                              ~observed:(Instance("Iterable", [])) in
  (* Try a call *)
  let () = check_expr ctx_with ~expected:(Instance("Boolean", []))
                      ~observed:(Call(("Iterable", []), "contains", [("BlackBox", [])]))
  in
  let empty_ctx = Context.init [] [] [] in
  let () = Format.printf "%s\n" (Pretty_print.string_of_shifter_t empty_ctx w_ref_equal) in
  ()
