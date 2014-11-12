open Definitions
open Well_formed
open Test_utils

(* Shape Shifter: testing super T in covariant positions *)

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
  let () = typecheck ctx (C c_black_box) in
  (* Assert methods *)
  let () = check_method_names ctx ~expected:["doesNothing"]
                              ~observed:(Instance("BlackBox", [])) in
  ()

(* Shape shifter: just allocate space for the new thing, don't actually clone it *)
let w_dummy_clone =
  Shifter ( "DummyClone"
          , [] (* type parameters *)
          , [Cloneable.s_cloneable] (* shapes it links to *)
          )

let () = (* Make Iterable<BlackBox> with and without RefEqual *)
  let ctx =
    let cc = [C c_black_box] in
    let sc = [("BlackBox", w_dummy_clone)] in
    let tc = [("THIS", Bot, Instance("BlackBox", []))] in
    Context.init cc sc tc
  in
  let () = check_expr ctx ~expected:(Instance("BlackBox", []))
                      ~observed:(Call(("BlackBox", []), "clone", []))
  in
  let () = Format.printf "%s\n" (Pretty_print.string_of_shifter_t ctx w_dummy_clone) in
  ()
