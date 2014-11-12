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

(* Shape shifter: just allocate space for the new thing, don't actually clone it *)
let w_dummy_clone =
  Shifter ( "DummyClone"
          , [] (* type parameters *)
          , [Cloneable.s_cloneable] (* shapes it links to *)
          )

let () = (* Make Iterable<BlackBox> with and without RefEqual *)
  let empty_ctx = Context.init [C c_black_box] [] [] in
  (* Check well-formed, check methods *)
  let () = typecheck empty_ctx (C c_black_box) [] in
  let () = Format.printf "/* [shifter_dummy_clone] checking default BlackBox methods */\n" in
  let () = check_method_names empty_ctx ~expected:["doesNothing"]
                              ~observed:(Instance("BlackBox", [])) in
  (* Make sure cloning fails *)
  let () = Format.printf "/* [shifter_dummy_clone] asserting that BlackBox.clone() fails */\n" in
  let () = check_expr_false empty_ctx ~expected:(Instance("BlackBox", []))
                      ~observed:(Call(("BlackBox", []), "clone", []))
  in
  (* Add shifter, try cloning *)
  let ctx =
    let cc = [C c_black_box] in
    let sc = [("BlackBox", w_dummy_clone)] in
    let tc = [("THIS", Bot, Instance("BlackBox", []))] in
    Context.init cc sc tc
  in
  let () = Format.printf "/* [shifter_dummy_clone] asserting that <BlackBox with Dummy_Clone>.clone() succeeds */\n" in
  let () = check_expr ctx ~expected:(Instance("BlackBox", []))
                      ~observed:(Call(("BlackBox", []), "clone", []))
  in
  let () = Format.printf "/* [shifter_dummy_clone] all tests pass! */\n" in
  let () = Format.printf "%s\n" (Pretty_print.string_of_shifter_t empty_ctx w_dummy_clone) in
  ()
