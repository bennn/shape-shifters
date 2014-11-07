open Definitions
open Well_formed
open Test_utils

let test_object  () =
  let () = print_hdr "testing Object" in
  let () = test (class_ok     Object.ctx Object.c_object) in
  let () = test (interface_ok Object.ctx Object.i_object) in
  let () = test (shape_ok     Object.ctx Object.s_object) in
  ()

let test_boolean () =
  let () = print_hdr "testing Booleans" in
  let () = test (interface_ok Boolean.ctx Boolean.i_boolean) in
  let () = test (class_ok     Boolean.ctx Boolean.c_true) in
  let () = test (class_ok     Boolean.ctx Boolean.c_false) in
  ()

(*** RUN TESTS ***)
let () =
  begin
    test_object ();
    test_boolean ();
    Format.printf "--- ALL TESTS PASS ---"
  end
