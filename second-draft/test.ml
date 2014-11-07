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
  let () = List.iter (fun x -> test (interface_ok Mf_boolean.ctx x)) Boolean.interfaces in
  let () = List.iter (fun x -> test (class_ok Mf_boolean.ctx x))     Boolean.classes in
  ()

let test_mf_boolean () =
  let () = print_hdr "testing malformed booleans" in
  let () = List.iter (fun x -> test (not (interface_ok Mf_boolean.ctx x))) Mf_boolean.interfaces in
  let () = List.iter (fun x -> test (not (class_ok Mf_boolean.ctx x)))     Mf_boolean.classes in
  ()

let test_sample1 () =
  let () = print_hdr "testing Sample1" in
  let () = List.iter (fun x -> test (shape_ok Sample1.ctx x))     Sample1.shapes in
  let () = List.iter (fun x -> test (interface_ok Sample1.ctx x)) Sample1.interfaces in
  let () = List.iter (fun x -> test (class_ok Sample1.ctx x))     Sample1.classes in
  ()

(*** RUN TESTS ***)
let () =
  begin
    (* test_object (); *)
    (* test_boolean (); *)
    (* test_mf_boolean (); *)
    test_sample1 ();
    Format.printf "--- ALL TESTS PASS ---"
  end
