open Definitions
open Subtype
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
  let () = List.iter (fun x -> test (interface_ok Boolean.ctx x)) Boolean.interfaces in
  let () = List.iter (fun x -> test (class_ok Boolean.ctx x))     Boolean.classes in
  ()

let test_number () =
  let () = print_hdr "testing Numbers" in
  let () = List.iter (fun x -> test (shape_ok Number.ctx x))     Number.shapes in
  let () = List.iter (fun x -> test (interface_ok Number.ctx x)) Number.interfaces in
  let () = List.iter (fun x -> test (class_ok Number.ctx x))     Number.classes in
  ()

let test_mf_boolean () =
  let () = print_hdr "testing malformed booleans" in
  let () = List.iter (fun x -> test (not (interface_ok Mf_boolean.ctx x))) Mf_boolean.interfaces in
  let () = List.iter (fun x -> test (not (class_ok Mf_boolean.ctx x)))     Mf_boolean.classes in
  ()

let test_sample1 () =
  let () = print_hdr "testing Sample1" in
  let () = (* unparameterized shapes *)
    List.iter (fun x -> test (shape_ok Sample1.ctx0 x)) Sample1.shapes
  in
  let () =
    let ctx' =
      context_addvar Sample1.ctx0
                     Sample1.i_container_param
                     (Bot, Top)
    in
    let () = Format.printf "-? Container<Bot> valid " in
    test (interface_ok ctx' Sample1.i_container)
  in
  let () = (* container top <: container bot && container bot </: container top *)
    let vm1 = varmap_addvar empty_varmap
                              Sample1.i_container_param
                              (Top, Top)
    and vm2 = varmap_addvar empty_varmap
                              Sample1.i_container_param
                              (Bot, Top)
    in
    let () = Format.printf "-? Container<Top> <: Container<Bot> " in
    let () = test (subtype Sample1.ctx0 (Instance("Container", vm1)) (Instance("Container", vm2))) in
    let () = Format.printf "-? Container<Bot> <//: Container<Top> " in
    let () = test (not (subtype Sample1.ctx0 (Instance("Container", vm2)) (Instance("Container", vm1)))) in
    ()
  in
  let () = (* container boolean <: container true && container true </: container boolean *)
    let vm1 = varmap_addvar empty_varmap
                              Sample1.i_container_param
                              (Instance("Boolean", empty_varmap), Top)
    and vm2 = varmap_addvar empty_varmap
                              Sample1.i_container_param
                              (Instance("True", empty_varmap), Top)
    in
    let () = Format.printf "-? Container<Boolean> <//: Container<True> " in
    let () = test (subtype Sample1.ctx0 (Instance("Container", vm1)) (Instance("Container", vm2))) in
    let () = Format.printf "-? Container<True> <//: Container<Boolean> " in
    let () = test (not (subtype Sample1.ctx0 (Instance("Container", vm2)) (Instance("Container", vm1)))) in
    ()
  in
  ()

(*** RUN TESTS ***)
let () =
  begin
    (* test_object (); *)
    (* test_boolean (); *)
    (* test_mf_boolean (); *)
    (* test_number (); *)
    test_sample1 ();
    Format.printf "--- ALL TESTS PASS ---"
  end
