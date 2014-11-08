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
  let () =
    List.iter (fun x -> let () = Format.printf "-? well-formed %s" (string_of_shape_t x) in
               test (shape_ok Sample1.ctx0 x)) Sample1.shapes
  in
  let () =
    List.iter (fun x -> let () = Format.printf "-? well-formed %s" (string_of_inter_t x) in
               test (interface_ok Sample1.inter_ctx x)) Sample1.interfaces
  in
  let () =
    List.iter (fun x -> let () = Format.printf "-? well-formed %s" (string_of_class_t x) in
               test (class_ok Sample1.c_array_ctx x)) Sample1.classes
  in
  let () = print_subhdr "iContainer" in
  let () = (* container top <: container bot && container bot </: container top *)
    let vm1 = varmap_addvar empty_varmap
                              Sample1.i_container_param
                              (Top, Top)
    and vm2 = varmap_addvar empty_varmap
                              Sample1.i_container_param
                              (Bot, Top)
    and vm3 = varmap_addvar empty_varmap
                              Sample1.i_container_param
                              (Instance("Boolean", empty_varmap), Top)
    and vm4 = varmap_addvar empty_varmap
                              Sample1.i_container_param
                              (Instance("True", empty_varmap), Top)
    in
    let () = Format.printf "-? Container<Top> <: Container<Bot> " in
    let () = test (subtype Sample1.ctx0 (Instance("Container", vm1)) (Instance("Container", vm2))) in
    let () = Format.printf "-? Container<Bot> <//: Container<Top> " in
    let () = test (not (subtype Sample1.ctx0 (Instance("Container", vm2)) (Instance("Container", vm1)))) in
    let () = Format.printf "-? Container<Boolean> <: Container<True> " in
    let () = test (subtype Sample1.ctx0 (Instance("Container", vm3)) (Instance("Container", vm4))) in
    let () = Format.printf "-? Container<True> <//: Container<Boolean> " in
    let () = test (not (subtype Sample1.ctx0 (Instance("Container", vm4)) (Instance("Container", vm3)))) in
    ()
  in
  let () = print_subhdr "iIterator" in
  let () = (* interator boolean <: iterator top && iterator boolean </: iterator true *)
    let vm1 = varmap_addvar empty_varmap
                              Sample1.i_iterator_param
                              (Bot, Instance("Boolean", empty_varmap))
    and vm2 = varmap_addvar empty_varmap
                              Sample1.i_iterator_param
                              (Bot, Top)
    in
    let () = Format.printf "-? Iterator<Boolean> <: Iterator<Top> " in
    let () = test (subtype Sample1.ctx0 (Instance("Iterator", vm1)) (Instance("Iterator", vm2))) in
    let () = Format.printf "-? Iterator<Boolean> <//: Iterator<True> " in
    let () = test (not (subtype Sample1.ctx0 (Instance("Iterator", vm2)) (Instance("Iterator", vm1)))) in
    ()
  in
  let () = print_subhdr "iIterable" in
  let () = (* ITERABLE boolean <: iterable top && iterable boolean </: iterable true *)
    let vm1 = varmap_addvar empty_varmap
                              Sample1.i_iterable_param
                              (Bot, Instance("Boolean", empty_varmap))
    and vm2 = varmap_addvar empty_varmap
                              Sample1.i_iterable_param
                              (Bot, Top)
    in
    let () = Format.printf "-? Iterable<Boolean> <: Iterable<Top> " in
    let () = test (subtype Sample1.ctx0 (Instance("Iterable", vm1)) (Instance("Iterable", vm2))) in
    let () = Format.printf "-? Iterable<Boolean> <//: Iterable<True> " in
    let () = test (not (subtype Sample1.ctx0 (Instance("Iterable", vm2)) (Instance("Iterable", vm1)))) in
    ()
  in
  let () = print_subhdr "iIndexed" in
  let () = (* INDEXED boolean <: indexed top && indexed boolean </: indexed true *)
    let vm1 = varmap_addvar empty_varmap
                              Sample1.i_indexed_param
                              (Bot, Instance("Boolean", empty_varmap))
    and vm2 = varmap_addvar empty_varmap
                              Sample1.i_indexed_param
                              (Bot, Top)
    in
    let () = Format.printf "-? Indexed<Boolean> <: Indexed<Top> " in
    let () = test (subtype Sample1.ctx0 (Instance("Indexed", vm1)) (Instance("Indexed", vm2))) in
    let () = Format.printf "-? Indexed<Boolean> <//: Indexed<True> " in
    let () = test (not (subtype Sample1.ctx0 (Instance("Indexed", vm2)) (Instance("Indexed", vm1)))) in
    ()
  in
  let () = print_subhdr "iList" in
  let () = (* LIST subtyping *)
    let vm1 = varmap_addvar Sample1.i_list_vm
                            Sample1.i_list_param
                            (Bot, Instance("Boolean", empty_varmap))
    and vm2 = varmap_addvar Sample1.i_list_vm
                            Sample1.i_list_param
                            (Bot, Top)
    and vm3 = varmap_addvar Sample1.i_list_vm
                            Sample1.i_list_param
                            (Bot, Bot)
    in
    let vm4 = varmap_addvar Sample1.i_list_vm
                            Sample1.i_list_param
                            (Bot, Instance("List", vm1))
    and vm5 = varmap_addvar Sample1.i_list_vm
                            Sample1.i_list_param
                            (Bot, Instance("List", vm2))
    in
    let vm6 = varmap_addvar Sample1.i_list_vm
                            Sample1.i_list_param
                            (Bot, Instance("List", vm5))
    in
    let () = Format.printf "-? List<Boolean> <: List<Top> " in
    let () = test (subtype Sample1.ctx0 (Instance("List", vm1)) (Instance("List", vm2))) in
    let () = Format.printf "-? List<Boolean> <//: List<True> " in
    let () = test (not (subtype Sample1.ctx0 (Instance("List", vm2)) (Instance("List", vm1)))) in
    let () = Format.printf "-? List<Boolean> <: List<Boolean> " in
    let () = test (subtype Sample1.ctx0 (Instance("List", vm1)) (Instance("List", vm1))) in
    let () = Format.printf "-? List<Bot> <: List<Bot> " in
    let () = test (subtype Sample1.ctx0 (Instance("List", vm3)) (Instance("List", vm3))) in
    let () = Format.printf "-? List<List<Boolean>> <: List<List<Top>> " in
    let () = test (subtype Sample1.ctx0 (Instance("List", vm4)) (Instance("List", vm5))) in
    let () = Format.printf "-? List<List<Top>> <//: List<List<Boolean>> " in
    let () = test (not (subtype Sample1.ctx0 (Instance("List", vm5)) (Instance("List", vm4)))) in
    let () = Format.printf "-? List<List<Boolean>> <: List<Top> " in
    let () = test (subtype Sample1.ctx0 (Instance("List", vm4)) (Instance("List", vm2))) in
    let () = Format.printf "-? List<List<List<Top>>> <: Top " in
    let () = test (subtype Sample1.ctx0 (Instance("List", vm6)) Top) in
    let () = Format.printf "-? List<List<Top>> <//: List<List<List<Top>>> " in
    let () = test (not (subtype Sample1.ctx0 (Instance("List", vm5)) (Instance("List", vm6)))) in
    ()
  in
  let () = print_subhdr "iSet" in
  let () = (* SET subtyping *)
    let vm1 = varmap_addvar Sample1.i_set_vm
                            Sample1.i_set_param
                            (Instance("Boolean", empty_varmap), Instance("Boolean", empty_varmap))
    and vm2 = varmap_addvar Sample1.i_set_vm
                            Sample1.i_set_param
                            (Top, Top)
    and vm3 = varmap_addvar Sample1.i_set_vm
                            Sample1.i_set_param
                            (Bot, Bot)
    in
    let vm4 = varmap_addvar Sample1.i_set_vm
                            Sample1.i_set_param
                            (Instance("Set", vm1), Instance("Set", vm1))
    and vm5 = varmap_addvar Sample1.i_set_vm
                            Sample1.i_set_param
                            (Instance("Set", vm2), Instance("Set", vm2))
    in
    let () = Format.printf "-? Set<Boolean> <//: Set<Top> " in
    let () = test (not (subtype Sample1.ctx0 (Instance("Set", vm1)) (Instance("Set", vm2)))) in
    let () = Format.printf "-? Set<Boolean> <//: Set<True> " in
    let () = test (not (subtype Sample1.ctx0 (Instance("Set", vm2)) (Instance("Set", vm1)))) in
    let () = Format.printf "-? Set<Boolean> <: Set<Boolean> " in
    let () = test (subtype Sample1.ctx0 (Instance("Set", vm1)) (Instance("Set", vm1))) in
    let () = Format.printf "-? Set<Bot> <: Set<Bot> " in
    let () = test (subtype Sample1.ctx0 (Instance("Set", vm3)) (Instance("Set", vm3))) in
    let () = Format.printf "-? Set<Set<Boolean>> <//: Set<Set<Top>> " in
    let () = test (not (subtype Sample1.ctx0 (Instance("Set", vm4)) (Instance("Set", vm5)))) in
    let () = Format.printf "-? Set<Set<Top>> <//: Set<Set<Boolean>> " in
    let () = test (not (subtype Sample1.ctx0 (Instance("Set", vm5)) (Instance("Set", vm4)))) in
    let () = Format.printf "-? Set<Set<Boolean>> <//: Set<Top> " in
    let () = test (not (subtype Sample1.ctx0 (Instance("Set", vm4)) (Instance("Set", vm2)))) in
    let () = Format.printf "-? Set<Set<Boolean>> <: Set<Set<Boolean>> " in
    let () = test (subtype Sample1.ctx0 (Instance("Set", vm4)) (Instance("Set", vm4))) in
    ()
  in
  let () = print_subhdr "cArray" in
  let vm1 = varmap_addvar Sample1.c_array_vm
                          Sample1.c_array_param
                          (Instance("Boolean", empty_varmap), Instance("Boolean", empty_varmap))
  and vm2 = varmap_addvar Sample1.c_array_vm
                          Sample1.c_array_param
                          (Top, Top)
  and vm3 = varmap_addvar Sample1.c_array_vm
                          Sample1.c_array_param
                          (Bot, Bot)
  in
  let vm4 = varmap_addvar Sample1.c_array_vm
                          Sample1.c_array_param
                          (Instance("Array", vm1), Instance("Array", vm1))
  and vm5 = varmap_addvar Sample1.c_array_vm
                          Sample1.c_array_param
                          (Instance("Array", vm2), Instance("Array", vm2))
  in
  let () = Format.printf "-? Array<Boolean> <//: Array<Top> " in
  let () = test (not (subtype Sample1.ctx0 (Instance("Array", vm1)) (Instance("Array", vm2)))) in
  let () = Format.printf "-? Array<Boolean> <//: Array<True> " in
  let () = test (not (subtype Sample1.ctx0 (Instance("Array", vm2)) (Instance("Array", vm1)))) in
  let () = Format.printf "-? Array<Boolean> <: Array<Boolean> " in
  let () = test (subtype Sample1.ctx0 (Instance("Array", vm1)) (Instance("Array", vm1))) in
  let () = Format.printf "-? Array<Bot> <: Array<Bot> " in
  let () = test (subtype Sample1.ctx0 (Instance("Array", vm3)) (Instance("Array", vm3))) in
  let () = Format.printf "-? Array<Array<Boolean>> <//: Array<Array<Top>> " in
  let () = test (not (subtype Sample1.ctx0 (Instance("Array", vm4)) (Instance("Array", vm5)))) in
  let () = Format.printf "-? Array<Array<Top>> <//: Array<Array<Boolean>> " in
  let () = test (not (subtype Sample1.ctx0 (Instance("Array", vm5)) (Instance("Array", vm4)))) in
  let () = Format.printf "-? Array<Array<Boolean>> <//: Array<Top> " in
  let () = test (not (subtype Sample1.ctx0 (Instance("Array", vm4)) (Instance("Array", vm2)))) in
  let () = Format.printf "-? Array<Array<Boolean>> <: Array<Array<Boolean>> " in
  let () = test (subtype Sample1.ctx0 (Instance("Array", vm4)) (Instance("Array", vm4))) in
  ()

(* let test_collections () = *)
(*   let () = print_hdr "testing java.util.Collections" in *)
(*   () *)

(*** RUN TESTS ***)
let () =
  begin
    (* test_object (); *)
    (* test_boolean (); *)
    (* test_mf_boolean (); *)
    (* test_number (); *)
    test_sample1 ();
    (* test_collections (); *)
    Format.printf "--- ALL TESTS PASS ---\n"
  end
