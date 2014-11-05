open Definitions

let test v = assert v

let () = print_endline "******* Testing well-formed classes"
let () = List.iter (fun x -> test (class_ok x)) Classes.wellformed_classes
let () = List.iter (fun x -> test (class_ok x)) Booleans.wellformed
let () = List.iter (fun x -> test (class_ok x)) Life_forms.c_wellformed
let () = List.iter (fun x -> test (class_ok x)) Simple_shapes.c_wellformed

let () = print_endline "******* Testing well-formed interfaces"
let () = List.iter (fun x -> test (interface_ok x)) Life_forms.i_wellformed

let () = print_endline "******* Testing well-formed shapes"
let () = List.iter (fun x -> test (shape_ok x)) Simple_shapes.s_wellformed

let () = print_endline "******* Testing  mal-formed classes"
let () = List.iter (fun x -> test (not (class_ok x))) Classes.malformed_classes
let () = List.iter (fun x -> test (not (class_ok x))) Booleans.malformed
let () = List.iter (fun x -> test (not (class_ok x))) Life_forms.c_malformed
let () = List.iter (fun x -> test (not (class_ok x))) Simple_shapes.c_malformed

(* let () = print_endline "---ALL (wellformed) TESTS PASS---" *)
