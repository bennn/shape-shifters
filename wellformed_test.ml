open Definitions

let test v = assert v

let () = print_endline "******* Testing well-formed *******"
let () = List.iter (fun x -> test (class_ok x)) Classes.wellformed_classes
let () = List.iter (fun x -> test (class_ok x)) Booleans.wellformed

let () = print_endline "******* Testing  mal-formed *******"
let () = List.iter (fun x -> test (not (class_ok x))) Classes.malformed_classes
let () = List.iter (fun x -> test (not (class_ok x))) Booleans.malformed

let () = print_endline "---ALL (wellformed) TESTS PASS---"
