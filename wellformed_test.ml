open Definitions
open Classes

let test v = assert v

let () = print_endline "******* Testing well-formed *******"
let () = List.iter (fun x -> test (class_ok x)) wellformed_classes

let () = print_endline "******* Testing  mal-formed *******"
let () = List.iter (fun x -> test (class_ok x)) malformed_classes

let () = print_endline "---ALL (wellformed) TESTS PASS---"
