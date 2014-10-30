open Definitions
open Classes

let test v = assert v

let () = print_endline "******* Testing classes *******"
let () = List.iter (fun x -> test (class_ok x)) all_classes


let () = print_endline "---ALL (classok) TESTS PASS---"
