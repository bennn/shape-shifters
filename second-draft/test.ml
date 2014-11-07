open Definitions
open Well_formed
open Test_utils

let () = print_hdr "testing Booleans"
let () =
  let obj = Instance("Boolean", empty_c) in
  assert (type_ok Boolean.ctx obj)
let () =
  let obj = Instance("True", empty_c) in
  assert (type_ok Boolean.ctx obj)
let () =
  let obj = Instance("False", empty_c) in
  assert (type_ok Boolean.ctx obj)


let () = Format.printf "--- ALL TESTS PASS ---"
