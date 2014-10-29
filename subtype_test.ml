open Definitions

let test x = assert x


let test_bot_bot = test (subtype Bot Bot)
let test_bot_top = test (subtype Bot Top)
let test_top_top = test (subtype Top Top)
let test_top_bot = test (not (subtype Top Bot))

let () = print_endline "---ALL (subtype) TESTS PASS---"
