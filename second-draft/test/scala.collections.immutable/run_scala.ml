open Definitions
open Well_formed
open Test_utils

let () =
  let () = ignore Traversable.i_traversable in
  (* let () = ignore Iterable.i_iterable in *)
  Format.printf "/* success! */\n"
