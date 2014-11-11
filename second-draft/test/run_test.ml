open Test_utils

let () =
  (* let () = load_class True.c_true in *)
  (* let () = load_class Integer.c_zero in *)
  let () = load_class Iterable.i_iterable in
  Format.printf "/* success! */\n"
