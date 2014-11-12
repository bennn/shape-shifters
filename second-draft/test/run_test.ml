open Test_utils

(* The final one prints, the first ones are libraries *)
let () =
  (* let () = load_class True.c_true in *)
  (* let () = load_class Integer.c_zero in *)
  let () = load_class Dummy_clone.c_black_box in
  (* let () = load_class Indexed.i_indexed in *)
  Format.printf "/* success! */\n"
