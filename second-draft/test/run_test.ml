open Test_utils

(* The final one prints, the first ones are libraries *)
let () =
  (* let () = load_class True.c_true in *)
  (* let () = load_class Integer.c_zero in *)
  (* let () = load_class Ref_equal.w_ref_equal in *)
  (* let () = load_class Dummy_clone.w_dummy_clone in *)

  (* let () = load_class My_array.c_my_array in *)
  (* let () = load_class Hash_set.c_hash_set in *)
  (* let () = load_class Red_black_tree.c_red_black_tree in *)

  let () = load_class Extension_methods.class_a in

  Format.printf "/* success! */\n"
