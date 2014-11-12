open Test_utils

let () =
  (* Shapes *)
  let () = load_class Addable.s_addable in
  let () = load_class Cloneable.s_cloneable in
  let () = load_class Comparable.s_comparable in
  let () = load_class Equatable.s_equatable in
  let () = load_class Hashable.s_hashable in
  (* Classes/Interfaces *)
  let () = load_class Boolean.i_boolean in
  let () = load_class Container.i_container in
  let () = load_class Hash_set.c_hash_set in
  let () = load_class Indexed.i_indexed in
  let () = load_class Iterable.i_iterable in
  let () = load_class Iterator.i_iterator in
  let () = load_class My_array.c_my_array in
  let () = load_class My_list.i_my_list in
  let () = load_class My_set.i_my_set in
  let () = load_class Number.i_number in
  let () = load_class Red_black_tree.c_red_black_tree in
  (* Shifters *)
  let () = load_class Ref_equal.w_ref_equal in
  let () = load_class Dummy_clone.w_dummy_clone in
  let () = load_class Finite_fun.c_function in
  (* Misc. test *)
  let () = load_class Test_boolean.c_true in
  let () = load_class Test_extensions.class_a in
  let () = load_class Test_number.c_zero in
  Format.printf "/* success! */\n"
