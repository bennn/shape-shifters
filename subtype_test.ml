open Definitions
open Classes

let test x = assert x

let () = print_endline "******* Testing the basics *******"
let test_bot_bot = test (subtype Bot Bot)
let test_bot_top = test (subtype Bot Top)
let test_top_top = test (subtype Top Top)
let test_top_bot = test (not (subtype Top Bot))

let () = print_endline "******* Testing un-parameterized classes *******"
let test_num_num = test (subtype c_number c_number)
let test_int_num = test (subtype c_integer c_number)
let test_num_int = test (not (subtype c_number c_integer))
let test_int_flt = test (not (subtype c_integer c_float))
let test_flt_int = test (not (subtype c_float c_integer))
let test_str_num = test (not (subtype c_string c_number))
let test_num_str = test (not (subtype c_number c_string))
let test_a_e     = test (not (subtype c_a c_e))
let test_e_a     = test (subtype c_e c_a)
let test_b_a     = test (subtype c_b c_a)
let test_d_c     = test (subtype c_d c_c)
let test_c_d     = test (not (subtype c_c c_d))

let () = print_endline "******* Testing parameterized classes *******"
let test_strlist_strlist = test (subtype c_string_list c_string_list)
let test_intlist_strlist = test (not (subtype c_string_list c_integer_list))
let test_numlist_strlist = test (not (subtype c_string_list c_integer_list))
let test_strlist_toplist = test (subtype c_string_list c_top_list)
let test_toplist_strlist = test (not (subtype c_top_list c_string_list))
let test_botlist_strlist = test (subtype c_bot_list c_string_list)
let test_strlist_botlist = test (not (subtype c_string_list c_bot_list))
let test_intlist_numlist = test (subtype c_integer_list c_number_list)
let test_numlist_intlist = test (not (subtype c_number_list c_integer_list))
let test_toplist_top     = test (subtype c_top_list Top)
let test_botlist_top     = test (subtype c_bot_list Top)

let test_intarray_numarray = test (not (subtype c_integer_array c_number_array))
let test_numarray_intarray = test (not (subtype c_number_array c_integer_array))
let test_intarray_intlist  = test (subtype c_integer_array c_integer_list)
let test_intlist_intarray  = test (not (subtype c_integer_list c_integer_array))
let test_intarray_numlist  = test (subtype c_integer_array c_number_list)
let test_intarray_toplist  = test (subtype c_integer_array c_top_list)
let test_intarray_top      = test (subtype c_integer_array Top)

let () = print_endline "---ALL (subtype) TESTS PASS---"
