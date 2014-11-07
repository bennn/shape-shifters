open Definitions
open Test_utils

let s_object = Shape     ("S_Object", [], [])
let i_object = Interface ("I_Object", [], [], [], [])
let c_object = Class     ("C_Object", [], [], [], [], [])

let class_c =
  StringMap.add (name_of_inter_t i_object) (I i_object)
    (StringMap.add (name_of_class_t c_object) (C c_object)
      StringMap.empty)
let shape_c =
  StringMap.add (name_of_shape_t s_object) s_object
    StringMap.empty
let var_c   = empty_c

let ctx = context_init class_c shape_c var_c
