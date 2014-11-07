open Definitions
open Test_utils

let c1 = Class ( "MFTrue1"
                   , []
                   , []
                   , [(NoCond, Boolean.i_boolean)]
                   , []
                   , [ (NoCond, (Boolean.m_sig_and, Null))
                     ; (NoCond, (Boolean.m_sig_not, Null))])

let class_c =
  StringMap.add (name_of_class_t c1) (C c1)
    Boolean.class_c
let shape_c = StringMap.empty
let var_c   = empty_c

let ctx = context_init class_c shape_c var_c
let classes = [

]

let interfaces = [

]
