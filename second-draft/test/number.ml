open Definitions
open Test_utils

let m_sig_add = Method( Instance("Number", empty_varmap)
                      , "add"
                      , [Arg(Instance("Number", empty_varmap), "that")])
let m_sig_sub = Method( Instance("Number", empty_varmap)
                      , "sub"
                      , [Arg(Instance("Number", empty_varmap), "that")])
let i_number = Interface ("Number"
                         , []
                         , []
                         , []
                         , [ (NoCond, m_sig_add)
                           ; (NoCond, m_sig_sub)
                           ])

let c_integer = Class ("Integer"
                      , []
                      , []
                      , [(NoCond, i_number)]
                      , []
                      , [ (NoCond, (m_sig_add, Null))
                        ; (NoCond, (m_sig_sub, Null))])

let class_c =
  StringMap.add (name_of_class_t c_integer) (C c_integer)
  (StringMap.add (name_of_inter_t i_number) (I i_number)
    StringMap.empty)
let shape_c = StringMap.empty
let var_c = empty_varmap
let ctx = context_init class_c shape_c var_c

let shapes = [
]
let interfaces = [
]
let classes = [
  c_integer
]
