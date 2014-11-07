open Definitions
open Test_utils

let c1 = Class ( "MF1"
                   , []
                   , []
                   , [(NoCond, Boolean.i_boolean)]
                   , []
                   , [] )

(* Less-specific return type *)
let c2 = Class ( "MF2"
                   , []
                   , []
                   , [(NoCond, Boolean.i_boolean)]
                   , []
                   , [ (NoCond, (Method( Top
                                       , "and"
                                       , [Arg(Instance("Boolean", empty_varmap), "that")])
                                , Null))
                     ; (NoCond, (Boolean.m_sig_not, Null))])

(* More specific arg type Bot *)
let c3 = Class ( "MF3"
                   , []
                   , []
                   , [(NoCond, Boolean.i_boolean)]
                   , []
                   , [ (NoCond, (Method( Bot
                                       , "and"
                                       , [Arg (Bot, "that")])
                                , Null))
                     ; (NoCond, (Boolean.m_sig_not, Null))])

(* More specific arg type True *)
let c4 = Class ( "MF4"
                   , []
                   , []
                   , [(NoCond, Boolean.i_boolean)]
                   , []
                   , [ (NoCond, (Method( Bot
                                       , "and"
                                       , [Arg (Instance(name_of_class_t Boolean.c_true, empty_varmap), "that")])
                                , Null))
                     ; (NoCond, (Boolean.m_sig_not, Null))])

let class_c =
  StringMap.add (name_of_class_t c1) (C c1)
    Boolean.class_c
let shape_c = StringMap.empty
let var_c   = empty_varmap

let ctx = context_init class_c shape_c var_c
let classes = [
  c1
; c2
; c3
; c4
]

let interfaces = [

]
