open Definitions
open Test_utils

let s_equatable = Shape ( "Equatable"
                        , []
                        , [(NoCond, Method( Instance("Boolean", empty_c)
                                          , "equals"
                                          , [Arg(TVar "THIS", "that")]))])

let s_cloneable = Shape ( "Cloneable"
                        , []
                        , [(NoCond, Method( TVar "THIS"
                                          , "clone"
                                          , []))])

let s_comparable = Shape ( "Comparable"
                         , [(NoCond, s_equatable)]
                         , [(NoCond, Method( Instance("Boolean", empty_c)
                                           , "leq"
                                           , [Arg(TVar "THIS", "that")]))])

let s_hashable = Shape ( "Hashable"
                       , [(NoCond, s_equatable)]
                       , [(NoCond, Method( Instance("Integer", empty_c)
                                         , "hash"
                                         , []))])

let s_addable = Shape ( "Addable"
                      , []
                      , [(NoCond, Method( TVar "THIS"
                                        , "plus"
                                        , [Arg (TVar "THIS", "that")]))])

let class_c =
  merge_disjoint Number.class_c Boolean.class_c
let shape_c =
  StringMap.empty
let var_c   = empty_c

let ctx = context_init class_c shape_c var_c

let shapes = [
  s_equatable
; s_cloneable
; s_comparable
; s_hashable
; s_addable
]

let interfaces = [
]

let classes = [
]
