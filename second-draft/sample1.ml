open Definitions
open Test_utils

let s_equatable = Shape ( "Equatable"
                        , []
                        , [(NoCond, Method( Instance("Boolean", empty_c)
                                          , "equals"
                                          , [Arg(TVar "THIS", "that")]))
                        ])


let class_c =
  Boolean.class_c
let shape_c =
  StringMap.empty
let var_c   = empty_c

let ctx = context_init class_c shape_c var_c

let shapes = [
  s_equatable
]

let interfaces = [
]

let classes = [
]
