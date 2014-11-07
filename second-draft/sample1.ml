open Definitions
open Test_utils

let s_equatable = Shape ( "Equatable"
                        , []
                        , [(NoCond, Method( Instance("Boolean", empty_varmap)
                                          , "equals"
                                          , [Arg(TVar "THIS", "that")]))])

let s_cloneable = Shape ( "Cloneable"
                        , []
                        , [(NoCond, Method( TVar "THIS"
                                          , "clone"
                                          , []))])

let s_comparable = Shape ( "Comparable"
                         , [(NoCond, s_equatable)]
                         , [(NoCond, Method( Instance("Boolean", empty_varmap)
                                           , "leq"
                                           , [Arg(TVar "THIS", "that")]))])

let s_hashable = Shape ( "Hashable"
                       , [(NoCond, s_equatable)]
                       , [(NoCond, Method( Instance("Integer", empty_varmap)
                                         , "hash"
                                         , []))])

let s_addable = Shape ( "Addable"
                      , []
                      , [(NoCond, Method( TVar "THIS"
                                        , "plus"
                                        , [Arg (TVar "THIS", "that")]))])

let i_container_param = "Container_E"
let i_container = Interface ( "Container"
                            , [i_container_param]
                            , []
                            , []
                            , [(NoCond, Method( Instance("Boolean", empty_varmap)
                                              , "contains"
                                              , [Arg (TVar i_container_param, "elem")]))])

let class_c =
  StringMap.add (name_of_inter_t i_container) (I i_container)
    (merge_disjoint Number.class_c Boolean.class_c)

let ctx0 = context_init class_c
                        StringMap.empty
                        empty_varmap

let shapes = [
  s_equatable
; s_cloneable
; s_comparable
; s_hashable
; s_addable
]

(* let interfaces = [ *)
(*   i_container *)
(* ] *)

(* let classes = [ *)
(* ] *)
