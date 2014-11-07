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

let i_iterator_param = "Iterator_E"
let i_iterator = Interface ( "Iterator"
                           , [i_iterator_param]
                           , []
                           , []
                           , [(NoCond, Method( Bot (* uses same param *)
                                             , "getNext"
                                             , []))
                             ;(NoCond, Method( TVar i_iterator_param
                                             , "getCurrent"
                                             , []))
                             ;(NoCond, Method( Instance("Boolean", empty_varmap)
                                             , "isFinished"
                                             , []))
                           ])

let i_iterable_param = "Iterable_E"
let i_iterable = Interface ( "Iterable"
                           , [i_iterable_param]
                           , []
                           , []
                           , [(NoCond, Method( Instance("Iterator", empty_varmap)
                                             , "getIterator"
                                             , []))
                             ;(NoCond, Method( Instance("Integer", empty_varmap)
                                             , "length"
                                             , []))])

let i_indexed_param = "Indexed_E"
let i_indexed = Interface ( "Indexed"
                          , [i_indexed_param]
                          , [(NoCond, i_iterable)]
                          , []
                          , [(NoCond, Method( TVar i_indexed_param
                                            , "get"
                                            , [ Arg( Instance("Integer", empty_varmap), "index")]))])

let class_c =
  StringMap.add (name_of_inter_t i_iterator) (I i_iterator)
  (StringMap.add (name_of_inter_t i_container) (I i_container)
    (merge_disjoint Number.class_c Boolean.class_c))

let ctx0 = context_init class_c
                        StringMap.empty
                        empty_varmap

let inter_varmap =
  let vm1 = varmap_addvar empty_varmap i_container_param (Top, Top) in
  let vm2 = varmap_addvar vm1          i_iterator_param  (Top, Top) in
  let vm3 = varmap_addvar vm2          i_iterable_param  (Top, Top) in
  let vm4 = varmap_addvar vm3          i_indexed_param   (Top, Top) in
  vm4
let inter_ctx =
  context_init class_c
               StringMap.empty
               inter_varmap


let shapes = [
  s_equatable
; s_cloneable
; s_comparable
; s_hashable
; s_addable
]

let interfaces = [
  i_container
; i_iterator
; i_iterable
; i_indexed
]

(* let classes = [ *)
(* ] *)
