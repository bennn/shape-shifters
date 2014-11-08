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

let i_list_param = "List_E"
let i_list_vm =
  let vm1 = varmap_addvar empty_varmap i_indexed_param   (Bot, TVar i_list_param) in
  let vm2 = varmap_addvar vm1          i_container_param (TVar i_list_param, Top) in
  vm2
let i_list =
  (* Varmap for List<super E> *)
  let super_vm = varmap_addvar i_list_vm i_list_param (Bot, Super(i_list_param)) in
  let tSuper = Instance("List", super_vm) in
  Interface ( "List"
            , [i_list_param]
            , [ (NoCond, i_indexed)
              ; (Sat(i_list_param, s_equatable)
                , i_container)]
            , [ (Sat(i_list_param, s_equatable)
                , s_equatable)
              ; (Sat(i_list_param, s_cloneable)
                , s_cloneable)
              ; (Sat(i_list_param, s_addable)
                , s_addable)]
            , [( SuperSat(i_list_param, s_equatable)
               , Method( Instance("Boolean", empty_varmap)
                       , "equals"
                       , [Arg( tSuper, "that")]))
              ; ( SuperSat(i_list_param, s_cloneable)
                , Method( tSuper
                        , "clone"
                        , []))
              ; ( SuperSat(i_list_param, s_addable)
                , Method( tSuper
                        , "plus"
                        , [Arg( tSuper, "that")]))
            ])

let i_set_param = "Set_E"
let i_set_vm =
  let vm1 = varmap_addvar empty_varmap i_indexed_param   (Bot, TVar i_set_param) in
  let vm2 = varmap_addvar vm1          i_container_param (TVar i_set_param, Top) in
  vm2
let i_set =
  Interface ( "Set"
            , [i_set_param]
            , [( NoCond, i_iterable)
              ;( NoCond, i_container)]
            , [(NoCond, s_equatable)]
            , [(NoCond, Method( Instance("Boolean", empty_varmap)
                              , "equals"
                              , [Arg( Instance("Set", empty_varmap)
                                    , "that")]))]
            )

let c_array_param = "Array_E"
let c_array_mf1 = Class ( "ArrayMF1"
                    , [c_array_param]
                    , []
                    , [(NoCond, i_list)]
                    , [ (Sat(c_array_param, s_equatable), s_equatable)
                      ; (Sat(c_array_param, s_cloneable), s_cloneable)
                      ; (Sat(c_array_param, s_addable)  , s_addable)]
                    , [])
let c_array =
  let super_vm = varmap_addvar empty_varmap c_array_param (Super(i_list_param), Super(i_list_param)) in
  let tSuper = Instance("Array", super_vm) in
  Class ( "Array"
        , [c_array_param]
        , []
        , [(NoCond, i_list)]
        , [ (Sat(c_array_param, s_equatable), s_equatable)
          ; (Sat(c_array_param, s_cloneable), s_cloneable)
          ; (Sat(c_array_param, s_addable)  , s_addable)]
        , [ (NoCond
            , (Method( TVar c_array_param
                     , "get"
                     , [Arg(Instance("Integer", empty_varmap), "index")])
              , Null))
          ; ( NoCond
            , (Method( Bot
                     , "set"
                     , [ Arg(Instance("Integer", empty_varmap), "index")
                       ; Arg(TVar c_array_param               , "elem")])
              , Null))
          ; ( NoCond
            , (Method( Instance("Integer", empty_varmap)
                     , "length"
                     , [])
              , Null))
          ; ( NoCond
            , (Method( Instance("Iterator", varmap_addvar empty_varmap i_iterator_param   (Bot, TVar c_array_param))
                     , "getIterator"
                     , [])
              , Null))
          ; ( SuperSat(c_array_param, s_cloneable)
            , (Method( tSuper
                     , "clone"
                     , [])
              , Null))
          ; ( SuperSat(c_array_param, s_addable)
            , (Method( tSuper
                     , "plus"
                     , [Arg( Instance("Indexed",
                                      varmap_addvar empty_varmap i_indexed_param (Bot, TVar c_array_param))
                           , "that")])
              , Null))
        ])


let class_c =
  StringMap.add (name_of_class_t c_array) (C c_array)
  (StringMap.add (name_of_inter_t i_set) (I i_set)
  (StringMap.add (name_of_inter_t i_list) (I i_list)
  (StringMap.add (name_of_inter_t i_indexed) (I i_indexed)
  (StringMap.add (name_of_inter_t i_iterable) (I i_iterable)
  (StringMap.add (name_of_inter_t i_iterator) (I i_iterator)
  (StringMap.add (name_of_inter_t i_container) (I i_container)
    (merge_disjoint Number.class_c Boolean.class_c)))))))

let ctx0 = context_init class_c
                        StringMap.empty
                        empty_varmap

let inter_varmap =
  let vm1 = varmap_addvar empty_varmap i_container_param (Top, Top) in
  let vm2 = varmap_addvar vm1          i_iterator_param  (Top, Top) in
  let vm3 = varmap_addvar vm2          i_iterable_param  (Top, Top) in
  let vm4 = varmap_addvar vm3          i_indexed_param   (Top, Top) in
  let vm5 = varmap_addvar vm4          i_list_param      (Top, Top) in
  let vm6 = varmap_addvar vm5          i_set_param       (Top, Top) in
  vm6
let inter_ctx =
  context_init class_c
               StringMap.empty
               inter_varmap

let c_array_vm =
  let vm1 = varmap_addvar empty_varmap i_iterator_param  (Bot, TVar c_array_param) in
  let vm2 = varmap_addvar vm1          i_indexed_param   (Bot, TVar c_array_param) in
  let vm3 = varmap_addvar vm2          i_list_param      (Bot, TVar c_array_param) in
  let vm4 = varmap_addvar vm3          i_iterable_param  (Bot, TVar c_array_param) in
  vm4
let c_array_ctx =
  context_init class_c
               StringMap.empty
               (varmap_addvar c_array_vm c_array_param (Top, Top))

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
; i_list
; i_set
]
let classes = [
  c_array
]
let malformed_classes = [
  c_array_mf1
]
