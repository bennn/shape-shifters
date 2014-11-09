open Definitions

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
