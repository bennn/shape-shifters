open Definitions
open Test_utils

let m_sig_and = Method( Instance("Boolean", empty_c)
                      , "and"
                      , [Arg(Instance("Boolean", empty_c), "that")])
let m_sig_not = Method( Instance("Boolean", empty_c)
                      , "not"
                      , [])

let i_boolean = Interface ("Boolean"
                          , []
                          , []
                          , []
                          , [ (NoCond, m_sig_and)
                            ; (NoCond, m_sig_not)])

let c_true = Class ( "True"
                   , []
                   , []
                   , [(NoCond, i_boolean)]
                   , []
                   , [ (NoCond, (m_sig_and, Null))
                     ; (NoCond, (m_sig_not, Null))])

let c_false = Class ( "False"
                    , []
                    , []
                    , [(NoCond, i_boolean)]
                    , []
                   , [ (NoCond, (m_sig_and, Null))
                     ; (NoCond, (m_sig_not, Null))])

(* return a subtype of the declared *)
let c_true2 = Class ( "True2"
                   , []
                   , []
                   , [(NoCond, i_boolean)]
                   , []
                   , [ (NoCond, (Method( Bot
                                       , "and"
                                       , [Arg(Instance("Boolean", empty_c), "that")])
                                , Null))
                     ; (NoCond, (m_sig_not, Null))])

let c_true3 = Class ( "True3"
                    , []
                    , [(NoCond, c_true)]
                    , []
                    , []
                    , [] )

let class_c =
  StringMap.add (name_of_inter_t i_boolean) (I i_boolean)
    (StringMap.add (name_of_class_t c_true) (C c_true)
      (StringMap.add (name_of_class_t c_false) (C c_false)
        StringMap.empty))
let shape_c = StringMap.empty
let var_c   = empty_c

let ctx = context_init class_c shape_c var_c

let interfaces = [
  i_boolean
]

let classes = [
  c_true
; c_false
; c_true2
; c_true3
]
