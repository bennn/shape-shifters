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

let class_c =
  StringMap.add "Boolean" (I i_boolean)
    (StringMap.add "True" (C c_true)
      (StringMap.add "False" (C c_false)
        StringMap.empty))
let shape_c = StringMap.empty
let var_c   = empty_c

let ctx = context_init class_c shape_c var_c
