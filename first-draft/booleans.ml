open Definitions

let i_boolean_abs = Interface { intr = { iname = "iBoolean_abs"
                                   ; iparam = "iBool_ABS"
                                   ; iextends = None
                                   ; isatisfies = None
                                   ; ifields = []
                                   ; imethods = []
                                   }
                          ; itau_i = Bot
                          ; itau_o = Top
                          }

let i_boolean = Interface { intr = { iname = "iBoolean"
                                   ; iparam = "iBoolean_param"
                                   ; iextends = Some i_boolean_abs
                                   ; isatisfies = None
                                   ; ifields = []
                                   ; imethods = [
                                     (Method (IType i_boolean_abs
                                            , "not"
                                            , []))
                                   ; (Method (IType i_boolean_abs
                                             , "and"
                                             , [Arg (IType i_boolean_abs, "that")]))
                                   ; (Method (IType i_boolean_abs
                                             , "or"
                                             , [Arg (IType i_boolean_abs, "that")]))]
                                   }
                          ; itau_i = Bot
                          ; itau_o = Top
                          }

let c_true_malformed1 = Class { cls = { name = "True1"
                                     ; param = "TRUE1_PARAM"
                                     ; extends = Top
                                     ; implements = Some i_boolean
                           ; satisfies = None
                                     ; fields = []
                                     ; methods = []
                                     }
                             ; tau_i = Bot
                             ; tau_o = Top
                             }
let c_true_malformed2 = Class { cls = { name = "True2"
                                     ; param = "TRUE2_PARAM"
                                     ; extends = Top
                                     ; implements = Some i_boolean
                           ; satisfies = None
                                     ; fields = []
                                     ; methods = [
                                       (Method (IType i_boolean_abs
                                               , "or"
                                               , [Arg (IType i_boolean_abs, "that")]))]
                                     }
                             ; tau_i = Bot
                             ; tau_o = Top
                             }

(* TODO error cannot check interface *)
let c_true = Class { cls = { name = "True"
                           ; param = "True_param"
                           ; extends = Top
                           ; implements = Some i_boolean
                           ; satisfies = None
                           ; fields = []
                           ; methods = [
                             (Method (IType i_boolean_abs
                                     , "not"
                                     , []))
                           ; (Method (IType i_boolean_abs
                                     , "and"
                                     , [Arg (IType i_boolean_abs, "that")]))
                           ; (Method (IType i_boolean_abs
                                     , "or"
                                     , [Arg (IType i_boolean_abs, "that")]))]
                           }
                   ; tau_i = Bot
                   ; tau_o = Top
                   }

let c_false = Class { cls = { name = "False"
                            ; param = "False_param"
                            ; extends = Top
                            ; implements = Some i_boolean
                           ; satisfies = None
                            ; fields = []
                            ; methods = [
                             (Method (IType i_boolean_abs
                                     , "not"
                                     , []))
                           ; (Method (IType i_boolean_abs
                                     , "and"
                                     , [Arg (IType i_boolean_abs, "that")]))
                           ; (Method (IType i_boolean_abs
                                     , "or"
                                     , [Arg (IType i_boolean_abs, "that")]))
                           ; (Method (CType Top
                                     , "special_false_method"
                                     , [Arg (IType i_boolean_abs, "that")
                                       ; Arg (CType Top, "and_another")]))
                            ]
                           }
                   ; tau_i = Bot
                   ; tau_o = Top
                   }

let wellformed = [
  c_true
; c_false
]

let malformed = [
  c_true_malformed1
; c_true_malformed2
]
