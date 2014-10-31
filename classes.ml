open Definitions

let c_number = Class { cls = { name = "Number"
                           ; param = "Number_Param"
                           ; extends = Top
                           ; implements = None
                           ; fields = []
                           ; methods = []
                           }
                   ; tau_i = Bot
                   ; tau_o = Top
                   }

let c_integer = Class { cls = { name = "Integer"
                           ; param = "Integer_Param"
                           ; extends = c_number
                           ; implements = None
                           ; fields = []
                           ; methods = []
                           }
                   ; tau_i = Bot
                   ; tau_o = Top
                   }

let c_float   = Class { cls = { name = "Float"
                           ; param = "Float_Param"
                           ; extends = c_number
                           ; implements = None
                           ; fields = []
                           ; methods = []
                           }
                   ; tau_i = Bot
                   ; tau_o = Top
                   }

let c_string  = Class { cls = { name = "String"
                            ; param = "String_Param"
                            ; extends = Top
                            ; implements = None
                            ; fields = []
                            ; methods = []
                            }
                      ; tau_i = Bot
                      ; tau_o = Top
                      }

(* a few classes in linear order, to see if subtr is transitive *)
let c_a = Class { cls = { name = "A"; param = "A_PARAM"
                          ; extends = Top ; implements = None
                          ; fields = []; methods = []}
                    ; tau_i = Bot; tau_o = Top}
let c_b = Class { cls = { name = "B"; param = "B_PARAM"
                          ; extends = c_a; implements = None
                          ; fields = []; methods = []}
                    ; tau_i = Bot; tau_o = Top}
let c_c = Class { cls = { name = "C"; param = "C_PARAM"
                          ; extends = c_b; implements = None
                          ; fields = []; methods = []}
                    ; tau_i = Bot; tau_o = Top}
let c_d = Class { cls = { name = "D"; param = "D_PARAM"
                          ; extends = c_c; implements = None
                          ; fields = []; methods = []}
                    ; tau_i = Bot; tau_o = Top}
let c_e = Class { cls = { name = "E"; param = "E_PARAM"
                          ; extends = c_d; implements = None
                          ; fields = []; methods = []}
                    ; tau_i = Bot; tau_o = Top}
let c_string_list = Class { cls = { name = "List"
                                  ; param = "List_STRING_PARAM"
                                  ; extends = Top
                                  ; implements = None
                                  ; fields = []
                                  ; methods = []
                                  }
                          ; tau_i = Bot
                          ; tau_o = c_string
                          }

let c_number_list = Class { cls = { name = "List"
                ; param = "List_NUMBER_PARAM"
                ; extends = Top
                ; implements = None
                ; fields = []
                ; methods = []
                }
                    ; tau_i = Bot
                    ; tau_o = c_number
                    }

let c_integer_list = Class { cls = { name = "List"
                ; param = "List_INTEGER_PARAM"
                ; extends = Top
                ; implements = None
                ; fields = []
                ; methods = []
                }
                    ; tau_i = Bot
                    ; tau_o = c_integer
                    }

let c_top_list     = Class {cls = { name = "List"
                ; param = "List_TOP_PARAM"
                ; extends = Top
                ; implements = None
                ; fields = []
                ; methods = []
                }
                         ; tau_i = Bot
                         ; tau_o = Top
                         }
let c_bot_list   = Class {cls = { name = "List"
                ; param = "List_BOT_PARAM"
                ; extends = Top
                ; implements = None
                ; fields = []
                ; methods = []
                }
                         ; tau_i = Top
                         ; tau_o = Bot
                         }
let c_number_array = Class { cls = { name = "Array"
                                   ; param = "ARRAY_NUM_PARAM"
                                   ; extends = c_number_list
                                   ; implements = None
                                   ; fields = []
                                   ; methods = []
                                   }
                           ; tau_i = c_number
                           ; tau_o = c_number
                           }
let c_integer_array = Class { cls = { name = "Array"
                                   ; param = "ARRAY_INTEGER_PARAM"
                                   ; extends = c_integer_list
                                   ; implements = None
                                   ; fields = []
                                   ; methods = []
                                   }
                            ; tau_i = c_integer
                            ; tau_o = c_integer
                            }

let c_boolean = Class { cls = { name = "Boolean"
                                  ; param = "BOOL_PARAM"
                                  ; extends = Top
                                  ; implements = None
                                  ; fields = []
                                  ; methods = [
                                    Method(CType Top
                                          , "negate"
                                          , [])
                                  ; Method(CType Top
                                          , "and"
                                          , [Arg (CType Bot, "that")])
                                  ]}
                          ; tau_i = Bot
                          ; tau_o = Top
                          }

(* extend something that has methods *)
let c_boolean2 = Class { cls = { name = "Boolean2"
                               ; param = "BOOL2_PARAM"
                               ; extends = c_boolean
                               ; implements = None
                               ; fields = [Field (CType c_boolean, "bool2_field")]
                               ; methods = []
                               }
                       ; tau_i = Bot
                       ; tau_o = Top
                       }

(* extend something that has methods, add your own new method *)
let c_boolean3 = Class { cls = { name = "Boolean3"
                               ; param = "BOOL3_PARAM"
                               ; extends = c_boolean
                               ; implements = None
                               ; fields = []
                               ; methods = [
                                 Method(CType Bot
                                        , "or"
                                        , [Arg (CType c_boolean, "that")])
                               ]}
                       ; tau_i = Bot
                       ; tau_o = Top
                       }

let c_boolean4 = Class { cls = { name = "Boolean4"
                               ; param = "BOOL4_PARAM"
                               ; extends = c_boolean
                               ; implements = None
                               ; fields = [Field (CType c_boolean, "bool4_field")]
                               ; methods = [
                                 Method(CType c_boolean
                                       , "negate"
                                       , [])
                               ; Method(CType c_boolean
                                       , "and"
                                       , [Arg (CType c_boolean, "that")])
                               ]}
                       ; tau_i = Bot
                       ; tau_o = Top
                       }

(* malformed: cannot overwrite method with a subtype arg type *)
let c_bad_boolean5 = Class { cls = { name = "Boolean5_bad"
                               ; param = "BOOL5_PARAM"
                               ; extends = c_boolean4
                               ; implements = None
                               ; fields = []
                               ; methods = [
                                 Method(CType c_boolean
                                       , "and"
                                       , [Arg (CType Bot, "that")])
                               ]}
                       ; tau_i = Bot
                       ; tau_o = Top
                       }
(* malformed: *)
let c_bad_boolean6 = Class { cls = { name = "Boolean6_bad"
                                   ; param = "BOOL6_PARAM"
                                   ; extends = c_boolean4
                                   ; implements = None
                                   ; fields = []
                                   ; methods = [
                                     Method(CType c_boolean4
                                           , "and"
                                           , [Arg (CType Bot, "that")])
                                   ]}
                           ; tau_i = Bot
                           ; tau_o = Top
                           }

let wellformed_classes = [
  c_number
  ; c_integer
  ; c_float
  ; c_string
  ; c_a
  ; c_b
  ; c_c
  ; c_d
  ; c_e
  ; c_string_list
  ; c_number_list
  ; c_integer_list
  ; c_top_list
  ; c_bot_list
  ; c_number_array
  ; c_integer_array
  ; c_boolean
  ; c_boolean2
  ; c_boolean3
  ; c_boolean4
]

let malformed_classes = [
  c_bad_boolean5
  ; c_bad_boolean6
]
