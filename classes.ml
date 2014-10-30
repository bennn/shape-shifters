open Definitions

let c_number = Class { cls = { name = "Number"
                           ; param = "Number_Param"
                           ; extends = Top
                           ; fields = []
                           ; methods = []
                           }
                   ; tau_i = Bot
                   ; tau_o = Top
                   }

let c_integer = Class { cls = { name = "Integer"
                           ; param = "Integer_Param"
                           ; extends = c_number
                           ; fields = []
                           ; methods = []
                           }
                   ; tau_i = Bot
                   ; tau_o = Top
                   }

let c_float   = Class { cls = { name = "Float"
                           ; param = "Float_Param"
                           ; extends = c_number
                           ; fields = []
                           ; methods = []
                           }
                   ; tau_i = Bot
                   ; tau_o = Top
                   }

let c_string  = Class { cls = { name = "String"
                            ; param = "String_Param"
                            ; extends = Top
                            ; fields = []
                            ; methods = []
                            }
                      ; tau_i = Bot
                      ; tau_o = Top
                      }

(* a few classes in linear order, to see if subtr is transitive *)
let c_a = Class { cls = { name = "A"; param = "A_PARAM"
                          ; extends = Top; fields = []; methods = []}
                    ; tau_i = Bot; tau_o = Top}
let c_b = Class { cls = { name = "B"; param = "B_PARAM"
                          ; extends = c_a; fields = []; methods = []}
                    ; tau_i = Bot; tau_o = Top}
let c_c = Class { cls = { name = "C"; param = "C_PARAM"
                          ; extends = c_b; fields = []; methods = []}
                    ; tau_i = Bot; tau_o = Top}
let c_d = Class { cls = { name = "D"; param = "D_PARAM"
                          ; extends = c_c; fields = []; methods = []}
                    ; tau_i = Bot; tau_o = Top}
let c_e = Class { cls = { name = "E"; param = "E_PARAM"
                          ; extends = c_d; fields = []; methods = []}
                    ; tau_i = Bot; tau_o = Top}
let c_string_list = Class { cls = { name = "List"
                ; param = "List_STRING_PARAM"
                ; extends = Top
                ; fields = []
                ; methods = []
                }
                    ; tau_i = Bot
                    ; tau_o = c_string
                    }

let c_number_list = Class { cls = { name = "List"
                ; param = "List_NUMBER_PARAM"
                ; extends = Top
                ; fields = []
                ; methods = []
                }
                    ; tau_i = Bot
                    ; tau_o = c_number
                    }

let c_integer_list = Class { cls = { name = "List"
                ; param = "List_INTEGER_PARAM"
                ; extends = Top
                ; fields = []
                ; methods = []
                }
                    ; tau_i = Bot
                    ; tau_o = c_integer
                    }

let c_top_list     = Class {cls = { name = "List"
                ; param = "List_TOP_PARAM"
                ; extends = Top
                ; fields = []
                ; methods = []
                }
                         ; tau_i = Bot
                         ; tau_o = Top
                         }
let c_bot_list   = Class {cls = { name = "List"
                ; param = "List_BOT_PARAM"
                ; extends = Top
                ; fields = []
                ; methods = []
                }
                         ; tau_i = Top
                         ; tau_o = Bot
                         }
let c_number_array = Class { cls = { name = "Array"
                                   ; param = "ARRAY_NUM_PARAM"
                                   ; extends = c_number_list
                                   ; fields = []
                                   ; methods = []
                                   }
                           ; tau_i = c_number
                           ; tau_o = c_number
                           }
let c_integer_array = Class { cls = { name = "Array"
                                   ; param = "ARRAY_INTEGER_PARAM"
                                   ; extends = c_integer_list
                                   ; fields = []
                                   ; methods = []
                                   }
                            ; tau_i = c_integer
                            ; tau_o = c_integer
                            }

let all_classes = [
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
]
