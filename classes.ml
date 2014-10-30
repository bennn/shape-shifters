open Definitions

let c_number = Class { cls = { name = "Number"
                           ; param = TVar "X"
                           ; variance = Invariant
                           ; extends = Top
                           ; fields = []
                           ; methods = []
                           }
                   ; tau_i = Bot
                   ; tau_o = Top
                   }

let c_integer = Class { cls = { name = "Integer"
                           ; param = TVar "X"
                           ; variance = Invariant
                           ; extends = c_number
                           ; fields = []
                           ; methods = []
                           }
                   ; tau_i = Bot
                   ; tau_o = Top
                   }

let c_float   = Class { cls = { name = "Float"
                           ; param = TVar "X"
                           ; variance = Invariant
                           ; extends = c_number
                           ; fields = []
                           ; methods = []
                           }
                   ; tau_i = Bot
                   ; tau_o = Top
                   }

let c_string  = Class { cls = { name = "String"
                            ; param = TVar "X"
                            ; variance = Invariant
                            ; extends = Top
                            ; fields = []
                            ; methods = []
                            }
                      ; tau_i = Bot
                      ; tau_o = Top
                      }

(* a few classes in linear order, to see if subtr is transitive *)
let c_a = Class { cls = { name = "A"; param = TVar "X"; variance = Invariant
                          ; extends = Top; fields = []; methods = []}
                    ; tau_i = Bot; tau_o = Top}
let c_b = Class { cls = { name = "B"; param = TVar "X"; variance = Invariant
                          ; extends = c_a; fields = []; methods = []}
                    ; tau_i = Bot; tau_o = Top}
let c_c = Class { cls = { name = "C"; param = TVar "X"; variance = Invariant
                          ; extends = c_b; fields = []; methods = []}
                    ; tau_i = Bot; tau_o = Top}
let c_d = Class { cls = { name = "D"; param = TVar "X"; variance = Invariant
                          ; extends = c_c; fields = []; methods = []}
                    ; tau_i = Bot; tau_o = Top}
let c_e = Class { cls = { name = "E"; param = TVar "X"; variance = Invariant
                          ; extends = c_d; fields = []; methods = []}
                    ; tau_i = Bot; tau_o = Top}
let list_sig  = { name = "List"
                ; param = TVar "X"
                ; variance = Covariant
                ; extends = Top
                ; fields = []
                ; methods = []
                }
let c_string_list = Class { cls = list_sig
                    ; tau_i = Bot
                    ; tau_o = c_string
                    }

let c_number_list = Class { cls = list_sig
                    ; tau_i = Bot
                    ; tau_o = c_number
                    }

let c_integer_list = Class { cls = list_sig
                    ; tau_i = Bot
                    ; tau_o = c_integer
                    }

let c_top_list     = Class {cls = list_sig
                         ; tau_i = Bot
                         ; tau_o = Top
                         }
let c_bot_list   = Class {cls = list_sig
                         ; tau_i = Top
                         ; tau_o = Bot
                         }
let c_number_array = Class { cls = { name = "Array"
                                   ; param = TVar "X"
                                   ; variance = Invariant
                                   ; extends = c_number_list
                                   ; fields = []
                                   ; methods = []
                                   }
                           ; tau_i = c_number
                           ; tau_o = c_number
                           }
let c_integer_array = Class { cls = { name = "Array"
                                   ; param = TVar "X"
                                   ; variance = Invariant
                                   ; extends = c_integer_list
                                   ; fields = []
                                   ; methods = []
                                   }
                            ; tau_i = c_integer
                            ; tau_o = c_integer
                            }
