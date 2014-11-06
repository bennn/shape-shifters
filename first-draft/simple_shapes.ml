open Definitions

let i_boolean = Interface { intr = { iname = "iBoolean"
                                  ; iparam = "iBool_PARAM"
                                  ; iextends = None
                                  ; isatisfies = None
                                  ; ifields = []
                                  ; imethods = []
                                  }
                         ; itau_i = Bot
                         ; itau_o = Top
                         }

let i_number = Interface { intr = { iname = "iNumber"
                                  ; iparam = "iNum_PARAM"
                                  ; iextends = None
                                  ; isatisfies = None
                                  ; ifields = []
                                  ; imethods = []
                                  }
                         ; itau_i = Bot
                         ; itau_o = Top
                         }

let s_triangle = Shape { shp = { sname = "Triangle"
                               ; sextends = None
                               ; sfields = []
                               ; smethods = [Method(IType i_boolean, "isEquilateral", [])]
                               }
                       ; stau_i = Bot
                       ; stau_o = Top
                       }

let s_square = Shape { shp = { sname = "Square"
                             ; sextends = None
                             ; sfields = []
                             ; smethods = [Method(IType i_number, "getDiagonal", [])]
                             }
                     ; stau_i = Bot
                     ; stau_o = Top
                     }

let s_circle = Shape { shp = { sname = "Circle"
                             ; sextends = None
                             ; sfields = []
                             ; smethods = [Method(IType i_number, "getRadius", [])]
                             }
                     ; stau_i = Bot
                     ; stau_o = Top
                     }

let s_circle2 = Shape { shp = { sname = "Circle2"
                              ; sextends = Some s_circle
                              ; sfields = []
                              ; smethods = [Method(IType i_boolean, "specialProperty", [Arg(IType i_number, "someArg")])]
                              }
                      ; stau_i = Bot
                      ; stau_o = Top
                      }

let c_block_malformed = Class { cls = {name = "Block1"
                                      ; param = "B_APRAM"
                                      ; extends = Top
                                      ; implements = None
                                      ; satisfies = Some s_circle2
                                      ; fields = []
                                      ; methods = []
                                      }
                              ; tau_i = Bot
                              ; tau_o = Top
                              }

let c_block = Class { cls = { name = "Block2"
                            ; param = "BPARAM2"
                            ; extends = Top
                            ; implements = None
                            ; satisfies = Some s_circle2
                            ; fields = []
                            ; methods = [ Method(IType i_number, "getRadius", [])
                                        ; Method(IType i_boolean, "specialProperty", [Arg(IType i_number, "someArg")])
                            ]}
                    ; tau_i = Bot
                    ; tau_o = Top
                    }

(* getRadius returns boolean instead of number *)
let c_block_malformed2 = Class { cls = { name = "Block3"
                            ; param = "BPARAM3"
                            ; extends = Top
                            ; implements = None
                            ; satisfies = Some s_circle2
                            ; fields = []
                            ; methods = [ Method(IType i_boolean, "getRadius", [])
                                        ; Method(IType i_boolean, "specialProperty", [Arg(IType i_number, "someArg")])
                            ]}
                    ; tau_i = Bot
                    ; tau_o = Top
                    }

(* not implementing the s_circle method *)
let c_block_malformed3 = Class { cls = { name = "Block4"
                                       ; param = "BPARAM4"
                                       ; extends = Top
                                       ; implements = None
                                       ; satisfies = Some s_circle2
                                       ; fields = []
                                       ; methods = [Method(IType i_boolean, "specialProperty", [Arg(IType i_number, "someArg")])]
                                       }
                               ; tau_i = Bot
                               ; tau_o = Top
                               }

let s_wellformed = [
  s_triangle
; s_square
; s_circle
; s_circle2
]

let c_wellformed = [
  c_block
]

let c_malformed = [
  c_block_malformed
]
