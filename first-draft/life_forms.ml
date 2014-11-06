open Definitions

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
let i_living_thing = Interface { intr = { iname = "iLivingThing"
                                        ; iparam = "LT_PARAM"
                                        ; iextends = None
                                   ; isatisfies = None
                                        ; ifields = [Field(IType i_number, "pulseRate")]
                                        ; imethods = [Method(IType i_number, "getPulse", [])]
                                        }
                               ; itau_i = Bot
                               ; itau_o = Top
                               }
let i_plant = Interface { intr = { iname = "iPlant"
                                 ; iparam = "P_PARAM"
                                 ; iextends = Some i_living_thing
                                   ; isatisfies = None
                                 ; ifields = []
                                 ; imethods = [Method(CType Bot, "photosynthesize", [Arg(IType i_number, "numRays")])
                                              ;Method(IType i_number, "countFlowers", [Arg(IType i_number, "unused")])
                                              ]
                                 }
                        ; itau_i = Bot
                        ; itau_o = Top
                        }

let i_animal = Interface { intr = { iname = "iAnimal"
                                  ; iparam = "A_PARAM"
                                  ; iextends = Some i_living_thing
                                   ; isatisfies = None
                                  ; ifields = []
                                  ; imethods = [Method(CType Bot, "eat", [Arg(IType i_living_thing, "that")])
                                               ;Method(IType i_number, "countLegs", [Arg(IType i_number, "unused")])
                                               ]
                                  }
                         ; itau_i = Bot
                         ; itau_o = Top
                         }

let i_carnivore = Interface { intr = { iname = "iCarnivore"
                                  ; iparam = "C_PARAM"
                                  ; iextends = Some i_animal
                                   ; isatisfies = None
                                  ; ifields = []
                                  ; imethods = [Method(CType Bot, "eatAnimal", [Arg(IType i_animal, "prey")])
                                               ; Method(IType i_number, "isPrey", [Arg(IType i_animal, "other")])
                                               ]
                                  }
                         ; itau_i = Bot
                         ; itau_o = Top
                         }

let i_herbivore = Interface { intr = { iname = "iHerbivore"
                                  ; iparam = "H_PARAM"
                                  ; iextends = Some i_animal
                                   ; isatisfies = None
                                  ; ifields = []
                                  ; imethods = [Method(CType Bot, "eatPlant", [Arg(IType i_plant, "that")])
                                               ]
                                  }
                         ; itau_i = Bot
                         ; itau_o = Top
                         }

(* has only i_animal methods *)
let c_alligator_malformed1 = Class { cls = { name = "cAlligator1"
                                           ; param = "Alligator_PARAM1"
                                           ; extends = Top
                                           ; implements = Some i_carnivore
                                           ; satisfies = None
                                           ; fields = []
                                           ; methods = [Method(CType Bot, "eat", [Arg(IType i_living_thing, "that")])
                                                        ;Method(IType i_number, "countLegs", [Arg(IType i_number, "unused")])
                                                        ]
                                           }
                                   ; tau_i = Bot
                                   ; tau_o = Top
                                   }

(* has only i_carnivore methods *)
let c_alligator_malformed2 = Class { cls = { name = "cAlligator2"
                                  ; param = "Alligator_PARAM2"
                                  ; extends = Top
                                  ; implements = Some i_carnivore
                                           ; satisfies = None
                                  ; fields = []
                                  ; methods = [Method(CType Bot, "eatAnimal", [Arg(IType i_animal, "prey")])
                                              ; Method(IType i_number, "isPrey", [Arg(IType i_animal, "other")])
                                              ]
                                  }
                         ; tau_i = Bot
                         ; tau_o = Top
                         }

(* missing i_living_thing methods *)
let c_alligator_malformed3 = Class { cls = { name = "cAlligator3"
                                  ; param = "Alligator_PARAM3"
                                  ; extends = Top
                                  ; implements = Some i_carnivore
                                           ; satisfies = None
                                  ; fields = []
                                  ; methods = [Method(CType Bot, "eat", [Arg(IType i_living_thing, "that")])
                                               ;Method(IType i_number, "countLegs", [Arg(IType i_number, "unused")])
                                               ;Method(CType Bot, "eatAnimal", [Arg(IType i_animal, "prey")])
                                               ;Method(IType i_number, "isPrey", [Arg(IType i_animal, "other")])
                                               ]
                                  }
                         ; tau_i = Bot
                         ; tau_o = Top
                         }

(* invalid return type for getPulse *)
let c_alligator_malformed4 = Class { cls = { name = "cAlligator4"
                                  ; param = "Alligator_PARAM4"
                                  ; extends = Top
                                  ; implements = Some i_carnivore
                                           ; satisfies = None
                                  ; fields = []
                                  ; methods = [Method(CType Bot, "eat", [Arg(IType i_living_thing, "that")])
                                               ;Method(IType i_number, "countLegs", [Arg(IType i_number, "unused")])
                                               ;Method(CType Bot, "eatAnimal", [Arg(IType i_animal, "prey")])
                                               ;Method(IType i_number, "isPrey", [Arg(IType i_animal, "other")])
                                               ;Method(CType Top, "getPulse", [])
                                               ]
                                  }
                         ; tau_i = Bot
                         ; tau_o = Top
                         }

(* invalid arg type for eatAnimal *)
let c_alligator_malformed5 = Class { cls = { name = "cAlligator"
                                  ; param = "Alligator_PARAM"
                                  ; extends = Top
                                  ; implements = Some i_carnivore
                                           ; satisfies = None
                                  ; fields = []
                                  ; methods = [Method(CType Bot, "eat", [Arg(IType i_living_thing, "that")])
                                               ;Method(IType i_number, "countLegs", [Arg(IType i_number, "unused")])
                                               ;Method(CType Bot, "eatAnimal", [Arg(IType i_herbivore, "prey")])
                                               ;Method(IType i_number, "isPrey", [Arg(IType i_animal, "other")])
                                               ;Method(IType i_number, "getPulse", [])
                                               ]
                                  }
                         ; tau_i = Bot
                         ; tau_o = Top
                         }

let c_alligator = Class { cls = { name = "cAlligator"
                                  ; param = "Alligator_PARAM"
                                  ; extends = Top
                                  ; implements = Some i_carnivore
                                           ; satisfies = None
                                  ; fields = []
                                  ; methods = [Method(CType Bot, "eat", [Arg(IType i_living_thing, "that")])
                                               ;Method(IType i_number, "countLegs", [Arg(IType i_number, "unused")])
                                               ;Method(CType Bot, "eatAnimal", [Arg(IType i_animal, "prey")])
                                               ;Method(IType i_number, "isPrey", [Arg(IType i_animal, "other")])
                                               ;Method(IType i_number, "getPulse", [])
                                               ]
                                  }
                         ; tau_i = Bot
                         ; tau_o = Top
                         }

let i_wellformed = [
  i_number
; i_living_thing
; i_plant
; i_animal
]
let c_malformed = [
  c_alligator_malformed1
; c_alligator_malformed2
; c_alligator_malformed3
; c_alligator_malformed4
; c_alligator_malformed5
]
let c_wellformed = [
  c_alligator
]
