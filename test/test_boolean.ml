open Definitions
open Well_formed
open Test_utils

(* Sample classes *)
let c_malformed1 = (* No methods *)
  Class ("MF1", [], [], [(NoCond, Boolean.i_boolean)], []
             , [])

let c_malformed2 = (* Method missing because of "sat" condition *)
  Class ( "MF2"
        , []
        , []
        , [(NoCond, Boolean.i_boolean)]
        , []
        , [ (Sat("DUMMY_VAR", Equatable.s_equatable), (Method( Instance("Boolean", [])
                            , "and"
                            , [Arg(Instance("Boolean", []), "that")])
                     , Return Null))
          ; (NoCond, (Method( Instance("Boolean", [])
                            , "not"
                            , [])
                     , Return Null))
        ])

let c_malformed3 = (* (other) Method missing because of "sat" condition *)
  Class ("MF3", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [])
                                , "and"
                                , [Arg(Instance("Boolean", []), "that")])
                          , Return Null))
               ; (Sat("DUMMY_VAR", Equatable.s_equatable), (Method( Instance("Boolean", [])
                                , "not"
                                , [])
                          , Return Null))
        ])

let c_malformed4 = (* Missing 'and' *)
  Class ("MF4", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [])
                                , "not"
                                , [])
                          , Return Null))
        ])

let c_malformed5 = (* renamed 'not' *)
  Class ("MF5", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [])
                                , "and"
                                , [Arg(Instance("Boolean", []), "that")])
                          , Return Null))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "notnot_whosthere"
                                , [])
                          , Return Null))
        ])

let c_malformed6 = (* duplicate method *)
  Class ("MF1", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [])
                                , "and"
                                , [Arg(Instance("Boolean", []), "that")])
                          , Return Null))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "not"
                                , [])
                          , Return Null))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "and"
                                , [])
                          , Return Null))
        ])

let c_malformed7 = (* new method, malformed *)
  Class ("MF7", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [])
                                , "and"
                                , [Arg(Instance("Boolean", []), "that")])
                          , Return Null ))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "not"
                                , [])
                          , Return Null))
               ; (NoCond, (Method( Bot
                                , "or"
                                , [Arg(Instance("Boolean", []), "that")])
                          , Return (Call(("Boolean", []), "and", [("True", [])])) ))
        ])

let c_malformed8 = (* invalid return type *)
  Class ("MF8", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Top
                                , "and"
                                , [Arg(Instance("Boolean", []), "that")])
                          , Return (New("Boolean", [])) ))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "not"
                                , [])
                          , Return Null))
        ])

let c_malformed9 = (* Invalid arg type for 'and' *)
  Class ("MF9", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [])
                                , "and"
                                , [Arg(Instance("True", []), "that")])
                          , Return Null))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "not"
                                , [])
                          , Return Null))
        ])

let c_malformed10 = (* Too many args to 'not' *)
  Class ("MF10", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [])
                                , "and"
                                , [Arg(Instance("True", []), "that")])
                          , Return Null))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "not"
                                , [Arg(Top, "nothing")])
                          , Return Null))
        ])

let c_malformed11 = (* 'and' arg renamed *)
  Class ("MF11", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [])
                                , "and"
                                , [Arg(Instance("True", []), "another")])
                          , Return Null))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "not"
                                , [])
                          , Return Null))
        ])

let c_malformed12 = (* Too many params in return type *)
  Class ("MF12", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [("ANY", Top, Top); ("NONE", Bot, Top)])
                                , "and"
                                , [Arg(Instance("Boolean", []), "that")])
                          , Return Null))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "not"
                                , [])
                          , Return Null))
        ])

let c_malformed13 = (* Too many params in arg type *)
  Class ("MF13", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [])
                                , "and"
                                , [Arg(Instance("Boolean", [("ANY", Top, Top)]), "that")])
                          , Return Null))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "not"
                                , [])
                          , Return Null))
        ])

let c_malformed14 = (* Too many params in new type *)
  Class ("MF14", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [])
                                , "and"
                                , [Arg(Instance("Boolean", []), "that")])
                          , Return (New("True", [("Ops", Top, Top)])) ))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "not"
                                , [])
                          , Return Null))
        ])

let c_malformed15 = (* trying to instantiate a New interface *)
  Class ("MF15", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [])
                                , "and"
                                , [Arg(Instance("Boolean", []), "that")])
                          , Return (New("Boolean", [])) ))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "not"
                                , [])
                          , Return Null))
        ])

let c_malformed16 = (* Too many params in call type *)
  Class ("MF16", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [])
                                , "and"
                                , [Arg(Instance("Boolean", [("ANY", Top, Top)]), "that")])
                          , Return (Call(("True", [("Yo", Bot, Bot)]), "not", [])) ))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "not"
                                , [])
                          , Return Null))
        ])

let c_true_outline = (* Methods implemented with "null" *)
  Class ("TrueOutline", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [])
                                , "and"
                                , [Arg(Instance("Boolean", []), "that")])
                          , Return Null))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "not"
                                , [])
                          , Return Null))
        ])

let c_true_args = (* Method args are supertypes *)
  Class ("TrueArgs", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [])
                                , "and"
                                , [Arg(Top, "that")])
                          , Return Null))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "not"
                                , [])
                          , Return Null))
        ])

let c_true_ret = (* Method args are supertypes *)
  Class ("TrueRet", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Bot
                                , "and"
                                , [Arg(Instance("Boolean", []), "that")])
                          , Return Null))
               ; (NoCond, (Method( Instance("True", [])
                                , "not"
                                , [])
                          , Return Null))
        ])

let c_true =
  Class ("True", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [])
                                , "and"
                                , [Arg(Instance("Boolean", []), "that")])
                          , Return (New("True", [])) ))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "not"
                                , [])
                          , Return (New("False", []))))
        ])

let c_false =
  Class ("False", [], [], [(NoCond, Boolean.i_boolean)], []
             , [ (NoCond, (Method( Instance("Boolean", [])
                                , "and"
                                , [Arg(Instance("Boolean", []), "that")])
                          , Return (New("True", [])) ))
               ; (NoCond, (Method( Instance("Boolean", [])
                                , "not"
                                , [])
                          , Return (New("True", []))))
        ])

(* Tests *)
let test_malformed () =
  let malformed = [ C c_malformed1; C c_malformed2; C c_malformed3; C c_malformed4
                  ; C c_malformed5; C c_malformed6; C c_malformed7; C c_malformed8
                  ; C c_malformed9; C c_malformed10; C c_malformed11; C c_malformed12] in
  let ctx =
    let cc = ClassContext.of_list (I Boolean.i_boolean :: C c_true :: malformed) in
    let tc = TypeContext.of_list [("DUMMY_VAR", Top, Top)] in
    Context.init cc [] tc
  in
  List.iter (fun m -> typecheck_false ctx m []) malformed

let test_true_outline () =
  let ctx =
    let cc = ClassContext.of_list [ I Boolean.i_boolean
                                  ; C c_true_outline] in
    Context.init cc [] []
  in
  typecheck ctx (C c_true_outline) []

let test_true_args () =
  let ctx =
    let cc = ClassContext.of_list [ I Boolean.i_boolean
                                  ; C c_true_args] in
    Context.init cc [] []
  in typecheck ctx (C c_true_args) []

let test_true_ret () =
  let ctx =
    let cc = ClassContext.of_list [ I Boolean.i_boolean
                                  ; C c_true
                                  ; C c_true_ret] in
    Context.init cc [] []
  in typecheck ctx (C c_true_ret) []

let test_true_false () =
  let ctx =
    let cc = ClassContext.of_list [ I Boolean.i_boolean
                                  ; C c_true
                                  ; C c_false]
    in
    Context.init cc [] []
  in
  let () = typecheck ctx (C c_true) []  in
  let () = typecheck ctx (C c_false) [] in
  ()

(* Main *)
let () =
  let () = Format.printf "/* [test_boolean] testing malformed classes ... */\n" in
  let () = test_malformed () in
  let () = Format.printf "/* [test_boolean] testing well-formed classes ... */\n" in
  let () = test_true_outline () in
  let () = test_true_args () in
  let () = test_true_ret () in
  let () = test_true_false () in
  ()
