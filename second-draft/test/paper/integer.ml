open Definitions
open Well_formed
open Test_utils

let c_zero =
  Class ( "Zero"
            , []
            , []
            , [ (NoCond, Number.i_integer)]
            , [ (NoCond, Comparable.s_comparable)
              ; (NoCond, Addable.s_addable)]
            , [ (NoCond, (Method( Instance("Integer", [])
                               , "max3"
                               , [ Arg(Instance("Number", []), "num1")
                                 ; Arg(Instance("Number", []), "num2")])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Integer", [])
                               , "succ"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Long", [])
                               , "longValue"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Integer", [])
                               , "intValue"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Zero", [])
                               , "plus"
                               , [Arg(Instance("Number",[]), "that")])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Boolean", [])
                               , "isZero"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Boolean", [])
                          , "leq"
                          , [Arg(TVar "THIS", "that")])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Boolean", [])
                               , "equals"
                               , [Arg(TVar "THIS", "that")])
                         , Return Null ))
            ])

let c_malformed1 = (* Missing 'leq' method *)
  Class ( "MF1"
            , []
            , []
            , [ (NoCond, Number.i_integer)]
            , [ (NoCond, Comparable.s_comparable)
              ; (NoCond, Addable.s_addable)]
            , [ (NoCond, (Method( Instance("Integer", [])
                               , "max3"
                               , [ Arg(Instance("Number", []), "num1")
                                 ; Arg(Instance("Number", []), "num2")])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Integer", [])
                               , "succ"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Long", [])
                               , "longValue"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Integer", [])
                               , "intValue"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("MF1", [])
                               , "plus"
                               , [Arg(Instance("Number",[]), "that")])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Boolean", [])
                               , "isZero"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Boolean", [])
                               , "equals"
                               , [Arg(TVar "THIS", "that")])
                         , Return Null ))
            ])

let c_malformed2 = (* missing 'equals' method *)
  Class ( "MF2"
            , []
            , []
            , [ (NoCond, Number.i_integer)]
            , [ (NoCond, Comparable.s_comparable)
              ; (NoCond, Addable.s_addable)]
            , [ (NoCond, (Method( Instance("Integer", [])
                               , "max3"
                               , [ Arg(Instance("Number", []), "num1")
                                 ; Arg(Instance("Number", []), "num2")])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Integer", [])
                               , "succ"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Long", [])
                               , "longValue"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Integer", [])
                               , "intValue"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("MF2", [])
                               , "plus"
                               , [Arg(Instance("Number",[]), "that")])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Boolean", [])
                               , "isZero"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Boolean", [])
                          , "leq"
                          , [Arg(TVar "THIS", "that")])
                         , Return Null ))
            ])

let c_malformed3 = (* 'add' returns number, not THIS=Zero *)
  Class ( "MF3"
            , []
            , []
            , [ (NoCond, Number.i_integer)]
            , [ (NoCond, Comparable.s_comparable)
              ; (NoCond, Addable.s_addable)]
            , [ (NoCond, (Method( Instance("Integer", [])
                               , "max3"
                               , [ Arg(Instance("Number", []), "num1")
                                 ; Arg(Instance("Number", []), "num2")])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Integer", [])
                               , "succ"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Long", [])
                               , "longValue"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Integer", [])
                               , "intValue"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Number", [])
                               , "plus"
                               , [Arg(Instance("Number",[]), "that")])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Boolean", [])
                               , "isZero"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Boolean", [])
                          , "leq"
                          , [Arg(TVar "THIS", "that")])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Boolean", [])
                               , "equals"
                               , [Arg(TVar "THIS", "that")])
                         , Return Null ))
            ])

let c_malformed4 = (* missing arg to max3 *)
  Class ( "MF4"
            , []
            , []
            , [ (NoCond, Number.i_integer)]
            , [ (NoCond, Comparable.s_comparable)
              ; (NoCond, Addable.s_addable)]
            , [ (NoCond, (Method( Instance("Integer", [])
                               , "max3"
                               , [ Arg(Instance("Number", []), "num2")])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Integer", [])
                               , "succ"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Long", [])
                               , "longValue"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Integer", [])
                               , "intValue"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("MF4", [])
                               , "plus"
                               , [Arg(Instance("Number",[]), "that")])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Boolean", [])
                               , "isZero"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Boolean", [])
                          , "leq"
                          , [Arg(TVar "THIS", "that")])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Boolean", [])
                               , "equals"
                               , [Arg(TVar "THIS", "that")])
                         , Return Null ))
            ])

let c_malformed5 = (* Does not sat addable, but has a plus method *)
  Class ( "MF5"
            , []
            , []
            , [ (NoCond, Number.i_integer)]
            , [ (NoCond, Comparable.s_comparable)]
            , [ (NoCond, (Method( Instance("Integer", [])
                               , "max3"
                               , [ Arg(Instance("Number", []), "num1")
                                 ; Arg(Instance("Number", []), "num2")])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Integer", [])
                               , "succ"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Long", [])
                               , "longValue"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Integer", [])
                               , "intValue"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Bot
                               , "plus"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Boolean", [])
                               , "isZero"
                               , [])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Boolean", [])
                          , "leq"
                          , [Arg(TVar "THIS", "that")])
                         , Return Null ))
              ; (NoCond, (Method( Instance("Boolean", [])
                               , "equals"
                               , [Arg(TVar "THIS", "that")])
                         , Return Null ))
            ])

let base_classes = [ I Boolean.i_boolean; I Number.i_number;
                     I Number.i_integer;  I Number.i_long]
(* Tests *)
let test_malformed () =
  let malformed = [C c_malformed1; C c_malformed2; C c_malformed3; C c_malformed4;
                   C c_malformed5] in
  let ctx =
    let cc = ClassContext.of_list (malformed @ base_classes) in
    Context.init cc [] []
  in
  List.iter (typecheck_false ctx) malformed

let test_zero () =
  let ctx =
    let cc = ClassContext.of_list (C c_zero :: base_classes) in
    Context.init cc [] []
  in
  typecheck ctx (C c_zero)

let () =
  let () = test_malformed () in
  let () = test_zero      () in
  ()
