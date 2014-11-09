open Definitions
open Well_formed
open Test_utils

let param_A = "FUNCTION_PARAM_A"
let param_B = "FUNCTION_PARAM_B"
let i_function =
  Interface ( "Function"
            , [param_A; param_B]
            , []
            , []
            , [(NoCond, Method( TVar param_B
                              , "apply"
                              , [Arg(TVar param_A, "that")]))]
            )

let () =
  let ctx =
    let vm = add_vars empty_varmap
                      [ (param_A, (Top, Top))
                      ; (param_B, (Top, Top))]
    in
    context_init StringMap.empty StringMap.empty vm
  in
  let () = typecheck (interface_ok ctx i_function) in
  let () = Format.printf "%s\n" (Pretty_print.pinter_t ctx i_function) in
  ()
