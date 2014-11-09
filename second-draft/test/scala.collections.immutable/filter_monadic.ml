open Definitions
open Well_formed
open Test_utils

let param_A = "FM_PARAM_A"
let param_B = "FM_PARAM_B"
let i_filter_monadic =
  Interface ( "FilterMonadic"
            , [param_A; param_B]
            , []
            , []
            , [ (NoCond, Method( TVar param_B
                              , "map"
                              , [Arg( Instance("Function", empty_varmap)
                                    , "f")] ))
              ; (NoCond, Method( Instance("FilterMonadic", add_vars empty_varmap
                                                                    [(param_A, (TVar param_A, TVar param_A))
                                                                    ;(param_B, (TVar param_B, TVar param_B))])
                               , "withFilter"
                               , [Arg( Instance("Function", empty_varmap)
                                     , "p")]))
              ])

let () =
  let ctx =
    let cc =
      (StringMap.add (name_of_inter_t i_filter_monadic) (I i_filter_monadic)
      (StringMap.add (name_of_inter_t Function.i_function) (I Function.i_function)
                     StringMap.empty))
    in
    let vm = add_vars empty_varmap
                      [ (param_A, (Top, Top))
                      ; (param_B, (Top, Top))
                      ; (Function.param_A, (TVar param_A, TVar param_A))
                      ; (Function.param_B, (TVar param_B, TVar param_B))]
    in
    context_init cc StringMap.empty vm
  in
  let () = typecheck (interface_ok ctx i_filter_monadic) in
  let () = Format.printf "%s\n" (Pretty_print.pinter_t ctx i_filter_monadic) in
  ()
