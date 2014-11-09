open Definitions
open Well_formed
open Test_utils

let param = "TRAVERSABLE_PARAM"
let i_traversable =
  Interface ( "Traversable"
            , [param]
            , [(NoCond, Filter_monadic.i_filter_monadic)]
            , [(NoCond, Has_builder.s_has_builder)]
            , [ (NoCond, Method( Bot
                              , "forEach"
                              , [Arg(Instance("Function", empty_varmap), "f")] ))
              ; (NoCond, Method( Instance("Boolean", empty_varmap)
                               , "isEmpty"
                               , []))
              ]
            )
let () =
  let ctx =
    let cc =
      (StringMap.add (name_of_inter_t Filter_monadic.i_filter_monadic) (I Filter_monadic.i_filter_monadic)
      (StringMap.add (name_of_inter_t Boolean.i_boolean) (I Boolean.i_boolean)
      (StringMap.add (name_of_inter_t Function.i_function) (I Function.i_function)
                     StringMap.empty)))
    in
    let vm = add_vars empty_varmap
                      [ (param, (Top, Top))
                      ; (Filter_monadic.param_A, (Top, Top))
                      ; (Filter_monadic.param_B, (Top, Top))
                      ; (Function.param_A, (TVar Filter_monadic.param_A, TVar Filter_monadic.param_A))
                      ; (Function.param_B, (TVar Filter_monadic.param_B, TVar Filter_monadic.param_B))]
    in
    context_init cc StringMap.empty vm
  in
  let () = typecheck (interface_ok ctx i_traversable) in
  let () = Format.printf "%s\n" (Pretty_print.pinter_t ctx i_traversable) in
  ()
