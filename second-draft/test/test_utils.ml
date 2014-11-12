open Definitions
open Well_formed

module StringSet = Set.Make (String)

let assert_true = function
  | true  -> ()
  | false -> failwith "ERROR: test failed"

let assert_false = function
  | false -> ()
  | true  -> failwith "WHOOPS: test did not fail, but should have"

(* [check_expr ctx ~expected ~observed] Typecheck the expression [~actual],
   make sure the result is a subtype of [~expected]. *)
let check_expr ctx ~expected:tt ~observed:expr : unit =
  (* factor through [method_body_ok], as it's the only place we destruct expressions *)
  let dummy_method = Method(tt, "dummy", []) in
  let dummy_body   = Return expr in
  assert_true (method_body_ok ctx (dummy_method,dummy_body))
(* [check_expr_false ctx ~expected ~observed] Opposite result as [check_expr ctx ~exp ~obs] *)
let check_expr_false ctx ~expected:tt ~observed:expr : unit =
  (* factor through [method_body_ok], as it's the only place we destruct expressions *)
  let dummy_method = Method(tt, "dummy", []) in
  let dummy_body   = Return expr in
  assert_false (method_body_ok ctx (dummy_method,dummy_body))

let check_method_names ctx ~expected:names ~observed:tt : unit =
  let names' = method_names ctx tt in
  let exp_set  = StringSet.of_list names in
  let act_set = StringSet.of_list (method_names ctx tt) in
  begin match (StringSet.equal exp_set act_set) with
  | true  -> ()
  | false -> failwith (Format.sprintf "ERROR: expected names '%s' do not match actual names '%s'. The diff is '%s'."
                                      (string_of_list (fun x -> x) names)
                                      (string_of_list (fun x -> x) names')
                                      (string_of_list (fun x -> x) (StringSet.fold (fun x acc -> x :: acc) (StringSet.diff exp_set act_set) [])))
  end

let load_class c = ignore c

let typecheck (ctx:Context.t) (st:sig_t) (cs:cond_t list) : unit =
  let () = assert_true (context_ok ctx cs) in
  begin match st with
  | C c -> assert_true (class_ok ctx c)
  | I i -> assert_true (interface_ok ctx i)
  end

let typecheck_false (ctx:Context.t) (st:sig_t) (cs:cond_t list) : unit =
  begin match (context_ok ctx cs) with
  | true ->
     begin match st with
     | C c -> assert_false (class_ok ctx c)
     | I i -> assert_false (interface_ok ctx i)
     end
  | false -> ()
  end
