open Definitions
open Well_formed

let assert_true = function
  | true  -> ()
  | false -> failwith "ERROR: test failed"

let assert_false = function
  | false -> ()
  | true  -> failwith "WHOOPS: test did not fail, but should have"

let load_class c = ignore c

let typecheck (ctx:Context.t) (st:sig_t) : unit =
  begin match st with
  | C c -> assert_true (class_ok ctx c)
  | I i -> assert_true (interface_ok ctx i)
  end

let typecheck_false (ctx:Context.t) (st:sig_t) : unit =
  begin match st with
  | C c -> assert_false (class_ok ctx c)
  | I i -> assert_false (interface_ok ctx i)
  end
