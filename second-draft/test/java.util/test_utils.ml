open Definitions

let empty_varmap = fun x ->
  if x = "THIS"
  then (Bot, Bot)
  else failwith (Format.sprintf "unbound variable '%s'" x)

let typecheck (b:bool) : unit =
  begin match b with
  | true  -> ()
  | false -> failwith "ERROR: typechecking failed"
  end
