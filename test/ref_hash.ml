open Definitions
open Well_formed
open Test_utils

(* Shape Shifter: reference hashing (yuck!) *)

(* Shape shifter *)
let w_ref_hash =
  Shifter ( "RefHash"
          , [] (* type parameters *)
          , [Hashable.s_hashable] (* shapes it links to *)
          )
