(*************************************************************************)
(*                                                                       *)
(*                                OCaPIC                                 *)
(*                                                                       *)
(*                             Benoit Vaugon                             *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../LICENSE-en.                                            *)
(*                                                                       *)
(*************************************************************************)

external round : unit -> unit = "caml_random_round"
external bits : unit -> int = "caml_random_bits"
external bool : unit -> bool = "caml_random_bool"

let rec intaux bound =
  let r = bits () in
  let v = r mod bound in
  if r - v > 0x3FFF - bound + 1 then intaux bound else v
;;

let int bound =
  if bound <= 0 then invalid_arg "Random.int" else intaux bound
;;

let rec int32aux n =
  let b1 = Int32.of_int (bits ()) in
  let b2 = Int32.shift_left (Int32.of_int (bits () land 1)) 30 in
  let r = Int32.logor b1 b2 in
  let v = Int32.rem r n in
  if Int32.sub r v > Int32.add (Int32.sub Int32.max_int n) Int32.one
  then int32aux n
  else v
;;

let int32 bound =
  if bound <= Int32.zero
  then invalid_arg "Random.int32"
  else int32aux bound
;;

let rec int64aux n =
  let b1 = Int64.of_int (bits ()) in
  let b2 = Int64.shift_left (Int64.of_int (bits ())) 30 in
  let b3 = Int64.shift_left (Int64.of_int (bits () land 7)) 60 in
  let r = Int64.logor b1 (Int64.logor b2 b3) in
  let v = Int64.rem r n in
  if Int64.sub r v > Int64.add (Int64.sub Int64.max_int n) Int64.one
  then int64aux n
  else v
;;

let int64 bound =
  if bound <= Int64.zero
  then invalid_arg "Random.int64"
  else int64aux bound
;;
