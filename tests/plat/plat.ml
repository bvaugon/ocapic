(*************************************************************************)
(*                                                                       *)
(*                                OCaPIC                                 *)
(*                                                                       *)
(*                             Benoit Vaugon                             *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../../LICENSE-en.                                         *)
(*                                                                       *)
(*************************************************************************)

open Pic

type color = Black | White
type key = Table of int * int | Stack of color * int | Unknow

let key_of_ind ind =
  match ind with
    | 0b00100000 -> Table (0, 0+0)
    | 0b01000100 -> Table (0, 1+0)
    | 0b01100000 -> Table (0, 2+0)
    | 0b10001100 -> Table (0, 3+0)
    | 0b10100000 -> Table (1, 0+0)
    | 0b00000100 -> Table (1, 1+0)
    | 0b00100100 -> Table (1, 2+0)
    | 0b00101100 -> Table (1, 3+0)
    | 0b01000000 -> Table (2, 0+0)
    | 0b11000000 -> Table (2, 1+0)
    | 0b01100100 -> Table (2, 2+0)
    | 0b01101100 -> Table (2, 3+0)
    | 0b10000000 -> Table (3, 0+0)
    | 0b10000100 -> Table (3, 1+0)
    | 0b11000100 -> Table (3, 2+0)
    | 0b01101000 -> Table (3, 3+0)

    | 0b11100000 -> Stack (Black, 0+0)
    | 0b11100100 -> Stack (Black, 1+0)
    | 0b00001000 -> Stack (Black, 2+0)

    | 0b11101000 -> Stack (White, 0+0)
    | 0b00001100 -> Stack (White, 1+0)
    | 0b00101000 -> Stack (White, 2+0)

    | _  -> Unknow
;;

let string_of_key key =
  match key with
    | Table (i, j) -> "(" ^ (string_of_int i) ^ ", " ^ (string_of_int j) ^ ")"
    | Stack (White, n) -> "White " ^ (string_of_int n)
    | Stack (Black, n) -> "Black " ^ (string_of_int n)
    | Unknow -> "???"
;;

let string_of_ind ind =
  let s = Bytes.create 5 in
  Bytes.set s 0 ((if ((ind lsr 2) land 1) = 0 then '0' else '1'));
  Bytes.set s 1 ((if ((ind lsr 3) land 1) = 0 then '0' else '1'));
  Bytes.set s 2 ((if ((ind lsr 5) land 1) = 0 then '0' else '1'));
  Bytes.set s 3 ((if ((ind lsr 6) land 1) = 0 then '0' else '1'));
  Bytes.set s 4 ((if ((ind lsr 7) land 1) = 0 then '0' else '1'));
  Bytes.unsafe_to_string s
;;

let rec f old_ind =
  let ind = read_reg PORTD in
  if ind <> old_ind then
    let key = key_of_ind ind in
    Display.write_string (string_of_ind ind);
    Display.write_string_at 2 0 (string_of_key key);
    f ind
  else
    f old_ind
in
f 1234
;;
