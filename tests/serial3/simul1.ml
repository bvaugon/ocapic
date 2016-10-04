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

open Serial;;

type t =
  | Clear | Moveto of int * int | Int of int | Int32 of int32 | Int64 of int64
  | Float of float | Floats of float array | Text of string

let (chan : (t, (int * int array array * float array) list) channel) =
  open_prog "./usart 'ocapic_lcd_simulator 20x4 e=RD0 rs=RD2 rw=RD1 bus=PORTB'"
;;

let f x =
  send chan x;
  receive chan;
;;

let clear () = f Clear;;
let moveto l c = f (Moveto (l, c));;
let print_int i = f (Int i);;
let print_int32 n = f (Int32 n);;
let print_int64 n = f (Int64 n);;
let print_float d = f (Float d);;
let print_floats fs = f (Floats fs);;
let print_string s = f (Text s);;
