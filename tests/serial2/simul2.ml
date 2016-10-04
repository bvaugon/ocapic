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

type t = Clear | Moveto of int * int | Int of int | Text of string

let (chan : (t, unit) channel) =
  open_prog
    "ocasim usart.hex 'ocapic_lcd_simulator 20x4 e=RD0 rs=RD2 rw=RD1 bus=PORTB'"
;;

let clear () = send chan Clear;;
let moveto l c = send chan (Moveto (l, c));;
let print_int i = send chan (Int i);;
let print_string s = send chan (Text s);;
