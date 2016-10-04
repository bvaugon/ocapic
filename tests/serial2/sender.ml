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

(* Type des valeurs transferees de l'ordinateur vers le PIC *)
type t = Clear | Moveto of int * int | Int of int | Text of string

(* Cannal de communication avec le PIC via le port serie *)
let (chan : (t, unit) channel) = open_tty "/dev/ttyUSB0";;

(* Utilitaires *)
let clear () = send chan Clear;;
let moveto l c = send chan (Moveto (l, c));;
let print_int i = send chan (Int i);;
let print_string s = send chan (Text s);;
