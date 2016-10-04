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

open Pic;;
open Types;;

set_bit IRCF1;;
set_bit IRCF0;;
set_bit PLLEN;;

module Disp = Lcd.Connect (
  struct
    let bus_size = Lcd.Eight
    let e  = LATA0
    let rs = LATA1
    let rw = LATA3
    let bus = PORTB
  end
);;
open Disp;;

init ();;
config ();;

let string_of_size s =
  match s with
    | Minus -> "."
    | Small -> "\161"
    | Medium -> "o"
    | Big -> "\219"
;;

let string_of_column c =
  let s = Bytes.create 1 in
  Bytes.set s 0 (char_of_int ((int_of_char 'A') + c));
  Bytes.unsafe_to_string s
;;

let string_of_line l =
  let s = Bytes.create 1 in
  Bytes.set s 0 (char_of_int ((int_of_char '1') + l));
  Bytes.unsafe_to_string s
;;

let write_action a =
  clear ();
  home ();
  match a with
    | Add (g, i, j) ->
      if g.color = White then print_string "W" else print_string "B";
      print_string (string_of_size g.size);
      print_string " in ";
      print_string (string_of_column i);
      print_string (string_of_line j);
    | Move (i, j, k, l) ->
      print_string (string_of_column i);
      print_string (string_of_line j);
      print_string " -> ";
      print_string (string_of_column k);
      print_string (string_of_line l);
    | Nothing -> invalid_arg "write_action"
;;

let write_string_at l c s =
  moveto l c;
  print_string s;
;;

let write_string s =
  clear ();
  home ();
  print_string s;
;;
