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

module Disp = Lcd.Connect (
  struct
    let bus_size = Lcd.Four
    let e  = Pic.LATD0
    let rs = Pic.LATD2
    let rw = Pic.LATD1
    let bus = Pic.PORTC
  end
);;
open Disp;;

init ();;
config ();;

Printf.fprintf output "%.4g" (-1678.5e-30);;

(*
  let x = float_of_int 12 *. float_of_int 7 in
  let y = ~-. x in
  let n = int_of_float y in
  print_int n;
  ;;
  print_char ' ';;
  print_int (int_of_float 50.2654824574);;
*)

(*
  let rec f x =
  Printf.fprintf output "%f" x;
  f (3. *. x);
  in
  try f 1.
  with
  | Failure x -> clear (); home (); print_string x
  | Invalid_argument x -> clear (); home (); print_string x
  | Out_of_memory -> clear (); home (); print_string "o";
  | _ -> clear (); home (); print_string "?"
  ;;
*)
