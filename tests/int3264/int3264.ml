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
open Int32;;

set_bit IRCF1;;
set_bit IRCF0;;
set_bit PLLEN;;

module Disp = Lcd.Connect (
  struct
    let bus_size = Lcd.Eight
    let e  = Pic.LATD0
    let rs = Pic.LATD2
    let rw = Pic.LATD1
    let bus = Pic.PORTB
  end
);;
open Disp;;

init ();;
config ();;

(*
  open Int64;;

  let print c n =
  let s = to_string n in
  let l = String.length s in
  clear ();
  print_char c;
  print_char ' ';
  if l > 10 then (
  print_string (String.sub s 0 10);
  moveto 2 2;
  print_string (String.sub s 10 (l - 10));
  ) else
  print_string s;
  Sys.sleep 10000;
  Sys.sleep 10000;
  Sys.sleep 10000;
  ;;

  while true do
  clear ();
  let x = Random.int64 max_int in
  let y = Random.int64 (shift_right x 1) in
  print 'x' x;
  print 'y' y;
  print '+' (add x y);
  print '*' (mul x y);
  print '/' (div x y);
  print '%' (rem x y);
  done;;
*)

(*
  try
  print_int (int_of_string "abc")
  with
  | Out_of_memory -> print_string "Out_of_memory"
  | Stack_overflow -> print_string "Stack_overflow"
  | Failure msg ->
  print_string "Failure";
  moveto 2 0;
  print_string msg;
  | Invalid_argument f ->
  print_string "Invalid_argument";
  moveto 2 0;
  print_string f;
  | Division_by_zero -> print_string "Division_by_zero"
  | _ -> print_string "Unknown exception"
  ;;
*)

let tbl = Hashtbl.create 4 in
Hashtbl.add tbl 78919991l "Hello";
Hashtbl.add tbl 1918281l "world";
Hashtbl.add tbl 182766172l " ";
Hashtbl.add tbl 1198211l "!";
List.iter (fun x -> print_string (Hashtbl.find tbl x))
  [ 78919991l ; 182766172l ; 1918281l ; 1198211l ];

for i = -5 to 5 do
  for j = -10 to 10 do
    let x = of_int i in
    let y = of_int j in
    moveto 2 0;
    Printf.fprintf output "%ld <= %ld = %b" x y (x <= y);
    print_string "                ";
    Sys.sleep 400;
  done
done
