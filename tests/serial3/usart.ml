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
open Lcd;;
open Serial;;

set_bit IRCF1;;
set_bit IRCF0;;
set_bit PLLEN;;

module Disp = Connect (
  struct
    let bus_size = Eight
    let e  = LATD0
    let rs = LATD2
    let rw = LATD1
    let bus = PORTB
  end
);;
open Disp;;

init ();;
config ~cursor:Underscore ();;

print_string "Hello";;
moveto 2 0;;

type t =
  | Clear | Moveto of int * int | Int of int | Int32 of int32 | Int64 of int64
  | Float of float | Floats of float array | Text of string

(* 19200 bauds *)
let (chan : ((int * int array array * float array) list, t) channel) =
  open_channel 34
;;

while true do
  match receive chan with
    | Clear ->
      clear ();
      send chan [(1, [|[| 1 ; 2 ; 3 ; 4|]|], [|2. ; 3.|]);
                 (3, [|[| 3 ; 1 ; 4 ; 2|]|], [|4. ; 5.|])];
    | Moveto (l, c) ->
      moveto l c;
      send chan [(0, [|[|0; 172 ; 1; 498 ; 182; 11|]|], [|172.|]);
                 (1, [|[||]|], [|498.|]) ; (182, [||], [|11.;12.|])];
    | Int i ->
      print_int i;
      send chan [(0, [|[|0; 1|]|], [|1.;1.;1.|])];
    | Int32 n ->
      print_string (Int32.to_string n);
      send chan [(-1, [|[|-1; 18 ; -1; -1|]|], [|18.;81.;11.;88.|]);
                 (-1, [||], [|-1.;-2.|])];
    | Int64 n ->
      print_string (Int64.to_string n);
      send chan [(1, [|[||]|], [||]);
                 (123, [|[|1; 7 ; 123; 987|]|], [| 987. ; 789. |])];
    | Float f ->
      print_int (int_of_float f);
      send chan [(1098, [||], [|2481.;18729.;1121.|]);
                 (1821, [||], [|8382.;-191821.|])];
    | Text t ->
      print_string t;
      let l =
        (1, [|[|1|]|], [|-1.|]) :: (2, [|[|1;2|]|], [|-2.|])
        :: (3, [|[|1;2;3|]|], [|-3.|]) :: (4, [|[||]|], [|-4.|]) :: []
      in
      send chan l;
    | Floats fs ->
      Array.iter (fun f -> print_int (int_of_float f) ; print_char ' ') fs;
      send chan [(0, [|[||];[|1988;3121|];[||];[|1|];[||]|], [||]);
                 (0, [||], [||]); (0, [||], [||])];
done
