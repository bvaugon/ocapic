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

set_bit IRCF1;;
set_bit IRCF0;;
set_bit PLLEN;;

module Disp = Lcd.Connect (
  struct
    let bus_size = Lcd.Eight
    let e  = Pic.LATD0
    let rs = Pic.LATD2
    let rw = Pic.LATD1
    let bus = Pic.PORTC
  end
);;
open Disp;;

init ();;
config ();;

(*** lib ***)

type interruption_behavior =
  | Interruption_ignore
  | Interruption_handle of (bit -> unit)
;;

let handlers = ref [];;

let the_handler bit =
  try (List.assq bit !handlers) bit
  with Not_found -> ()
;;

let interruption bit ib =
  let old =
    try
      let old = Interruption_handle (List.assq bit !handlers) in
      handlers := List.remove_assq bit !handlers;
      old
    with Not_found -> Interruption_ignore
  in
  match ib with
    | Interruption_ignore -> old
    | Interruption_handle f ->
      set_interruption_handler the_handler;
      handlers := (bit, f) :: !handlers ; old
;;

let set_interruption bit ib = ignore (interruption bit ib);;

(*** user ***)

let my_handler =
  let counter = ref 0 in
  fun _ ->
    let (li, co) = current_position () in
    moveto 2 0;
    print_string "         ";
    moveto 2 0;
    Printf.fprintf output "Interruption %d" !counter;
    incr counter;
    moveto li co;
    Sys.sleep 100;
;;

set_bit INT0IE;;
set_bit GIE_GIEH;;
set_interruption INT0IF (Interruption_handle my_handler);;

while true do
  for i = 0 to max_int do
    clear_bit GIE_GIEH;
    moveto 1 0;
    Printf.fprintf output "Loop %d" i;
    set_bit GIE_GIEH;
    Sys.sleep 100;
  done
done
