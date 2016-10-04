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

open Sys;;
open Pic;;

let flag_pin = RD2
let send_pin = RA4
let but_pins = [| RB0; RB1; RB2; RB3; RB4; RB5; RB6; RB7; RC4; RC5; RC6; RC7; |]
let but_nb   = Array.length but_pins

let read_pins =
  let rec aux i acc =
    if i < 0 then acc else
      aux (i - 1) ((acc lsl 1) lor (if test_bit but_pins.(i) then 1 else 0))
  in
  fun () -> aux (but_nb - 1) 0

let send_int n =
  for i = 0 to but_nb - 1 do
    if n land (1 lsl i) <> 0 then (
      set_bit   send_pin; sleep 2;
      clear_bit send_pin; sleep 2;
    ) else (
      clear_bit send_pin; sleep 2;
      set_bit   send_pin; sleep 2;
    )
  done

let hash word = ((word * 129) lxor (word lsr 5) lxor word lxor 0x555) land 0xFFF

let send word =
  let key = hash word in
  set_bit send_pin;
  sleep 4;
  send_int word;
  set_bit send_pin;
  sleep 4;
  clear_bit send_pin;
  sleep 4;
  send_int key;
  clear_bit send_pin
    
let () =
  set_bit IRCF1;
  set_bit IRCF0;
  set_bit PLLEN;
  clear_bit (tris_of_pin send_pin);
  while true do
    let word = read_pins () in
    send (if test_bit flag_pin then word else 0);
    sleep 20;
  done
