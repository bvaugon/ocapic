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

let rec_pin  = PORTE_RE3
let out_pins = [| RB0; RB1; RB2; RB3; RB4; RB5; RB6; RB7; RC4; RC5; RC6; RC7; |]
let out_nb   = Array.length out_pins

let write_pins word =
  for i = 0 to out_nb - 1 do
    (if word land (1 lsl i) <> 0 then set_bit else clear_bit) out_pins.(i);
  done

let wait b = while test_bit rec_pin <> b do () done

let receive_bit () =
  let res = test_bit rec_pin in
  sleep 2;
  if test_bit rec_pin = res then raise Exit;
  sleep 2;
  res

let receive_int =
  let rec aux i mask acc =
    if i = out_nb then acc else
      aux (i + 1) (mask lsl 1) (acc lor (if receive_bit () then mask else 0))
  in
  fun () -> aux 0 1 0

let hash word = ((word * 129) lxor (word lsr 5) lxor word lxor 0x555) land 0xFFF

let receive () =
  wait true;
  sleep 5;
  let word = receive_int () in
  sleep 1;
  wait false;
  sleep 5;
  let key = receive_int () in
  if hash word <> key then raise Exit;
  word

let () =
  set_bit IRCF1;
  set_bit IRCF0;
  set_bit PLLEN;
  Array.iter (fun pin -> clear_bit (tris_of_pin pin)) out_pins;
  Array.iter (fun pin -> set_bit pin) out_pins;
  sleep 1000;
  Array.iter (fun pin -> clear_bit pin) out_pins;
  sleep 1000;
  while true do
    begin try
      let word = receive () in
      write_pins word;
      with Exit -> () end;
    sleep 2;
  done
