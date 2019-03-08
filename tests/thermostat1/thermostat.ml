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

(***)

exception StartStop;;

type kind = Nothing | Plus | Minus | Twice

(***)

let max_temp =   99;; (* 80.0°C *)
let def_temp =  654;; (* 32.5°C *)
let min_temp = 1010;; (*  2.0°C *)

let output = RD1;;
let plus_button = RD5;;
let minus_button = RD4;;

(***)

set_bit IRCF1;;
set_bit IRCF0;;
set_bit PLLEN;;

(***)

write_reg TRISD 0b11111101;;
clear_bit RD1;;

(***)

let disp = connect ~bus_size:Lcd.Four ~e:LATD2 ~rs:LATD3 ~rw:LATD6 ~bus:PORTB;;

disp.init ();;
disp.config ();;
disp.register_bitmap '\000'
  0b01110_01010 0b01110_00000_00000 0b00000_00000_00000;;
disp.register_bitmap '\001'
  0b00000_00100 0b01000_10100_01000 0b00100_00000_00000;;
disp.register_bitmap '\002'
  0b00000_00100 0b00010_00101_00010 0b00100_00000_00000;;
disp.register_bitmap '\003'
  0b00000_10000 0b01000_10100_01000 0b10000_00000_00000;;
disp.register_bitmap '\004'
  0b00000_00001 0b00010_00101_00010 0b00001_00000_00000;;
disp.print_string "\00132.5\000C\002\n 32.5\000C ";;

(***)

let print_str n s =
  disp.moveto n 1;
  disp.print_string s;
;;

let str_of_temp temp =
  if temp < max_temp then "++.+"
  else if temp > min_temp then "--.-"
  else Printf.sprintf "%4.1f" (float_of_int (1033 - temp) /. 11.67)
;;

let print_temp n temp = print_str n (str_of_temp temp);;

let read_temp () =
  write_reg ADCON2 0b10111110;
  write_reg ADCON1 0b00111110;
  write_reg ADCON0 0b00000011;
  while test_bit GO_NOT_DONE do () done;
  (read_reg ADRESH lsl 8) lor read_reg ADRES
;;

(***)

let wtemp = ref def_temp;;
let wstr = ref "";;

let rec offset_wtemp ofs =
  let old_wtemp = !wtemp in
  let new_wtemp = min min_temp (max max_temp (old_wtemp + ofs)) in
  if new_wtemp <> old_wtemp then
    let new_wstr = str_of_temp new_wtemp in
    wtemp := new_wtemp;
    if !wstr = new_wstr then offset_wtemp ofs else (
      wstr := new_wstr;
      print_str 1 new_wstr;
    );
;;

let set_wtemp temp =
  if temp <> !wtemp then (
    wtemp := temp;
    print_temp 1 temp;
  );
;;

(***)

let ctemp = ref def_temp;;

let update_ctemp () =
  let otemp = !ctemp in
  let temp = read_temp () in
  let delta = otemp - temp in
  let ntemp =
    if delta > 20 || delta < -20 then temp
    else (3 * otemp + temp) lsr 2
  in
  if ntemp <> otemp then (
    print_temp 2 ntemp;
    ctemp := ntemp;
  );
;;

(***)

let prop = ref 0;;

let update_prop () =
  let delta = min 10 (max (-10) (!ctemp - !wtemp)) in
  let delta2 = if delta < 0 then -delta * delta else delta * delta in
  let offset = min 10 delta2 in
  let new_prop = min 100 (max 0 (!prop + offset)) in
  prop := new_prop;
  new_prop / 10
;;

let reset_prop () = prop := 0;;

(***)

let save_wtemp () =
  let temp = !wtemp in
  Eeprom.write 0 (temp mod 256);
  Eeprom.write 1 (temp / 256);
;;

let restore_wtemp () =
  let temp = Eeprom.read 0 + Eeprom.read 1 * 256 in
  if temp >= max_temp && temp <= min_temp then set_wtemp temp;
;;

(***)

let counter = ref 0;;
let kind = ref Nothing;;
let mem_wtemp = ref 0;;

let tic_button () =
  let p = not (test_bit plus_button) and m = test_bit minus_button in
  match (p, m, !kind, !counter) with
    | (true, false, Nothing, 0) ->
      mem_wtemp := !wtemp;
      offset_wtemp (-1);
      kind := Plus;
      incr counter;
    | (true, false, Plus, 10) ->
      offset_wtemp (-3);
    | (true, false, Plus, _) ->
      incr counter;

    | (false, true, Nothing, 0) ->
      mem_wtemp := !wtemp;
      offset_wtemp 1;
      kind := Minus;
      incr counter;
    | (false, true, Minus, 10) ->
      offset_wtemp 3;
    | (false, true, Minus, _) ->
      incr counter;

    | (true, true, Nothing, 0) ->
      counter := 1;
      kind := Twice;
    | (true, true, (Plus | Minus), n) ->
      if n > 0 && n < 10 then set_wtemp !mem_wtemp;
      counter := 1;
      kind := Twice;
    | (true, true, Twice, 20) ->
      raise StartStop;
    | (true, true, Twice, _) ->
      incr counter;

    | (_, _, _, n) ->
      if n > 0 then (
        save_wtemp ();
        counter := 0;
        kind := Nothing;
      );
;;

let wait_release () =
  while not (test_bit plus_button) || test_bit minus_button do
    Sys.sleep 50;
  done;
;;

(***)

restore_wtemp ();;
let rec wait_start () = (
  try
    update_ctemp ();
    for i = 1 to 10 do tic_button (); Sys.sleep 50; done;
    wait_start
  with StartStop ->
    disp.moveto 1 0; disp.print_char '\003';
    disp.moveto 1 7; disp.print_char '\004';
    wait_release ();
    wait_stop
) ()
and wait_stop () = (
  try
    update_ctemp ();
    let prop = update_prop () in
    for i = 1 to prop do tic_button (); set_bit output; Sys.sleep 50; done;
    for i = prop to 9 do tic_button (); clear_bit output; Sys.sleep 50; done;
    wait_stop
  with StartStop ->
    clear_bit output;
    reset_prop ();
    disp.moveto 1 0; disp.print_char '\001';
    disp.moveto 1 7; disp.print_char '\002';
    wait_release ();
    wait_start
) ()
in
wait_stop ();;
