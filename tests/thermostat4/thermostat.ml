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

type kind = Nothing | Plus | Minus | Go

(***)

(* 333 -> 37°C *)
(* 140 -> 64°C *)

let max_temp =   56;; (* 80.0°C *)
let def_temp =  131;; (* 64.0°C *)
let min_temp =  733;; (*  2.0°C *)

let output       = RC0;;
let go_button    = RE0;;
let plus_button  = RE2;;
let minus_button = RE1;;

(***)

set_bit IRCF1;;
set_bit IRCF0;;
set_bit PLLEN;;

(***)

let disp = connect ~bus_size:Lcd.Eight ~e:LATD7 ~rs:LATD5 ~rw:LATD6 ~bus:PORTB;;

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
disp.print_string "    \00164.0\000C\002\n     64.0\000C ";;

(***)

let disp_heating is_heating =
  let c = if is_heating then '\255' else ' ' in
  disp.moveto 1  0; for _i = 1 to 3 do disp.print_char c done;
  disp.moveto 2  0; for _i = 1 to 3 do disp.print_char c done;
  disp.moveto 1 13; for _i = 1 to 3 do disp.print_char c done;
  disp.moveto 2 13; for _i = 1 to 3 do disp.print_char c done;
;;

let heat b =
  disp_heating b;
  if b then clear_bit output else set_bit output
;;

clear_bit (tris_of_pin output);;
heat false;;

(***)

let print_str n s =
  disp.moveto n 5;
  disp.print_string s;
;;

let str_of_temp temp =
  if temp < max_temp then "++.+"
  else if temp > min_temp then "--.-"
  else
    let x = float_of_int temp in
    let y = -0.035 *. x +. 50000. /. (1.4 *. x +. 495.) -. 5.2 in
    Printf.sprintf "%4.1f" y
;;

let print_temp n temp =
  print_str n (str_of_temp temp);
;;

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

let prop = ref 5;;
let flag = ref false;;

let update_prop () =
  if !ctemp < !wtemp then (
    if !flag then (
      if !prop < 10 then incr prop;
      flag := false;
    );
    10
  ) else if !ctemp > !wtemp then (
    if !flag then (
      if !prop > 0 then decr prop;
      flag := false;
    );
    0
  ) else (
    flag := true;
    !prop
  )
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
  let g = test_bit go_button in
  let p = test_bit plus_button in
  let m = test_bit minus_button in
  match (g, p, m, !kind, !counter) with
    | (false, true, false, Nothing, 0) ->
      mem_wtemp := !wtemp;
      offset_wtemp (-1);
      kind := Plus;
      incr counter;
    | (false, true, false, Plus, 10) ->
      offset_wtemp (-3);
    | (false, true, false, Plus, _) ->
      incr counter;

    | (false, false, true, Nothing, 0) ->
      mem_wtemp := !wtemp;
      offset_wtemp 1;
      kind := Minus;
      incr counter;
    | (false, false, true, Minus, 10) ->
      offset_wtemp 3;
    | (false, false, true, Minus, _) ->
      incr counter;

    | (true, false, false, Nothing, 0) ->
      counter := 1;
      kind := Go;
    | (true, false, false, (Plus | Minus), n) ->
      if n > 0 && n < 10 then set_wtemp !mem_wtemp;
      counter := 1;
      kind := Go;
    | (true, false, false, Go, 20) ->
      raise StartStop;
    | (true, false, false, Go, _) ->
      incr counter;

    | (_, _, _, _, n) ->
      if n > 0 then (
        save_wtemp ();
        counter := 0;
        kind := Nothing;
      );
;;

let wait_release () =
  while test_bit go_button do
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
    disp.moveto 1  4; disp.print_char '\003';
    disp.moveto 1 11; disp.print_char '\004';
    wait_release ();
    wait_stop
) ()
and wait_stop () = (
  try
    update_ctemp ();
    let prop = update_prop () in
    for i = 1 to prop do tic_button (); heat true; Sys.sleep 50; done;
    for i = prop to 9 do tic_button (); heat false; Sys.sleep 50; done;
    wait_stop
  with StartStop ->
    heat false;
    reset_prop ();
    disp.moveto 1  4; disp.print_char '\001';
    disp.moveto 1 11; disp.print_char '\002';
    wait_release ();
    wait_start
) ()
in
wait_start ();;
