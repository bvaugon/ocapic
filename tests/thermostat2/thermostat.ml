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

open Pic

module Disp = Lcd.Connect (
  struct
    let bus_size = Lcd.Eight
    let e  = LATC5
    let rs = LATD3
    let rw = LATC4
    let bus = PORTB
  end
)

type button = Number of int | Star | Sharp

let get_button =
  let release_rc1 () = set_bit TRISC1 in
  let release_rc2 () = set_bit TRISC2 in
  let release_rc3 () = set_bit TRISC3 in
  let set_rc1 () = clear_bit TRISC1; set_bit RC1 in
  let set_rc2 () = clear_bit TRISC2; set_bit RC2 in
  let set_rc3 () = clear_bit TRISC3; set_bit RC3 in
  fun () ->
    let result =
      release_rc1 (); release_rc2 (); set_rc3 ();
      if test_bit RC0 then Some (Number 1) else
      if test_bit RE2 then Some (Number 4) else
      if test_bit RE1 then Some (Number 7) else
      if test_bit RE0 then Some Star       else (
        release_rc1 (); release_rc3 (); set_rc2 ();
        if test_bit RC0 then Some (Number 2) else
        if test_bit RE2 then Some (Number 5) else
        if test_bit RE1 then Some (Number 8) else
        if test_bit RE0 then Some (Number 0) else (
          release_rc2 (); release_rc3 (); set_rc1 ();
          if test_bit RC0 then Some (Number 3) else
          if test_bit RE2 then Some (Number 6) else
          if test_bit RE1 then Some (Number 9) else
          if test_bit RE0 then Some Sharp      else
            None)) in
    release_rc1 (); release_rc2 (); release_rc3 ();
    result

let read_temp () =
  write_reg ADCON2 0b10111110;
  write_reg ADCON1 0b00111101;
  write_reg ADCON0 0b00000111;
  while test_bit GO_NOT_DONE do () done;
  (read_reg ADRESH lsl 8) lor read_reg ADRES

let print_temp temp =
  Disp.moveto 2 0;
  Disp.print_int temp

let print_but but =
  Disp.moveto 1 0;
  match but with
  | Some (Number n) -> Disp.print_int n
  | Some Star       -> Disp.print_char '*'
  | Some Sharp      -> Disp.print_char '#'
  | None            -> Disp.print_string "Hello!"

let rec loop old_but old_temp =
  let new_but = get_button () in
  let new_temp = read_temp () in
  if old_but <> new_but || old_temp <> new_temp then (
    Disp.clear ();
    print_but new_but;
    print_temp new_temp;
  );
  if new_but = Some Sharp then clear_bit RA5
  else set_bit RA5;
  Sys.sleep 10;
  loop new_but new_temp

let () =
  set_bit IRCF1;
  set_bit IRCF0;
  set_bit PLLEN;

  Disp.init ();
  Disp.config ();
  Disp.print_string "Starting...";

  write_reg ADCON1 0x0F;
  clear_bit TRISA5;

  loop None 0
