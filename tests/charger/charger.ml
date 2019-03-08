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

(* Faster clock *)

set_bit IRCF1;;
set_bit IRCF0;;
set_bit PLLEN;;

(* Connection to LCD *)

let disp = connect ~bus_size:Lcd.Four ~e:LATD0 ~rs:LATD2 ~rw:LATD1 ~bus:PORTB;;

(* Configure ports *)

write_reg TRISC 0b01111111;;
clear_bit RC7;;

(* Configure display *)

disp.init ();;
disp.config ();;
disp.moveto 1 5;;
disp.print_string "Hello !";;
Sys.sleep 2000;;

(* Configure A/C converter *)

write_reg ADCON2 0b10111110;;
write_reg ADCON1 0b00001101;;

(* Configure TIMER0 *)

write_reg T0CON 0b10000111;;

(* Read analog value *)

let analog_read n =
  write_reg ADCON0 (0b00000011 lor (n lsl 2));
  while test_bit GO_NOT_DONE do () done;
  (read_reg ADRESH lsl 8) lor read_reg ADRES
;;

let read_tens =
  let mtens = ref 450 in (* 12.60V *)
  fun () ->
    let tens = analog_read 0 in
    let new_tens = (!mtens + tens) lsr 1 in
    mtens := new_tens;
    new_tens
;;

let read_curr =
  let mcurr = ref 0 in (* 0.0A *)
  fun () ->
    let curr = analog_read 1 in
    let new_curr = (!mcurr + curr) lsr 1 in
    mcurr := new_curr;
    new_curr
;;

let fix_tens curr tens = (* Assumes parasit resistor of 0.04 Ohm *)
  tens + ((curr / 5) lsl 1) (* 2 / 5  =  (0.04 * 200) / 20 *)
;;

(* Charge counter *)

let charge_low = ref 0;;
let charge_cnt = ref 0;;
let update_charge curr =
  let n = curr + !charge_low in
  if n > 7373 then ( (* 7373 = 0.1 / (50 / 1024 / 3600) *)
    charge_low := n - 7373;
    incr charge_cnt;
  ) else (
    charge_low := n;
  )
;;

(* Printing tools *)

let print_prop =
  let mprop = ref (-1) in
  fun prop ->
    if !mprop <> prop then (
      disp.moveto 1 13;
      mprop := prop;
      disp.print_int prop;
      disp.print_string "%  ";
    )
;;

let print_curr =
  let mcurr = ref (-1) in
  fun curr ->
    if !mcurr <> curr then (
      disp.moveto 2 13;
      mcurr := curr;
      let icurr = curr / 21 in
      let pcurr = min ((curr - icurr * 21) / 4) 9 in
      disp.print_int icurr;
      disp.print_char '.';
      disp.print_int pcurr;
      disp.print_string "A ";
    )
;;

let print_tens =
  let mtens = ref (-1) in
  fun tens ->
    if !mtens <> tens then (
      disp.moveto 3 13;
      mtens := tens;
      match tens with
        | 0    -> disp.print_string "[+]   "
        | 1023 -> disp.print_string "[-]   "
        | _ ->
          let rtens = tens + 43 in
          let itens = 10 + (1023 - rtens) / 205 in
          let ptens = min ((1023 - rtens - 205 * (itens - 10)) / 2) 99 in
          disp.print_int itens;
          disp.print_char '.';
          if ptens < 10 then disp.print_char '0';
          disp.print_int ptens;
          disp.print_char 'V';
    )
;;

let print_charge =
  let mcharge = ref (-1) in
  fun () ->
    let charge = !charge_cnt in
    if !mcharge <> charge then (
      disp.moveto 4 13;
      mcharge := charge;
      let icharge = charge / 10 in
      let pcharge = charge - icharge * 10 in
      disp.print_int icharge;
      disp.print_char '.';
      disp.print_int pcharge;
      disp.print_string "Ah";
    )
;;

(* Initialize display *)

disp.clear ();;
disp.moveto 1 0;;
disp.print_string "Proportion : ";;
print_prop 0;;    (* 0% *)

disp.moveto 2 0;;
disp.print_string "Courant    : ";;
print_curr 0;;    (* 0.0A *)

disp.moveto 3 0;;
disp.print_string "Tension    : ";;
print_tens 450;;  (* 12.60V *)

disp.moveto 4 0;;
disp.print_string "Charge     : ";;
print_charge ();; (* 0.0Ah *)

(* Control tools *)

let reset_timer0 () =
  write_reg TMR0H 0;
  write_reg TMR0 0;
;;

let test_timer0 () =
  ignore (read_reg TMR0);
  read_reg TMR0H < 122    (* 122 = 8000000 / 2^16 *)
;;

let wait_tic () =
  while not (test_bit RC3) do () done;
  while test_bit RC3 do () done;
;;

let cycle prop =
  match prop with
    | 100 ->
      set_bit RC7;
      while test_timer0 () do wait_tic () done;
    | _ ->
      let rec f i cnt =
        if test_timer0 () then
          if cnt * 100 < prop * i then (
            set_bit RC7;
            wait_tic ();
            f (i + 1) (cnt + 1)
          ) else (
            clear_bit RC7;
            wait_tic ();
            f (i + 1) cnt
          )
      in
      f 0 0;
;;

let rec loop prop =
  if not (test_bit RC6) then (
    clear_bit RC7;
    disp.moveto 1 0;
    disp.print_string "    DISJONCTION     ";
    while true do Sys.sleep 1000 done;
  ) else (
    reset_timer0 ();
    let curr = read_curr () in
    print_curr curr;
    update_charge curr;
    print_charge ();
    let tens = fix_tens curr (read_tens ()) in
    print_tens tens;
    let ofs =
      if tens > 170      then (* tens < 13.95V *) min ((tens - 70) / 100) 5
      else if tens > 150 then (* tens < 14.05V *) 0
      else                    (* tens > 14.05V *) (tens - 160) / 10
    in
    let new_prop = min (max (prop + ofs) 0) 100 in
    print_prop new_prop;
    cycle new_prop;
    loop new_prop;
  )
in
loop 0;
;;
