(*************************************************************************)
(*                                                                       *)
(*                                OCaPIC                                 *)
(*                                                                       *)
(*                             Benoit Vaugon                             *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../../../LICENSE-en.                                      *)
(*                                                                       *)
(*************************************************************************)

open Pic;;
open Serial;;

(* Pins *)

let clock = RB0;;
let data = RB1;;
let vpp = RB2;;
let vdd = RB3;;

let data_tris = tris_of_pin data;;

(* Constants *)

let load_conf_cmd        = 0b00_00_00;;
let load_prog_cmd        = 0b00_00_10;;
let load_data_cmd        = 0b00_00_11;;
let incr_addr_cmd        = 0b00_01_10;;
let read_prog_cmd        = 0b00_01_00;;
let read_data_cmd        = 0b00_01_01;;
let begin_erase_prog_cmd = 0b00_10_00;;
let begin_prog_cmd       = 0b01_10_00;;
let erase_prog_cmd       = 0b00_10_01;;
let erase_data_cmd       = 0b00_10_11;;
let erase_setup_1_cmd    = 0b00_00_01;;
let erase_setup_2_cmd    = 0b00_01_11;;

let start_time = 1;;
let erase_time = 5;;
let prog_time = 8;;

(* Initialisation *)

set_bit IRCF1;; set_bit IRCF0;; set_bit PLLEN;;
write_reg TRISB 0x00;;

(* Tools *)

let send_bits word bit_nb =
  let rec f word bit_nb =
    if bit_nb <> 0 then (
      if word land 0b1 = 1 then set_bit data else clear_bit data;
      set_bit clock;
      clear_bit clock;
      f (word lsr 1) (bit_nb - 1);
    );
  in
  clear_bit data_tris;
  f word bit_nb;
;;

let receive_bits bit_nb =
  let rec f word mask bit_nb =
    if bit_nb = 0 then word else (
      set_bit clock;
      let b = test_bit data in
      clear_bit clock;
      if b
      then f (word lor mask) (mask lsl 1) (bit_nb - 1)
      else f word (mask lsl 1) (bit_nb - 1)
    )
  in
  set_bit data_tris;
  let res = f 0 1 bit_nb in
  clear_bit data_tris;
  clear_bit data;
  res
;;

let send_cmd cmd = send_bits cmd 6;;
let send_data data = send_bits (data lsl 1) 16;;
let receive_data () = receive_bits 16 lsr 1;;

(* Routines *)

let load_prog word14 =
  send_cmd load_prog_cmd;
  send_data word14;
;;

let load_data word8 =
  send_cmd load_data_cmd;
  send_data word8;
;;

let load_conf () =
  send_cmd load_conf_cmd;
  send_data 0;
;;

let begin_prog () =
  send_cmd begin_prog_cmd;
  Sys.sleep prog_time;
;;

let begin_erase_prog () =
  send_cmd begin_erase_prog_cmd;
  Sys.sleep (erase_time + prog_time);
;;

let incr_addr () =
  send_cmd incr_addr_cmd;
;;

let read_prog () =
  send_cmd read_prog_cmd;
  receive_data ()
;;

let read_data () =
  send_cmd read_data_cmd;
  receive_data () lsr 6
;;

let erase_setup_1 () =
  send_cmd erase_setup_1_cmd;
;;

let erase_setup_2 () =
  send_cmd erase_setup_2_cmd;
;;

let erase_prog () =
  send_cmd erase_prog_cmd;
;;

let erase_data () =
  send_cmd erase_data_cmd;
;;

(* Functions *)

let start () =
  set_bit vpp;
  Sys.sleep start_time;
  set_bit vdd;
  Sys.sleep start_time;
;;

let stop () =
  clear_bit vpp;
  clear_bit vdd;
;;

let offset_address n =
  for i = 1 to n do
    incr_addr ();
  done;
;;

let bulk_erase_program () =
  load_prog 0x3FFF;
  erase_prog ();
  begin_erase_prog ();
  Sys.sleep erase_time;
;;

let bulk_erase_data () =
  load_data 0xFF;
  erase_data ();
  begin_erase_prog ();
  Sys.sleep erase_time;
;;

let disable_code_protection () =
  load_conf ();
  for i = 1 to 7 do incr_addr () done;
  erase_setup_1 ();
  erase_setup_2 ();
  begin_erase_prog ();
  Sys.sleep (erase_time + prog_time);
  erase_setup_1 ();
  erase_setup_2 ();
;;

let write_program prog =
  let f word14 =
    load_prog word14;
    begin_erase_prog ();
    incr_addr ();
  in
  Array.iter f prog
;;

let write_data data =
  let f word8 =
    load_data word8;
    begin_erase_prog ();
    incr_addr ();
  in
  Array.iter f data
;;

let read_program size =
  let prog = Array.make size 0 in
  for i = 0 to size - 1 do
    prog.(i) <- read_prog ();
    incr_addr ();
  done;
  prog
;;

let read_data size =
  let data = Array.make size 0 in
  for i = 0 to size - 1 do
    data.(i) <- read_data ();
    incr_addr ();
  done;
  data
;;

let write_id id1 id2 id3 id4 =
  let f word14 =
    load_prog word14;
    begin_erase_prog ();
    incr_addr ();
  in
  load_conf ();
  f id1; f id2; f id3; f id4;
;;

let read_id () =
  load_conf ();
  let id1 = read_prog () in incr_addr ();
  let id2 = read_prog () in incr_addr ();
  let id3 = read_prog () in incr_addr ();
  let id4 = read_prog () in incr_addr ();
  (id1, id2, id3, id4)
;;

let read_device_id () =
  load_conf ();
  for i = 1 to 6 do incr_addr () done;
  let device_id = read_prog () in incr_addr ();
  device_id
;;

let write_config_word word14 =
  load_conf ();
  for i = 1 to 7 do incr_addr () done;
  load_prog word14;
  begin_erase_prog ();
  incr_addr ();
;;

let read_config_word () =
  load_conf ();
  for i = 1 to 7 do incr_addr () done;
  let config_word = read_prog () in incr_addr ();
  config_word
;;

(* Channel *)

type c2p =
  | START
  | STOP
  | OFFSET_ADDRESS of int
  | BULK_ERASE_PROGRAM
  | BULK_ERASE_DATA
  | DISABLE_CODE_PROTECTION
  | WRITE_PROGRAM of int array
  | WRITE_DATA of int array
  | READ_PROGRAM of int
  | READ_DATA of int
  | WRITE_ID of (int * int * int * int)
  | READ_ID
  | READ_DEVICE_ID
  | WRITE_CONFIG_WORD of int
  | READ_CONFIG_WORD
;;

type p2c =
  | DONE
  | ID of (int * int * int * int)
  | DEVICE_ID of int
  | CONFIG_WORD of int
  | DATA of int array
  | ERROR of string
;;

let (chan : (p2c, c2p) channel) = open_channel 34;;

(* Loop *)

while true do
  try
    match receive chan with
      | START ->
        start ();
        send chan DONE;
      | STOP ->
        stop ();
        send chan DONE;
      | OFFSET_ADDRESS n ->
        offset_address n;
        send chan DONE;
      | BULK_ERASE_PROGRAM ->
        bulk_erase_program ();
        send chan DONE;
      | BULK_ERASE_DATA ->
        bulk_erase_data ();
        send chan DONE;
      | DISABLE_CODE_PROTECTION ->
        disable_code_protection ();
        send chan DONE;
      | WRITE_PROGRAM prog ->
        write_program prog;
        send chan DONE;
      | WRITE_DATA data ->
        write_data data;
        send chan DONE;
      | READ_PROGRAM size ->
        send chan (DATA (read_program size));
      | READ_DATA size ->
        send chan (DATA (read_data size));
      | WRITE_ID (id1, id2, id3, id4) ->
        write_id id1 id2 id3 id4;
        send chan DONE;
      | READ_ID ->
        send chan (ID (read_id ()));
      | READ_DEVICE_ID ->
        send chan (DEVICE_ID (read_device_id ()));
      | WRITE_CONFIG_WORD word14 ->
        write_config_word word14;
        send chan DONE;
      | READ_CONFIG_WORD ->
        send chan (CONFIG_WORD (read_config_word ()));
  with
    | Failure msg -> send chan (ERROR msg)
    | Stack_overflow -> send chan (ERROR "stack overflow")
    | Out_of_memory -> send chan (ERROR "out of memory")
    | _ -> send chan (ERROR "unknown error")
done;;
