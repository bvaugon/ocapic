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

let size = 4096
let stack_size = 32

(***)

let get_pre_triggers = Array.make 256 []
let get_post_triggers = Array.make 256 []
let set_pre_triggers = Array.make 256 []
let set_post_triggers = Array.make 256 []
let set_change_triggers = Array.make 256 []
let pre_touch_triggers = Array.make 256 []
let post_touch_triggers = Array.make 256 []

(***)

let pc = ref 0
let ws = ref 0
let statuss = ref 0
let bsrs = ref 0

let ram = Array.make size 0
let stack = Array.make stack_size 0

(***)

let register_get_pre_trigger sfr trig =
  assert (sfr >= Sfr.ofs && sfr < Sfr.ofs + 256);
  get_pre_triggers.(sfr - Sfr.ofs) <- trig :: get_pre_triggers.(sfr - Sfr.ofs)
;;

let register_get_post_trigger sfr trig =
  assert (sfr >= Sfr.ofs && sfr < Sfr.ofs + 256);
  get_post_triggers.(sfr - Sfr.ofs) <- trig :: get_post_triggers.(sfr - Sfr.ofs)
;;

let register_set_pre_trigger sfr trig =
  assert (sfr >= Sfr.ofs && sfr < Sfr.ofs + 256);
  set_pre_triggers.(sfr - Sfr.ofs) <- trig :: set_pre_triggers.(sfr - Sfr.ofs)
;;

let register_set_post_trigger sfr trig =
  assert (sfr >= Sfr.ofs && sfr < Sfr.ofs + 256);
  set_post_triggers.(sfr - Sfr.ofs) <- trig :: set_post_triggers.(sfr - Sfr.ofs)
;;

let register_set_change_trigger sfr trig =
  assert (sfr >= Sfr.ofs && sfr < Sfr.ofs + 256);
  set_change_triggers.(sfr - Sfr.ofs) <-
    trig :: set_change_triggers.(sfr - Sfr.ofs)
;;

let register_pre_touch_trigger sfr trig =
  assert (sfr >= Sfr.ofs && sfr < Sfr.ofs + 256);
  pre_touch_triggers.(sfr - Sfr.ofs) <-
    trig :: pre_touch_triggers.(sfr - Sfr.ofs)
;;

let register_post_touch_trigger sfr trig =
  assert (sfr >= Sfr.ofs && sfr < Sfr.ofs + 256);
  post_touch_triggers.(sfr - Sfr.ofs) <-
    trig :: post_touch_triggers.(sfr - Sfr.ofs)
;;

(***)

let run_change_triggers addr value : int =
  assert (addr >= 0 && addr < size);
  if addr < Sfr.ofs then value else
    let rec f l v =
      match l with
        | trig :: rest -> f rest (trig addr v)
        | [] -> v
    in
    f set_change_triggers.(addr - Sfr.ofs) value
;;

let run_pre_touch_triggers addr =
  assert (addr >= 0 && addr < size);
  if addr >= Sfr.ofs then
    List.iter (fun trig -> trig addr) pre_touch_triggers.(addr - Sfr.ofs);
;;

let run_post_touch_triggers addr =
  assert (addr >= 0 && addr < size);
  if addr >= Sfr.ofs then
    List.iter (fun trig -> trig addr) post_touch_triggers.(addr - Sfr.ofs);
;;

(***)

let rel_addr f =
  assert (f >= 0 && f < 256);
  (ram.(Sfr.bsr) lsl 8) lor f
;;

let stack_addr z =
  assert (z >= 0 && z < 0x60);
  ((ram.(Sfr.fsr2h) lsl 8) lor ram.(Sfr.fsr2l)) + z
;;

(***)

let untrig_aget addr =
  assert (addr >= 0 && addr < size);
  ram.(addr)
;;

let unsafe_aget addr =
  if addr < Sfr.ofs then ram.(addr)
  else
    let sfr = addr - Sfr.ofs in
    let pre_value = ram.(addr) in
    List.iter (fun trig -> trig addr pre_value) get_pre_triggers.(sfr);
    let post_value = ram.(addr) in
    List.iter (fun trig -> trig addr pre_value post_value)
      get_post_triggers.(sfr);
    post_value
;;

let aget addr =
  assert (addr >= 0 && addr < size);
  unsafe_aget addr
;;

let atest_bit addr bit =
  assert (addr >= 0 && addr < size);
  assert (bit >= 0 && bit < 8);
  ((unsafe_aget addr) land (1 lsl bit)) <> 0
;;

(***)

let untrig_aset addr value =
  assert (addr >= 0 && addr < size);
  assert (value >= 0 && value < 256);
  ram.(addr) <- value;
;;

let unsafe_aset addr new_value =
  let old_value = ram.(addr) in
  if addr < Sfr.ofs then ram.(addr) <- new_value
  else
    let sfr = addr - Sfr.ofs in
    List.iter (fun trig -> trig addr old_value new_value)
      set_pre_triggers.(sfr);
    ram.(addr) <- new_value;
    List.iter (fun trig -> trig addr old_value new_value)
      set_post_triggers.(sfr);
;;

let aset addr value =
  assert (addr >= 0 && addr < size);
  assert (value >= 0 && value < 256);
  unsafe_aset addr value;
;;

let aset_bit addr bit =
  assert (addr >= 0 && addr < size);
  assert (bit >= 0 && bit < 8);
  unsafe_aset addr (run_change_triggers addr (ram.(addr) lor (1 lsl bit)));
;;

let aclear_bit addr bit =
  assert (addr >= 0 && addr < size);
  assert (bit >= 0 && bit < 8);
  unsafe_aset addr (run_change_triggers addr
                      (ram.(addr) land (lnot (1 lsl bit))));
;;

(***)

let print oc =
  Array.iteri (
    fun i n ->
      if i mod 16 = 0 then Printf.fprintf oc "%03X:" i;
      Printf.fprintf oc " %02x" n;
      if i mod 16 = 15 then Printf.fprintf oc "\n";
  ) ram;
  Printf.fprintf oc "\n%!";
;;

(***)

let reset () =
  Array.fill ram 0 (Array.length ram) 0;
  ram.(Sfr.intcon2) <- 0b11110101;
  ram.(Sfr.intcon3) <- 0b11000000;
  ram.(Sfr.t0con)   <- 0b11111111;
  ram.(Sfr.osccon)  <- 0b01000000;
  ram.(Sfr.hlvdcon) <- 0b00000101;
  ram.(Sfr.rcon)    <- 0b00011100;
  ram.(Sfr.pr2)     <- 0b11111111;
  ram.(Sfr.cmcon)   <- 0b00000111;
  ram.(Sfr.txsta)   <- 0b00000010;
  ram.(Sfr.ipr2)    <- 0b11011111;
  ram.(Sfr.ipr1)    <- 0b11111111;
  ram.(Sfr.trise)   <- 0b00000111;
  ram.(Sfr.trisd)   <- 0b11111111;
  ram.(Sfr.trisc)   <- 0b11111111;
  ram.(Sfr.trisb)   <- 0b11111111;
  ram.(Sfr.trisa)   <- 0b11111111;
  pc := 0;
;;

reset ();;
