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

open Instr

(***)

let src_addr f a =
  if a = O then
    if f < 0x60 then Ram.stack_addr f
    else Sfr.ofs + f
  else Ram.rel_addr f
;;

let dst_addr src d =
  if d = O then Sfr.wreg else src
;;

(***)

let c = 0 and dc = 1 and z = 2 and ov = 3 and n = 4

let set_status bit = Ram.aset_bit Sfr.status bit
let clear_status bit = Ram.aclear_bit Sfr.status bit
let test_status bit = Ram.atest_bit Sfr.status bit

let def_c r =
  if r land 0x100 = 0 then clear_status c else set_status c;
;;

let def_dc v r =
  if v lor 0b1111 = r lor 0b1111 then clear_status dc else set_status dc;
;;

let def_z r =
  if r land 0xFF = 0 then set_status z else clear_status z;
;;

let def_ov v r =
  if v land 0x80 = r land 0x80 then clear_status ov else set_status ov;
;;

let def_n r =
  if r land 0x80 = 0 then clear_status n else set_status n;
;;

let def_cdczovn v r = def_c r; def_dc v r; def_z r; def_ov v r; def_n r;;

let def_zn r = def_z r; def_n r;;

let def_czn r = def_c r; def_n r; def_z r;;

let def_cdczovn' v r =
  def_dc v r; def_z r; def_ov v r; def_n r;
  if r < 0 then clear_status c else set_status c;
;;

(***)

let get_tblptr () =
  Ram.aget Sfr.tblptrl + 256 * Ram.aget Sfr.tblptrh +
    65536 * Ram.aget Sfr.tblptru
;;

let set_tblptr addr =
  assert (addr >= 0 && addr < 0x400000);
  Ram.aset Sfr.tblptrl (addr land 0xFF);
  Ram.aset Sfr.tblptrh ((addr lsr 8) land 0xFF);
  Ram.aset Sfr.tblptru (addr lsr 16);
;;

(***)

let read_program program addr =
  (try assert (addr >= 0 && addr < Array.length program) with exn -> Printf.printf "addr = %d, len = %d\n" addr (Array.length program); raise exn);
  let value = program.(addr) in
  assert (value >= 0 && value < 256);
  value
;;

let write_program program addr value =
  assert (addr >= 0 && addr < Array.length program);
  assert (value >= 0 && value < 256);
  program.(addr) <- value;
;;

(***)

let get_fsr f =
  match f with
    | 0 -> Ram.aget Sfr.fsr0l + 256 * Ram.aget Sfr.fsr0h
    | 1 -> Ram.aget Sfr.fsr1l + 256 * Ram.aget Sfr.fsr1h
    | 2 -> Ram.aget Sfr.fsr2l + 256 * Ram.aget Sfr.fsr2h
    | _ -> assert false
;;

let set_fsr f v =
  let l = v land 0xFF and h = (v lsr 8) land 0x0F in
  assert (v < 65536);
  match f with
    | 0 -> Ram.aset Sfr.fsr0l l; Ram.aset Sfr.fsr0h h;
    | 1 -> Ram.aset Sfr.fsr1l l; Ram.aset Sfr.fsr1h h;
    | 2 -> Ram.aset Sfr.fsr2l l; Ram.aset Sfr.fsr2h h;
    | _ -> assert false
;;

(***)

let set_pc n =
  assert (n >= 0 && n < 32768);
  Ram.untrig_aset Sfr.pcl ((2 * n) land 0xFF);
  Ram.pc := n;
;;

let add_pc n = set_pc (!Ram.pc + n);;

let incr_pc () = add_pc 1;;

(***)

let store_fast () =
  Ram.ws := Ram.aget Sfr.wreg;
  Ram.statuss := Ram.aget Sfr.status;
  Ram.bsrs := Ram.aget Sfr.bsr;
;;

let restore_fast () =
  Ram.aset Sfr.wreg !Ram.ws;
  Ram.aset Sfr.status !Ram.statuss;
  Ram.aset Sfr.bsr !Ram.bsrs;
;;

(***)

let set_tos k =
  assert (k >= 0 && k < 65536);
  Ram.aset Sfr.tosl (k land 0xFF);
  Ram.aset Sfr.tosh ((k lsr 8) land 0xFF);
;;

let stack_push k =
  assert (k >= 0 && k < 1048576 && k mod 2 = 0);
  let n = Ram.aget Sfr.stkptr in
  if n >= Ram.stack_size then failwith "PIC stack overflow";
  if n < 0 then failwith "PIC stack undeflow";
  Ram.stack.(n) <- k;
  set_tos k;
  Ram.aset Sfr.stkptr (n + 1);
;;

let stack_pop () =
  let n = Ram.aget Sfr.stkptr - 1 in
  if n >= Ram.stack_size then failwith "PIC stack overflow";
  if n < 0 then failwith "PIC stack underflow";
  let k = Ram.stack.(n) in
  set_tos k;
  Ram.aset Sfr.stkptr n;
  k
;;
