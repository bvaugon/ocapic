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
open Tools

(***)

let bytecode_count_low = ref 0;;
let bytecode_count_high = ref 0;;

let get_bytecode_count () =
  if !bytecode_count_high = 0 then string_of_int !bytecode_count_low else
    let low = Int64.of_int !bytecode_count_low and high = Int64.of_int !bytecode_count_high in
    let n = Int64.add low (Int64.shift_left high (Sys.word_size - 2)) in
    Int64.to_string n
;;

let incr_bytecode_count () =
  let n = !bytecode_count_low + 1 in
  if n > 0 then bytecode_count_low := n else (
    bytecode_count_low := 0;
    incr bytecode_count_high;
  );
;;

(***)

let time_low = ref 0;;
let time_high = ref 0;;

let get_time () =
  if !time_high = 0 then string_of_int !time_low else
    let low = Int64.of_int !time_low and high = Int64.of_int !time_high in
    let n = Int64.add low (Int64.shift_left high (Sys.word_size - 2)) in
    Int64.to_string n
;;

let incr_time () =
  let n = !time_low + 1 in
  if n > 0 then time_low := n else (
    time_low := 0;
    incr time_high;
  );
;;

(***)

let trace = ref false;;
let set_trace b = trace := b;;

let print_instr code lstfile pc =
  if !trace then
    let pc2 = pc * 2 in
    Printf.printf "[%s] 0x%04X:    " (get_time ()) pc2;
    begin match Lstfile.find_line pc2 lstfile with
      | None -> Instr.print stdout code.(pc)
      | Some line -> print_endline line
    end;
    flush stdout;
;;

let print_echo echo =
  List.iter (function
    | Lstfile.String s -> print_string s;
    | Lstfile.Time -> print_string (get_time ())) echo;
  print_newline ();
;;

(***)

let addwf f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let v = Ram.aget src in
  let r = Ram.run_change_triggers dst (v + Ram.aget Sfr.wreg) in
  def_cdczovn v r;
  Ram.aset dst (r land 0xFF);
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let addwfc f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let cval = if test_status c then 1 else 0 in
  let v = Ram.aget src in
  let r = Ram.run_change_triggers dst (v + Ram.aget Sfr.wreg + cval) in
  def_cdczovn v r;
  Ram.aset dst (r land 0xFF);
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let andwf f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let r = Ram.run_change_triggers dst (Ram.aget src land Ram.aget Sfr.wreg) in
  def_zn r;
  Ram.aset dst r;
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let clrf f a =
  let reg = src_addr f a in
  Ram.run_pre_touch_triggers reg;
  set_status z;
  Ram.aset reg (Ram.run_change_triggers reg 0);
  Ram.run_post_touch_triggers reg;
  incr_pc ();
;;

let comf f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let r = Ram.run_change_triggers dst (Ram.aget src lxor 0xFF) in
  def_zn r;
  Ram.aset dst r;
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let cpfseq f a =
  let reg = src_addr f a in
  Ram.run_pre_touch_triggers reg;
  if Ram.aget reg = Ram.aget Sfr.wreg then (incr_time (); add_pc 2)
  else incr_pc ();
  Ram.run_post_touch_triggers reg;
;;

let cpfsgt f a =
  let reg = src_addr f a in
  Ram.run_pre_touch_triggers reg;
  if Ram.aget reg > Ram.aget Sfr.wreg then (incr_time (); add_pc 2)
  else incr_pc ();
  Ram.run_post_touch_triggers reg;
;;

let cpfslt f a =
  let reg = src_addr f a in
  Ram.run_pre_touch_triggers reg;
  if Ram.aget reg < Ram.aget Sfr.wreg then (incr_time (); add_pc 2)
  else incr_pc ();
  Ram.run_post_touch_triggers reg;
;;

let decf f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let v = Ram.aget src in
  let r = Ram.run_change_triggers dst (v - 1) in
  def_cdczovn v r;
  Ram.aset dst (r land 0xFF);
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let decfsz f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let r = Ram.run_change_triggers dst ((Ram.aget src - 1) land 0xFF) in
  Ram.aset dst r;
  if r = 0 then (incr_time (); add_pc 2) else incr_pc ();
  Ram.run_post_touch_triggers src;
;;

let dcfsnz f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let r = Ram.run_change_triggers dst ((Ram.aget src - 1) land 0xFF) in
  Ram.aset dst r;
  if r <> 0 then (incr_time (); add_pc 2) else incr_pc ();
  Ram.run_post_touch_triggers src;
;;

let incf f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let v = Ram.aget src in
  let r = Ram.run_change_triggers dst (v + 1) in
  def_cdczovn v r;
  Ram.aset dst (r land 0xFF);
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let incfsz f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let r = Ram.run_change_triggers dst ((Ram.aget src + 1) land 0xFF) in
  Ram.aset dst r;
  if r = 0 then (incr_time (); add_pc 2) else incr_pc ();
  Ram.run_post_touch_triggers src;
;;

let infsnz f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let r = Ram.run_change_triggers dst ((Ram.aget src + 1) land 0xFF) in
  Ram.aset dst r;
  if r <> 0 then (incr_time (); add_pc 2) else incr_pc ();
  Ram.run_post_touch_triggers src;
;;

let iorwf f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let r = Ram.run_change_triggers dst (Ram.aget src lor Ram.aget Sfr.wreg) in
  def_zn r;
  Ram.aset dst r;
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let movf f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let r = Ram.run_change_triggers dst (Ram.aget src) in
  def_zn r;
  if src <> dst then Ram.aset dst r;
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let movff f1 f2 =
  assert (f2 <> Sfr.pcl);
  assert (f2 <> Sfr.tosu && f2 <> Sfr.tosh && f2 <> Sfr.tosl);
  Ram.run_pre_touch_triggers f1;
  if f1 <> f2 then Ram.run_pre_touch_triggers f2;
  Ram.aset f2 (Ram.run_change_triggers f2 (Ram.aget f1));
  Ram.run_post_touch_triggers f1;
  if f1 <> f2 then Ram.run_post_touch_triggers f2;
  incr_time ();
  add_pc 2;
;;

let movwf f a =
  let reg = src_addr f a in
  Ram.run_pre_touch_triggers reg;
  Ram.aset reg (Ram.run_change_triggers reg (Ram.aget Sfr.wreg));
  Ram.run_post_touch_triggers reg;
  incr_pc ();
;;

let mulwf f a =
  let reg = src_addr f a in
  Ram.run_pre_touch_triggers reg;
  let r = Ram.aget reg * Ram.aget Sfr.wreg in
  Ram.aset Sfr.prodl (r land 0xFF);
  Ram.aset Sfr.prodh ((r lsr 8) land 0xFF);
  Ram.run_post_touch_triggers reg;
  incr_pc ();
;;

let negf f a =
  let reg = src_addr f a in
  Ram.run_pre_touch_triggers reg;
  let v = Ram.aget reg in
  let r = Ram.run_change_triggers reg ((v lxor 0xFF) + 1) in
  def_cdczovn v r;
  Ram.aset reg (r land 0xFF);
  Ram.run_post_touch_triggers reg;
  incr_pc ();
;;

let rlcf f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let r =
    Ram.run_change_triggers dst
      ((Ram.aget src lsl 1) lor (if test_status c then 1 else 0))
  in
  def_czn r;
  Ram.aset dst (r land 0xFF);
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let rlncf f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let v = Ram.aget src in
  let r =
    Ram.run_change_triggers dst
      (((v lsl 1) lor (if v land 0x80 = 0 then 0 else 1)) land 0xFF)
  in
  def_zn r;
  Ram.aset dst r;
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let rrcf f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let v = Ram.aget src in
  let r =
    Ram.run_change_triggers dst
      ((v lsr 1) lor (if test_status c then 0x80 else 0))
  in
  if v land 1 = 0 then clear_status c else set_status c;
  def_zn r;
  Ram.aset dst r;
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let rrncf f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let v = Ram.aget src in
  let r =
    Ram.run_change_triggers dst
      (((v lsr 1) lor (if v land 1 = 0 then 0 else 0x80)))
  in
  def_zn r;
  Ram.aset dst r;
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let setf f a =
  let reg = src_addr f a in
  Ram.run_pre_touch_triggers reg;
  Ram.aset reg (Ram.run_change_triggers reg 0xFF);
  Ram.run_post_touch_triggers reg;
  incr_pc ();
;;

let subfwb f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let v = Ram.aget src in
  let r =
    Ram.run_change_triggers dst
      ((Ram.aget Sfr.wreg) - v - (if test_status c then 0 else 1))
  in
  def_cdczovn' v r;
  Ram.aset dst (r land 0xFF);
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let subwf f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let v = Ram.aget src in
  let r = Ram.run_change_triggers dst (v - (Ram.aget Sfr.wreg)) in
  def_cdczovn' v r;
  Ram.aset dst (r land 0xFF);
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let subwfb f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let v = Ram.aget src in
  let r =
    Ram.run_change_triggers dst
      (v - (Ram.aget Sfr.wreg) - (if test_status c then 0 else 1))
  in
  def_cdczovn' v r;
  Ram.aset dst (r land 0xFF);
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let swapf f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let v = Ram.aget src in
  let r =
    Ram.run_change_triggers dst
      (((v lsl 4) lor (v lsr 4)) land 0xFF)
  in
  Ram.aset dst r;
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let tstfsz f a =
  let reg = src_addr f a in
  Ram.run_pre_touch_triggers reg;
  if Ram.aget reg = 0 then (incr_time (); add_pc 2) else incr_pc ();
  Ram.run_post_touch_triggers reg;
;;

let xorwf f d a =
  let src = src_addr f a in
  Ram.run_pre_touch_triggers src;
  let dst = dst_addr src d in
  let r =
    Ram.run_change_triggers dst (Ram.aget src lxor Ram.aget Sfr.wreg)
  in
  def_zn r;
  Ram.aset dst r;
  Ram.run_post_touch_triggers src;
  incr_pc ();
;;

let bcf f b a =
  let reg = src_addr f a in
  Ram.run_pre_touch_triggers reg;
  Ram.aclear_bit reg b;
  Ram.run_post_touch_triggers reg;
  incr_pc ();
;;

let bsf f b a =
  let reg = src_addr f a in
  Ram.run_pre_touch_triggers reg;
  Ram.aset_bit reg b;
  Ram.run_post_touch_triggers reg;
  incr_pc ();
;;

let btfsc f b a =
  let reg = src_addr f a in
  Ram.run_pre_touch_triggers reg;
  if Ram.atest_bit reg b then incr_pc () else (incr_time (); add_pc 2);
  Ram.run_post_touch_triggers reg;
;;

let btfss f b a =
  let reg = src_addr f a in
  Ram.run_pre_touch_triggers reg;
  if Ram.atest_bit reg b then (incr_time (); add_pc 2) else incr_pc ();
  Ram.run_post_touch_triggers reg;
;;

let btg f b a =
  let reg = src_addr f a in
  Ram.run_pre_touch_triggers reg;
  if Ram.atest_bit reg b then Ram.aclear_bit reg b else Ram.aset_bit reg b;
  Ram.run_post_touch_triggers reg;
  incr_pc ();
;;

let bc ofs =
  if test_status c then (incr_time (); add_pc (ofs + 1)) else incr_pc ();
;;

let bn ofs =
  if test_status n then (incr_time (); add_pc (ofs + 1)) else incr_pc ();
;;

let bnc ofs =
  if test_status c then incr_pc () else (incr_time (); add_pc (ofs + 1));
;;

let bnn ofs =
  if test_status n then incr_pc () else (incr_time (); add_pc (ofs + 1));
;;

let bnov ofs =
  if test_status ov then incr_pc () else (incr_time (); add_pc (ofs + 1));
;;

let bnz ofs =
  if test_status z then incr_pc () else (incr_time (); add_pc (ofs + 1));
;;

let bov ofs =
  if test_status ov then (incr_time (); add_pc (ofs + 1)) else incr_pc ();
;;

let bra ofs =
  incr_time ();
  add_pc (ofs + 1);
;;

let bz ofs =
  if test_status z then (incr_time (); add_pc (ofs + 1)) else incr_pc ();
;;

let call k s =
  stack_push (2 * !Ram.pc + 4);
  incr_time ();
  set_pc k;
  if s = I then store_fast ();
;;

let clrwdt () =
  Ram.aset_bit Sfr.rcon 2; (* set RCON.PD *)
  Ram.aset_bit Sfr.rcon 3; (* set RCON.TO *)
  (* Should reset WDT counter and postscaler *)
  incr_pc ();
;;

let daw () =
  let v = Ram.aget Sfr.wreg in
  let l = v land 0b1111 in
  let h = (v lsr 4) + (if test_status dc then 1 else 0) in
  let nl = if l > 9 || test_status dc then (l + 6) land 0xF else l in
  let nh = if h > 9 || test_status c then (h + 6) land 0xF else h in
  Ram.aset Sfr.wreg ((nh lsl 4) lor nl);
  incr_pc ();
;;

let goto n =
  incr_time ();
  set_pc n;
;;

let nop () =
  incr_pc ();
;;

let pop () =
  ignore (stack_pop ());
;;

let push () =
  stack_push (2 * !Ram.pc + 2);
  incr_pc ();
;;

let rcall n =
  incr_time ();
  stack_push (2 * !Ram.pc + 2);
  set_pc (!Ram.pc + n + 1);
;;

let reset () =
  Ram.reset ();
  incr_pc ();
;;

let retfie s =
  incr_time ();
  set_pc ((stack_pop () land (lnot 1)) / 2);
  (* Should restore GIE *)
  if s = I then restore_fast ();
;;

let retlw k =
  incr_time ();
  Ram.aset Sfr.wreg k;
  set_pc ((stack_pop () land (lnot 1)) / 2);
;;

let return s =
  incr_time ();
  set_pc ((stack_pop () land (lnot 1)) / 2);
  if s = I then restore_fast ();
;;

let sleep () =
  Thread.delay 0.1;
  incr_pc ();
;;

let addlw k =
  let v = Ram.aget Sfr.wreg in
  let r = v + k in
  def_cdczovn v r;
  Ram.aset Sfr.wreg (r land 0xFF);
  incr_pc ();
;;

let andlw k =
  let r = Ram.aget Sfr.wreg land k in
  def_zn r;
  Ram.aset Sfr.wreg r;
  incr_pc ();
;;

let iorlw k =
  let r = Ram.aget Sfr.wreg lor k in
  def_zn r;
  Ram.aset Sfr.wreg r;
  incr_pc ();
;;

let lfsr f k =
  incr_time ();
  set_fsr f k;
  add_pc 2;
;;

let movlb k =
  Ram.aset Sfr.bsr k;
  incr_pc ();
;;

let movlw k =
  Ram.aset Sfr.wreg k;
  incr_pc ();
;;

let mullw k =
  let r = Ram.aget Sfr.wreg * k in
  Ram.aset Sfr.prodl (r land 0xFF);
  Ram.aset Sfr.prodh ((r lsr 8) land 0xFF);
  incr_pc ();
;;

let sublw k =
  let v = Ram.aget Sfr.wreg in
  let r = k - v in
  def_cdczovn' v r;
  Ram.aset Sfr.wreg (r land 0xFF);
  incr_pc ();
;;

let xorlw k =
  let r = Ram.aget Sfr.wreg lxor k in
  def_zn r;
  Ram.aset Sfr.wreg r;
  incr_pc ();
;;

let tblrd program =
  let addr = get_tblptr () in
  Ram.aset Sfr.tablat (read_program program addr);
  incr_time ();
  incr_pc ();
;;

let tblrdpostinc program =
  let addr = get_tblptr () in
  Ram.aset Sfr.tablat (read_program program addr);
  set_tblptr (addr + 1);
  incr_time ();
  incr_pc ();
;;

let tblrdpostdec program =
  let addr = get_tblptr () in
  Ram.aset Sfr.tablat (read_program program addr);
  set_tblptr (addr - 1);
  incr_time ();
  incr_pc ();
;;

let tblrdpreinc program =
  let addr = get_tblptr () + 1 in
  set_tblptr addr;
  Ram.aset Sfr.tablat (read_program program addr);
  incr_time ();
  incr_pc ();
;;

let tblwt program =
  let addr = get_tblptr () in
  write_program program addr (Ram.aget Sfr.tablat);
  incr_time ();
  incr_pc ();
;;

let tblwtpostinc program =
  let addr = get_tblptr () in
  write_program program addr (Ram.aget Sfr.tablat);
  set_tblptr (addr + 1);
  incr_time ();
  incr_pc ();
;;

let tblwtpostdec program =
  let addr = get_tblptr () in
  write_program program addr (Ram.aget Sfr.tablat);
  set_tblptr (addr - 1);
  incr_time ();
  incr_pc ();
;;

let tblwtpreinc program =
  let addr = get_tblptr () + 1 in
  set_tblptr addr;
  write_program program addr (Ram.aget Sfr.tablat);
  incr_time ();
  incr_pc ();
;;

let addfsr f k =
  set_fsr f (get_fsr f + k);
  incr_pc ();
;;

let addulnk k =
  set_fsr 2 (get_fsr 2 + k);
  incr_time ();
  set_pc ((stack_pop () land (lnot 1)) / 2);
;;

let callw () =
  stack_push (2 * !Ram.pc + 2);
  incr_time ();
  set_pc ((Ram.aget Sfr.wreg + 256 * Ram.aget Sfr.pclath +
             65536 * Ram.aget Sfr.pclatu) / 2);
;;

let movsf z f =
  let zreg = get_fsr 2 + z in
  Ram.run_pre_touch_triggers zreg;
  if zreg <> f then Ram.run_pre_touch_triggers f;
  Ram.aset f (Ram.run_change_triggers f (Ram.aget zreg));
  Ram.run_post_touch_triggers zreg;
  if zreg <> f then Ram.run_post_touch_triggers f;
  incr_time ();
  add_pc 2;
;;

let movss z1 z2 =
  let ofs = get_fsr 2 in
  let zreg1 = ofs + z1 in
  let zreg2 = ofs + z2 in
  Ram.run_pre_touch_triggers zreg1;
  if zreg1 <> zreg2 then Ram.run_pre_touch_triggers zreg2;
  Ram.aset zreg2 (Ram.run_change_triggers zreg2 (Ram.aget zreg1));
  Ram.run_post_touch_triggers zreg1;
  if zreg1 <> zreg2 then Ram.run_post_touch_triggers zreg2;
  incr_time ();
  add_pc 2;
;;

let pushl k =
  let addr = get_fsr 2 in
  Ram.run_pre_touch_triggers addr;
  Ram.aset addr (Ram.run_change_triggers addr k);
  Ram.run_post_touch_triggers addr;
  set_fsr 2 (addr - 1);
  incr_pc ();
;;

let subfsr f k =
  set_fsr f (get_fsr f - k);
  incr_pc ();
;;

let subulnk k =
  set_fsr 2 (get_fsr 2 - k);
  incr_time ();
  set_pc ((stack_pop () land (lnot 1)) / 2);
;;

let invalid x =
  let msg = Printf.sprintf "execution of an invalid instruction: 0x%04X" x in
  failwith msg;
;;

let undefined () =
  failwith "execution of undefined code (out of program memory)";
;;

(***)

let run program code lstfile =
  let csize = Array.length code in
  let rec loop () =
    let pc = !Ram.pc in
    if pc < 0 || pc >= csize then failwith "PC overflow";
    let flags = Lstfile.find_flags (2 * pc) lstfile in
    if flags.Lstfile.disable_trace then trace := false;
    if flags.Lstfile.enable_trace then trace := true;
    if flags.Lstfile.dump_ram then Ram.print stdout;
    List.iter print_echo flags.Lstfile.echo_list;
    print_instr code lstfile pc;
    if 2 * pc >= 0x900 && 2 * pc <= 0x900 + 0x7F then incr_bytecode_count ();
    incr_time ();
    begin try match code.(pc) with
      | ADDWF (f, d, a)  -> addwf f d a
      | ADDWFC (f, d, a) -> addwfc f d a
      | ANDWF (f, d, a)  -> andwf f d a
      | CLRF (f, a)      -> clrf f a
      | COMF (f, d, a)   -> comf f d a
      | CPFSEQ (f, a)    -> cpfseq f a
      | CPFSGT (f, a)    -> cpfsgt f a
      | CPFSLT (f, a)    -> cpfslt f a
      | DECF (f, d, a)   -> decf f d a
      | DECFSZ (f, d, a) -> decfsz f d a
      | DCFSNZ (f, d, a) -> dcfsnz f d a
      | INCF (f, d, a)   -> incf f d a
      | INCFSZ (f, d, a) -> incfsz f d a
      | INFSNZ (f, d, a) -> infsnz f d a
      | IORWF (f, d, a)  -> iorwf f d a
      | MOVF (f, d, a)   -> movf f d a
      | MOVFF (f1, f2)   -> movff f1 f2
      | MOVWF (f, a)     -> movwf f a
      | MULWF (f, a)     -> mulwf f a
      | NEGF (f, a)      -> negf f a
      | RLCF (f, d, a)   -> rlcf f d a
      | RLNCF (f, d, a)  -> rlncf f d a
      | RRCF (f, d, a)   -> rrcf f d a
      | RRNCF (f, d, a)  -> rrncf f d a
      | SETF (f, a)      -> setf f a
      | SUBFWB (f, d, a) -> subfwb f d a
      | SUBWF (f, d, a)  -> subwf f d a
      | SUBWFB (f, d, a) -> subwfb f d a
      | SWAPF (f, d, a)  -> swapf f d a
      | TSTFSZ (f, a)    -> tstfsz f a
      | XORWF (f, d, a)  -> xorwf f d a
      | BCF (f, b, a)    -> bcf f b a
      | BSF (f, b, a)    -> bsf f b a
      | BTFSC (f, b, a)  -> btfsc f b a
      | BTFSS (f, b, a)  -> btfss f b a
      | BTG (f, b, a)    -> btg f b a
      | BC n             -> bc n
      | BN n             -> bn n
      | BNC n            -> bnc n
      | BNN n            -> bnn n
      | BNOV n           -> bnov n
      | BNZ n            -> bnz n
      | BOV n            -> bov n
      | BRA n            -> bra n
      | BZ n             -> bz n
      | CALL (k, s)      -> call k s
      | CLRWDT           -> clrwdt ()
      | DAW              -> daw ()
      | GOTO n           -> goto n
      | NOP              -> nop ()
      | POP              -> pop ()
      | PUSH             -> push ()
      | RCALL n          -> rcall n
      | RESET            -> reset ()
      | RETFIE s         -> retfie s
      | RETLW k          -> retlw k
      | RETURN s         -> return s
      | SLEEP            -> sleep ()
      | ADDLW k          -> addlw k
      | ANDLW k          -> andlw k
      | IORLW k          -> iorlw k
      | LFSR (f, k)      -> lfsr f k
      | MOVLB k          -> movlb k
      | MOVLW k          -> movlw k
      | MULLW k          -> mullw k
      | SUBLW k          -> sublw k
      | XORLW k          -> xorlw k
      | TBLRD            -> tblrd program
      | TBLRDPOSTINC     -> tblrdpostinc program
      | TBLRDPOSTDEC     -> tblrdpostdec program
      | TBLRDPREINC      -> tblrdpreinc program
      | TBLWT            -> tblwt program
      | TBLWTPOSTINC     -> tblwtpostinc program
      | TBLWTPOSTDEC     -> tblwtpostdec program
      | TBLWTPREINC      -> tblwtpreinc program
      | ADDFSR (f, k)    -> addfsr f k
      | ADDULNK k        -> addulnk k
      | CALLW            -> callw ()
      | MOVSF (z, f)     -> movsf z f
      | MOVSS (z1, z2)   -> movss z1 z2
      | PUSHL k          -> pushl k
      | SUBFSR (f, k)    -> subfsr f k
      | SUBULNK k        -> subulnk k
      | Invalid x        -> invalid x
      | Undefined        -> undefined ()
      with
        | Failure msg ->
          Printf.eprintf "Error: (pc=0x%X) %s\n" (2 * pc) msg;
          raise Exit;
        | exn ->
          Printf.eprintf "Error: (pc=0x%X)\n" (2 * pc);
          raise exn;
    end;
    loop ();
  in
  loop ()
;;
