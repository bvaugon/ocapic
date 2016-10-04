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

open Tools;;

(* INDF, POSTINC, PREDEC, ... *)

let register (f, regs) =
  let reg_get_trig reg =
    Ram.register_get_pre_trigger reg (
      fun _ _ ->
        let addr = get_fsr f in
        if reg = addr then failwith "cyclic read through FSR";
        let v = Ram.aget addr in
        Ram.untrig_aset reg v;
    );
  and reg_set_trig reg =
    Ram.register_set_post_trigger reg (
      fun _ _ v ->
        let addr = get_fsr f in
        if reg = addr then failwith "cyclic write through FSR";
        Ram.aset addr v;
    );
  in
  List.iter reg_get_trig regs;
  List.iter reg_set_trig regs;
in
List.iter register [
  (0, [ Sfr.indf0 ; Sfr.preinc0 ; Sfr.postinc0 ; Sfr.postdec0 ]);
  (1, [ Sfr.indf1 ; Sfr.preinc1 ; Sfr.postinc1 ; Sfr.postdec1 ]);
  (2, [ Sfr.indf2 ; Sfr.preinc2 ; Sfr.postinc2 ; Sfr.postdec2 ]);
];
;;

let register (f, preinc, postinc, postdec) =
  let inc_trig _ = set_fsr f (get_fsr f + 1) in
  let dec_trig _ = set_fsr f (get_fsr f - 1) in
  Ram.register_pre_touch_trigger preinc inc_trig;
  Ram.register_post_touch_trigger postinc inc_trig;
  Ram.register_post_touch_trigger postdec dec_trig;
in
List.iter register [
  (0, Sfr.preinc0, Sfr.postinc0, Sfr.postdec0);
  (1, Sfr.preinc1, Sfr.postinc1, Sfr.postdec1);
  (2, Sfr.preinc2, Sfr.postinc2, Sfr.postdec2);
]
;;

(* PCL, PCLATH, PCLATU *)

let pcl_trig _ _ new_pcl =
  let n = 65536 * Ram.untrig_aget Sfr.pclatu +
    256 * Ram.untrig_aget Sfr.pclath + new_pcl in
  assert (n >= 0 && n < 65536);
  Tools.set_pc (n / 2);
in
Ram.register_set_post_trigger Sfr.pcl pcl_trig;
;;

(* PORT *)

external pic_read_reg : int -> int = "caml_pic_read_reg";;
external pic_write_reg : int -> int -> unit = "caml_pic_write_reg";;

let register reg =
  Ram.register_get_pre_trigger reg (
    fun _ _ ->
      let new_v = pic_read_reg (reg - Sfr.ofs - 0x80) in
      Ram.untrig_aset reg new_v;
  );
  Ram.register_set_post_trigger reg (
    fun _ _ v ->
      pic_write_reg (reg - Sfr.ofs - 0x80) v;
  );
in
List.iter register [
  Sfr.porta; Sfr.portb; Sfr.portc; Sfr.portd; Sfr.porte;
  Sfr.trisa; Sfr.trisb; Sfr.trisc; Sfr.trisd; Sfr.trise;
  Sfr.adcon0; Sfr.adcon1; Sfr.adcon2; Sfr.adresl; Sfr.adresh;
]
;;

let register lat =
  let port = lat - Sfr.lata + Sfr.porta in
  Ram.register_get_pre_trigger lat (
    fun _ _ -> Ram.untrig_aset lat (Ram.aget port);
  );
  Ram.register_set_post_trigger lat (
    fun _ _ v -> Ram.aset port v;
  );
in
List.iter register [ Sfr.lata; Sfr.latb; Sfr.latc; Sfr.latd; Sfr.late ]
;;

(* EEPROM *)

external eeprom_read : int -> int = "caml_eeprom_read";;
external eeprom_write : int -> int -> unit = "caml_eeprom_write";;

Ram.register_set_post_trigger Sfr.eecon1 (
  fun _ b a ->
    if a land 0x80 = 0 && a land 0x40 = 0 then (
      if a land 0x1 <> 0 && b land 0x1 = 0 then (
        Ram.aset Sfr.eecon1 (a land (lnot 0x1));
        let addr = Ram.aget Sfr.eeadr + 256 * Ram.aget Sfr.eeadrh in
        let v = eeprom_read addr in
        Ram.aset Sfr.eedata v;
      );
      if a land 0x4 <> 0 && a land 0x2 <> 0 && b land 0x2 = 0 then (
        Ram.aset Sfr.eecon1 (a land (lnot 0x2));
        let addr = Ram.aget Sfr.eeadr + 256 * Ram.aget Sfr.eeadrh in
        let v = Ram.aget Sfr.eedata in
        eeprom_write addr v;
      )
    );
);

(* USART *)

Ram.register_set_post_trigger Sfr.txreg (
  fun _ _ v -> output_byte stdout v; flush stdout;
);

Ram.register_get_pre_trigger Sfr.pir1 (
  fun _ _ -> Ram.aset_bit Sfr.pir1 4; Ram.aset_bit Sfr.pir1 5;
);

Ram.register_get_pre_trigger Sfr.txsta (
  fun _ _ -> Ram.aset_bit Sfr.txsta 1;
);

Ram.register_get_pre_trigger Sfr.rcreg (
  fun _ _ -> Ram.aset Sfr.rcreg (input_byte stdin);
);

(* Unimplemented bits *)

let register f mask =
  Ram.register_set_change_trigger f (fun _ v -> v land mask);
in
register Sfr.tosu    0b00011111;
register Sfr.stkptr  0b11011111;
register Sfr.pclatu  0b00011111;
register Sfr.tblptru 0b00111111;
register Sfr.intcon2 0b11110101;
register Sfr.intcon3 0b11011011;
register Sfr.fsr0h   0b00001111;
register Sfr.fsr1h   0b00001111;
register Sfr.bsr     0b00001111;
register Sfr.fsr2h   0b00001111;
register Sfr.status  0b00011111;
register Sfr.hlvdcon 0b10111111;
register Sfr.wdtcon  0b00000001;
register Sfr.rcon    0b11011111;
register Sfr.t2con   0b01111111;
register Sfr.adcon0  0b00111111;
register Sfr.adcon1  0b00111111;
register Sfr.adcon2  0b10111111;
register Sfr.ccp2con 0b00111111;
register Sfr.baudcon 0b11111011;
register Sfr.eeadrh  0b00000011;
register Sfr.eecon1  0b11011111;
register Sfr.ipr2    0b11011111;
register Sfr.pir2    0b11011111;
register Sfr.pie2    0b11011111;
register Sfr.osctune 0b11011111;
register Sfr.trise   0b11110111;
register Sfr.late    0b00000111;
register Sfr.porte   0b00001111;
;;

(* Init Trigs module *)

let init () = ();;
