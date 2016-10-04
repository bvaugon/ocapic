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

type bus_size = Four | Eight
type direction = Left | Right
type mode = Cursor_left | Cursor_right | Display_left | Display_right
type state = On | Off
type cursor = Invisible | Underscore | Block
type font = F5x8 | F5x10
type lmode = One | Two

module type S =
sig
  val bus_size : bus_size
  val e : bit
  val rs : bit
  val rw : bit
  val bus : reg
end

module Connect (D : S) =
struct
  open D

  let tris = tris_of_port bus;;

  (***)

  let rec send c =
    if bus_size <> Four then
      begin
        clear_bit rw;
        write_reg tris 0x00;
        write_reg bus c;
        set_bit e;
        clear_bit e;
      end
    else
      begin
        clear_bit rw;
        write_reg tris ((read_reg tris) land 0x0F);
        let oldp = read_reg bus land 0x0F in
        write_reg bus ((c land 0xF0) lor oldp);
        set_bit e;
        clear_bit e;
        write_reg bus (((c lsl 4) land 0xF0) lor oldp);
        set_bit e;
        clear_bit e;
      end

  and receive () =
    if bus_size <> Four then
      begin
        write_reg tris 0xFF;
        set_bit rw;
        set_bit e;
        let res = read_reg bus in
        clear_bit e;
        res
      end
    else
      begin
        write_reg tris ((read_reg tris) lor 0xF0);
        set_bit rw;
        set_bit e;
        let tmp = read_reg bus land 0xF0 in
        clear_bit e;
        set_bit e;
        let res = (read_reg bus lsr 4) lor tmp in
        clear_bit e;
        res
      end

  and send_data d = set_bit rs; send d;

  and send_instr i = clear_bit rs; send i;

  and wait_ready () =
    if bus_size <> Four then write_reg tris 0xFF
    else write_reg tris ((read_reg tris) lor 0xF0);
    clear_bit rs;
    set_bit rw;
    set_bit e;
    while read_reg bus land 0b10000000 <> 0 do () done;
    clear_bit e;
    if bus_size = Four then ( set_bit e; clear_bit e );

  (***)

  and clear () = wait_ready (); send_instr 0b1;

  and home () = wait_ready (); send_instr 0b10;

  (***)

  and init () =
    if bus_size <> Four then
      begin
        clear_bit (tris_of_pin e);
        clear_bit (tris_of_pin rs);
        clear_bit (tris_of_pin rw);
        wait_ready ();
        clear ();
        home ();
      end
    else
      begin
        clear_bit (tris_of_pin e);
        clear_bit (tris_of_pin rs);
        clear_bit (tris_of_pin rw);
        wait_ready ();
        clear_bit rs;
        clear_bit rw;
        write_reg tris ((read_reg tris) land 0x0F);
        write_reg bus (0x20 lor ((read_reg bus) land 0x0F));
        set_bit e;
        clear_bit e;
        clear ();
        home ();
      end

  and config ?(mode=Cursor_right) ?(disp=On) ?(cursor=Invisible)
      ?(lmode=Two) ?(font=F5x8) () =
    wait_ready ();
    send_instr (
      match mode with
        | Cursor_left -> 0b100
        | Cursor_right -> 0b110
        | Display_left -> 0b111
        | Display_right -> 0b101
    );
    wait_ready ();
    send_instr (
      (if disp = On then 0b1100 else 0b1000) lor
        (match cursor with
          | Invisible -> 0b1000
          | Underscore -> 0b1010
          | Block -> 0b1011)
    );
    wait_ready ();
    send_instr (
      (if bus_size <> Four then 0b110000 else 0b100000) lor
        (if lmode = Two then 0b101000 else 0b100000) lor
        (if font = F5x10 then 0b100100 else 0b100000)
    );

  (***)

  and current_position () =
    wait_ready ();
    clear_bit rs;
    let p = receive () land 0b01111111 in
    if p >= 0x54 then (4, p - 0x54) else if p >= 0x40 then (2, p - 0x40)
      else if p >= 0x14 then (3, p - 0x14) else (1, p)

  and moveto line column =
    let ofs = match line with 2 -> 0x40 | 3 -> 0x14 | 4 -> 0x54 | _ -> 0x00 in
    wait_ready ();
    send_instr (0b10000000 lor (ofs + column));

  and shift_cursor d =
    wait_ready ();
    send_instr (if d = Left then 0b10000 else 0b10100);

  and shift_display d =
    wait_ready ();
    send_instr (if d = Left then 0b11000 else 0b11100);

  (***)

  and register_bitmap c u h l =
    let (li, co) = current_position () in
    send_instr (0b1000000 lor (((int_of_char c) land 0b111) lsl 3));
    send_data ((u lsr 5) land 0b11111);
    send_data (u land 0b11111);
    send_data ((h lsr 10) land 0b11111);
    send_data ((h lsr 5) land 0b11111);
    send_data (h land 0b11111);
    send_data ((l lsr 10) land 0b11111);
    send_data ((l lsr 5) land 0b11111);
    send_data (l land 0b11111);
    moveto li co;

  (***)

  and print_char c =
    if c = '\n' then begin
      let (li, _) = current_position () in
      moveto (succ li) 0;
    end else begin
      wait_ready ();
      send_data (int_of_char c);
    end;

  and print_string str =
    for i = 0 to String.length str - 1 do
      print_char str.[i];
    done;

  and print_int i = print_string (string_of_int i);;

  let output : out_channel = Obj.magic print_char;;

end

type display = {
  clear : unit -> unit;
  (** Clear the LCD display. *)
  home : unit -> unit;
  (** Return cursor and display to their initial positions. *)
  (***)
  init : unit -> unit;
  (** Initialise the LCD display.
      Should be called once on the LCD power on. *)
  config : ?mode:mode -> ?disp:state -> ?cursor:cursor ->
             ?lmode:lmode -> ?font:font -> unit -> unit;
  (** Configure the LCD display.
      Should be called after the LCD initialisation. *)
  (***)
  current_position : unit -> int * int;
  (** Get the current position of the cursor as (line, column). *)
  moveto : int -> int -> unit;
  (** moveto line column  moves the cursor at (line, column). *)
  shift_cursor : direction -> unit;
  (** Shift cursor of 1 character. *)
  shift_display : direction -> unit;
  (** Shift display of 1 character. *)
  (***)
  register_bitmap : char -> int -> int -> int -> unit;
  (** Register a new character bitmap. *)
  (***)
  print_char : char -> unit;
  (** Write the character on the LCD display. *)
  print_string : string -> unit;
  (** Write the string on the LCD display at the current cursor position. *)
  print_int : int -> unit;
  (** Write one integer on the LCD display, in decimal. *)
  output : out_channel;
}

let connect ~bus_size ~e ~rs ~rw ~bus =
  let module Disp = Connect (
    struct
      let bus_size = bus_size
      let e = e
      let rs = rs
      let rw = rw
      let bus = bus
    end
  ) in
  {
    clear = Disp.clear;
    home = Disp.home;
    init = Disp.init;
    config = Disp.config;
    current_position = Disp.current_position;
    moveto = Disp.moveto;
    shift_cursor = Disp.shift_cursor;
    shift_display = Disp.shift_display;
    register_bitmap = Disp.register_bitmap;
    print_char = Disp.print_char;
    print_string = Disp.print_string;
    print_int = Disp.print_int;
    output = Disp.output;
  }
