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

(** Character type LCD display communication interface. *)

(** Possible sizes of data connection between the PIC and the LCD display *)
type bus_size = Four | Eight

(** Direction of cursor or display shift. *)
type direction = Left | Right

(** Writing mode: cursor or display moving and direction setting. *)
type mode = Cursor_left | Cursor_right | Display_left | Display_right

(** State of the LCD display. *)
type state = On | Off

(** Cursor mode. *)
type cursor = Invisible | Underscore | Block

(** Line mode. *)
type lmode = One | Two

(** Characters font. *)
type font = F5x8 | F5x10

(** Inforamtions about connection between the PIC and the LCD display. *)
module type S =
sig
  val bus_size : bus_size
  (** Size of the bus. *)

  val e : Pic.bit
  (** Pin of the pic connected to the [e] entry of the LCD display. *)

  val rs : Pic.bit
  (** Pin of the pic connected to the [rs] entry of the LCD display. *)

  val rw : Pic.bit
  (** Pin of the pic connected to the [rw] entry of the LCD display. *)

  val bus : Pic.reg
(** Port of the pic connected to the bus of the LCD display.
    In case of 4 bit bus, the 4th highest bits of the port are used. *)
end

(** Connects the PIC to the LCD display.
    Takes informations about connection in the module C. *)
module Connect : functor (C : S) ->
sig
  val clear : unit -> unit
  (** Clear the LCD display. *)

  val home : unit -> unit
  (** Return cursor and display to their initial positions. *)

  (***)

  val init : unit -> unit
  (** Initialise the LCD display.
      Should be called once on the LCD power on. *)

  val config : ?mode:mode -> ?disp:state -> ?cursor:cursor ->
    ?lmode:lmode -> ?font:font -> unit -> unit
  (** Configure the LCD display.
      Should be called after the LCD initialisation. *)

  (***)

  val current_position : unit -> int * int
  (** Get the current position of the cursor as (line, column). *)

  val moveto : int -> int -> unit
  (** moveto line column  moves the cursor at (line, column). *)

  val shift_cursor : direction -> unit
  (** Shift cursor of 1 character. *)

  val shift_display : direction -> unit
  (** Shift display of 1 character. *)

  (***)

  val register_bitmap : char -> int -> int -> int -> unit
  (** Register a new character bitmap. *)

  (***)

  val print_char : char -> unit
  (** Write the character on the LCD display. *)

  val print_string : string -> unit
  (** Write the string on the LCD display at the current cursor position. *)

  val print_int : int -> unit
  (** Write one integer on the LCD display, in decimal. *)

  val output : out_channel
(** Display view as output channel. Can be used as argument of
    Printf.fprintf. *)

end

type display = {
  (** Clears the LCD display. *)
  clear : unit -> unit;

  (** Returns cursor and display to their initial positions. *)
  home : unit -> unit;

  (***)

  (** Initialises the LCD display.
      Should be called once on the LCD power on. *)
  init : unit -> unit;

  (** Configures the LCD display.
      Should be called after the LCD initialisation. *)
  config : ?mode:mode -> ?disp:state -> ?cursor:cursor ->
             ?lmode:lmode -> ?font:font -> unit -> unit;

  (***)

  (** Get the current position of the cursor as (line, column). *)
  current_position : unit -> int * int;

  (** moveto line column  moves the cursor at (line, column). *)
  moveto : int -> int -> unit;

  (** Shifts cursor of 1 character. *)
  shift_cursor : direction -> unit;

  (** Shifts display of 1 character. *)
  shift_display : direction -> unit;

  (***)

  (** Register a new character bitmap. *)
  register_bitmap : char -> int -> int -> int -> unit;

  (***)

  (** Write the character on the LCD display. *)
  print_char : char -> unit;

  (** Write the string on the LCD display at the current cursor position. *)
  print_string : string -> unit;

  (** Write one integer on the LCD display, in decimal. *)
  print_int : int -> unit;

  (** Display view as output channel. Can be used as argument of
      Printf.fprintf. *)
  output : out_channel;
}

val connect : bus_size:bus_size -> e:Pic.bit -> rs:Pic.bit -> rw:Pic.bit ->
  bus:Pic.reg -> display
(** Create a connection to an Lcd display. Offers the same features as
    Connect but in another style. *)
