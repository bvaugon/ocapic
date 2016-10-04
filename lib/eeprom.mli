(*************************************************************************)
(*                                                                       *)
(*                                OCaPIC                                 *)
(*                                                                       *)
(*                             Benoit Vaugon                             *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../LICENSE-en.                                            *)
(*                                                                       *)
(*************************************************************************)

val size : int
(** Size of the EEPROM in byte. *)

val read : int -> int
(** Read a byte in the EEPROM at a given address.  Raise
    [Invalid_argument "Eeprom.read"] if the address is outside the
    range 0 to [size]. *)

val write : int -> int -> unit
(** [write addr value] writes the byte [value] at the address [addr]
    in the EEPROM. The integer [value] is written modulo 256. Raise
    [Invalid_argument "Eeprom.write"] if the address is outside the
    range 0 to [size]. Raise [Failure "Eeprom.write"] if an error
    occur during the write operation. *)

external refresh : unit -> unit = "caml_eeprom_refresh"
(** Refresh the EEPROM (rewrite all values). *)

val fill : int -> int -> int -> unit
(** [fill ofs len value] writes [value] in the EEPROM between
    addresses [ofs] and [ofs + len - 1]. The integer [value] is
    written modulo 256. Raise [Invalid_argument "Eeprom.fill"] if
    [ofs] or [len] defines invalid addresses in the EEPROM. *)

val iter : int -> int -> (int -> unit) -> unit
(** [iter ofs len f] applies function [f] on all content of the
    EEPROM between addresses [ofs] and [ofs + len - 1]. Raise
    [Invalid_argument "Eeprom.iter"] if [ofs] or [len] defines invalid
    addresses in the EEPROM. *)

val iteri : int -> int -> (int -> int -> unit) -> unit
(** Same as {!Eeprom.iter} but the function is applied to the
    address as first argument, and the EEPROM content as second
    argument. *)

val read_array : int -> int -> int array
(** [read_array ofs len] creates an array of size [len] and fill it
    with the EEPROM content from address [ofs] to [ofs + len -
    1]. Raise [Invalid_argument "Eeprom.to_array"] if [ofs] or [len]
    defines invalid addresses in the EEPROM or if [len] is greater
    than the maximum size of an array. *)

val write_array : int -> int array -> unit
(** [write_array ofs tbl] writes the [tbl] content in the EEPROM
    between addresses [ofs] to [ofs + {!Array.length} tbl -
    1]. Integers are written modulo 256. Raise [Invalid_argument
    "Eeprom.write_array"] if [ofs] or [len] defines invalid addresses in
    the EEPROM. *)

val read_string : int -> int -> string
(** [read_string ofs len] creates a string of size [len] and fill it
    with the EEPROM content from address [ofs] to [ofs + len -
    1]. Raise [Invalid_argument "Eeprom.read_string"] if [ofs] or
    [len] defines invalid addresses in the EEPROM or if [len] is
    greater than the maximum size of a string. *)

val write_string : int -> string -> unit
(** [write_string ofs str] writes the [str] content in the EEPROM
    between addresses [ofs] to [ofs + {!String.length} str -
    1]. Raise [Invalid_argument "Eeprom.write_string"] if [ofs] or
    [len] defines invalid addresses in the EEPROM. *)
