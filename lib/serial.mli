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

(** PIC Serial library

    This module can be used by a program running [on a PIC] to
    exchange OCaml values with a Computer. Do not confuse this module
    with the Serial module running on the computer (documented in
    manpage section [3o]).
*)

(** Type of a connection. Parameter 'a is the type of the "pic to
    computer" values and 'b is the type of the "computer to pic"
    values. *)
type ('a, 'b) channel

(** Open a serial connection to a computer. *)
external open_channel : int -> ('a, 'b) channel =
    "caml_serial_open_channel"

(** Send an OCaml value to a computer through a serial connection.
    The program execution is blocked until the value is received. *)
external send : ('a, 'b) channel -> 'a -> unit =
    "caml_serial_send"

(** Receive an OCaml value from a computer through a serial
    connection.  The program execution is blocked until the value is
    transmitted. *)
external receive : ('a, 'b) channel -> 'b =
    "caml_serial_receive"
