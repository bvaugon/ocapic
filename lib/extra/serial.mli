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

(** Computer Serial library for OCaPIC

    This module can be used by a program running [on a computer] to
    exchange OCaml values with a PIC microcontroller. Do not confuse
    this module with the Serial module running on the PIC (documented
    in manpage section [3p]).
*)

(** Exception raised in case of invalid values transmition or
    communication protocol collision. *)
exception Exn of string

(** Type of a connection. Parameter 'a is the type of the "computer
    to pic" values and 'b is the type of the "pic to computer"
    values. *)
type ('a, 'b) channel

(** [open_tty filename] opens the file [filename] (ex:
    "/dev/tty0") to create a connection to the PIC
    microcontroller. The optionnal parameter [send_delay] defines
    the time (in seconds) to wait before all byte sended (default:
    0.0025). The optionnal parameter [receive_delay] defines the
    time (in seconds) to wait an answer of the PIC before
    resending a value (default: 0.01). These parameters can be
    adapted to improve performances depending on system driver and
    line perturbations. Warning: if [receive_delay] is too small,
    the protocol may loop. *)
val open_tty : ?send_delay:float -> ?receive_delay:float -> string ->
  ('a, 'b) channel

(** Use open_prog instead of open_tty to simulate a serial
    communication to the PIC. [open_prog cmnd] runs the command
    [cmnd] in the shell /bin/sh.  [cmnd] is usually the pic
    executable file. *)
val open_prog : string -> ('a, 'b) channel

(** [send chan value] transmits an OCaml value to the PIC. Raise
    [Serial.Exn "send: collision"] if the PIC tries to send a value
    at the same time. Raise [Serial.Exn msg] if [value] contains
    invalid data as closures, incomptible integers, or unknown
    custom blocks. *)
val send : ('a, 'b) channel -> 'a -> unit

(** Receive an OCaml value from the PIC. Raise [Serial.Exn
    "receive: collision"] if the PIC tries to receive a value at
    the same time. *)
val receive : ('a, 'b) channel -> 'b
