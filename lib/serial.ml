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

type ('a, 'b) channel

external open_channel : int -> ('a, 'b) channel =
    "caml_serial_open_channel"

external send : ('a, 'b) channel -> 'a -> unit =
    "caml_serial_send"

external receive : ('a, 'b) channel -> 'b =
    "caml_serial_receive"
