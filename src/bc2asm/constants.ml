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

type gc_algo = Stop_and_copy | Mark_and_compact

let externals_anchor gc_algo =
  match gc_algo with
    | Stop_and_copy    -> 0x1600
    | Mark_and_compact -> 0x1a00

let int32_custom_adr gc_algo = externals_anchor gc_algo - 0x20
let int64_custom_adr gc_algo = externals_anchor gc_algo - 0x10

let atom0_adr = 0xF88
let heap1_anchor = 0
