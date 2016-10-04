(*************************************************************************)
(*                                                                       *)
(*                                OCaPIC                                 *)
(*                                                                       *)
(*                             Benoit Vaugon                             *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../../../LICENSE-en.                                      *)
(*                                                                       *)
(*************************************************************************)

open Printf;;

let usage () =
  eprintf "Usage: %s <file.hex>\n%!" Sys.argv.(0);
  exit 0;
;;

if Array.length Sys.argv <> 2 then usage ();;
let filename = Sys.argv.(1);;
if not (Filename.check_suffix filename ".hex") then usage ();;

try Com.write filename
with Failure msg ->
  eprintf "Error: %s\n%!" msg;
  exit 1;
;;
