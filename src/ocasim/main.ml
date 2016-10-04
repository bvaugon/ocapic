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

let error msg =
  Printf.eprintf "%s\n%!" msg;
  exit 1;
;;

let usage () =
  error (Printf.sprintf "Usage: %s <file.hex> [ components ... ]" Sys.argv.(0));
;;

let hexname =
  match Array.to_list Sys.argv with
    | [] | [ _ ] -> usage ()
    | _ :: fname :: rest ->
      if List.mem "-trace" rest then Interp.set_trace true;
      fname
;;

if not (Filename.check_suffix hexname ".hex") then usage ();;

Trigs.init ();;

try
  let hexname = Sys.argv.(1) in
  let lstname = Filename.chop_suffix hexname ".hex" ^ ".lst" in
  let hexfile = Hexfile.parse hexname in
  let lstfile = Lstfile.parse lstname in
  let flash = Flash.parse hexfile in
  let code = Code.parse flash in
  Interp.run flash.Flash.program code lstfile;
with Sys_error msg | Failure msg -> error ("Error: " ^ msg);;
