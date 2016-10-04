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

open Printf;;

if Array.length Sys.argv <> 2 then (
  eprintf "Usage: %s ttyXX\n" Sys.argv.(0);
  exit 1;
);;

let file = Filename.concat "/dev" Sys.argv.(1);;

let oc = open_out file;;

let send c =
  output_char oc c;
  flush oc;
  for i = 0 to 10000 do () done;
;;

printf "Connected to %s.\n" file;;

while true do
  let l =
    try read_line ()
    with End_of_file -> printf "Bye.\n"; exit 0;
  in
  String.iter send l;
done
