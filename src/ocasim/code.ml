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

open Flash

type t = Instr.t array

let parse { program = program ; config = _ } =
  let psize = Array.length program in
  let csize = psize / 2 in
  let code = Array.make csize Instr.Undefined in
  for i = 0 to csize - 2 do
    let ofs = 2 * i in
    code.(i) <- Instr.parse program.(ofs+1) program.(ofs) program.(ofs+3)
      program.(ofs+2);
  done;
  code.(csize-1) <- Instr.parse program.(2*(csize-1)+1) program.(2*(csize-1))
    (-1) (-1);
  code
;;

let print oc code =
  let f n instr = Printf.fprintf oc "%04x:\t%a" (2 * n) Instr.print instr in
  Array.iteri f code;
;;
