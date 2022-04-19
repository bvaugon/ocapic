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

type t = string array

module Int = struct type t = int let compare x y = compare x y end
module Premap = Map.Make(Int)

let remap prims code =
  let map = ref Premap.empty in
  let cpt = ref 0 in
  let remap_instr instr =
    match instr.Instr.bc with
      | Instr.Ccall (_, ({ Instr.old_ind = p ; Instr.new_ind = _ } as prim)) ->
        if not (Premap.mem p !map) then (
          map := Premap.add p !cpt !map;
          prim.Instr.new_ind <- !cpt;
          incr cpt;
        ) else
          prim.Instr.new_ind <- Premap.find p !map;
      | _ -> ()
  in
  let () = Array.iter remap_instr code in
  let remap = Array.make !cpt "" in
  if !cpt > 128 then
    failwith (Printf.sprintf "Too much used primitives: %d (>128)" !cpt);
  Premap.iter (fun p r -> remap.(r) <- prims.(p)) !map;
  remap
;;

let parse prims =
  prims
;;

let length remap = Array.length remap * 4

let print_useprim oc remap =
  Array.iter (Printf.fprintf oc "#define caml_useprim_%s\n") remap;
;;

let export gc_algo oc remap =
  Printf.fprintf oc "        org     0x%x\n\
caml_externals:\n\
" (Constants.externals_anchor gc_algo);
  Array.iter (Printf.fprintf oc "        goto    %s\n") remap;
  output_char oc '\n';
;;
