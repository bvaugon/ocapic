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

open Instr

let compute_ptrs code =
  let affect_ptr ptr = ptr.pointed <- code.(ptr.old_ofs) in
  Array.iter (fun instr ->
    match instr.Instr.bc with
    | Pushretaddr ptr | Branch ptr | Branchif ptr | Branchifnot ptr
    | Pushtrap ptr
    | Closure (_, ptr)
    | Beq (_, ptr) | Bneq (_, ptr) | Blint (_, ptr) | Bleint (_, ptr)
    | Bgtint (_, ptr) | Bgeint (_, ptr) | Bultint (_, ptr) | Bugeint (_, ptr) ->
      affect_ptr ptr
    | Closurerec (_, _, ptr, tab) ->
      affect_ptr ptr;
      Array.iter affect_ptr tab
    | Switch (_, tab) ->
      Array.iter affect_ptr tab
    | _ ->
      ()
  ) code
;;

let resolve_addr prims_length full_code gc_algo =
  let size = Array.length full_code in
  let rec compute_addr ind curr_addr =
    if ind < size then
      let next_addr = curr_addr + sizeof_bc full_code.(ind).bc in
      full_code.(ind).new_addr <- curr_addr;
      compute_addr (ind + 1) next_addr;
  in
  compute_ptrs full_code;
  compute_addr 0 (Constants.externals_anchor gc_algo + prims_length);
;;

let check code =
  Array.iter (fun instr -> Instr.check_bc instr.bc) code;
;;

let parse code =
  Array.mapi Instr.parse code
;;

let print oc code =
  Array.iter (print_instr oc) code;
  flush oc;
;;

let export oc code =
  let put_byte =
    let cpt = ref 0 in
    fun byte ->
      if !cpt land 0b111 = 0 then
        output_string oc "\n        db      "
      else
        output_string oc ", ";
      Printf.fprintf oc "0x%02x" byte;
      incr cpt;
  in
  output_string oc "caml_bytecode:";
  Array.iter (fun instr -> export_bc put_byte instr.bc) code;
  output_char oc '\n';
;;
