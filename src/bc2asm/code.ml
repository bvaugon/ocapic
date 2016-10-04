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
  let nb_instr = Array.length code in
  let nb_bc = code.(nb_instr - 1).old_addr + 1 in
  let indirect = Array.make nb_bc None in
  let grep_instr instr =
    indirect.(instr.old_addr) <- Some instr;
  in
  let search bc_ind =
    if bc_ind < 0 || bc_ind >= nb_bc then failwith "invalid offset";
    match indirect.(bc_ind) with
      | None -> failwith "invalid offset";
      | Some instr -> instr
  in
  let affect_ptr instr =
    let update_pointed delta ptr =
      let pointed = search (instr.old_addr + delta + ptr.old_ofs) in
      ptr.pointed <- pointed;
    in
    match instr.bc with
      | Pushretaddr ptr | Branch ptr | Branchif ptr | Branchifnot ptr
      | Pushtrap ptr ->
        update_pointed 1 ptr;

      | Closure (_, ptr) | Beq (_, ptr) | Bneq (_, ptr) | Blint (_, ptr)
      | Bleint (_, ptr) | Bgtint (_, ptr) | Bgeint (_, ptr) | Bultint (_, ptr)
      | Bugeint (_, ptr) ->
        update_pointed 2 ptr;

      | Closurerec (_, _, ptr, tab) ->
        update_pointed 3 ptr;
        Array.iter (update_pointed 3) tab;

      | Switch (_, tab) ->
        Array.iter (update_pointed 2) tab

      | _ -> ();
  in
  Array.iter grep_instr code;
  Array.iter affect_ptr code;
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

let parse ic index =
  let (offset, length) =
    try Index.find_section index Index.Code
    with Not_found -> failwith "code section not found"
  in
  seek_in ic offset;
  let cpt = ref 0 in
  let nb_bc = length lsr 2 in
  let read =
    let buf4 = Bytes.create 4 in
    fun () ->
      incr cpt;
      if !cpt > nb_bc then raise End_of_file;
      really_input ic buf4 0 4;
      let res =
        (int_of_char (Bytes.get buf4 0)) lor (int_of_char (Bytes.get buf4 1) lsl 8) lor
          (int_of_char (Bytes.get buf4 2) lsl 16) lor (int_of_char (Bytes.get buf4 3) lsl 24)
      in
      match Sys.word_size with
        | 32 -> res
        | 64 -> (res lsl 32) asr 32
        | ws -> failwith (Printf.sprintf "Unsupported architecture: \
                                          word size is %d" ws)
  in
  let rec f i acc =
    let old_addr = !cpt in
    match Instr.parse read with
      | Some bc ->
        let instr = {
          old_addr = old_addr;
          new_addr = -1;
          bc = bc;
        }
        in
        f (i + 1) (instr :: acc)
      | None -> acc
  in
  let code = Array.of_list (f 0 []) in
  let nb_instr = Array.length code in
  for i = 0 to nb_instr / 2 - 1 do
    let s = nb_instr - i - 1 in
    let tmp = code.(i) in
    code.(i) <- code.(s);
    code.(s) <- tmp;
  done;
  code
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
