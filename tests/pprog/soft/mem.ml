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

open Printf
open Hex

type t = {
  program : int array;
  id : (int * int * int * int) option;
  config_word : int option;
}

let id_addr = 0x2000;;
let config_word_addr = 0x2007;;

let iter f hex =
  let hexlen = Array.length hex in
  let dontknow s =
    failwith ("Don't know what to do with an hexfile line " ^ s)
  in
  let esar_error () = dontknow "'Extended Segment Address Record'" in
  let ssar_error () = dontknow "'Start Segment Address Record'" in
  let slar_error () = dontknow "'Start Linear Address Record'" in
  let rec fold i ofs =
    if i = hexlen then
      failwith "Invalid hexfile: no End of file at the end of hexfile";
    match hex.(i) with
      | Extended_LAR addr -> fold (succ i) (addr lsl 16);
      | Data (addr, data) ->
        Array.iteri (fun i d -> f (ofs + addr + i) d) data;
        fold (succ i) ofs;
      | Eof ->
        if i <> pred hexlen then
          failwith "Invalid hexfile: End of file before the end of hexfile";
      | Extended_SAR _ -> esar_error ()
      | Start_SAR (_, _) -> ssar_error ()
      | Start_LAR _ -> slar_error ()
  in
  fold 0 0
;;

let compute_psize hex =
  let psize = ref 0 in
  let f addr _ =
    if addr >= 0 && addr < 2 * id_addr && addr < 2 * config_word_addr then
      let old_psize = !psize in
      let new_psize = addr + 1 in
      if old_psize < new_psize then psize := new_psize;
  in
  iter f hex;
  !psize
;;

let compute_program hex =
  let psize = compute_psize hex in
  let program = Array.make ((psize + 1) / 2) 0 in
  let f addr data =
    if addr >= 0 && addr < psize then
      let ind = addr / 2 in
      if addr land 1 = 1 then program.(ind) <- program.(ind) lor (data lsl 8)
      else program.(ind) <- program.(ind) lor data
  in
  iter f hex;
  program
;;

let compute_id hex =
  let idb = Array.make 8 (-1) in
  let f addr data =
    if addr >= 2 * id_addr && addr < 2 * id_addr + 8 then
      idb.(addr - 2 * id_addr) <- data
  in
  let get_id i =
    let l = idb.(2 * i) and h = idb.(2 * i + 1) in
    if l <> -1 && h <> -1 then l + (h lsl 8)
    else if l = -1 && h = -1 then -1
    else failwith "incomplete id"
  in
  let id1 = get_id 0 and id2 = get_id 1 and id3 = get_id 2 and id4 = get_id 3 in
  iter f hex;
  if id1 <> -1 && id2 <> -1 && id3 <> -1 && id4 <> -1 then
    Some (id1, id2, id3, id4)
  else if id1 = -1 && id2 = -1 && id3 = -1 && id4 = -1 then
    None
  else
    failwith "incomplete id"
;;

let compute_config_word hex =
  let config_word_low = ref (-1) in
  let config_word_high = ref (-1) in
  let f addr data =
    if addr = 2 * config_word_addr then config_word_low := data
    else if addr = 2 * config_word_addr + 1 then config_word_high := data
  in
  iter f hex;
  if !config_word_low <> -1 && !config_word_high <> -1 then
    Some (!config_word_low + (!config_word_high lsl 8))
  else if !config_word_low = -1 && !config_word_high = -1 then
    None
  else
    failwith "incomplete config word"
;;

let parse hex = {
  program = compute_program hex;
  id = compute_id hex;
  config_word = compute_config_word hex;
};;

let print oc {
  program = program;
  id = id;
  config_word = config_word;
} =
  let f i b =
    if b <> -1 then fprintf oc " %04X" b
    else fprintf oc " --";
    if i mod 8 = 7 then fprintf oc "\n";
  in
  fprintf oc "Program:\n";
  Array.iteri f program;
  if Array.length program mod 8 <> 0 then fprintf oc "\n";
  begin match id with
    | Some (id1, id2, id3, id4) ->
      fprintf oc "\nId: 0x%04X 0x%04X 0x%04X 0x%04X\n" id1 id2 id3 id4
    | None -> ()
  end;
  begin match config_word with
    | Some cw -> fprintf oc "\nConfig word: %04X\n" cw
    | None -> ()
  end;
;;
