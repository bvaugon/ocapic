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

open Hexfile

type t = {
  program : int array;
  config : int array;
}

let parse hexfile =
  let len = Array.length hexfile in
  let dontknow s =
    failwith ("Don't know what to do with an hexfile line " ^ s)
  in
  let esar_error () = dontknow "'Extended Segment Address Record'" in
  let ssar_error () = dontknow "'Start Segment Address Record'" in
  let slar_error () = dontknow "'Start Linear Address Record'" in
  let rec compute_sizes i ofs psize csize =
    if i = len then
      failwith "Invalid hexfile: no End of file at the end of hexfile";
    match hexfile.(i) with
      | Extended_LAR addr ->
        compute_sizes (succ i) (addr lsl 16) psize csize
      | Data (addr, data) ->
        let size = ofs + addr + Array.length data in
        if ofs = 0 then
          compute_sizes (succ i) ofs (max size psize) csize
        else if ofs = 0x300000 then
          compute_sizes (succ i) ofs psize (max (size - 0x300000) csize)
        else
          failwith (Printf.sprintf
                      "Invalid hexfile: invalid address (0x%x)" ofs)
      | Eof ->
        if i <> pred len then
          failwith "Invalid hexfile: End of file before the end of hexfile"
        else (psize, csize)
      | Extended_SAR _ -> esar_error ()
      | Start_SAR (_, _) -> ssar_error ()
      | Start_LAR _ -> slar_error ()
  in
  let (psize, csize) = compute_sizes 0 0 0 0 in
  let program = Array.make psize (-1) in
  let config = Array.make csize (-1) in
  let rec fill_arrays i ofs =
    match hexfile.(i) with
      | Extended_LAR addr -> fill_arrays (succ i) (addr lsl 16);
      | Data (addr, data) ->
        if ofs = 0 then
          Array.blit data 0 program (ofs+addr) (Array.length data)
        else
          Array.blit data 0 config (ofs+addr-0x300000) (Array.length data);
        fill_arrays (succ i) ofs;
      | Eof -> ()
      | Extended_SAR _ -> esar_error ()
      | Start_SAR (_, _) -> ssar_error ()
      | Start_LAR _ -> slar_error ()
  in
  fill_arrays 0 0;
  { program = program ; config = config }
;;

let print oc { program = program ; config = config } =
  let f i b =
    if b <> -1 then Printf.fprintf oc " %02X" b
    else Printf.fprintf oc " --";
    if i mod 16 = 15 then Printf.fprintf oc "\n";
  in
  Printf.fprintf oc "Program:\n";
  Array.iteri f program;
  if Array.length program mod 16 <> 0 then Printf.fprintf oc "\n";
  Printf.fprintf oc "Config:\n";
  Array.iteri f config;
  if Array.length config mod 16 <> 0 then Printf.fprintf oc "\n";
;;
