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

type hexline =
  | Data of int * int array
  | Eof
  | Extended_SAR of int
  | Start_SAR of int * int
  | Extended_LAR of int
  | Start_LAR of Int32.t

type t = hexline array

let parse_line ind str =
  let len = String.length str in
  let error msg =
    failwith (Printf.sprintf "invalid hex file, line %d: %s" ind msg)
  in
  let check b msg = if not b then error msg in
  let int_of_hexchar c =
    if c >= '0' && c <= '9' then int_of_char c - int_of_char '0'
    else if c >= 'A' && c <= 'F' then int_of_char c - int_of_char 'A' + 10
    else if c >= 'a' && c <= 'f' then int_of_char c - int_of_char 'a' + 10
    else error "invalid hexadecimal character"
  in
  let get_byte i = int_of_hexchar str.[i] * 16 + int_of_hexchar str.[i+1] in
  let rec compute_cs i acc =
    if i = len - 2 then (((acc lxor 0xFF) + 1) land 0xFF)
    else compute_cs (i + 2) (acc + get_byte i)
  in
  check (len >= 10) "line length < 10";
  check (str.[0] = ':') "start code <> ':'";
  let sz = get_byte 1 in
  let addr = get_byte 3 * 256 + get_byte 5 in
  let ty = get_byte 7 in
  let cs = get_byte (len - 2) in
  check (sz * 2 + 11 = len) "invalid byte count";
  check (cs = compute_cs 1 0) "invalid check sum";
  match ty with
    | 0 ->
      let data = Array.init sz (fun i -> get_byte (2 * i + 9)) in
      Data (addr, data)
    | 1 ->
      check (sz = 0 && addr = 0) "invalid EOF";
      Eof
    | 2 ->
      check (sz = 2 && addr = 0) "invalid Extended SAR";
      Extended_SAR (get_byte 9 * 256 + get_byte 11);
    | 3 ->
      check (sz = 4 && addr = 0) "invalid Start SAR";
      Start_SAR (get_byte 9, get_byte 11);
    | 4 ->
      check (sz = 2 && addr = 0) "invalid Extended LAR";
      Extended_LAR (get_byte 9 * 256 + get_byte 11);
    | 5 ->
      check (sz = 4 && addr = 0) "invalid Start LAR";
      let b0 = get_byte 11 and b1 = get_byte 9 and
          b2 = get_byte 15 and b3 = get_byte 13 in
      Start_LAR (Int32.add (Int32.of_int (b0 + 256 * b1 + 65536 * b2))
                   (Int32.mul 16777216l (Int32.of_int b3)))
    | _ -> error "invalid record type";
;;

let parse filename =
  let ic =
    try open_in filename
    with _ -> failwith ("file not found: " ^ filename)
  in
  let rec f ind acc =
    match try Some (input_line ic) with End_of_file -> None with
      | Some line -> f (ind + 1) (parse_line ind line :: acc)
      | None -> acc
  in
  let l = f 0 [] in
  close_in ic;
  Array.of_list (List.rev l)
;;

let print oc hexfile =
  let print_line line =
    begin match line with
      | Data (addr, data) ->
        Printf.fprintf oc "%04x:" addr;
        Array.iter (Printf.fprintf oc " %02X") data;
      | Eof ->
        Printf.fprintf oc "End Of File";
      | Extended_SAR addr ->
        Printf.fprintf oc "Extended Segment Address Record: 0x%X" addr;
      | Start_SAR (cr, ip) ->
        Printf.fprintf oc "Start Segment Address Record: cr = 0x%X, ip = 0x%X"
          cr ip;
      | Extended_LAR addr ->
        Printf.fprintf oc "Extended Linear Address Record: 0x%X" addr;
      | Start_LAR addr ->
        Printf.fprintf oc "Start Linear Address Record: 0x%lX" addr;
    end;
    Printf.fprintf oc "\n";
  in
  Array.iter print_line hexfile
;;

let export oc hexfile =
  let export_line line =
    let cs = ref 0 in
    let print_byte b = Printf.fprintf oc "%02X" b ; cs := !cs + b in
    Printf.fprintf oc ":";
    begin match line with
      | Data (addr, data) ->
        print_byte (Array.length data);
        print_byte (addr lsr 8); print_byte (addr land 0xFF);
        print_byte 0;
        Array.iter print_byte data;
      | Eof ->
        List.iter print_byte [ 0 ; 0 ; 0 ; 1 ];
      | Extended_SAR addr ->
        List.iter print_byte [ 2 ; 0 ; 0 ; 2 ];
        print_byte (addr lsr 8); print_byte (addr land 0xFF);
      | Start_SAR (cr, ip) ->
        List.iter print_byte [ 2 ; 0 ; 0 ; 3 ];
        print_byte cr; print_byte ip;
      | Extended_LAR addr ->
        List.iter print_byte [ 2 ; 0 ; 0 ; 4 ];
        print_byte (addr lsr 8); print_byte (addr land 0xFF);
      | Start_LAR addr ->
        List.iter print_byte [ 4 ; 0 ; 0 ; 5 ];
        print_byte (Int32.to_int
                      (Int32.logand (Int32.shift_right addr 8) 255l));
        print_byte (Int32.to_int
                      (Int32.logand (Int32.shift_right addr 0) 255l));
        print_byte (Int32.to_int
                      (Int32.logand (Int32.shift_right addr 24) 255l));
        print_byte (Int32.to_int
                      (Int32.logand (Int32.shift_right addr 16) 255l));
    end;
    Printf.fprintf oc "%02X\n" (((!cs lxor 0xFF) + 1) land 0xFF);
  in
  Array.iter export_line hexfile
;;
