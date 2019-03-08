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

open Printf

let fsr_nb = 6 * 128;;

let line_counter = ref 0;;
let read_line () =
  incr line_counter;
  read_line ()
;;

let rec search_register_files () =
  let line = read_line () in
  let len = String.length line in
  if len <= 27 || String.sub line 0 27 <> ";----- Register Files -----" then
    search_register_files ()
;;

let search_space line len i =
  let rec f i =
    if i = len then assert false;
    if line.[i] = ' ' || line.[i] = '\t' then i else f (succ i)
  in
  f i
;;

let search_char line len i =
  let rec f i =
    if i = len then assert false;
    if line.[i] <> ' ' && line.[i] <> '\t' then i else f (succ i)
  in
  f i
;;

let check_name name =
  let error () =
    failwith (sprintf "invalid register or bit name: `%s'" name)
  in
  let check_char c =
    match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> ()
      | _ -> error ()
  in
  let len = String.length name in
  if len = 0 then error ();
  String.iter check_char name;
  match name.[0] with
    | 'A' .. 'Z' -> ()
    | _ -> error ()
;;

let parse_def line =
  let len = String.length line in
  let search_space = search_space line len in
  let search_char = search_char line len in
  let check_equ equ = if equ <> "EQU" then assert false in
  let parse_addr str =
    if String.length str <> 7 then assert false;
    if str.[0] <> 'H' || str.[1] <> '\'' || str.[6] <> '\'' then assert false;
    let ioc c =
      if c >= '0' && c <= '9' then int_of_char c - int_of_char '0'
      else if c >= 'A' && c <= 'F' then int_of_char c - int_of_char 'A' + 10
      else if c >= 'a' && c <= 'f' then int_of_char c - int_of_char 'a' + 10
      else assert false
    in
    ioc str.[5] + 16 * ioc str.[4] + 256 * ioc str.[3] + 4096 * ioc str.[2]
  in
  let check_comment start =
    let rec f i =
      if i = len then ()
      else if line.[i] = ';' then ()
      else if line.[i] = ' ' || line.[i] = '\t' then f (succ i)
      else assert false
    in
    f start
  in
  let beg_name = 0 in
  let end_name = search_space beg_name in
  let beg_equ = search_char end_name in
  let end_equ = search_space beg_equ in
  let beg_addr = search_char end_equ in
  let end_addr = beg_addr + 7 in
  let name = String.sub line beg_name (end_name - beg_name) in
  let equ = String.sub line beg_equ (end_equ - beg_equ) in
  let addr = String.sub line beg_addr (end_addr - beg_addr) in
  check_comment end_addr;
  check_name name;
  check_equ equ;
  (name, parse_addr addr)
;;

let parse_regs () =
  search_register_files ();
  let rec f acc =
    let line = read_line () in
    let len = String.length line in
    if len = 0 then f acc
    else if len > 6 && String.sub line 0 6 = ";-----" then (line, acc)
    else if line.[0] = ';' then f acc
    else f (parse_def line :: acc)
  in
  f []
;;

let parse_reg_bits line =
  let len = String.length line in
  if len < 13 then assert false;
  if String.sub line 0 7 <> ";----- " then assert false;
  let rec parse_names beg_rname acc =
    let rec f i =
      if i = len then assert false;
      if line.[i] = ',' then (true, i)
      else if line.[i] = ' ' || line.[i] = '\t' then (false, i)
      else f (succ i)
    in
    let (is_comma, end_rname) = f beg_rname in
    let name = String.sub line beg_rname (end_rname - beg_rname) in
    check_name name;
    if is_comma then (
      if end_rname + 6 < len && String.sub line end_rname 6 = ", and "
      then parse_names (end_rname + 6) (name :: acc)
      else if succ end_rname < len && line.[succ end_rname] = ' ' then
        parse_names (end_rname + 2) (name :: acc)
      else
        assert false
    ) else (
      if end_rname + 5 < len && String.sub line end_rname 5 = " and "
      then parse_names (end_rname + 5) (name :: acc)
      else if end_rname + 5 < len &&
          String.uppercase_ascii (String.sub line end_rname 5) = " BITS"
      then (name :: acc)
      else assert false
    )
  in
  let reg_names = parse_names 7 [] in
  let bits = Array.make 8 None in
  let rec read_bits () =
    let line = read_line () in
    let len = String.length line in
    if len = 0 then read_bits ()
    else if len > 6 && String.sub line 0 6 = ";-----" then Some line
    else if len > 6 && String.sub line 0 6 = ";=====" then None
    else if line.[0] = ';' then read_bits ()
    else
      let (bit_name, bit_addr) = parse_def line in
      check_name bit_name;
      if bit_addr > 7 then assert false;
      begin
        match bits.(bit_addr) with
        | None -> bits.(bit_addr) <- Some bit_name
        | Some _ -> ()
      end;
      read_bits ()
  in
  (reg_names, bits, read_bits ())
;;

let parse_bits line =
  let rec f line acc =
    match line with
      | None -> acc
      | Some l ->
        let (reg_name, bits, next) = parse_reg_bits l in
        f next ((reg_name, bits) :: acc)
  in
  f (Some line) []
;;

let compute_table regs bits =
  let highest_ind = 0x0FFF in
  let lowest_ind = highest_ind - fsr_nb + 1 in
  let htbl = Hashtbl.create fsr_nb in
  let table = Array.make fsr_nb None in
  let add_reg reg ind =
    match table.(ind) with
      | None | Some (_, None) -> table.(ind) <- reg
      | _ -> ()
  in
  let register_reg (name, addr) =
    if addr < lowest_ind || addr > highest_ind then
      failwith (sprintf "invalid register addr: %s @ 0x%04X" name addr);
    let ind = addr - lowest_ind in
    if Hashtbl.mem htbl name then
      failwith (sprintf "duplicate register definition: %s" name);
    Hashtbl.add htbl name ind;
    add_reg (Some (name, None)) ind;
  in
  let register_bits (reg_names, bits) =
    let f reg_name =
      try
        let ind = Hashtbl.find htbl reg_name in
        add_reg (Some (reg_name, Some bits)) ind;
      with Not_found ->
        try
          let n = String.index reg_name 'n' in
          let len = String.length reg_name in
          let pref = String.sub reg_name 0 n in
          let suff = String.sub reg_name (succ n) (len - n - 1) in
          let f name ind acc =
            if String.length name = len && String.sub name 0 n = pref &&
              String.sub name (succ n) (len - n - 1) = suff then
              (name, ind) :: acc
            else
              acc
          in
          let regs = Hashtbl.fold f htbl [] in
          if regs = [] then raise Not_found;
          List.iter (fun (name, ind) -> add_reg (Some (name, Some bits)) ind)
            regs;
        with Not_found ->
          eprintf "Warning: address of register %s unknown\n%!" reg_name;
    in
    List.iter f reg_names;
  in
  List.iter register_reg regs;
  List.iter register_bits bits;
  table;
;;

let overload_bits table =
  let htbl = Hashtbl.create (fsr_nb * 8) in
  let iter_bits f =
    for reg_ind = 0 to pred fsr_nb do
      match table.(reg_ind) with
        | Some (reg_name, Some bits) ->
          let exec_f bit_ind = function
            | Some bit_name -> f reg_name reg_ind bit_name bit_ind
            | None -> ()
          in
          Array.iteri exec_f bits;
        | _ -> ()
    done;
  in
  let register_regs () =
    for reg_ind = 0 to pred fsr_nb do
      match table.(reg_ind) with
        | Some (reg_name, _) -> Hashtbl.add htbl reg_name ()
        | _ -> ()
    done
  in
  let register_bits () =
    let f _ _ bit_name _ =
      Hashtbl.add htbl bit_name ()
    in
    iter_bits f
  in
  let overload_bits () =
    let f reg_name reg_ind bit_name bit_ind =
      match Hashtbl.find_all htbl bit_name with
        | [] -> assert false (* impossible *)
        | [ _ ] -> ()
        | _ ->
          let new_name = sprintf "%s_%s" reg_name bit_name in
          if Hashtbl.mem htbl new_name then
            failwith (sprintf "ambiguous bit overloading: %s" new_name);
          Hashtbl.add htbl new_name ();
          match table.(reg_ind) with
            | Some (_, Some bits) -> bits.(bit_ind) <- Some new_name;
            | _ -> assert false (* impossible *)
    in
    iter_bits f
  in
  register_regs ();
  register_bits ();
  overload_bits ();
;;

let print_table table =
  for i = 0 to 127 do
    match table.(i + 5 * 128) with
      | None -> printf "--\n";
      | Some (reg_name, None) -> printf "%s  - - - - - - - -\n" reg_name;
      | Some (reg_name, Some bits) ->
        let print_bit bit =
          match bit with
            | None -> printf "- "
            | Some bit_name -> printf "%s " bit_name
        in
        printf "%s  " reg_name;
        Array.iter print_bit bits;
        printf "\n";
  done;
;;

try
  let (next, regs) = parse_regs () in
  let bits = parse_bits next in
  let table = compute_table regs bits in
  overload_bits table;
  print_table table;
with
  | Assert_failure(src, line, col) ->
    eprintf "\
Parsing error line %d\n\
Parsing failure: file `%s', line %d, column %d\n\
" !line_counter src line col;
    exit 1;
  | Failure msg ->
    eprintf "Error: line %d: %s\n" !line_counter msg;
    exit 1;
  | End_of_file ->
    eprintf "Parsing error: end of file unexpected\n";
    exit 1;
;;
