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

type echo =
  | String of string
  | Time
;;

type flags = {
  dump_ram      : bool;
  enable_trace  : bool;
  disable_trace : bool;
  echo_list     : echo list list;
};;

type t = {
  table : string array;
  flags : flags array;
};;

(***)

let parse_echo fname line_ind str =
  let n = String.length str in
  let echo_of_var v = match v with
    | "TIME" -> Time
    | _ ->
      Printf.eprintf "Warning: file %S, at line %d, unknown variable %S\n%!"
        fname line_ind v;
      String ("@" ^ v)
  and sub i j = String.sub str i (j - i) in
  let rec read_string i j =
    if j = n then
      if i = j then [] else [ String (sub i j) ]
    else if str.[j] = '@' then
      let var = read_var (j + 1) (j + 1) in
      if i = j then var else String (sub i j) :: var
    else
      read_string i (j + 1)
  and read_var i j =
    if j = n then [ echo_of_var (sub i j) ] else
      match str.[j] with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> read_var i (j + 1)
        | '@' when i = j -> String "@" :: read_string (j + 1) (j + 1)
        | _ -> echo_of_var (sub i j) :: read_string j j
  in
  read_string 0 0
;;

(***)

let no_flags = {
  dump_ram      = false;
  enable_trace  = false;
  disable_trace = false;
  echo_list     = [];
};;

let search_substring str s =
  let str_len = String.length str and s_len = String.length s in
  let rec f str_ind s_ind =
    if s_ind = s_len then Some str_ind
    else if str_ind = str_len then None
    else if str.[str_ind] = s.[s_ind] then f (str_ind + 1) (s_ind + 1)
    else f (str_ind + 1) 0
  in f 0 0
;;

let contains_substring str s =
  search_substring str s <> None
;;

let compute_flags fname line_ind line flags =
  let dump_ram =
    (!flags).dump_ram      || contains_substring line "OCASIM_DUMP_RAM"
  and enable_trace =
    (!flags).enable_trace  || contains_substring line "OCASIM_ENABLE_TRACE"
  and disable_trace =
    (!flags).disable_trace || contains_substring line "OCASIM_DISABLE_TRACE"
  and echo_list =
    match search_substring line "OCASIM_ECHO" with
      | None -> (!flags).echo_list
      | Some ind ->
        let s = String.sub line ind (String.length line - ind) in
        try
          let str = Scanf.sscanf s " %S" (fun str -> str) in
          let echo = parse_echo fname line_ind str in
          (!flags).echo_list @ [ echo ]
        with _ ->
          Printf.eprintf "Warning: file %S, at line %d, invalid echo:\n%S\n%!"
            fname line_ind line;
          (!flags).echo_list
  in
  if dump_ram || enable_trace || disable_trace || echo_list <> [] then (
    if enable_trace && disable_trace then
      Printf.eprintf
        "Warning: file %S, at line %d, enable and disable trace\n%!"
        fname line_ind;
    flags := {
      dump_ram      = dump_ram;
      enable_trace  = enable_trace;
      disable_trace = disable_trace;
      echo_list     = echo_list;
    };
  );
;;

(***)

let parse_line line lines flags =
  let len = String.length line in
  let is_HEX i =
    match line.[i] with
      | '0' .. '9' | 'A' .. 'Z' -> true
      | _ -> false
  and is_space i = line.[i] = ' ' in
  let rec are_p p i j = if i > j then true else p i && are_p p (i + 1) j in
  let are_HEX i j = are_p is_HEX i j and are_spaces i j = are_p is_space i j in
  if len > 29 && are_HEX 0 3 && is_space 4 && are_HEX 5 8 && is_space 9 &&
    (are_HEX 10 13 || are_spaces 10 13) && is_space 14 &&
    (are_HEX 15 19 || (are_spaces 15 18 && line.[19] = 'M')) &&
    are_spaces 20 28
  then
    let pc2 = Scanf.sscanf (String.sub line 0 4) "%X" (fun pc2 -> pc2) in
    let str = String.sub line 29 (len - 29) in
    let str' = Scanf.sscanf str "%_[ ]%s@\n" (fun res -> res) in
    lines := (pc2, !flags, str') :: !lines;
    flags := no_flags;
;;

(***)

let t_of_lines lines =
  let rec compute_max acc max =
    match acc with
      | [] -> max
      | (pc2, _, _) :: rest when pc2 + 1 > max -> compute_max rest (pc2 + 1)
      | _ :: rest -> compute_max rest max
  in
  let size = compute_max lines 0 in
  let table = Array.make size "" in
  let flags = Array.make size no_flags in
  let fead_table (pc2, fl, str) =
    if table.(pc2) = "" then (
      flags.(pc2) <- fl;
      table.(pc2) <- str;
    );
  in
  List.iter fead_table lines;
  { table = table ; flags = flags }
;;

(***)

let parse fname =
  try
    let ic = open_in fname and lines = ref [] and flags = ref no_flags in
    let line_ind = ref 0 in
    try
      while true do
        let line = input_line ic in
        incr line_ind;
        compute_flags fname !line_ind line flags;
        parse_line line lines flags;
      done;
      None
    with End_of_file ->
      Some (t_of_lines !lines)
  with Sys_error _ ->
    Printf.eprintf "Warning: bad traces, file %S not found\n%!" fname;
    None
;;

(***)

let find_line pc2 lst = match lst with
  | None -> None
  | Some { table = table; flags = _ } ->
    let len = Array.length table in
    if pc2 < 0 || pc2 >= len || table.(pc2) = "" then None else Some table.(pc2)
;;

let find_flags pc2 lst = match lst with
  | None -> no_flags
  | Some { table = _; flags = flags } ->
    let len = Array.length flags in
    if pc2 < 0 || pc2 >= len then no_flags else flags.(pc2)
;;
