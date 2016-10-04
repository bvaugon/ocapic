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

module SMap = Map.Make(String);;

let bindings = ref (SMap.add "XINST" "ON" SMap.empty);;

let add_binding name value =
  let name = String.uppercase_ascii name in
  let value = String.uppercase_ascii value in
  try
    let old_value = SMap.find name !bindings in
    if old_value <> value then
      failwith (Printf.sprintf "Redefinition of configuration binding %S" name);
  with Not_found ->
    bindings := SMap.add name value !bindings;
;;

let register_config s =
  let len = String.length s in
  let error () =
    failwith (Printf.sprintf "Invalid configuration binding: %S" s)
  in
  let rec skip_spaces i =
    if i < len && (s.[i] = ' ' || s.[i] = '\t') then skip_spaces (succ i)
    else i
  in
  let rec skip_word i =
    if i = len then i
    else if ((s.[i] >= 'a' && s.[i] <= 'z') || (s.[i] >= 'A' && s.[i] <= 'Z') ||
                (s.[i] >= '0' && s.[i] <= '9') || s.[i] = '_')
    then skip_word (succ i)
    else if s.[i] = ' ' || s.[i] = '\t' || s.[i] = '=' then i
    else error ()
  in
  let name_beg = skip_spaces 0 in
  if name_beg = len || s.[name_beg] = '=' then error ();
  if s.[name_beg] = '!' then
    let name_beg = skip_spaces (succ name_beg) in
    if name_beg = len then error ();
    let name_end = skip_word name_beg in
    let name = String.sub s name_beg (name_end - name_beg) in
    let str_end = skip_spaces name_end in
    if str_end <> len then error ();
    add_binding name "OFF";
  else
    let name_end = skip_word name_beg in
    let name = String.sub s name_beg (name_end - name_beg) in
    let eq_ind = skip_spaces name_end in
    if eq_ind = len then
      add_binding name "ON"
    else if s.[eq_ind] = '=' then
      let value_beg = skip_spaces (succ eq_ind) in
      if value_beg = len then error ();
      let value_end = skip_word value_beg in
      let str_end = skip_spaces value_end in
      if str_end <> len then error ();
      let value = String.sub s value_beg (value_end - value_beg) in
      add_binding name value;
    else
      error ()
;;

let register_configs s =
  let len = String.length s in
  let rec search_comma i =
    if i = len || s.[i] = ',' then i
    else search_comma (succ i)
  in
  let rec loop i =
    if i < len then (
      let j = search_comma i in
      register_config (String.sub s i (j - i));
      loop (succ j);
    )
  in
  loop 0;
;;

let print_configs oc =
  let print_config name value =
    Printf.fprintf oc "\tconfig\t%s = %s\n" name value
  in
  SMap.iter print_config !bindings;
;;
