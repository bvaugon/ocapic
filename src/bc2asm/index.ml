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

type section_name = Code | Dlpt | Dlls | Prim | Data | Symb | Crcs | Dbug

type t = (section_name * int * int) list

let parse ic =
  let magic_str1 = "Caml1999X011" in
  let magic_str2 = "Caml1999X023" in
  let magic_size = String.length magic_str1 in
  let file_length = in_channel_length ic in
  let buf_magic = Bytes.create magic_size in
  let buf4 = Bytes.create 4 in
  let read_int offset =
    seek_in ic offset;
    input_binary_int ic
  in
  let read_name offset =
    seek_in ic offset;
    really_input ic buf4 0 4;
    match Bytes.to_string buf4 with
    | "CODE" -> Code
    | "DLPT" -> Dlpt
    | "DLLS" -> Dlls
    | "PRIM" -> Prim
    | "DATA" -> Data
    | "SYMB" -> Symb
    | "CRCS" -> Crcs
    | "DBUG" -> Dbug
    |  name  -> failwith (Printf.sprintf "invalid section name: `%s'" name)
  in
  seek_in ic (file_length - magic_size);
  really_input ic buf_magic 0 magic_size;
  if Bytes.to_string buf_magic <> magic_str1 && Bytes.to_string buf_magic <> magic_str2
  then failwith "invalid bytecode file (magic string does not match)";
  let size = read_int (file_length - magic_size - 4) in
  let rec f ind next_offset rem =
    if ind <> -1 then
      let descr_offset = file_length - magic_size - 4 - (size - ind) lsl 3 in
      let name = read_name descr_offset in
      let length = read_int (descr_offset + 4) in
      let new_offset = next_offset - length in
      f (ind - 1) new_offset ((name, new_offset, length) :: rem)
    else
      rem
  in
  f (size - 1) (file_length - magic_size - 4 - size lsl 3) []
;;

let find_section index name =
  let (_, offset, length) = List.find (fun (n, _, _) -> name = n) index in
  (offset, length)
;;

let print oc index =
  let string_of_sname sname =
    match sname with
      | Code -> "CODE" | Dlpt -> "DLPT" | Dlls -> "DLLS" | Prim -> "PRIM"
      | Data -> "DATA" | Symb -> "SYMB" | Crcs -> "CRCS" | Dbug -> "DBUG"
  in
  let print_descr (name, offset, length) =
    Printf.fprintf oc "%s %8d %8d\n" (string_of_sname name) offset length
  in
  List.iter print_descr index
;;
