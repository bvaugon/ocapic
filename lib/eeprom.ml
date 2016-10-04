(*************************************************************************)
(*                                                                       *)
(*                                OCaPIC                                 *)
(*                                                                       *)
(*                             Benoit Vaugon                             *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../LICENSE-en.                                            *)
(*                                                                       *)
(*************************************************************************)

external get_size : unit -> int = "caml_eeprom_get_size"

external unsafe_read : int -> int = "caml_eeprom_read"

external unsafe_write : int -> int -> unit = "caml_eeprom_write"

let size = get_size ()

let read addr =
  if addr < 0 || addr >= size then invalid_arg "Eeprom.read";
  unsafe_read addr
;;

let write addr value =
  if addr < 0 || addr >= size then invalid_arg "Eeprom.write";
  unsafe_write addr value;
;;

external refresh : unit -> unit = "caml_eeprom_refresh"

let fill ofs len value =
  if ofs < 0 || len < 0 || ofs > size - len then invalid_arg "Eeprom.fill";
  for adr = ofs to pred (ofs + len) do
    unsafe_write adr value;
  done;
;;

let iter ofs len f =
  if ofs < 0 || len < 0 || ofs > size - len then invalid_arg "Eeprom.iter";
  for adr = ofs to pred (ofs + len) do
    f (unsafe_read adr);
  done;
;;

let iteri ofs len f =
  if ofs < 0 || len < 0 || ofs > size - len then invalid_arg "Eeprom.iteri";
  for adr = ofs to pred (ofs + len) do
    f adr (unsafe_read adr);
  done;
;;

let read_array ofs len =
  if ofs < 0 || len < 0 || ofs > size - len || len > Sys.max_array_length
  then invalid_arg "Eeprom.read_array";
  let tbl = Array.make len 0 in
  for i = 0 to pred len do
    tbl.(i) <- unsafe_read (ofs + i);
  done;
  tbl
;;

let write_array ofs tbl =
  let len = Array.length tbl in
  if ofs < 0 || ofs > size - len then invalid_arg "Eeprom.write_array";
  for i = 0 to pred len do
    unsafe_write (ofs + i) tbl.(i);
  done
;;

let read_string ofs len =
  if ofs < 0 || len < 0 || ofs > size - len || len > Sys.max_string_length
  then invalid_arg "Eeprom.read_string";
  let str = Bytes.make len ' ' in
  for i = 0 to pred len do
    Bytes.set str i (char_of_int (unsafe_read (ofs + i)));
  done;
  Bytes.unsafe_to_string str
;;

let write_string ofs str =
  let len = String.length str in
  if ofs < 0 || ofs > size - len then invalid_arg "Eeprom.write_string";
  for i = 0 to pred len do
    unsafe_write (ofs + i) (int_of_char str.[i]);
  done
;;
