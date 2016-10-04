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

(* Tools *)

let split str =
  let rec f i res =
    try
      let ind = String.index_from str i ' ' in
      let len = ind - i in
      if len <> 0 then
        f (ind + 1) ((String.sub str i len) :: res)
      else
        f (ind + 1) res
    with Not_found ->
      let strlen = String.length str in
      let len = strlen - i in
      if len <> 0 then
        (String.sub str i len) :: res
      else
        res
  in
  List.rev (f 0 [])

(***)

type bit = Unimpl | Unnamed | Named of string
type reg = (string * bit array) option

let bit_of_string str =
  match str with
    | "-" -> Unimpl
    | "+" -> Unnamed
    | _ -> Named str
;;

let reg_of_string str =
  if str = "--" then
    None
  else
    let l = split str in
    if List.length l <> 9 then
      begin
        Printf.eprintf "Syntax error: `%s'\n" str;
        exit 1
      end;
    let bits = Array.make 8 Unimpl in
    for i = 0 to 7 do
      bits.(i) <- bit_of_string (List.nth l (i + 1));
    done;
    Some (List.hd l, bits)
;;

(***)

let main () =
  let tbl = Array.make 0x80 None in
  for i = 0 to 0x7F do
    tbl.(i) <- reg_of_string (read_line ());
  done;
  Printf.printf "type reg =";
  for i = 0 to 0x7F do
    let name =
      match tbl.(i) with
        | None -> Printf.sprintf "Unimpl_%02x" (i + 0x80);
        | Some (name, _) -> name
    in
    if i mod 4 = 0 then Printf.printf "\n";
    Printf.printf " | %s" name;
  done;
  Printf.printf "\
\n\
(** Type of Special Function Registers. *)\n\
\n\
type bit =";
  for i = 0 to 0x407F do
    let print bit =
      if i mod 4 = 0 then Printf.printf "\n";
      match bit with
        | Unimpl -> Printf.printf " | Unimpl_%04x" i;
        | Unnamed -> Printf.printf " | Unnamed_%04x" i;
        | Named s -> Printf.printf " | %s" s;
    in
    let r = i land 0x7F in
    let b = i lsr 7 in
    let nb =
      match b with
        | 0b1 -> 0
        | 0b10 -> 1
        | 0b100 -> 2
        | 0b1000 -> 3
        | 0b10000 -> 4
        | 0b100000 -> 5
        | 0b1000000 -> 6
        | 0b10000000 -> 7
        | _ -> -1
    in
    if nb = -1 then
      print Unimpl
    else
      match tbl.(r) with
        | None -> print Unimpl
        | Some (_, bits) -> print bits.(nb)
  done;
  Printf.printf "\
\n\
(** Type of bits of Special Function Registers. *)\n\
\n\
external read_reg : reg -> int = \"caml_pic_read_reg\";;\n\
(** Reads value of a Special Function Register value.\n    \
Return value between 0 and 255. *)\n\
\n\
external write_reg : reg -> int -> unit = \"caml_pic_write_reg\";;\n\
(** [write_reg reg value] writes value in the Special Function Register reg.\n\
\    value should be between 0 and 255. *)\n\
\n\
external set_bit : bit -> unit = \"caml_pic_set_bit\";;\n\
(** Sets a Special Function Register bit. *)\n\
\n\
external clear_bit : bit -> unit = \"caml_pic_clear_bit\";;\n\
(** Clears a Special Function Register bit. *)\n\
\n\
external test_bit : bit -> bool = \"caml_pic_test_bit\";;\n\
(** Read a Special Function Register bit.\n    \
Return [true] if 1 and [false] if 0. *)\n\
\n\
external tris_of_port : reg -> reg = \"caml_pic_tris_of_port\";;\n\
(** Convertion from a port to its configuration register. *)\n\
\n\
external tris_of_pin : bit -> bit = \"caml_pic_tris_of_port\";;\n\
(** Convertion from a pin to its configuration bit. *)\n\
\n\
external set_interruption_handler : (bit -> unit) -> unit =\n\
\ \ \"caml_set_interruption_handler\";;\n\
(** Register an interruption handler. *)\n\
\n\
external clear_interruption_handler : unit -> unit =\n\
\ \ \"caml_set_interruption_handler\";;\n\
(** Remove the interruption handler. *)\n\
"
;;

main ()
