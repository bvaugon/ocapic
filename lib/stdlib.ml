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

module Pervasives = struct

(* Exceptions *)

external raise : exn -> 'a = "%raise"
external raise_notrace : exn -> 'a = "%raise_notrace"

let failwith s = raise(Failure s)
let invalid_arg s = raise(Invalid_argument s)
let invalid_format () = failwith "invalid format"

exception Exit

(* Composition operators *)

external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

(* Debugging *)

external __LOC__ : string = "%loc_LOC"
external __FILE__ : string = "%loc_FILE"
external __LINE__ : int = "%loc_LINE"
external __MODULE__ : string = "%loc_MODULE"
external __POS__ : string * int * int * int = "%loc_POS"

external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"
external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"
external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"

(* Comparisons *)

external (=) : 'a -> 'a -> bool = "%equal"
external (<>) : 'a -> 'a -> bool = "%notequal"
external (<) : 'a -> 'a -> bool = "%lessthan"
external (>) : 'a -> 'a -> bool = "%greaterthan"
external (<=) : 'a -> 'a -> bool = "%lessequal"
external (>=) : 'a -> 'a -> bool = "%greaterequal"
external compare: 'a -> 'a -> int = "%compare"

let min x y = if x <= y then x else y
let max x y = if x >= y then x else y

external (==) : 'a -> 'a -> bool = "%eq"
external (!=) : 'a -> 'a -> bool = "%noteq"

(* Boolean operations *)

external not : bool -> bool = "%boolnot"
external (&) : bool -> bool -> bool = "%sequand"
external (&&) : bool -> bool -> bool = "%sequand"
external (or) : bool -> bool -> bool = "%sequor"
external (||) : bool -> bool -> bool = "%sequor"

(* Integer operations *)

external (~-) : int -> int = "%negint"
external (~+) : int -> int = "%identity"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external (+) : int -> int -> int = "%addint"
external (-) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external (/) : int -> int -> int = "%divint"
external (mod) : int -> int -> int = "%modint"

let abs x = if x >= 0 then x else -x

external (land) : int -> int -> int = "%andint"
external (lor) : int -> int -> int = "%orint"
external (lxor) : int -> int -> int = "%xorint"

let lnot x = x lxor (-1)

external (lsl) : int -> int -> int = "%lslint"
external (lsr) : int -> int -> int = "%lsrint"
external (asr) : int -> int -> int = "%asrint"

let min_int = -16384
let max_int = 16383

(* String operations -- more in module String *)

external string_length : string -> int = "%string_length"
external string_create: int -> string = "caml_create_string"
external string_blit : string -> int -> string -> int -> int -> unit
  = "caml_blit_string" [@@noalloc]
external string_fill : string -> int -> int -> char -> unit
  = "caml_fill_string" [@@noalloc]

let (^) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = string_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  s

let string_sub str start len =
  let res = string_create len in
  string_blit str start res 0 len;
  res

(* Character operations -- more in module Char *)

external int_of_char : char -> int = "%identity"
external unsafe_char_of_int : int -> char = "%identity"
let char_of_int n =
  if n < 0 || n > 255 then invalid_arg "char_of_int" else unsafe_char_of_int n

(* Unit operations *)

external ignore : 'a -> unit = "%ignore"

(* Pair operations *)

external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"

(* String conversion functions *)

let string_of_bool b =
  if b then "true" else "false"
let bool_of_string = function
  | "true" -> true
  | "false" -> false
  | _ -> invalid_arg "bool_of_string"

external string_unsafe_set : string -> int -> char -> unit= "%string_unsafe_set"

let string_of_int n =
  let size =
    if n >= 0 && n < 10 then 1
    else if n > -10 && n < 100 then 2
    else if n > -100 && n < 1000 then 3
    else if n > -1000 && n < 10000 then 4
    else if n > -10000 then 5 else 6
  in
  let str = string_create size in
  let rec f i k =
    if k < 10 || i = 0 then
      string_unsafe_set str i (unsafe_char_of_int (k + int_of_char '0'))
    else begin
      string_unsafe_set str i (unsafe_char_of_int (k mod 10 + int_of_char '0'));
      f (pred i) (k / 10);
    end
  in
  if n >= 0 then
    f (pred size) n
  else if n = min_int then begin
    string_unsafe_set str 0 '-'; string_unsafe_set str 1 '1';
    string_unsafe_set str 2 '6'; string_unsafe_set str 3 '3';
    string_unsafe_set str 4 '8'; string_unsafe_set str 5 '4';
  end else begin
    string_unsafe_set str 0 '-';
    f (pred size) (-n);
  end;
  str
;;

external int_of_string : string -> int = "caml_int_of_string"

external string_get : string -> int -> char = "%string_safe_get"
external string_set : string -> int -> char -> unit = "%string_safe_set"

(* List operations -- more in module List *)

let rec (@) l1 l2 =
  match l1 with
      [] -> l2
    | hd :: tl -> hd :: (tl @ l2)

(* References *)

type 'a ref = { mutable contents: 'a }
external ref: 'a -> 'a ref = "%makemutable"
external (!): 'a ref -> 'a = "%field0"
external (:=): 'a ref -> 'a -> unit = "%setfield0"
external incr: int ref -> unit = "%incr"
external decr: int ref -> unit = "%decr"

(* Result type *)

type ('a,'b) result = Ok of 'a | Error of 'b

(* Formats *)

type ('a, 'b, 'c, 'd, 'e, 'f) format6
   = ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6
   = Format of ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.fmt
               * string

type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6

type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4

let string_of_format (Format (_, str)) = str

external format_of_string :
 ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
 ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"

let ( ^^ ) (Format (fmt1, str1)) (Format (fmt2, str2)) =
  Format (CamlinternalFormatBasics.concat_fmt fmt1 fmt2,
          str1 ^ "%," ^ str2)

(* General output functions *)

type out_channel = char -> unit

let output_char oc = oc;;

let output_string oc str =
  for i = 0 to string_length str - 1 do oc (string_get str i) done;
;;

external unsafe_string_of_bytes : bytes -> string = "%identity"

let output_bytes oc bytes =
  output_string oc (unsafe_string_of_bytes bytes)

let output_substring oc s ofs len =
  if ofs < 0 || len < 0 || ofs > string_length s - len
  then invalid_arg "output_substring";
  for i = ofs to ofs + len - 1 do oc (string_get s i) done;
;;

let output_int oc n = output_string oc (string_of_int n);;

let output oc s ofs len =
  let s = unsafe_string_of_bytes s in
  if ofs < 0 || len < 0 || ofs > string_length s - len
  then invalid_arg "output";
  for i = ofs to ofs + len - 1 do oc (string_get s i) done;
;;

let output_byte oc n = oc (unsafe_char_of_int (n land 0xFF));;

(* Floating-point operations *)

external (~-.) : float -> float = "%negfloat"
external (~+.) : float -> float = "%identity"
external (+.) : float -> float -> float = "%addfloat"
external (-.) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external (/.) : float -> float -> float = "%divfloat"
external abs_float : float -> float = "%absfloat"
external float_of_int : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"

let pow x exp =
  let rec pow_aux x exp =
    if exp = 0 then 1.
    else if exp = 1 then x
    else if exp land 1 = 0 then let x' = pow_aux x (exp lsr 1) in x' *. x'
    else let x' = pow_aux x (exp lsr 1) in x *. x' *. x'
  in
  if exp = min_int then 0.
  else if exp < 0 then 1. /. pow_aux x (-exp)
  else pow_aux x exp
;;

(*
  external ( ** ) : float -> float -> float = "caml_power_float" "pow" "float"
  external exp : float -> float = "caml_exp_float" "exp" "float"
  external acos : float -> float = "caml_acos_float" "acos" "float"
  external asin : float -> float = "caml_asin_float" "asin" "float"
  external atan : float -> float = "caml_atan_float" "atan" "float"
  external atan2 : float -> float -> float = "caml_atan2_float" "atan2" "float"
  external cos : float -> float = "caml_cos_float" "cos" "float"
  external cosh : float -> float = "caml_cosh_float" "cosh" "float"
  external log : float -> float = "caml_log_float" "log" "float"
  external log10 : float -> float = "caml_log10_float" "log10" "float"
  external sin : float -> float = "caml_sin_float" "sin" "float"
  external sinh : float -> float = "caml_sinh_float" "sinh" "float"
  external sqrt : float -> float = "caml_sqrt_float" "sqrt" "float"
  external tan : float -> float = "caml_tan_float" "tan" "float"
  external tanh : float -> float = "caml_tanh_float" "tanh" "float"
  external ceil : float -> float = "caml_ceil_float" "ceil" "float"
  external floor : float -> float = "caml_floor_float" "floor" "float"
  external mod_float : float -> float -> float = "caml_fmod_float" "fmod" "float"
  external frexp : float -> float * int = "caml_frexp_float"
  external ldexp : float -> int -> float = "caml_ldexp_float"
  external modf : float -> float * float = "caml_modf_float"
  external float_of_bits : int64 -> float = "caml_int64_float_of_bits"
  let infinity =
  float_of_bits 0x7F_F0_00_00_00_00_00_00L
  let neg_infinity =
  float_of_bits 0xFF_F0_00_00_00_00_00_00L
  let nan =
  float_of_bits 0x7F_F0_00_00_00_00_00_01L
  let max_float =
  float_of_bits 0x7F_EF_FF_FF_FF_FF_FF_FFL
  let min_float =
  float_of_bits 0x00_10_00_00_00_00_00_00L
  let epsilon_float =
  float_of_bits 0x3C_B0_00_00_00_00_00_00L

  type fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

  val classify_float : float -> fpclass
*)

(**********************************************************************)
(**********************************************************************)
(****************************** Format ********************************)
(**********************************************************************)
(**********************************************************************)

let valid_float_lexem s =
  let l = string_length s in
  let rec loop i =
    if i >= l then s ^ "." else
      match string_get s i with
        | '0' .. '9' | '-' | '+' | ' ' -> loop (i+1)
        | _ -> s
  in
  loop 0
;;

let format_float fmt x =
  let len = string_length fmt in
  let rec read_intp i intp plus minus =
    if i = len then invalid_format ();
    match string_get fmt i with
      | '+' ->
        read_intp (i + 1) intp true minus
      | '-' ->
        read_intp (i + 1) intp plus true
      | '.' ->
        if i + 1 = len then invalid_format () else
          begin match string_get fmt (i + 1) with
            | '0' .. '9' as c ->
              read_decp (i + 2) intp (int_of_char c - 48) plus minus
            | ('e' | 'E' | 'f' | 'F' | 'g' | 'G') as c ->
              if i + 2 <> len then invalid_format ();
              run intp 6 plus minus c
            | _ ->
              invalid_format ()
          end
      | '0' .. '9' as c ->
        read_intp (i + 1) (intp * 10 + int_of_char c - 48) plus minus
      | ('e' | 'E' | 'f' | 'F' | 'g' | 'G') as c ->
        if i <> len - 1 then invalid_format ();
        run intp 6 plus minus c
      | _ ->
        invalid_format ()
  and read_decp i intp decp plus minus =
    if i = len then invalid_format ();
    match string_get fmt i with
      | '+' ->
        read_decp (i + 1) intp decp true minus
      | '-' ->
        read_decp (i + 1) intp decp plus true
      | '0' .. '9' as c ->
        read_decp (i + 1) intp (10 * decp + int_of_char c - 48) plus minus
      | ('e' | 'E' | 'f' | 'F' | 'g' | 'G') as c ->
        if i <> len - 1 then invalid_format ();
        run intp decp plus minus c
      | _ ->
        invalid_format ()
  and run intp decp plus minus c =
    let s =
      if x <> 0. && x *. 2. = x then
        if x > 0. then
          if plus then "+inf" else "inf"
        else "-inf"
      else if x <> x then "nan"
      else
        let abs_x = abs_float x in
        let s =
          match c with
            | 'e' -> run_e abs_x decp false
            | 'E' -> run_e abs_x decp true
            | 'f' -> run_f abs_x decp
            | 'g' -> run_g abs_x decp false
            | 'G' -> run_g abs_x decp true
            | 'F' -> valid_float_lexem (run_f x decp)
            | _ -> invalid_format ()
        in
        if x >= 0. then if plus then "+" ^ s else s
        else "-" ^ s
    in
    pad intp minus s
  and run_e x decp maj =
    let adj = 0.5 *. pow 10. ~-decp in
    if x = 0. then print_e x 0 decp maj 0 '+'
    else if x >= 1. then run_posexp_e adj x decp maj 0
    else run_negexp_e adj (x *. 10.) decp maj 1
  and run_posexp_e adj x decp maj exp =
    let adj_x = x +. adj in
    if x > 10. then run_posexp_e adj (x /. 10.) decp maj (exp + 1) else
      let n = int_of_float adj_x in
      if n < 10 then print_e adj_x n decp maj exp '+'
      else run_posexp_e adj (x /. 10.) decp maj (exp + 1)
  and run_negexp_e adj x decp maj exp =
    let adj_x = x +. adj in
    let n = int_of_float adj_x in
    if n >= 1 then print_e adj_x n decp maj exp '-'
    else run_negexp_e adj (x *. 10.) decp maj (exp + 1)
  and print_e x n decp maj exp exp_sign =
    let dec_sz = if decp = 0 then 1 else decp + 2 in
    let str = string_create (dec_sz + 4) in
    let m = if n < 0 then 0 else if n > 9 then 9 else n in
    string_set str 0 (unsafe_char_of_int (m + 48));
    if decp <> 0 then string_set str 1 '.';
    string_set str dec_sz (if maj then 'E' else 'e');
    string_set str (dec_sz + 1) exp_sign;
    string_set str (dec_sz + 2) (unsafe_char_of_int (exp / 10 + 48));
    string_set str (dec_sz + 3) (unsafe_char_of_int (exp mod 10 + 48));
    let dec_acc = ref ((x -. (float_of_int m)) *. 10.) in
    for i = dec_sz - decp to dec_sz - 1 do
      let n = int_of_float !dec_acc in
      let m = if n < 0 then 0 else if n > 9 then 9 else n in
      string_set str i (unsafe_char_of_int (m + 48));
      dec_acc := (!dec_acc -. float_of_int m) *. 10.;
    done;
    str
  and run_f x decp =
    let adj_x = x +. 0.5 *. pow 10. ~-decp in
    let intp_sz = compute_intp_sz_f adj_x 1 in
    let str = string_create (intp_sz + 1 + decp) in
    let dec = fill_intp_f adj_x str (intp_sz - 1) in
    string_set str intp_sz '.';
    ignore (fill_decp_f dec str (intp_sz + 1) decp);
    str
  and compute_intp_sz_f x n =
    if x < 10. && int_of_float x < 10 then n
    else compute_intp_sz_f (x /. 10.) (n + 1)
  and fill_intp_f x s i =
    let y = if i = 0 then x else fill_intp_f (x /. 10.) s (i - 1) in
    let n = int_of_float y in
    let m = if n < 0 then 0 else if n > 9 then 9 else n in
    string_set s i (unsafe_char_of_int (m + 48));
    (y -. float_of_int m) *. 10.
  and fill_decp_f dec s i decp =
    if decp > 0 then
      let n = int_of_float dec in
      let m = if n < 0 then 0 else if n > 9 then 9 else n in
      string_set s i (unsafe_char_of_int (m + 48));
      fill_decp_f ((dec -. float_of_int m) *. 10.) s (i + 1) (decp - 1)
  and run_g x decp maj =
    if x > 1e6 || x < ~-. 1e6 || (x > 0. && x < 1. /. 1e6) ||
      (x < 0. && x > ~-. (1. /. 1e6))
    then
      let str = run_e x decp maj in
      let len = string_length str in
      simplify str (len - 5) ^ (string_sub str (len - 4) 4)
    else
      let str = run_f x decp in
      let len = string_length str in
      simplify str (len - 1)
  and simplify s i =
    match string_get s i with
      | '.' -> string_sub s 0 i
      | '1' .. '9' -> string_sub s 0 (i + 1)
      | _ -> simplify s (i - 1)
  and pad intp minus s =
    let len = string_length s in
    if intp <= len then s else
      let res = string_create intp in
      if minus then (
        string_blit s 0 res 0 len;
        string_fill res len (intp - len) ' ';
      ) else (
        string_fill res 0 (intp - len) ' ';
        string_blit s 0 res (intp - len) len;
      );
      res
  in
  if len = 0 || string_get fmt 0 <> '%' then invalid_format ();
  read_intp 1 0 false false
;;

let string_of_float f = valid_float_lexem (format_float "%.12g" f);;

let float_of_string str =
  let len = string_length str in
  let msg = "float_of_string" in
  let rec read_intp i acc =
    if i = len then acc else
      match string_get str i with
        | '0' .. '9' as c ->
          read_intp (i + 1) (10. *. acc +. float_of_int (int_of_char c - 48))
        | '.' ->
          read_decp (i + 1) acc 0.1
        | 'e' | 'E' ->
          read_expp (i + 1) acc
        | _ ->
          failwith msg
  and read_decp i acc coeff =
    if i = len then acc else
      match string_get str i with
        | '0' .. '9' as c ->
          read_decp (i + 1)
            (acc +. coeff *. (float_of_int (int_of_char c - 48)))
            (coeff *. 0.1)
        | 'e' | 'E' ->
          read_expp (i + 1) acc
        | _ -> failwith msg
  and read_expp i acc =
    if i = len then failwith msg;
    match string_get str i with
      | '0' .. '9' as c ->
        read_expp_rest (i + 1) acc (int_of_char c - 48) true
      | '+' ->
        if i + 1 = len then failwith msg;
        begin match string_get str (i + 1) with
          | '0' .. '9' as c ->
            read_expp_rest (i + 2) acc (int_of_char c - 48) true
          | _ -> failwith msg
        end
      | '-' ->
        if i + 1 = len then failwith msg;
        begin match string_get str (i + 1) with
          | '0' .. '9' as c ->
            read_expp_rest (i + 2) acc (int_of_char c - 48) false
          | _ -> failwith msg
        end
      | _ -> failwith msg
  and read_expp_rest i acc exp es =
    if i = len then
      if es then acc *. (pow 10. exp) else acc *. (pow 10. (-exp))
    else
      match string_get str i with
        | '0' .. '9' as c ->
          read_expp_rest (i + 1) acc (10 * exp + int_of_char c - 48) es
        | _ -> failwith msg
  in
  if len = 0 then failwith msg;
  match string_get str 0 with
    | '0' .. '9' as c -> read_intp 1 (float_of_int (int_of_char c - 48))
    | '+' ->
      if len = 1 then failwith msg;
      begin match string_get str 1 with
        | '0' .. 'c' as c -> read_intp 2 (float_of_int (int_of_char c - 48))
        | '.' -> read_decp 2 0. 0.1
        | _ -> failwith msg
      end
    | '-' ->
      if len = 1 then failwith msg;
      begin match string_get str 1 with
        | '0' .. 'c' as c ->
          ~-. (read_intp 2 (float_of_int (int_of_char c - 48)))
        | '.' ->
          ~-. (read_decp 2 0. 0.1)
        | _ ->
          failwith msg
      end
    | '.' -> read_decp 1 0. 0.1
    | _ -> failwith msg
;;

end

include Pervasives

module ArrayLabels        = Stdlib__arrayLabels
module Array              = Stdlib__array
module Char               = Stdlib__char
module List               = Stdlib__list
module BytesLabels        = Stdlib__bytesLabels
module Bytes              = Stdlib__bytes
module Sys                = Stdlib__sys
module Map                = Stdlib__map
module Eeprom             = Stdlib__eeprom
module Gc                 = Stdlib__gc
module Genlex             = Stdlib__genlex
module String             = Stdlib__string
module Int32              = Stdlib__int32
module Int64              = Stdlib__int64
module Random             = Stdlib__random
module Hashtbl            = Stdlib__hashtbl
module Lazy               = Stdlib__lazy
module ListLabels         = Stdlib__listLabels
module MoreLabels         = Stdlib__moreLabels
module Nap                = Stdlib__nap
module Nativeint          = Stdlib__nativeint
module Obj                = Stdlib__obj
module Oo                 = Stdlib__oo
module Queue              = Stdlib__queue
module Serial             = Stdlib__serial
module Set                = Stdlib__set
module Sort               = Stdlib__sort
module Stack              = Stdlib__stack
module StdLabels          = Stdlib__stdLabels
module Stream             = Stdlib__stream
module StringLabels       = Stdlib__stringLabels
module Uchar              = Stdlib__uchar
module Buffer             = Stdlib__buffer
module Printf             = Stdlib__printf
