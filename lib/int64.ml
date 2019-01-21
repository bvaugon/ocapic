(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Module [Int64]: 64-bit integers *)

external neg : int64 -> int64 = "%int64_neg"
external add : int64 -> int64 -> int64 = "%int64_add"
external sub : int64 -> int64 -> int64 = "%int64_sub"
external mul : int64 -> int64 -> int64 = "%int64_mul"
external div : int64 -> int64 -> int64 = "%int64_div"
external rem : int64 -> int64 -> int64 = "%int64_mod"
external logand : int64 -> int64 -> int64 = "%int64_and"
external logor : int64 -> int64 -> int64 = "%int64_or"
external logxor : int64 -> int64 -> int64 = "%int64_xor"
external shift_left : int64 -> int -> int64 = "%int64_lsl"
external shift_right : int64 -> int -> int64 = "%int64_asr"
external shift_right_logical : int64 -> int -> int64 = "%int64_lsr"
external of_int : int -> int64 = "%int64_of_int"
external to_int : int64 -> int = "%int64_to_int"
external of_float : float -> int64
  = "caml_int64_of_float" "caml_int64_of_float_unboxed"
  [@@unboxed] [@@noalloc]
external to_float : int64 -> float
  = "caml_int64_to_float" "caml_int64_to_float_unboxed"
  [@@unboxed] [@@noalloc]
external of_int32 : int32 -> int64 = "%int64_of_int32"
external to_int32 : int64 -> int32 = "%int64_to_int32"
external of_nativeint : nativeint -> int64 = "%int64_of_nativeint"
external to_nativeint : int64 -> nativeint = "%int64_to_nativeint"

let zero = 0L
let one = 1L
let minus_one = -1L
let succ n = add n 1L
let pred n = sub n 1L
let abs n = if n >= 0L then n else neg n
let min_int = 0x8000000000000000L
let max_int = 0x7FFFFFFFFFFFFFFFL
let lognot n = logxor n (-1L)

external of_string : string -> int64 = "caml_int64_of_string"

external bits_of_float : float -> int64
  = "caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed"
  [@@unboxed] [@@noalloc]
external float_of_bits : int64 -> float
  = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
  [@@unboxed] [@@noalloc]

type t = int64

let compare (x: t) (y: t) = Pervasives.compare x y
let equal (x: t) (y: t) = compare x y = 0

(***)

external unsafe_char_of_int : int -> char = "%identity"
external raise_ios : unit -> 'a = "caml_raise_ios_failure"

(***)

let to_string n =
  let str = Bytes.create 20 in
  let rec f i k =
    if k < 10L then begin
      Bytes.set str i (unsafe_char_of_int (to_int k + int_of_char '0'));
      i
    end else begin
      Bytes.set str i (unsafe_char_of_int (to_int (rem k 10L) + int_of_char '0'));
      f (i - 1) (div k 10L);
    end
  in
    if n >= zero then
      let b = f 19 n in Bytes.sub_string str b (20 - b)
    else if n = min_int then
      let _ = f 19 max_int in
        Bytes.set str 19 '8';
        Bytes.set str 0 '-';
        (Bytes.to_string str)
    else
      let b = f 19 (neg n) - 1 in
      Bytes.set str b '-';
      Bytes.sub_string str b (20 - b)
;;

(***)

let of_string s =
  let len = String.length s in
  let rec gen_conv f i acc =
    if i = len then acc
    else gen_conv f (i + 1) (f acc s.[i])
  in
  let dec i =
    let dec_conv acc c =
      let ofs =
        match c with
          | '0' .. '9' -> int_of_char c - int_of_char '0'
          | _ -> raise_ios ()
      in
      let acc10 = mul acc 10L in
      let res = add acc10 (of_int ofs) in
        if (div acc10 10L) <> acc || (sub res one) < minus_one then
          raise_ios ();
        res
    in
      gen_conv dec_conv i zero
  in
  let gen i =
    match s.[i] with
      | '0' ->
          if len = i + 1 then zero
          else begin match s.[i + 1] with
            | 'x' | 'X' ->
                if len = i + 2 then raise_ios ();
                let hex_conv acc c =
                  if shift_right_logical acc 60 <> zero then raise_ios ();
                  let ofs = match c with
                    | '0' .. '9' -> (int_of_char c - int_of_char '0')
                    | 'a' .. 'f' -> (int_of_char c - int_of_char 'a' + 10)
                    | 'A' .. 'F' -> (int_of_char c - int_of_char 'A' + 10)
                    | _ -> raise_ios ()
                  in
                    add (shift_left acc 4) (of_int ofs)
                in
                  gen_conv hex_conv (i + 2) zero
            | 'o' | 'O' ->
                if len = i + 2 then raise_ios ();
                let oct_conv acc c =
                  if shift_right_logical acc 61 <> zero then raise_ios ();
                  let ofs =
                    match c with
                      | '0' .. '7' -> int_of_char c - int_of_char '0'
                      | _ -> raise_ios ()
                  in
                    add (shift_left acc 3) (of_int ofs)
                in
                  gen_conv oct_conv (i + 2) zero
            | 'b' | 'B' ->
                if len = i + 2 then raise_ios ();
                let bin_conv acc c =
                  if acc < zero then raise_ios ();
                  match c with
                    | '0' -> shift_left acc 1
                    | '1' -> add (shift_left acc 1) one
                    | _ -> raise_ios ()
                in
                  gen_conv bin_conv (i + 2) zero
            | _ -> dec (i + 1)
          end
      | _ -> dec i
  in
    if len = 0 then raise_ios ();
    if s.[0] = '-' then if len = 1 then raise_ios () else neg (gen 1)
    else gen 0
;;

(***)

let format fmt n =
  let len = String.length fmt in
  let conv plus =
    match fmt.[len - 1] with
      | 'd' | 'i' | 'n' | 'l' | 'L' | 'N' ->
          if plus && n >= zero then "+" ^ to_string n
          else to_string n
      | c ->
          if n = zero then "0" else
          let (conv, div) =
            match c with
              | 'x' ->
                  let mask = 0b1111L in
                  ((fun k ->
                      let i = to_int (logand k mask) in
                        if i < 10 then char_of_int (int_of_char '0' + i)
                        else char_of_int (int_of_char 'a' + i - 10)),
                   (fun k -> shift_right_logical k 4))
              | 'X' ->
                  let mask = 0b1111L in
                  ((fun k ->
                      let i = to_int (logand k mask) in
                        if i < 10 then char_of_int (int_of_char '0' + i)
                        else char_of_int (int_of_char 'A' + i - 10)),
                   (fun k -> shift_right_logical k 4))
              | 'u' ->
                  ((fun k ->
                      if k = min_int then '8'
                      else if k < zero then
                        char_of_int (int_of_char '0' +
                                       (16 - (to_int (rem
                                   (neg k) 10L))) mod 10)
                      else
                        char_of_int (int_of_char '0' +
                               to_int (rem k 10L))),
                   (fun k ->
                      if k = min_int then
                        div max_int 10L
                      else if k < zero then add
                        (div (pred k) 10L)
                        (div max_int 5L)
                      else div k 10L))
              | 'o' ->
                  let mask = 0b111L in
                  ((fun k -> char_of_int (int_of_char '0' +
                                            (to_int (logand k
                                                             mask)))),
                   (fun k -> shift_right_logical k 3))
              | _ -> failwith "invalid format"
          in
          let buf = Bytes.create 20 in
          let rec f i n =
            if n <> zero then (
              Bytes.set buf i (conv n);
              f (i - 1) (div n);
            ) else
              Bytes.unsafe_to_string (Bytes.sub buf (i + 1) (19 - i))
          in
            f 19 n
  in
  let rec f i minus plus zero =
    match fmt.[i] with
      | '-' -> f (i + 1) true plus zero
      | '+' -> f (i + 1) minus true zero
      | '0' -> f (i + 1) minus plus true
      | ' ' | '#' -> f (i + 1) minus plus zero
      | '1' .. '9' ->
          let nstr = conv plus in
          let fsz = int_of_string (String.sub fmt i (len - i - 2)) in
          let nsz = String.length nstr in
            if nsz < fsz then
              if zero && not minus then
                let res = Bytes.make fsz '0' in
                  match nstr.[0] with
                    | ('-' as c) | ('+' as c) ->
                        Bytes.set res 0 c;
                        Bytes.blit_string nstr 1 res (fsz - nsz + 1) (nsz - 1);
                        Bytes.unsafe_to_string res
                    | _ ->
                        Bytes.blit_string nstr 0 res (fsz - nsz) nsz;
                        Bytes.unsafe_to_string res
              else
                let res = Bytes.make fsz ' ' in
                let ofs = if minus then 0 else fsz - nsz in
                  Bytes.blit_string nstr 0 res ofs nsz;
                  Bytes.unsafe_to_string res
            else
              nstr
      | _ -> conv plus
  in
    f 1 false false false
;;
