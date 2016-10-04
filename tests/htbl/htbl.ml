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

open Pic;;
open Obj;;

let t = Hashtbl.create 10;;

Hashtbl.add t 10 "abc";;
Hashtbl.add t 11 "cde";;
Hashtbl.add t 10 "fgh";;

write_reg PORTB (Hashtbl.hash "abc");;

write_reg PORTB max_int;;
write_reg PORTB min_int;;

let x = `I in
match x with
  | `I -> write_reg PORTB 19
  | _ -> ()
;;

type t

let hash_param =
  let count_cnt = ref 0 in
  let limit_cnt = ref 0 in
  let accu = ref 0 in
  let combine n = accu := !accu * 16313 + n in
  let combine_small n = accu := !accu * 19 + n in
  fun count limit obj ->
    count_cnt := count;
    limit_cnt := limit;
    let rec hash_aux (obj:t) =
      match tag (repr obj) with
        | Object_tag       ->
          combine (Array.unsafe_get (magic obj : int array) 1)
        | Infix_tag        -> ()
        | Forward_tag      -> hash_aux (magic (field (repr obj) 0));
        | Abstract_tag     -> ()
        | String_tag       ->
          for i = String.length (magic obj) - 1 to 0 do
            combine_small (magic (magic obj).[i])
          done
        | Double_tag       -> combine (magic (field (repr obj) 0));
        | Double_array_tag ->
          for i = size (repr obj) - 1 downto 0 do
            hash_aux (magic obj).(i);
          done
        | Custom_tag       -> invalid_arg "hash_param";
        | Int_tag          -> combine (magic obj);
        | Out_of_heap_tag  -> combine ((magic obj) lor 0);
        | _ ->
          for i = size (repr obj) - 1 downto 0 do
            hash_aux (magic obj).(i);
          done
    in
    hash_aux obj;
    !accu
;;
