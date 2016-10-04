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

open Types

let nb = 3

type t = goblet list array

let create color =
  let stk = [ { color = color ; size = Big    };
              { color = color ; size = Medium };
              { color = color ; size = Small  };
              { color = color ; size = Minus  }; ]
  in
  Array.make nb stk
;;

let white_stacks = create White
let black_stacks = create Black
let value = ref 0

let stacks_of_color c =
  match c with
    | White -> white_stacks
    | Black -> black_stacks
;;

let popables c =
  let stks = stacks_of_color c in
  let rec f l i =
    if i = nb then l else
      let nl =
        match stks.(i) with
          | g :: _ -> if List.memq g l then l else g :: l
          | [] -> l
      in
      f nl (succ i)
  in
  f [] 0
;;

let popi c i =
  value := !value + (if c = White then -20 else 20);
  let stks = stacks_of_color c in
  match stks.(i) with
    | g :: tl -> stks.(i) <- tl ; g
    | [] -> raise Exit
;;

let push c i g =
  value := !value + (if c = White then 20 else -20);
  let stks = stacks_of_color c in
  stks.(i) <- g :: stks.(i)
;;

let get_value () = !value
