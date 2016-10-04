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

set_bit IRCF1;;
set_bit IRCF0;;
set_bit PLLEN;;

let rec npremiers n =
  if n = 0 then [] else npremiers_aux (n - 1) [2]
and npremiers_aux n r =
  if n = 0 then r
  else npremiers_aux (n-1) (premier_suivant (List.hd r + 1) r :: r)
and premier_suivant n l =
  if est_premier n l then n else premier_suivant (n + 1) l
and est_premier n l =
  match l with
    | [] -> true
    | e::tl -> if n mod e = 0 then false else est_premier n tl
;;

let test () =
  let _ = npremiers 100 in ()
;;

write_reg TRISB 0;;
write_reg PORTB 0;;

try
  while true do
    set_bit RB0;
    test ();
    clear_bit RB0;
    test ();
  done
with
| Out_of_memory ->
  set_bit RB1
| Stack_overflow ->
  set_bit RB2
