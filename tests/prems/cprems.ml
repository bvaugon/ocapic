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

let rec npremiers n =
  if n = 0 then [] else npremiers_aux (n-1) [2]
and npremiers_aux n r =
  if n = 0 then r
  else npremiers_aux (n-1) (premier_suivant (List.hd r+1) r :: r)
and premier_suivant n l =
  if est_premier n l then n else premier_suivant (n+1) l
and est_premier n l =
  match l with
    | [] -> true
    | e::tl -> if n mod e = 0 then false else est_premier n tl
;;

let test () =
  let _ = npremiers 250 in ()
;;

for i = 1 to 80 do test () done
