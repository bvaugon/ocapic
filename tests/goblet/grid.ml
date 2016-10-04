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

(******************************************************************************)
(*                                                                            *)
(* Le module Grid offre des outils pour travailler sur une grille de gobelets *)
(* d'assez bas niveau avec peu de sécurité mais une grande efficacité.        *)
(* Une grille de gobelet peut être considéré comme le plateau sur lequel on   *)
(* pose les gobelets.                                                         *)
(*                                                                            *)
(******************************************************************************)

open Types

let table : goblet list array array = Array.make_matrix 4 4 [];;
let white_per_line = Array.make 4 0;;
let black_per_line = Array.make 4 0;;
let white_per_column = Array.make 4 0;;
let black_per_column = Array.make 4 0;;
let white_in_diag1 = ref 0;;
let black_in_diag1 = ref 0;;
let white_in_diag2 = ref 0;;
let black_in_diag2 = ref 0;;
let value = ref 500;;

(*
  Permet d'accèder a la "note" du joueur blanc pour cet etat de la grille.
  Cette note est comprise entre 0 et 1000.
  0    -> le joueur blanc a perdu.
  500  -> les joueurs sont à peu près à égalité.
  1000 -> le joueur blanc a gagné.
  n    -> plus n est grand et plus le joueur blanc est "plutôt bien parti".
*)
let get_value () =
  let v = !value + !Stacks.value in
  if v < 0 then 0
  else if v > 1000 then 1000
  else v
;;

(*
  Utilitaire privé très bas niveau servant à calculer les changements
  du champ `value'.
*)
let delta_value n =
  match n with
    | 0 -> 0
    | 1 -> 12
    | 2 -> 15
    | 3 -> 29
    | _ -> 2000
(*
  if n = 0 then 0
  else if n = 3 then 29
  else if n = 4 then 1000
  else 10
*)
;;

(*
  Utilitaire privé bas niveau.
  Modifie les compteurs de lignes, colonnes et diagonnales de la case (`i', `j')
  du plateau `grid' en considérant l'apparition d'un gobelet blanc sur
  cette case.
  Met a jour le champt `value'.
*)
let incr_white i j =
  white_per_line.(i) <- white_per_line.(i) + 1;
  value := !value + delta_value white_per_line.(i);
  white_per_column.(j) <- white_per_column.(j) + 1;
  value := !value + delta_value white_per_column.(j);
  if i = j then
    begin
      incr white_in_diag1;
      value := !value + delta_value !white_in_diag1;
    end
  else if i = 3 - j then
    begin
      incr white_in_diag2;
      value := !value + delta_value !white_in_diag2
    end
;;

(*
  Utilitaire privé bas niveau.
  Modifie les compteurs de lignes, colonnes et diagonnales de la case (`i', `j')
  du plateau `grid' en considérant l'apparition d'un gobelet noir sur
  cette case.
  Met à jour le champ `value'.
*)
let incr_black i j =
  black_per_line.(i) <- black_per_line.(i) + 1;
  value := !value - delta_value black_per_line.(i);
  black_per_column.(j) <- black_per_column.(j) + 1;
  value := !value - delta_value black_per_column.(j);
  if i = j then
    begin
      incr black_in_diag1;
      value := !value - delta_value !black_in_diag1;
    end
  else if i = 3 - j then
    begin
      incr black_in_diag2;
      value := !value - delta_value !black_in_diag2;
    end
;;

(*
  Utilitaire privé bas niveau.
  Modifie les compteurs de lignes, colonnes et diagonnales de la case (`i', `j')
  du plateau `grid' en considérant la disparition d'un gobelet blanc sur
  cette case.
  Met à jour le champ `value'.
*)
let decr_white i j =
  value := !value - delta_value white_per_line.(i);
  white_per_line.(i) <- white_per_line.(i) - 1;
  value := !value - delta_value white_per_column.(j);
  white_per_column.(j) <- white_per_column.(j) - 1;
  if i = j then
    begin
      value := !value - delta_value !white_in_diag1;
      decr white_in_diag1;
    end
  else if i = 3 - j then
    begin
      value := !value - delta_value !white_in_diag2;
      decr white_in_diag2;
    end
;;

(*
  Utilitaire privé bas niveau.
  Modifie les compteurs de lignes, colonnes et diagonnales de la case (`i', `j')
  du plateau `grid' en considérant la disparition d'un gobelet noir sur
  cette case.
  Met à jour le champ `value'.
*)
let decr_black i j =
  value := !value + delta_value black_per_line.(i);
  black_per_line.(i) <- black_per_line.(i) - 1;
  value := !value + delta_value black_per_column.(j);
  black_per_column.(j) <- black_per_column.(j) - 1;
  if i = j then
    begin
      value := !value + delta_value !black_in_diag1;
      decr black_in_diag1;
    end
  else if i = 3 - j then
    begin
      value := !value + delta_value !black_in_diag2;
      decr black_in_diag2;
    end
;;

(*
  Renvoie true ssi le gobelet `goblet' peut être ajouté sur la case (`i', `j').
  Attention : `i' et `j' doivent être compris entre 0 et 3.
  Ne vérifie pas que `goblet' est "ajoutable" par le joueur
  (ceci est normalement vérifié par le module Partie).
*)
let can_add_goblet goblet i j =
  begin
    match table.(i).(j) with
      | [] -> true
      | { color = White ; size = sd } :: _ ->
        goblet.size > sd &&
          (goblet.color = White ||
              (white_per_line.(i) >= 3 ||
                 white_per_column.(j) >= 3 ||
                 (i = j && !white_in_diag1 >= 3) ||
                 (i = 3 - j && !white_in_diag2 >= 3)))
      | { color = Black ; size = sd } :: _ ->
        goblet.size > sd &&
          (goblet.color = Black ||
              (black_per_line.(i) >= 3 ||
                 black_per_column.(j) >= 3 ||
                 (i = j && !black_in_diag1 >= 3) ||
                 (i = 3 - j && !black_in_diag2 >= 3)))
  end;
;;

(*
  Retourne true ssi le goblet `goblet' peut être posé sur la case (`i', `j').
  Ce gobelet a à priori été soulevé juste avant.
  Attention : `i' et `j' doivent être compris entre 0 et 3.
  Ne vérifie pas que c'est le bon joueur qui pose le gobelet
  (ceci est normalement vérifié par le module Partie).
*)
let can_put_goblet goblet i j =
  match table.(i).(j) with
    | [] -> true
    | { size = size } :: _ -> size < goblet.size
;;

(*
  Pose le gobelet `goblet' dans la case (`i', `j') du plateau `grid'.
  Attention : ne fait aucune vérification.
*)
let put_goblet goblet i j =
  begin
    match table.(i).(j) with
      | [] ->
        begin
          match goblet.color with
            | White -> incr_white i j
            | Black -> incr_black i j
        end
      | { color = White } :: _ ->
        if goblet.color = Black then
          begin
            decr_white i j;
            incr_black i j;
          end
      | { color = Black} :: _ ->
        if goblet.color = White then
          begin
            decr_black i j;
            incr_white i j;
          end
  end;
  table.(i).(j) <- goblet :: table.(i).(j);
;;

(*
  Retire le gobelet "du haut" de la case (`i', `j') du plateau `grid'.
  Attention : lance une exception si la case est vide.
  ne fait aucune autre vérification.
*)
let sub_goblet i j =
  match table.(i).(j) with
    | [] -> invalid_arg "sub_goblet";
    | { color = Black } :: ({ color = White } :: _ as tl) ->
      decr_black i j;
      incr_white i j;
      table.(i).(j) <- tl;
    | { color = White } :: ({ color = Black } :: _ as tl) ->
      decr_white i j;
      incr_black i j;
      table.(i).(j) <- tl;
    | { color = White } :: [] ->
      decr_white i j;
      table.(i).(j) <- [];
    | { color = Black } :: [] ->
      decr_black i j;
      table.(i).(j) <- [];
    | _ :: tl ->
      table.(i).(j) <- tl;
;;
