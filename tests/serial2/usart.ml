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
open Lcd;;
open Serial;;

(* Acceleration de l'horloge du PIC *)
set_bit IRCF1;; set_bit IRCF0;; set_bit PLLEN;;

(* Configuration de la connection PIC/Afficheur LCD *)
module Disp = Connect (
  struct
    let bus_size = Eight
    let e  = LATD0
    let rs = LATD2
    let rw = LATD1
    let bus = PORTB
  end
);;

(* Initialisation et configuration de l'afficheur *)
Disp.init ();; Disp.config ~cursor:Underscore ();;

(* Type des valeurs transferees de l'ordinateur vers le PIC *)
type t = Clear | Moveto of int * int | Int of int | Text of string

(* Cannal de communication avec l'ordinateur *)
let (chan : (unit, t) channel) = open_channel 34;; (* 19200 bauds *)

(* Boucle : reception d'une commande / transmission a l'ecran *)
while true do
  match receive chan with
    | Clear -> Disp.clear ()
    | Moveto (l, c) -> Disp.moveto l c
    | Int i -> Disp.print_int i
    | Text t -> Disp.print_string t
done
