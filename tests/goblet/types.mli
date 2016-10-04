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

type color_t = Black | White

type size_t = Minus | Small | Medium | Big

type goblet = { color : color_t ; size : size_t }

type action =
  | Add of goblet * int * int
  | Move of int * int * int * int
  | Nothing

type key =
  | Stack of color_t * int
  | Table of int * int
