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

set_bit IRCF1;;
set_bit IRCF0;;
set_bit PLLEN;;

let disp = connect ~bus_size:Four ~e:LATD0 ~rs:LATD2 ~rw:LATD1 ~bus:PORTB;;
disp.init ();;
disp.config ();;
disp.print_string "Hello";;
(*
class virtual ['a] list =
object(self)
  method virtual is_nil : bool
  method virtual head : 'a
  method virtual tail : 'a list
end;;

class ['a] nil =
object(self)
  inherit ['a] list
  method is_nil = true
  method head = failwith "[]#head"
  method tail = failwith "[]#tail"
end;;

class ['a] cons hd tl =
object(self)
  inherit ['a] list
  method is_nil = false
  method head = hd
  method tail = tl
end;;

let rec to_string l =
  if l#is_nil then "[]" else (string_of_int l#head) ^ " :: " ^ to_string l#tail
;;

let l = new cons 198 (new cons 162 (new cons 12 (new nil)));;
disp.moveto 2 0;;
disp.print_string (to_string l);;
disp.moveto 1 10;;
disp.print_int (Gc.heap_occupation ());;
*)


(*
  external init_aff : unit -> unit = "init_aff";;
  external print_int : int -> unit = "print_int";;
  external print_string : string -> unit = "print_string";;
  let stat () =
  Gc.run ();
  print_int (Gc.heap_occupation ());
  Sys.sleep 2000;
  ;;

  init_aff ();;
  stat ();;

  class point (x:int) (y:int) =
  object(self)
  method get_x = x
  method get_y = y
  end
  ;;

  stat ();;

  class point_colore c x y =
  object(self)
  inherit point x y as super
  method to_string =
  c ^ " " ^ (string_of_int super#get_x) ^ ", " ^ (string_of_int super#get_y)
  end
  ;;

  stat ();;

  let pc = new point_colore "r" 19 35 in
  stat ();
  print_string pc#to_string;
  Sys.sleep 2000;
  let p = (pc :> point) in
  stat ();
  print_int p#get_x;
  ;;
*)

(*
  open Pic

  external init_aff : unit -> unit = "init_aff";;
  external print_int : int -> unit = "print_int";;
  external print_string : string -> unit = "print_string";;
  let sleep () =
  Sys.sleep 5000;
  ;;

  set_bit IRCF1;;
  set_bit IRCF0;;
  set_bit PLLEN;;

  init_aff ();;
  print_int 0x4321;;
  sleep ();;

  class c x y =
  object(self)
  val mutable count = 0
  method get_x = x
  method get_y = y
  method get_sum () = count <- succ count ; x + y
  method get_diff () = count <- succ count ; x - y
  method get_count () = count
  method to_string = "(" ^  (string_of_int x) ^ ", " ^ (string_of_int y) ^ ")"
  end
  ;;

  Gc.run ();;
  print_int (Gc.heap_occupation ());;
  sleep ();;

  let o = new c 0x19 0x35 in
  print_int o#get_x;
  sleep ();
  print_int o#get_y;
  sleep ();
  print_int (o#get_sum ());
  sleep ();
  print_int (o#get_diff ());
  sleep ();
  print_int (o#get_count ());
  sleep ();
  print_string o#to_string;
  ;;
*)
