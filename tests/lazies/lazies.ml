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

let sleep () = Sys.sleep 4000;;

Display.write_string "a";;
sleep ();;

let x = lazy (
  Display.write_string "Hello";
  sleep ();
  42
);;

Gc.run ();;
Display.write_string "b";;
sleep ();;
let n = Lazy.force x;;
Gc.run ();;
sleep ();;
Display.write_string "c";;
sleep ();;
Display.write_int n;;
sleep ();;
let n = Lazy.force x;;
sleep ();;
Display.write_string "d";;
sleep ();;
Display.write_int n;;
sleep ();;
Display.write_string "e";;
