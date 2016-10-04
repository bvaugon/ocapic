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

let disp = connect ~bus_size:Lcd.Four ~e:LATD0 ~rs:LATD2 ~rw:LATD1 ~bus:PORTB;;

disp.init ();;
disp.config ();;
disp.print_string "Hello world";;
Sys.sleep 4;;

let rec f () = 4l
and g () = Int32.add (f ()) 1l
and h () = Int32.add (f ()) (g ());;

disp.clear ();;
Printf.fprintf disp.output "%b %b %b\n"
  (f () = g ()) (f () < g ()) (f () > g ());;
Sys.sleep 4;; 

let r = ref [];;
for i = 1 to 10 do
  r := 1 :: !r;
done;;

disp.clear ();;
try
  while true do
    Gc.run ();
    Printf.fprintf disp.output "%d %lx " (Gc.running_number ()) (h ());
    (*disp.print_int (Gc.running_number ());*)
    Sys.sleep 4;
    r := [];
  done;
with Out_of_memory ->
  disp.clear ();
  disp.print_string "OUT OF MEMORY";
;;
