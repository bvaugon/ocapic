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

let conv = [|
  0b00111111; (* 0 *)
  0b00000110; (* 1 *)
  0b01011011; (* 2 *)
  0b01001111; (* 3 *)
  0b01100110; (* 4 *)
  0b01101101; (* 5 *)
  0b01111101; (* 6 *)
  0b00000111; (* 7 *)
  0b01111111; (* 8 *)
  0b01101111; (* 9 *)
  0b10000000; (* . *)
|];;

write_reg TRISB 0x00;;
write_reg TRISA 0x00;;
write_reg PORTA 0b111;;

let print n =
  let c0 = n mod 10 in
  let c1 = (n / 10) mod 10 in
  let c2 = (n / 100) mod 10 in
  let write pin c =
    write_reg PORTB conv.(c);
    clear_bit pin;
    set_bit pin;
  in
  write RA0 c0;
  write RA1 c1;
  write RA2 c2;
;;

for i = 0 to 999 do
  print i;
  Sys.sleep 100;
done
