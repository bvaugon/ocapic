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

module Disp = Lcd.Connect (
  struct
    let bus_size = Lcd.Four
    let e  = LATD0
    let rs = LATD2
    let rw = LATD1
    let bus = PORTC
  end
);;

Disp.init ();; Disp.config ();;
write_reg TRISB 0b11110000;;

let buttons = [
  (RB3, RB4, '7'); (RB3, RB5, '8'); (RB3, RB6, '9'); (RB3, RB7, '/');
  (RB2, RB4, '4'); (RB2, RB5, '5'); (RB2, RB6, '6'); (RB2, RB7, '*');
  (RB1, RB4, '1'); (RB1, RB5, '2'); (RB1, RB6, '3'); (RB1, RB7, '-');
  (RB0, RB4, '+'); (RB0, RB5, '0'); (RB0, RB6, '='); (RB0, RB7, '+');
];;

while true do
  List.iter (fun (output, input, c) ->
    set_bit output;
    if test_bit input then (
      if c = '=' then Disp.clear () else Disp.print_char c;
      while test_bit input do () done;
    );
    clear_bit output;
  ) buttons;
done
