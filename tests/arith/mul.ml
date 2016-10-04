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

let ml_of_int x = ((x lsl 1) lor 1) land 0xFFFF;;

let int_of_ml x = ((x lsl 15) asr 16);;

let mul x y = ml_of_int (int_of_ml x * int_of_ml y);;

let op x y =
  let xL = (x lsr 1) land 0xFF in
  let yL = (y lsr 1) land 0xFF in
  let xH = (x lsr 9) land 0xFF in
  let yH = (y lsr 9) land 0xFF in
  let rL = (xL * yL) land 0xFF in
  let rH =
    ((xH * yL) land 0xFF) + ((xL * yH) land 0xFF) +
      (((xL * yL) lsr 8) land 0xFF)
  in
  ((rH lsl 9) lor (rL lsl 1) lor 1) land 0xFFFF
;;

for x = -0x4000 to 0x3FFF do
  Printf.printf "\r%d    %!" x;
  for y = -0x4000 to 0x3FFF do
    let rx = ml_of_int x in
    let ry = ml_of_int y in
    let m1 = mul rx ry in
    let m2 = op rx ry in
    if m1 <> m2 then (
      Printf.printf "%4d * %-4d  =  %-4d %-4d\n%!"
        x y (int_of_ml m1) (int_of_ml m2);
      Printf.printf "%04x   %04x  =  %04x %04x\n%!" rx ry m1 m2;
      failwith "op <> mul";
    )
  done
done;;

Printf.printf "Ok\n%!";;
