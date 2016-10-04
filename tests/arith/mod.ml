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

let mdu x y = ml_of_int (int_of_ml x mod int_of_ml y);;

let op rx ry =
  let x = ref (rx lsr 1) in
  let y = ref (ry lsr 1) in
  let p = ref 0 in
  if !x land 0x4000 <> 0 then x := -(!x) land 0x7FFF;
  if !y land 0x4000 <> 0 then y := -(!y) land 0x7FFF;
  begin
    let rec f () =
      y := !y lsl 1;
      incr p;
      if !x >= !y then f ();
    in
    f ();
  end;
  begin
    let rec g () =
      y := !y lsr 1;
      if !x >= !y then (
        x := !x - !y;
      );
      decr p;
      if !p <> 0 then g ();
    in
    g ();
  end;
  if rx land 0x8000 = 0 then
    (!x lsl 1) lor 1
  else
    ((-(!x) lsl 1) lor 1) land 0xFFFF
;;

for x = -0x4000 to 0x3FFF do
  Printf.printf "\r%d    %!" x;
  for y = -0x4000 to 0x3FFF do
    if y <> 0x000 then
      let rx = ml_of_int x in
      let ry = ml_of_int y in
      let d1 = mdu rx ry in
      let d2 = op rx ry in
      if d1 <> d2 then (
        Printf.printf "\n%4d %% %-4d  =  %-4d %-4d\n%!"
          x y (int_of_ml d1) (int_of_ml d2);
        Printf.printf "%04x   %04x  =  %04x %04x\n%!" rx ry d1 d2;
        failwith "op <> mul";
      )
  done
done;;

Printf.printf "\nOk\n%!";;
