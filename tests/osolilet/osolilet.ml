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

set_bit IRCF1;;
set_bit IRCF0;;
set_bit PLLEN;;
write_reg TRISB 0;;
write_reg PORTB 0;;

set_bit RB0;;

type peg = Out | Empty | Peg ;;

exception Found;;

let dir = [| [|0;1|]; [|1;0|];[|0;0-1|];[|0-1;0|] |];;

let counter = ref 0

let rec solve =
  fun board m  ->
    incr counter;
    if m = 31 then (
      match Array.get (Array.get board 4) 4 with
        | Peg -> true
        | _ -> false
    ) else (
      try (
        (if ((!counter) mod 50) = 0 then
            ()
        );
        (let i = ref 1 in
         while (!i) <= 7 do (
           let j = ref 1 in
           while (!j) <= 7 do (
             match Array.get (Array.get board (!i)) (!j) with
               | Peg ->
                 let k = ref 0 in
                 while (!k) <= 3 do (
                   let d1 = (Array.get (Array.get dir (!k)) 0) in
                   let d2 = (Array.get (Array.get dir (!k)) 1) in
                   let i1 = ((!i)+d1) in
                   let i2 = (i1+d1) in
                   let j1 = ((!j)+d2) in
                   let j2 = (j1+d2) in
                   match Array.get (Array.get board i1) j1 with
                     | Peg ->
                       (match Array.get (Array.get board (i2)) j2 with
                         | Empty -> (
                           Array.set (Array.get board (!i)) (!j) Empty;
                           Array.set (Array.get board i1) j1 Empty;
                           Array.set (Array.get board i2) j2 Peg;
                           (if solve board (m+1) then (
                             raise Found
                            ));
                           Array.set (Array.get board (!i)) (!j) Peg;
                           Array.set (Array.get board i1) j1 Peg;
                           Array.set (Array.get board i2) j2 Empty)
                         | _ -> ())
                     | _ -> ());
                   incr k
                 done
               | _ -> ());
             incr j
           done);
           incr i
         done);
        false
      ) with
        | Found -> true)
;;

begin try
        let i = ref 0 in
        while (!i) < 1 do
          let board = [|
            [| Out; Out; Out; Out;  Out ; Out; Out; Out; Out|];
            [| Out; Out; Out; Peg;  Peg ; Peg; Out; Out; Out|];
            [| Out; Out; Out; Peg;  Peg ; Peg; Out; Out; Out|];
            [| Out; Peg; Peg; Peg;  Peg ; Peg; Peg; Peg; Out|];
            [| Out; Peg; Peg; Peg; Empty; Peg; Peg; Peg; Out|];
            [| Out; Peg; Peg; Peg;  Peg ; Peg; Peg; Peg; Out|];
            [| Out; Out; Out; Peg;  Peg ; Peg; Out; Out; Out|];
            [| Out; Out; Out; Peg;  Peg ; Peg; Out; Out; Out|];
            [| Out; Out; Out; Out;  Out ; Out; Out; Out; Out|];
          |] in
          ignore (solve board 0);
          incr i;
        done;
  with
    | Stack_overflow -> set_bit RB1
    | Out_of_memory -> set_bit RB2
    | _ -> set_bit RB3
end;;

clear_bit RB0;;
