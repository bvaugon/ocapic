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
open Types;;

exception Bad_action

let key_of_ind ind =
  match ind with
    | 0b00100000 -> Table (0, 0+0)
    | 0b01000100 -> Table (0, 1+0)
    | 0b01100000 -> Table (0, 2+0)
    | 0b10001100 -> Table (0, 3+0)
    | 0b10100000 -> Table (1, 0+0)
    | 0b00000100 -> Table (1, 1+0)
    | 0b00100100 -> Table (1, 2+0)
    | 0b00101100 -> Table (1, 3+0)
    | 0b01000000 -> Table (2, 0+0)
    | 0b11000000 -> Table (2, 1+0)
    | 0b01100100 -> Table (2, 2+0)
    | 0b01101100 -> Table (2, 3+0)
    | 0b10000000 -> Table (3, 0+0)
    | 0b10000100 -> Table (3, 1+0)
    | 0b11000100 -> Table (3, 2+0)
    | 0b01101000 -> Table (3, 3+0)

    | 0b11100000 -> Stack (Black, 0+0)
    | 0b11100100 -> Stack (Black, 1+0)
    | 0b00001000 -> Stack (Black, 2+0)

    | 0b11101000 -> Stack (White, 0+0)
    | 0b00001100 -> Stack (White, 1+0)
    | 0b00101000 -> Stack (White, 2+0)

    | _  -> raise Bad_action
;;

let input player check =
  let error () = raise Bad_action in
  let rec wait_push () =
    Random.round ();
    let c0 = read_reg PORTD in
    if c0 = 0 then
      wait_push ()
    else
      let c1 = read_reg PORTD in
      if c0 = c1 then
        key_of_ind c0
      else
        wait_push ()
  in
  let rec wait_release () =
    if read_reg PORTD <> 0 then
      wait_release ()
    else
      if read_reg PORTD <> 0 then
        wait_release ();
  in
  let wait_click () =
    wait_release ();
    wait_push ()
  in
  let k0 = wait_click () in
  let k1 = wait_click () in
  try
    match k0, k1 with
      | Stack (c, p), Table (i, j) ->
        if player <> c then error ();
        let g = Stacks.popi c p in
        let a = Add (g, i, j) in
        if not (Grid.can_add_goblet g i j) || not (check a) then (
          Stacks.push c p g;
          error ();
        );
        Grid.put_goblet g i j;
        a
      | Table (i, j), Table (k, l) ->
        if i = k && j = l then error ();
        begin match Grid.table.(i).(j) with
          | { color = c } as g :: tl ->
            if player <> c then error ();
            let a = Move (i, j, k, l) in
            if not (Grid.can_put_goblet g k l) || not (check a) then
              error ();
            Grid.sub_goblet i j;
            Grid.put_goblet g k l;
            a
          | _ -> error ();
        end
      | _ -> error ()
  with _ -> error ()
;;

let win_msg = "You win";;
let cheater_msg = "Cheater";;

let check_end () =
  match Grid.get_value () with
    | 0 ->
      Display.write_string "Game";
      Display.write_string_at 2 4 "over";
      raise Exit;
    | 1000 ->
      Display.write_string win_msg;
      raise Exit;
    | _ -> ()
;;

let sleep () = Sys.sleep 1000;;

while true do
  try
    Display.write_string "Ready";
    Display.write_action (input White (fun _ -> true));
    check_end ();
    sleep ();
    Display.write_string "Refl...";
    let a = Ia.play () in
    begin match a with
      | Nothing ->
        Display.write_string win_msg;
        raise Exit;
      | action ->
        let rec check () =
          Display.write_action action;
          try ignore (input Black ((=) action)) with Bad_action ->
            Display.write_string cheater_msg;
            sleep ();
            check ();
        in
        check ();
        check_end ();
    end;
  with
    | Bad_action ->
      Display.write_string cheater_msg;
      sleep ();
    | Invalid_argument f ->
      Display.write_string f;
      raise Exit;
    | Failure f ->
      Display.write_string f;
      raise Exit;
    | Stack_overflow ->
      Display.write_string "SO";
      raise Exit;
    | Out_of_memory ->
      Display.write_string "OOM";
      raise Exit;
    | Exit -> raise Exit
    | _ ->
      Display.write_string "UE";
      raise Exit;
done
