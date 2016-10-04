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

open Types

exception Bad
exception Win of action

let compute_worst popable_whites best_value =
  let worst_value = ref 0 in
  let try_add g =
    for i = 0 to 3 do
      for j = 0 to 3 do
        if Grid.can_add_goblet g i j then (
          Grid.put_goblet g i j;
          let value = Grid.get_value () in
          Grid.sub_goblet i j;
          if value > best_value then raise Bad;
          if value > !worst_value then worst_value := value;
        );
      done
    done
  in
  List.iter try_add popable_whites;
  for i = 0 to 3 do
    for j = 0 to 3 do
      match Grid.table.(i).(j) with
        | { color = White } as g :: _ ->
          Grid.sub_goblet i j;
          for k = 0 to 3 do
            for l = 0 to 3 do
              if (i <> k || j <> l) && Grid.can_put_goblet g k l then (
                Grid.put_goblet g k l;
                let value = Grid.get_value () in
                Grid.sub_goblet k l;
                if value > best_value then (
                  Grid.put_goblet g i j;
                  raise Bad;
                );
                if value > !worst_value then worst_value := value;
              );
            done
          done;
          Grid.put_goblet g i j;
        | _ -> ()
    done
  done;
  !worst_value
;;

let play () =
  let popable_whites = Stacks.popables White in
  let popable_blacks = Stacks.popables Black in
  let best_value = ref 1001 in
  let best_action = ref Nothing in
  let try_add g =
    for i = 0 to 3 do
      for j = 0 to 3 do
        if Grid.can_add_goblet g i j then (
          Grid.put_goblet g i j;
          let value = Grid.get_value () in
          if value = 0 then (
            Grid.sub_goblet i j;
            raise (Win (Add (g, i, j)));
          );
          begin try
                  let worst = compute_worst popable_whites !best_value in
                  let best = !best_value in
                  if worst < best || (worst = best && Random.bool ()) then (
                    best_value := worst;
                    best_action := Add (g, i, j);
                  );
            with Bad -> () end;
          Grid.sub_goblet i j;
        );
      done
    done
  in
  try
    List.iter try_add popable_blacks;
    for i = 0 to 3 do
      for j = 0 to 3 do
        match Grid.table.(i).(j) with
          | { color = Black } as g :: _ ->
            Grid.sub_goblet i j;
            for k = 0 to 3 do
              for l = 0 to 3 do
                if (i <> k || j <> l) && Grid.can_put_goblet g k l then (
                  Grid.put_goblet g k l;
                  let value = Grid.get_value () in
                  if value = 0 then (
                    Grid.sub_goblet k l;
                    Grid.put_goblet g i j;
                    raise (Win (Move (i, j, k, l)));
                  );
                  begin try
                          let worst= compute_worst popable_whites !best_value in
                          let best = !best_value in
                          if worst < best || (worst = best && Random.bool ())
                          then (
                            best_value := worst;
                            best_action := Move (i, j, k, l);
                          );
                    with Bad -> () end;
                  Grid.sub_goblet k l;
                );
              done
            done;
            Grid.put_goblet g i j;
          | _ -> ()
      done
    done;
    !best_action
  with Win action -> action
;;
