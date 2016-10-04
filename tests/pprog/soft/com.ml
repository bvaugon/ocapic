(*************************************************************************)
(*                                                                       *)
(*                                OCaPIC                                 *)
(*                                                                       *)
(*                             Benoit Vaugon                             *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../../../LICENSE-en.                                      *)
(*                                                                       *)
(*************************************************************************)

open Printf;;
open Serial;;

(* Channel *)

type c2p =
  | START
  | STOP
  | OFFSET_ADDRESS of int
  | BULK_ERASE_PROGRAM
  | BULK_ERASE_DATA
  | DISABLE_CODE_PROTECTION
  | WRITE_PROGRAM of int array
  | WRITE_DATA of int array
  | READ_PROGRAM of int
  | READ_DATA of int
  | WRITE_ID of (int * int * int * int)
  | READ_ID
  | READ_DEVICE_ID
  | WRITE_CONFIG_WORD of int
  | READ_CONFIG_WORD
;;

type p2c =
  | DONE
  | ID of (int * int * int * int)
  | DEVICE_ID of int
  | CONFIG_WORD of int
  | DATA of int array
  | ERROR of string
;;

let (chan : (c2p, p2c) channel) =
  open_tty "/dev/ttyUSB0"
(*open_prog "../firm/firm ocapic_dip40_simulator"*)
(*open_prog "ocasim ../firm/firm.hex ocapic_dip40_simulator"*)
;;

(* Tools *)

let error ans what =
  match ans with
    | DONE -> failwith ("expected " ^ what)
    | ID (_, _, _, _) -> failwith "unexpected id"
    | DEVICE_ID _ -> failwith "unexpected device id"
    | CONFIG_WORD _ -> failwith "unexpected config word"
    | DATA _ -> failwith "unexpected data"
    | ERROR msg -> failwith msg
;;

let check_done () =
  match receive chan with DONE -> () | ans -> error ans ""
;;

let get_id () =
  match receive chan with ID id -> id | ans -> error ans "id"
;;

let get_device_id () =
  match receive chan with DEVICE_ID did -> did | ans -> error ans "device id"
;;

let get_config_word () =
  match receive chan with CONFIG_WORD cw -> cw | ans -> error ans "config word"
;;

let get_data () =
  match receive chan with DATA data -> data | ans -> error ans "data"
;;

(* Functions *)

let read_hex filename = Mem.parse (Hex.parse filename);;

let start () =
  send chan START;
  check_done ();
;;

let stop () =
  send chan STOP;
  check_done ();
;;

let offset_address n =
  send chan (OFFSET_ADDRESS n);
  check_done ();
;;

let bulk_erase_program () =
  send chan BULK_ERASE_PROGRAM;
  check_done ();
;;

let bulk_erase_data () =
  send chan BULK_ERASE_DATA;
  check_done ();
;;

let disable_code_protection () =
  send chan DISABLE_CODE_PROTECTION;
  check_done ();
;;

let write_program prog =
  send chan (WRITE_PROGRAM prog);
  check_done ();
;;

let write_data data =
  send chan (WRITE_DATA data);
  check_done ();
;;

let read_program size =
  send chan (READ_PROGRAM size);
  get_data ()
;;

let read_data size =
  send chan (READ_DATA size);
  get_data ()
;;

let write_id id1 id2 id3 id4 =
  send chan (WRITE_ID (id1, id2, id3, id4));
  check_done ();
;;

let read_id () =
  send chan READ_ID;
  get_id ()
;;

let read_device_id () =
  send chan READ_DEVICE_ID;
  get_device_id ()
;;

let write_config_word cw =
  send chan (WRITE_CONFIG_WORD cw);
  check_done ();
;;

let read_config_word () =
  send chan READ_CONFIG_WORD;
  get_config_word ()
;;

(***)

let write filename =
  let mem = read_hex filename in
  let size = Array.length mem.Mem.program in
  let step = 16 in
  let rec f ind =
    printf "\rWrite program: %d%% %!" (100 * ind / size);
    if ind + step < size then (
      write_program (Array.sub mem.Mem.program ind step);
      f (ind + step);
    ) else (
      write_program (Array.sub mem.Mem.program ind (size - ind));
      printf "\rWrite program: 100%% \n%!";
    )
  in
  let rec g ind acc =
    printf "\rRead program: %d%% %!" (100 * ind / size);
    if ind + step < size then (
      g (ind + step) (read_program step :: acc)
    ) else (
      let l = read_program (size - ind) :: acc in
      printf "\rRead program: 100%% %!";
      Array.concat (List.rev l)
    )
  in
  stop ();
  (* Erase *)
  printf "Bulk erase program... %!";
  start (); bulk_erase_program (); stop ();
  printf "done\n%!";

  (* Write configuration word *)
  begin match mem.Mem.config_word with
    | None -> ()
    | Some cw ->
      printf "Write configuration word (0x%04X)... %!" cw;
      start (); write_config_word cw; stop ();
      printf "done\n%!";
  end;

  (* Write id *)
  begin match mem.Mem.id with
    | None -> ()
    | Some (id1, id2, id3, id4) ->
      printf "Write id: 0x%04X 0x%04X 0x%04X 0x%04X... %!" id1 id2 id3 id4;
      start (); write_id id1 id2 id3 id4; stop ();
      printf "done\n%!";
  end;

  (* Write program *)
  if size <> 0 then ( start (); f 0; stop () );

  (* Verify configuration word *)
  begin match mem.Mem.config_word with
    | None -> ()
    | Some cw ->
      printf "Read configuration word... %!";
      start (); let rcw = read_config_word () in stop ();
                if cw = rcw then printf "ok\n%!"
                else printf "done\nError: configuration word = 0x%04X\n%!" rcw;
  end;

  (* Verify id *)
  begin match mem.Mem.id with
    | None -> ()
    | Some id ->
      printf "Read id... %!";
      start (); let (id1, id2, id3, id4) as rid = read_id () in stop ();
                if rid = id then printf "ok\n%!"
                else printf "done\nError: id = 0x%04X 0x%04X 0x%04X 0x%04X\n%!"
                  id1 id2 id3 id4
  end;

  (* Verify program *)
  if size <> 0 then (
    start (); let p = g 0 [] in stop ();
              if p = mem.Mem.program then printf "ok\n%!"
              else printf "\nError: writing program failed\n%!";
  );
;;
