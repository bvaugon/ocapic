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

exception Exn of string
exception Restart

(* Constants *)

let atom0_adr = 0xF88;;
let int32_custom_adr = 0x15E0;;
let int64_custom_adr = 0x15F0;;
let serialize_buffer_length = 2000;;

(* Types *)

type tictac = Tic | Tac

type flag =
  | New of tictac | End of tictac | Error | Oom | Get | Receive | Invalid of int

type ('a, 'b) channel = {
  input : in_channel;
  output : out_channel;
  buffer : int array;
  receive_delay : float;
  send_delay : float;
  main_mutex : Mutex.t;
  pinger_mutex : Mutex.t;
  ping_mutex : Mutex.t;
  receive_finalizer_mutex : Mutex.t;
  send_finalizer_mutex : Mutex.t;
  mutable send_cont : unit -> unit;
  mutable ping : flag option;
  mutable flag_cache : flag option;
  mutable tictac : tictac;
}

(* Flags *)

let flag_base = 0xA6;;

let newtic_flag  = flag_base lxor 0b00000000;;
let newtac_flag  = flag_base lxor 0b11111111;;
let endtic_flag  = flag_base lxor 0b00001111;;
let endtac_flag  = flag_base lxor 0b11110000;;
let receive_flag = flag_base lxor 0b00111100;;
let error_flag   = flag_base lxor 0b11000011;;
let get_flag     = flag_base lxor 0b00110011;;
let oom_flag     = flag_base lxor 0b11001100;;

let esc_flag  = 0xFF;;
let res1_flag = 0x03;;
let res2_flag = 0x1A;;
let res3_flag = 0x1C;;

let flag_of_byte b =
  if b = newtic_flag then New Tic
  else if b = newtac_flag then New Tac
  else if b = endtic_flag then End Tic
  else if b = endtac_flag then End Tac
  else if b = receive_flag then Receive
  else if b = error_flag then Error
  else if b = get_flag then Get
  else if b = oom_flag then Oom
  else Invalid b
;;

let byte_of_flag f =
  match f with
    | New Tic -> newtic_flag
    | New Tac -> newtac_flag
    | End Tic -> endtic_flag
    | End Tac -> endtac_flag
    | Receive -> receive_flag
    | Error -> error_flag
    | Get -> get_flag
    | Oom -> oom_flag
    | Invalid _ -> assert false
;;

(* Bytes *)

let low word = word land 0xFF;;

let high word = (word lsr 8) land 0xFF;;

let upper word = (word lsr 16) land 0xFF;;

(* Floats *)

let convert_float f =
  let (x, ex) = frexp f in
  if f = 0.0 || f = -0.0 || ex < -128 then (0, 0, 0, 0) else (
    let rec d i a r =
      if i < 0 then
        r
      else if a < 0.5 then
        d (pred i) (a *. 2.) r
      else
        d (pred i) ((a -. 0.5) *. 2.) (r lor (1 lsl i))
    in
    let s = if f < 0. then 1 else 0 in
    let m = (d 24 (abs_float x) 0 land 0xFFFFFF) lsr 1 in
    let b0 = low m in
    let b1 = high m in
    let b2 = ((upper m) land 0x7F) lor (s lsl 7) in
    let b3 = low (ex + 126) in
    (b0, b1, b2, b3)
  )
;;

let trevnoc_float b0 b1 b2 b3 =
  if b0 = 0 && b1 = 0 && b2 = 0 && b3 = 0 then 0. else
    let mantissa = float_of_int (b0 + b1*256 +(b2 lor 0x80)*256*256) in
    let sign = (b2 land 0x80) = 0x80 in
    let exp = float_of_int (b3 - 150) in
    let f = (2. ** exp) *. mantissa in
    if sign then ~-.f else f
;;

(* CRC *)

exception Invalid_hash

let hash data =
  let accl = ref 0x6B in
  let acch = ref 0x39 in
  let f n =
    accl := (!accl * !acch) land 0xFF;
    acch := !accl lxor !acch;
    accl := !accl lxor n;
    acch := !acch lxor n;
  in
  Array.iter f data;
  (!accl, !acch)
;;

let check_hash hashl hashh data =
  let (hashcl, hashch) = hash data in
  if hashl <> hashcl || hashh <> hashch then raise Invalid_hash;
;;

(* IO *)

let flush chan =
  flush chan.output;
  Thread.delay chan.send_delay;
;;

let send_byte chan b =
  if b = res1_flag || b = res2_flag || b = res3_flag || b = esc_flag then (
    output_byte chan.output esc_flag;
    flush chan;
    output_byte chan.output (b lxor esc_flag);
    flush chan;
  ) else (
    output_byte chan.output b;
    flush chan;
  )
;;

let send_flag chan f =
  let b = byte_of_flag f in
  send_byte chan b;
  send_byte chan b;
;;

let send_data chan data =
  Array.iter (send_byte chan) data;
  let (hashl, hashh) = hash data in
  send_byte chan hashl;
  send_byte chan hashh;
;;

(***)

let start_ping chan ping_flag =
  Mutex.lock chan.ping_mutex;
  chan.ping <- Some ping_flag;
  Mutex.unlock chan.ping_mutex;
  Mutex.unlock chan.pinger_mutex;
;;

let stop_ping chan =
  Mutex.lock chan.pinger_mutex;
  Mutex.lock chan.ping_mutex;
  chan.ping <- None;
  Mutex.unlock chan.ping_mutex;
;;

(***)

let receive_byte ping_flag chan =
  start_ping chan ping_flag;
  let b =
    let b = input_byte chan.input in
    if b = esc_flag then (input_byte chan.input) lxor esc_flag else b
  in
  stop_ping chan;
  b
;;

let receive_flag ping_flag chan =
  let flag =
    match chan.flag_cache with
      | Some flag -> chan.flag_cache <- None; flag
      | None ->
        let b0 = receive_byte ping_flag chan in
        let b1 = receive_byte ping_flag chan in
        if b0 = b1 then flag_of_byte b0
        else
          let b2 = receive_byte ping_flag chan in
          if b1 = b2 then flag_of_byte b1 else Error
  in
  flag
;;

let receive_data ping_flag chan size =
  let data = Array.make size 0 in
  for i = 0 to pred size do
    data.(i) <- receive_byte ping_flag chan;
  done;
  let hashl = receive_byte ping_flag chan in
  let hashh = receive_byte ping_flag chan in
  check_hash hashl hashh data;
  data
;;

let receive_pair ping_flag chan =
  let d = receive_data ping_flag chan 2 in
  (d.(0), d.(1))
;;

(* Channel creation *)

let create_receive_finalizer chan =
  let rec end_loop () =
    send_flag chan (End chan.tictac);
    match receive_flag (End chan.tictac) chan with
      | End t when chan.tictac = t -> ()
      | Error -> end_loop ()
      | New t when chan.tictac = t -> end_loop ()
      | flag -> chan.flag_cache <- Some flag
  in
  let rec finalizer_loop () =
    Mutex.lock chan.receive_finalizer_mutex;
    end_loop ();
    chan.tictac <- if chan.tictac = Tic then Tac else Tic;
    Mutex.unlock chan.main_mutex;
    finalizer_loop ();
  in
  Mutex.lock chan.receive_finalizer_mutex;
  ignore (Thread.create finalizer_loop ());
;;

let create_send_finalizer chan =
  let rec end_loop () =
    match receive_flag (New chan.tictac) chan with
      | Receive ->
        chan.send_cont ();
        Mutex.lock chan.send_finalizer_mutex;
        end_loop ();
      | End t when chan.tictac = t -> ()
      | Error -> end_loop ()
      | flag -> chan.flag_cache <- Some flag
  in
  let rec finalizer_loop () =
    Mutex.lock chan.send_finalizer_mutex;
    end_loop ();
    chan.tictac <- if chan.tictac = Tic then Tac else Tic;
    Mutex.unlock chan.main_mutex;
    finalizer_loop ();
  in
  Mutex.lock chan.send_finalizer_mutex;
  ignore (Thread.create finalizer_loop ());
;;

let create_pinger chan =
  let rec ping_loop () =
    Thread.delay chan.receive_delay;
    Mutex.lock chan.ping_mutex;
    match chan.ping with
      | Some ping_flag ->
        Mutex.unlock chan.ping_mutex;
        send_byte chan (byte_of_flag ping_flag);
        ping_loop ();
      | None ->
        Mutex.unlock chan.ping_mutex;
  in
  let rec thread_loop () =
    Mutex.lock chan.pinger_mutex;
    Mutex.unlock chan.pinger_mutex;
    ping_loop ();
    thread_loop ();
  in
  Mutex.lock chan.pinger_mutex;
  ignore (Thread.create thread_loop ());
;;

let create_chan send_delay receive_delay input output =
  let chan = {
    input = input;
    output = output;
    buffer = Array.make serialize_buffer_length 0;
    receive_delay = receive_delay;
    send_delay = send_delay;
    main_mutex = Mutex.create ();
    pinger_mutex = Mutex.create ();
    ping_mutex = Mutex.create ();
    receive_finalizer_mutex = Mutex.create ();
    send_finalizer_mutex = Mutex.create ();
    send_cont = (fun () -> ());
    ping = None;
    flag_cache = None;
    tictac = Tic;
  } in
  create_pinger chan;
  create_receive_finalizer chan;
  create_send_finalizer chan;
  chan
;;

let open_tty ?(send_delay=0.0025) ?(receive_delay=0.01) filename =
  let input = open_in_bin filename in
  let output = open_out_bin filename in
  create_chan send_delay receive_delay input output
;;

let open_prog prog =
  let ((input, output) as p) = Unix.open_process prog in
  at_exit (fun () -> ignore (Unix.close_process p));
  create_chan 0. 1e9 input output
;;

(* Send *)

let check_integer n =
  if n < -16384 || n > 16383 then
    let msg =
      Printf.sprintf "send: integer %d out of bounds [ -16384 ; 16383 ]" n
    in
    raise (Exn msg)
;;

(***)

let double_field x i = Array.get (Obj.obj x : float array) i;;

let serialize buffer obj =
  let error msg = raise (Exn ("send: " ^ msg)) in
  let adr = ref 0 in
  let stored = ref [] in
  let store_byte b =
    let a = !adr in
    if a >= serialize_buffer_length then error "too big value";
    buffer.(a) <- b;
    incr adr;
  in
  let curr_adr () = !adr in
  let search_obj obj =
    try Some (List.assq obj !stored)
    with Not_found -> None
  in
  let add_obj obj adr = stored := (obj, adr) :: !stored in
  let store_float f =
    let (b0, b1, b2, b3) = convert_float f in
    store_byte b0;
    store_byte b1;
    store_byte b2;
    store_byte b3;
  in
  let rec f o =
    add_obj o (curr_adr () + 2);
    let tag = Obj.tag o in
    let size = Obj.size o in
    if tag = Obj.lazy_tag then error "lazy block"
    else if tag = Obj.closure_tag then error "closure"
    else if tag = Obj.object_tag then error "object"
    else if tag = Obj.infix_tag then error "closure"
    else if tag = Obj.forward_tag then f (Obj.field o 0)
    else if tag = Obj.abstract_tag then error "abstract block"
    else if tag = Obj.string_tag then (
      let str = Obj.obj o in
      let len = String.length str in
      let sz = len / 2 + 1 in
      if len > 509 then error "too big string (size > 509)";
      store_byte tag;
      store_byte sz;
      let my_adr = curr_adr () in
      for i = 0 to pred len do
        store_byte (int_of_char str.[i]);
      done;
      store_byte 0;
      if len mod 2 = 0 then store_byte 1;
      my_adr
    ) else if tag = Obj.double_tag then (
      let (fl : float) = Obj.obj o in
      store_byte tag;
      store_byte 2;
      let my_adr = curr_adr () in
      store_float fl;
      my_adr
    ) else if tag = Obj.double_array_tag then (
      if size > 127 then error "too big float array (size > 127)";
      store_byte tag;
      store_byte (2 * size);
      let my_adr = curr_adr () in
      for i = 0 to pred size do
        let fi = double_field o i in
        store_float fi;
      done;
      my_adr
    ) else if tag = Obj.custom_tag then (
      let key = Obj.field o 0 in
      if key == Obj.field (Obj.repr 0l) 0 then (
        let n : Int32.t = Obj.obj o in
        let get_byte i =
          Int32.to_int (Int32.logand (Int32.shift_right n (8 * i)) 255l)
        in
        store_byte tag;
        store_byte 3;
        let my_adr = curr_adr () in
        store_byte (low int32_custom_adr);
        store_byte (high int32_custom_adr);
        for i = 0 to 3 do store_byte (get_byte i) done;
        my_adr
      ) else if key == Obj.field (Obj.repr 0L) 0 then (
        let n : Int64.t = Obj.obj o in
        let get_byte i =
          Int64.to_int (Int64.logand (Int64.shift_right n (8 * i)) 255L)
        in
        store_byte tag;
        store_byte 5;
        let my_adr = curr_adr () in
        store_byte (low int64_custom_adr);
        store_byte (high int64_custom_adr);
        for i = 0 to 7 do store_byte (get_byte i) done;
        my_adr
      ) else
          error "unknown custom block"
    ) else if tag = Obj.out_of_heap_tag then error "out of heap block"
      else if tag = 1002 (* [Obj.unaligned_tag] for compatibility with 3.10 *)
      then error "unaligned adress"
      else if tag > 255 then error "invalid block tag (> 255)"
      else if size = 0 then atom0_adr
      else (
        if size > 255 then error "too big block (size > 255)";
        store_byte tag;
        store_byte size;
        let my_adr = curr_adr () in
        adr := !adr + size * 2;
        if !adr >= serialize_buffer_length then error "too big value";
        for i = 0 to pred size do
          let fi = Obj.field o i in
          if Obj.is_int fi then (
            let (n : int) = Obj.obj fi in
            check_integer n;
            buffer.(my_adr + 2 * i) <- ((n lsl 1) lor 1) land 0xFF;
            buffer.(my_adr + 2 * i + 1) <- (n lsr 7) land 0xFF;
          ) else (
            let fi_adr =
              match search_obj fi with
                | Some adr -> adr
                | None -> f fi
            in
            if fi_adr = atom0_adr then (
              buffer.(my_adr + 2 * i) <- low atom0_adr;
              buffer.(my_adr + 2 * i + 1) <- high atom0_adr;
            ) else (
              buffer.(my_adr + 2 * i) <- -(low fi_adr);
              buffer.(my_adr + 2 * i + 1) <- high fi_adr;
            )
          )
        done;
        my_adr
      )
  in
  assert (f obj = 2);
  Array.sub buffer 0 !adr
;;

(***)

let set_adresses data adr =
  for i = 0 to Array.length data - 1 do
    let d = data.(i) in
    if d < 0 then
      let a = (data.(i+1) lsl 8) - d + adr in
      data.(i) <- low a;
      data.(i+1) <- high a;
  done
;;

(***)

let send chan value =
  let error msg = raise (Exn ("send: " ^ msg)) in
  let obj = Obj.repr value in
  let (is_literal, header, data) =
    if Obj.is_int obj then
      let (n : int) = Obj.obj obj in
      check_integer n;
      let accu = (n lsl 1) lor 1 in
      (true, [| low accu ; high accu |], [||])
    else if Obj.size obj = 0 then
      (true, [| low atom0_adr ; high atom0_adr |], [||])
    else
      let data = serialize chan.buffer obj in
      let size = Array.length data in
      (false, [| low size ; high size |], data)
  in
  let () = Mutex.lock chan.main_mutex in
  let new_tictac = New chan.tictac in
  let rec start () =
    send_flag chan new_tictac;
    try
      match receive_flag new_tictac chan with
        | Receive -> send_header ();
        | New _ -> error "collision";
        | _ -> raise Restart;
    with Invalid_hash | Restart ->
      continue ();
  and continue () =
    try
      match receive_flag new_tictac chan with
        | Receive -> send_header ();
        | New _ -> error "collision";
        | _ -> raise Restart
    with Invalid_hash | Restart ->
      continue ();
  and send_header () =
    send_data chan header;
    if is_literal then wait_end () else check_size ();
  and check_size () =
    match receive_flag new_tictac chan with
      | Get -> receive_adr ();
      | Oom -> error "too big value";
      | Receive -> send_header ();
      | _ -> raise Restart;
  and receive_adr () =
    let (bl, bh) = receive_pair new_tictac chan in
    let adr = bl lor (bh lsl 8) in
    set_adresses data adr;
    send_data chan data;
    wait_end ();
  and wait_end () =
    chan.send_cont <- resend;
    Mutex.unlock chan.send_finalizer_mutex;
  and resend () =
    try send_header ()
    with Invalid_hash | Restart -> continue ();
  in
  try start () with exn -> Mutex.unlock chan.main_mutex; raise exn;
;;

(* Receive *)

module IMap = Map.Make(struct type t = int let compare = compare end);;

let receive chan =
  let todos = ref IMap.empty in
  let values = ref IMap.empty in
  let error msg = raise (Exn ("receive: " ^ msg)) in
  let () = Mutex.lock chan.main_mutex in
  let rec start () =
    send_flag chan Receive;
    try
      match receive_flag Error chan with
        | New t when chan.tictac = t -> get_root ()
        | Receive -> error "collision"
        | _ -> raise Restart
    with Invalid_hash | Restart ->
      continue ()
  and continue () =
    try
      begin match receive_flag Error chan with
        | New t when chan.tictac = t ->
          send_flag chan Receive;
          get_root ()
        | Receive -> error "collision"
        | _ -> raise Restart
      end;
    with Invalid_hash | Restart ->
      continue ()
  and get_root () =
    let (bl, bh) = receive_pair Error chan in
    let d = get_data bl bh in
    dotodos ();
    Obj.obj d
  and get_data bl bh =
    if bl = low atom0_adr && bh = high atom0_adr then Obj.repr [||]
    else if bl land 0x1 = 0 then get_graph (bl lor (bh lsl 8))
    else
      let mask = if bh land 0x80 = 0 then 0 else (-1 lsl 15) in
      Obj.repr ((bl lsr 1) lor (bh lsl 7) lor mask)
  and get_graph adr =
    try IMap.find adr !values
    with Not_found ->
      let adrl = low adr and adrh = high adr in
      send_flag chan Get;
      send_data chan [| adrl ; adrh |];
      receive_block adr
  and receive_block adr =
    let (tag, size) = receive_pair Error chan in
    let block = receive_content adr tag size in
    values := IMap.add adr block !values;
    block
  and receive_content adr tag size =
    let data = receive_data Error chan (2 * size) in
    if tag = Obj.lazy_tag then error "lazy block"
    else if tag = Obj.closure_tag then error "closure"
    else if tag = Obj.object_tag then error "object"
    else if tag = Obj.infix_tag then error "infix block"
    else if tag = Obj.forward_tag then error "forward block"
    else if tag = Obj.abstract_tag then error "abstract block"
    else if tag = Obj.string_tag then (
      let len = 2*size - (if data.(2*size - 1) = 0 then 1 else 2) in
      if data.(len) <> 0 then error "invalid string";
      let s = Bytes.create len in
      for i = 0 to pred len do Bytes.set s i (char_of_int data.(i)) done;
      Obj.repr s
    ) else if tag = Obj.double_tag then (
      if size <> 2 then error "invalid double";
      Obj.repr (trevnoc_float data.(0) data.(1) data.(2) data.(3))
    ) else if tag = Obj.double_array_tag then (
      if size mod 2 <> 0 then
        error "invalid double array block";
      let f i =
        trevnoc_float data.(4*i) data.(4*i+1) data.(4*i+2) data.(4*i+3)
      in
      Obj.repr (Array.init (size / 2) f)
    ) else if tag = Obj.custom_tag then (
      let key = data.(0) + 256 * data.(1) in
      if key = int32_custom_adr then (
        if size <> 3 then error "invalid int32 block";
        let rec f i acc =
          if i = 1 then acc
          else f (pred i) (Int32.add (Int32.mul 256l acc)
                             (Int32.of_int data.(i)))
        in
        Obj.repr (f 5 0l)
      ) else if key = int64_custom_adr then (
        if size <> 5 then error "invalid int64 block";
        let rec f i acc =
          if i = 1 then acc
          else f (pred i) (Int64.add (Int64.mul 256L acc)
                             (Int64.of_int data.(i)))
        in
        Obj.repr (f 7 0L)
      ) else (
        error "unknown custom block";
      )
    ) else (
      let block = Obj.new_block tag size in
      todos := IMap.add adr (block, data, size) !todos;
      block
    )
  and dotodos () =
    let f adr (block, data, size) =
      for i = 0 to pred size do
        let d = get_data data.(2*i) data.(2*i+1) in
        Obj.set_field block i d
      done;
      todos := IMap.remove adr !todos;
    in
    while not (IMap.is_empty !todos) do
      IMap.iter f !todos;
    done
  in
  try let value = start () in Mutex.unlock chan.receive_finalizer_mutex; value
  with exn -> Mutex.unlock chan.main_mutex; raise exn;
;;
