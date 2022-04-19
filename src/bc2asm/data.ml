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

open Instr

type elem =
  | String of string
  | Float of float
  | Floats of float array
  | Int of int
  | Int_32 of Int32.t
  | Int_64 of Int64.t
  | Block of elem array * int
  | Closure of elem array
  | Out_of_heap of int
;;

type mem_item =
  | Val of int
  | Adr of int
  | FlpL of float
  | FlpH of float
  | Header of int * int
  | Charchar of int * int
;;

let exn_number = 12

let remap_code code =
  let f bc =
    match bc with
      | Getglobal ind ->
        if ind < exn_number then Const ind
        else Getglobal (ind - exn_number)
      | Pushgetglobal ind ->
        if ind < exn_number then Pushconst ind
        else Pushgetglobal (ind - exn_number)
      | Getglobalfield (n, p) ->
        if n < exn_number then
          let msg =
            Printf.sprintf
              "invalid instruction `GETGLOBALFIELD %d %d' (%d < %d)"
              n p n exn_number
          in
          failwith msg
        else
          Getglobalfield (n - exn_number, p)
      | Pushgetglobalfield (n, p) ->
        if n < exn_number then
          let msg =
            Printf.sprintf
              "invalid instruction `PUSHGETGLOBALFIELD %d %d' (%d < %d)"
              n p n exn_number
          in
          failwith msg
        else
          Pushgetglobalfield (n - exn_number, p)
      | Setglobal ind ->
        if ind < exn_number then
          let msg =
            Printf.sprintf
              "invalid instruction `SETGLOBAL %d' (%d < %d)"
              ind ind exn_number
          in
          failwith msg
        else
          Setglobal (ind - exn_number)
      | _ -> bc
  in
  let g instr = { instr with bc = f instr.bc } in
  Array.map g code
;;

module Int32Map = Map.Make(Int32)
module Int64Map = Map.Make(Int64)
module FloatMap = Map.Make(
  struct
    type t = float
    let compare (x:t) (y:t) =
      if (x = 0. && y = -0.) || (x = -0. && y = 0.) then 0 else compare x y
  end
)

let factor data =
  let int32s = ref Int32Map.empty in
  let int64s = ref Int64Map.empty in
  let floats = ref FloatMap.empty in
  let rec f d =
    match d with
      | String _ | Floats _ | Int _ | Closure _ | Out_of_heap _ -> d
      | Block (tab, tag) -> Block (Array.map f tab, tag)
      | Float f ->
        begin
          try Float (FloatMap.find f !floats)
          with Not_found -> floats := FloatMap.add f f !floats ; d
        end
      | Int_32 i ->
        begin
          try Int_32 (Int32Map.find i !int32s)
          with Not_found -> int32s := Int32Map.add i i !int32s ; d
        end
      | Int_64 i ->
        begin
          try Int_64 (Int64Map.find i !int64s)
          with Not_found -> int64s := Int64Map.add i i !int64s ; d
        end
  in
  Array.map f data
;;

let parse data =
  let rec elem_of_value value =
    match value with
    | OByteLib.Value.Int n               -> Int n
    | OByteLib.Value.Int32 n             -> Int_32 n
    | OByteLib.Value.Int64 n             -> Int_64 n
    | OByteLib.Value.Nativeint _         -> assert false
    | OByteLib.Value.Float x             -> Float x
    | OByteLib.Value.Float_array tbl     -> Floats tbl
    | OByteLib.Value.String s            -> String s
    | OByteLib.Value.Object fields       -> Block (Array.map elem_of_value fields, Obj.object_tag)
    | OByteLib.Value.Block (tag, fields) ->
      if tag = Obj.closure_tag
      then Closure (Array.map elem_of_value fields)
      else Block (Array.map elem_of_value fields, tag) in
  Array.map elem_of_value data
;;

module SSet = Set.Make (String)

let print_useprim oc data =
  let rec search acc elem =
    match elem with
      | Block (tab, _) -> Array.fold_left search acc tab
      | Int_32 _ -> SSet.add "caml_int32_custom" acc
      | Int_64 _ -> SSet.add "caml_int64_custom" acc
      | String _ | Float _ | Floats _ | Int _ | Closure _ | Out_of_heap _ ->
        acc
  in
  let used_primitives = Array.fold_left search SSet.empty data in
  SSet.iter (Printf.fprintf oc "#define caml_useprim_%s\n") used_primitives;
;;

let print oc data =
  let rec print_elem elem =
    match elem with
      | String str -> Printf.fprintf oc "\"%s\"" str
      | Float f -> Printf.fprintf oc "%f" f
      | Floats tbl ->
        Printf.fprintf oc "[|";
        Array.iter (Printf.fprintf oc " %f ") tbl;
        Printf.fprintf oc "|]";
      | Int n -> Printf.fprintf oc "%d" n
      | Int_32 n -> Printf.fprintf oc "%ld : Int32.t" n
      | Int_64 n -> Printf.fprintf oc "%Ld : Int64.t" n
      | Block (tab, tag) ->
        Printf.fprintf oc "[%d|" tag;
        Array.iter
          (fun e -> output_char oc ' ' ; print_elem e ; output_char oc ' ')
          tab;
        Printf.fprintf oc "|%d]" tag;
      | Closure tab ->
        Printf.fprintf oc "Closure [|";
        Array.iter
          (fun e -> output_char oc ' ' ; print_elem e ; output_char oc ' ')
          tab;
        Printf.fprintf oc "|]";
      | Out_of_heap p -> Printf.fprintf oc "@0x%08x" p;
  in
  let print_elem i elem =
    Printf.fprintf oc "%-3d   " i;
    print_elem elem;
    output_char oc '\n';
  in
  Array.iteri print_elem data;
;;

let to_init gc_algo data =
  let stack = Queue.create () in
  let heap = Queue.create () in
  let cache = ref [] in
  let add_cache obj adr = cache := (Obj.repr obj, adr) :: !cache in
  let search_cache obj =
    try Some (List.assq (Obj.repr obj) !cache)
    with Not_found -> None
  in
  let get_adr () = Adr (2 * Queue.length heap + Constants.heap1_anchor) in
  let push_stack x = Queue.push x stack in
  let push_heap x = Queue.push x heap in
  let check_float f =
    let (_, n) = frexp f in
    if n > 127 then
      failwith (Printf.sprintf "float out of range: %f" f);
  in
  let rec export e =
    match e with
      | Int i ->
        if i < -0x4000 || i > 0x7FFF then
          failwith (Printf.sprintf
                      "integer out of bounds in global data: %d" i);
        Val i
      | Int_32 n ->
        begin match search_cache n with
          | Some adr -> adr
          | None ->
            let get_byte i =
              Int32.to_int (Int32.logand (Int32.shift_right n (8 * i)) 255l)
            in
            push_heap (Header (3, Obj.custom_tag));
            let res = get_adr () in
            push_heap (Adr (Constants.int32_custom_adr gc_algo));
            push_heap (Charchar (get_byte 0, get_byte 1));
            push_heap (Charchar (get_byte 2, get_byte 3));
            add_cache n res;
            res
        end
      | Int_64 n ->
        begin match search_cache n with
          | Some adr -> adr
          | None ->
            let get_byte i =
              Int64.to_int (Int64.logand (Int64.shift_right n (8 * i)) 255L)
            in
            push_heap (Header (5, Obj.custom_tag));
            let res = get_adr () in
            push_heap (Adr (Constants.int64_custom_adr gc_algo));
            push_heap (Charchar (get_byte 0, get_byte 1));
            push_heap (Charchar (get_byte 2, get_byte 3));
            push_heap (Charchar (get_byte 4, get_byte 5));
            push_heap (Charchar (get_byte 6, get_byte 7));
            add_cache n res;
            res
        end
      | String str ->
        begin match search_cache str with
          | Some adr -> adr
          | None ->
            let str_len = String.length str in
            let blk_len = str_len / 2 + 1 in
            if blk_len > 255 then
              failwith (Printf.sprintf
                            "too large string in global data: \"%s\"" str);
            push_heap (Header (blk_len, Obj.string_tag));
            let res = get_adr () in
            for i = 0 to blk_len - 2 do
              let ci = int_of_char str.[2 * i] in
              let csi = int_of_char str.[2 * i + 1] in
              push_heap (Charchar (ci, csi))
            done;
            if str_len mod 2 = 0 then
              push_heap (Charchar (0, 1))
            else
              push_heap (Charchar (int_of_char str.[str_len - 1], 0));
            add_cache str res;
            res
        end
      | Block (tab, tag) ->
        let size = Array.length tab in
        if size = 0 then
          Adr Constants.atom0_adr
        else (
          if size > 255 then
            failwith (Printf.sprintf
                        "too big block in global data: %d > 255" size);
          let body = Array.make size (Val 42) in
          for i = 0 to size - 1 do body.(i) <- export tab.(i) done;
          push_heap (Header (size, tag));
          let res = get_adr () in
          for i = 0 to size - 1 do push_heap body.(i) done;
          res
        )
      | Float f ->
        begin match search_cache f with
          | Some adr -> adr
          | None ->
            check_float f;
            push_heap (Header (2, Obj.double_tag));
            let res = get_adr () in
            push_heap (FlpL f);
            push_heap (FlpH f);
            add_cache f res;
            res
        end
      | Floats tab ->
        begin match search_cache tab with
          | Some adr -> adr
          | None ->
            let size = Array.length tab in
            if size = 0 then
              failwith "float array of size 0"
            else if size > 127 then
              failwith (Printf.sprintf
                          "too big float array in global data: %d > 127" size)
            else (
              push_heap (Header (2 * size, Obj.double_array_tag));
              let res = get_adr () in
              for i = 0 to size - 1 do
                check_float tab.(i);
                push_heap (FlpL tab.(i));
                push_heap (FlpH tab.(i));
              done;
              add_cache tab res;
              res
            )
        end
      | Closure _ -> failwith "closure in global data unsupported"
      | Out_of_heap _ -> failwith "\"out of heap\" in global data"
  in
  Array.iter (fun e -> push_stack (export e)) data;
  (stack, heap)
;;

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
    let b0 = m land 0xFF in
    let b1 = (m lsr 8) land 0xFF in
    let b2 = ((m lsr 16) land 0x7F) lor (s lsl 7) in
    let b3 = (ex + 126) land 0xFF in
    (b0, b1, b2, b3)
  )
;;

let export gc_algo oc data =
  let (stack, heap) = to_init gc_algo data in
  let cpt = ref 0 in
  let put_byte byte =
    if !cpt land 0b111 = 0 then
      output_string oc "\n        db      "
    else
      output_string oc ", ";
    Printf.fprintf oc "0x%02x" byte;
    incr cpt;
  in
  let export e =
    match e with
      | Val n ->
        put_byte (((n land 0x7F) lsl 1) lor 1);
        put_byte ((n lsr 7) land 0xFF);
      | Adr n ->
        put_byte (n land 0xFF);
        put_byte (n lsr 8);
      | FlpL f ->
        let (b0, b1, _, _) = convert_float f in
        put_byte b0;
        put_byte b1;
      | FlpH f ->
        let (_, _, b2, b3) = convert_float f in
        put_byte b2;
        put_byte b3;
      | Header (size, tag) ->
        put_byte tag;
        put_byte size;
      | Charchar (c1, c2) ->
        put_byte c1;
        put_byte c2;
  in
  Printf.fprintf oc "caml_globals_init_stack:";
  Queue.iter export stack;
  cpt := 0;
  Printf.fprintf oc "\ncaml_globals_init_heap:";
  Queue.iter export heap;
  Printf.fprintf oc "\ncaml_globals_init_end:\n\n";
;;
