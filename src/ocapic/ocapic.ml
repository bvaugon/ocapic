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

type gc_algo = Stop_and_copy | Mark_and_compact;;

let pic_segment_nb = 15;;

let compile_only = ref false;;
let output_file = ref None;;
let verbose = ref false;;
let pp_commands = ref [];;

let where () = Printf.printf "%s\n%!" Config.libdir; exit 0;;
let ocaml () = Printf.printf "%s\n%!" Config.ocaml; exit 0;;
let ocamlc () = Printf.printf "%s\n%!" Config.ocamlc; exit 0;;

let spec =
  Arg.align [
    ("-c", Arg.Set compile_only,
     " Compile only");
    ("-o", Arg.String (fun o -> output_file := Some o),
     "<file.asm> Set output file");
    ("-pp", Arg.String (fun cmd -> pp_commands := cmd :: !pp_commands),
     "<command> Pipe sources through preprocessor <command>");
    ("-where", Arg.Unit where,
     " Print location of standard library and exit");
    ("-ocaml", Arg.Unit ocaml,
     " Print location of OCaml toplevel and exit");
    ("-ocamlc", Arg.Unit ocamlc,
     " Print location of OCaml bytecode compiler and exit");
    ("-version", Arg.Unit (fun () -> print_endline Config.version ; exit 0),
     " Print version and exit");
    ("-verbose", Arg.Set verbose,
     " Verbose mode");
  ]
;;

let usage =
  let me = Sys.argv.(0) in
  Printf.sprintf "\n\
Usages:\n\
\  %s [ <pic> ] [ OCAMLC_OPTS ] [ OPTIONS ] -c <src.ml> ...\n\
\  %s [ <pic> ] [ OCAMLC_OPTS ] [ OPTIONS ] -c <src.mli> ...\n\
\  %s <pic> [ OCAMLC_OPTS ] [ BC2ASM_OPTS ] [ OPTIONS ] <src.ml> ...\n\
\  %s <pic> [ OCAMLC_OPTS ] [ BC2ASM_OPTS ] [ OPTIONS ] <obj.cmo> ...\n\
\  %s ( -where | -ocaml | -ocamlc )\n\
\n\
Options:" me me me me me
;;

let error msg =
  Printf.eprintf "Error: %s\n" msg;
  Arg.usage spec usage;
  exit 1;
;;

if Array.length Sys.argv = 2 then
  match Sys.argv.(1) with
    | "-where" -> where ();
    | "-ocaml" -> ocaml ();
    | "-ocamlc" -> ocamlc ();
    | _ -> ();
;;

let asms = ref [];;
let mls = ref [];;
let cmos = ref [];;
let ocamlc_options = ref [];;
let bc2asm_args = ref [];;
let bc2asm_options = [
  "-config" ; "-configs" ; "-header" ; "-interp" ;
  "-runtime" ; "-stdlib" ; "-gc" ; "-stack-size" ; "-heap-size" ;
];;

let pic =
  if Array.length Sys.argv < 2 then error "argument requested";
  let p = Sys.argv.(1) in
  if List.mem p Config.supported_pics then (
    incr Arg.current;
    Some p
  ) else if String.contains p '.' || String.contains p '-' then (
    None
  ) else (
    error ("invalid or unsupported pic: " ^ p)
  )
;;

let unknow arg =
  let get_arg () =
    incr Arg.current;
    if !Arg.current >= Array.length Sys.argv then
      error ("option `" ^ arg ^ "' needs an argument");
    Sys.argv.(!Arg.current)
  in
  if List.mem arg bc2asm_options then
    bc2asm_args := (arg, get_arg ()) :: !bc2asm_args
  else if Filename.check_suffix arg ".asm" then
    asms := arg :: !asms
  else if Filename.check_suffix arg ".ml" then
    mls := arg :: !mls
  else if Filename.check_suffix arg ".mli" then
    mls := arg :: !mls
  else if Filename.check_suffix arg ".cmo" then
    cmos := arg :: !cmos
  else
    ocamlc_options := arg :: !ocamlc_options
;;

let execute prog args =
  let cmd = ref "" in
  if prog = Config.ocamlc then
    cmd := !cmd ^ Printf.sprintf "CAMLLIB=%s " (Filename.quote Config.libdir);
  cmd := !cmd ^ (Filename.quote prog);
  List.iter
    (fun arg -> cmd := Printf.sprintf "%s %s" !cmd (Filename.quote arg))
    args;
  if !verbose then Printf.printf "+ %s\n%!" !cmd;
  match Sys.command !cmd with
    | 0 -> ()
    | n -> exit n
;;

let compute_ocamlc_options pp_commands other_options =
  let add_camllib pp_command =
    Printf.sprintf "CAMLLIB=%s %s" Config.camllib pp_command
  in
  let rec f pp_commands acc =
    match pp_commands with
      | [] -> acc
      | pp_command :: rest -> f rest ("-pp" :: add_camllib pp_command :: acc)
  in
  f pp_commands (List.rev other_options)
;;

let nb_arg = Array.length Sys.argv in
while !Arg.current <> nb_arg do
  try
    Arg.parse_argv Sys.argv spec unknow usage;
  with
    | Arg.Help msg -> Printf.printf "%s" msg; exit 0;
    | Arg.Bad msg ->
      match Sys.argv.(!Arg.current) with
        | "-c" | "-o" | "-verbose" | "-version" ->
          Printf.eprintf "%s" msg; exit 2;
        | arg -> unknow arg
done;
asms := List.rev !asms;
mls := List.rev !mls;
cmos := List.rev !cmos;
ocamlc_options := compute_ocamlc_options !pp_commands !ocamlc_options;
begin
  let gc_algo_opt =
    try
      let s = List.assoc "-gc" !bc2asm_args in
      match String.lowercase_ascii s with
        | "stop_and_copy"    -> Some Stop_and_copy
        | "mark_and_compact" -> Some Mark_and_compact
        | _ -> error (Printf.sprintf "invalid GC algorithm: %S" s)
    with Not_found -> None
  in
  let heap_size_opt =
    try
      let heap_size = int_of_string (List.assoc "-heap-size" !bc2asm_args) in
      let total_heap_size = match gc_algo_opt with
        | Some Mark_and_compact -> heap_size + 1
        | Some Stop_and_copy | None -> 2 * heap_size
      in
      if heap_size < 0 || total_heap_size > pic_segment_nb - 1 then
        error (Printf.sprintf "invalid heap size: %d" heap_size);
      Some heap_size
    with
      | Not_found -> None
      | Failure _ -> error "invalid heap size, not an integer"
  and stack_size_opt =
    try
      let stack_size = int_of_string (List.assoc "-stack-size" !bc2asm_args) in
      if stack_size < 0 || stack_size > pic_segment_nb - 2 then
        error (Printf.sprintf "invalid stack size: %d" stack_size);
      Some stack_size
    with
      | Not_found -> None
      | Failure _ -> error "invalid stack size, not an integer"
  in
  match (stack_size_opt, heap_size_opt) with
    | (Some stack_size, Some heap_size) ->
      let total_heap_size = match gc_algo_opt with
        | Some Mark_and_compact -> heap_size + 1
        | Some Stop_and_copy | None -> 2 * heap_size
      in
      if stack_size + total_heap_size > pic_segment_nb then
        error "incompatible stack/heap sizes";
    | _ -> ()
end;
if !compile_only then
  begin
    if !bc2asm_args <> [] then
      error (Printf.sprintf "compile only, invalid option `%s'"
               (fst (List.hd !bc2asm_args)));
    if !output_file <> None then
      error "compile only, invalid option `-o'";
    if !asms <> [] then
      error "compile only, don't know what to do with .asm files";
    if !cmos <> [] then
      error "compile only, don't know what to do with .cmo files";
    if !mls = [] then error ".ml or .mli file(s) needed";
    match pic with
      | Some p ->
        execute Config.ocamlc ("-custom" :: "-c" :: "-I" :: ("+" ^ p) ::
                                  !ocamlc_options @ !mls);
      | None ->
        execute Config.ocamlc ("-custom" :: "-c" :: !ocamlc_options @ !mls);
  end
else
  let pic =
    match pic with
      | Some p -> p
      | None -> error "pic requested"
  in
  let ml_not_mli =
    List.filter (fun f -> Filename.check_suffix f ".ml") !mls
  in
  let rec last l =
    match l with
      | [] -> invalid_arg "Ocapic.last"
      | e::[] -> e
      | _::tl -> last tl
  in
  let verbose_opt = if !verbose then [ "-verbose" ] else [] in
  if !cmos = [] && ml_not_mli = [] then error ".ml or .cmo file(s) needed";
  let output_base =
    match !output_file with
      | None ->
        if ml_not_mli <> [] then
          Filename.chop_suffix (last ml_not_mli) ".ml"
        else
          Filename.chop_suffix (last !cmos) ".cmo"
      | Some f ->
        if not (Filename.check_suffix f ".asm") then
          error "output filename should end with .asm extension";
        Filename.chop_suffix f ".asm"
  in
  let bc2asm_args =
    List.fold_left (fun acc (a, v) -> a :: v :: acc) [] !bc2asm_args
  in
  execute Config.ocamlc
    ("-custom" :: "-I" :: ("+" ^ pic) :: "piclib.cma" :: !ocamlc_options @
        verbose_opt @ !cmos @ !mls @ [ "-o" ; output_base ]);
  execute Config.ocamlclean
    (verbose_opt @ [ output_base ; "-o" ; output_base ]);
  execute Config.bc2asm (pic :: bc2asm_args @ [ output_base ] @ !asms);
;;
