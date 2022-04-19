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

let pic_segment_nb = 15;;

let interp = ref (Config.libdir ^ "/interp.asm");;
let runtime = ref (Config.libdir ^ "/runtime.asm");;
let stdlib = ref (Config.libdir ^ "/stdlib.asm");;

let stack_size = ref 0;;
let heap_size = ref 0;;
let output = ref None;;
let header = ref None;;
let verbose = ref false;;
let bytecode = ref None;;
let externs = ref [];;
let gc_algo = ref Constants.Stop_and_copy;;

let set_gc_algo s = match String.lowercase_ascii s with
  | "stop_and_copy"    -> gc_algo := Constants.Stop_and_copy;
  | "mark_and_compact" -> gc_algo := Constants.Mark_and_compact;
  | _ -> failwith (Printf.sprintf "invalid GC algorithm: %S" s)
;;

let spec =
  Arg.align [
    ("-o", Arg.String (fun o -> output := Some o),
     "<file.asm> Set output file (default: <bytecode>.asm)");
    ("-config", Arg.String Configs.register_config,
     "<name=value> Add a 'config' directive");
    ("-configs", Arg.String Configs.register_configs,
     "<name=value,...> Add multiple 'config' directives");
    ("-header", Arg.String (fun h -> header := Some h),
     "<file.inc> Set pic header file to include");
    ("-interp", Arg.Set_string interp,
     "<file.asm> Set bytecode interpreter");
    ("-runtime", Arg.Set_string runtime,
     "<file.asm> Set assembler runtime");
    ("-stdlib", Arg.Set_string stdlib,
     "<file.asm> Set assembler standard library");
    ("-gc", Arg.String set_gc_algo,
     "<algo> Set GC algorithm (STOP_AND_COPY | MARK_AND_COMPACT)");
    ("-stack-size", Arg.Set_int stack_size,
     "<seg-nb> Set stack size (default: 1 -> 174 levels)");
    ("-heap-size", Arg.Set_int heap_size,
     "<seg-nb> Set heap size (default: 7 -> 1792 bytes)");
    ("-verbose", Arg.Set verbose,
     " Verbose mode");
    ("-version", Arg.Unit (fun () -> print_endline Config.version ; exit 0),
     " Print version and exit");
  ]
;;

let usage =
  Printf.sprintf "\n\
Usage: %s <pic> [ OPTIONS ] <bytecode> [ <externs.asm> ... ]\n\
\n\
Options:" Sys.argv.(0)
;;

let error msg =
  Printf.printf "Error: %s\n" msg;
  Arg.usage spec usage;
  exit 1;
;;

let unknow arg =
  if Filename.check_suffix arg ".asm" then
    externs := arg :: !externs
  else if !bytecode = None then
    bytecode := Some arg
  else
    error (Printf.sprintf "don't know what to do with: `%s'" arg);
;;

let pic =
  if Array.length Sys.argv < 2 then error "pic requested";
  if not (List.mem Sys.argv.(1) Config.supported_pics) then
    error (Printf.sprintf "invalid or unsupported pic: `%s'" Sys.argv.(1));
  incr Arg.current;
  Sys.argv.(1)
;;

try Arg.parse spec unknow usage with Failure msg ->
  Printf.eprintf "Error: %s\n%!" msg;
  exit 1;
;;

let bytecode =
  match !bytecode with
    | None -> error "bytecode file undefined";
    | Some bytecode -> bytecode
;;

let header =
  match !header with
    | None -> "p" ^ pic ^ ".inc"
    | Some h -> h
;;

let output =
  match !output with
    | None -> bytecode ^ ".asm"
    | Some o -> o
;;

let heap_size = match !heap_size, !gc_algo with
  |  0, Constants.Stop_and_copy    -> 7
  |  0, Constants.Mark_and_compact -> 13
  | sz, _                          -> sz
and stack_size = match !stack_size with
  | 0 -> 1
  | sz -> sz
;;

let total_heap_size = match !gc_algo with
  | Constants.Stop_and_copy    -> heap_size * 2
  | Constants.Mark_and_compact -> heap_size + 1
;;

if heap_size < 0 || total_heap_size > pic_segment_nb - 1 then
  error (Printf.sprintf "invalid heap size: %d" heap_size);;
if stack_size < 0 || stack_size > pic_segment_nb - 2 then
  error (Printf.sprintf "invalid stack size: %d" stack_size);;
if stack_size + total_heap_size > pic_segment_nb then
  error "incompatible stack/heap sizes";;

let print_include oc = Printf.fprintf oc "        include \"%s\"\n" in
try
  let bytefile = OByteLib.Bytefile.read bytecode in
  let prims = Prim.parse bytefile.OByteLib.Bytefile.prim in
  let code = Code.parse bytefile.OByteLib.Bytefile.code in
  let data = Data.parse bytefile.OByteLib.Bytefile.data in
  let code = Data.remap_code code in
  let premap = Prim.remap prims code in
  Code.resolve_addr (Prim.length premap) code !gc_algo;
  if !verbose then Data.print stdout data;
  if !verbose then Code.print stdout code;
  Code.check code;
  let oc = open_out_bin output in
  Printf.fprintf oc "        processor %s\n" pic;
  output_char oc '\n';
  print_include oc header;
  output_char oc '\n';
  Configs.print_configs oc;
  output_char oc '\n';
  Printf.fprintf oc "        errorlevel -302\n";
  Printf.fprintf oc "        errorlevel -1301\n";
  output_char oc '\n';
  begin match !gc_algo with
    | Constants.Stop_and_copy ->
      Printf.fprintf oc "CAML_STOP_AND_COPY_GC\n"
    | Constants.Mark_and_compact ->
      Printf.fprintf oc "CAML_MARK_AND_COMPACT_GC\n"
  end;
  Printf.fprintf oc "STACK_SIZE  EQU   0x%X\n" stack_size;
  Printf.fprintf oc "HEAP_SIZE   EQU   0x%X\n" heap_size;
  output_char oc '\n';
  Data.print_useprim oc data;
  Prim.print_useprim oc premap;
  output_char oc '\n';
  print_include oc !interp;
  print_include oc !runtime;
  output_char oc '\n';
  Prim.export !gc_algo oc premap;
  Code.export oc code;
  Data.export !gc_algo oc data;
  List.iter (print_include oc) !externs;
  print_include oc !stdlib;
  Printf.fprintf oc "\n        end\n";
  close_out oc;
with Failure msg ->
  Printf.eprintf "Error: %s\n%!" msg;
  begin try Sys.remove output with _ -> () end;
  exit 1;
;;
