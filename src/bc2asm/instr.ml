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

type instr = {
  old_addr : int;
  mutable new_addr : int;
  mutable bc : bc;
}
and ptr = {
  old_ofs : int;
  mutable pointed : instr;
}
and prim = {
  old_ind : int;
  mutable new_ind : int;
}
and bc =
  | Acc of int
  | Push
  | Pushacc of int
  | Pop of int
  | Assign of int
  | Envacc of int
  | Pushenvacc of int
  | Pushretaddr of ptr
  | Apply of int
  | Appterm of int * int
  | Return of int
  | Restart
  | Grab of int
  | Closure of int * ptr
  | Closurerec of int * int * ptr * ptr array
  | Offsetclosurem2
  | Offsetclosure of int
  | Pushoffsetclosurem2
  | Pushoffsetclosure of int
  | Getglobal of int
  | Pushgetglobal of int
  | Getglobalfield of int * int
  | Pushgetglobalfield of int * int
  | Setglobal of int
  | Atom of int
  | Pushatom of int
  | Makeblock of int * int
  | Makefloatblock of int
  | Getfield of int
  | Getfloatfield of int
  | Setfield of int
  | Setfloatfield of int
  | Vectlength
  | Getvectitem
  | Setvectitem
  | Getstringchar
  | Setstringchar
  | Branch of ptr
  | Branchif of ptr
  | Branchifnot of ptr
  | Switch of int * ptr array
  | Boolnot
  | Pushtrap of ptr
  | Poptrap
  | Raise
  | Reraise
  | Raisenotrace
  | Checksignals
  | Ccall of int * prim
  | Const of int
  | Pushconst of int
  | Negint
  | Addint
  | Subint
  | Mulint
  | Divint
  | Modint
  | Andint
  | Orint
  | Xorint
  | Lslint
  | Lsrint
  | Asrint
  | Eq
  | Neq
  | Ltint
  | Leint
  | Gtint
  | Geint
  | Offsetint of int
  | Offsetref of int
  | Isint
  | Getmethod
  | Beq of int * ptr
  | Bneq of int * ptr
  | Blint of int * ptr
  | Bleint of int * ptr
  | Bgtint of int * ptr
  | Bgeint of int * ptr
  | Ultint
  | Ugeint
  | Bultint of int * ptr
  | Bugeint of int * ptr
  | Getpubmet of int * int
  | Getdynmet
  | Stop
  | Event
  | Break
;;

let parse read =
  let read_ptr () = {
    old_ofs = read ();
    pointed = { old_addr = -1 ; new_addr = -1 ; bc = Break };
  } in
  let read_prim () = {
    old_ind = read ();
    new_ind = -1;
  } in
  try
    let opcode = read () in
    let instrs =
      try
        match opcode with
          | 0 ->   Acc 0
          | 1 ->   Acc 1
          | 2 ->   Acc 2
          | 3 ->   Acc 3
          | 4 ->   Acc 4
          | 5 ->   Acc 5
          | 6 ->   Acc 6
          | 7 ->   Acc 7
          | 8 ->   Acc (read ())
          | 9 ->   Push
          | 10 ->  Pushacc 0
          | 11 ->  Pushacc 1
          | 12 ->  Pushacc 2
          | 13 ->  Pushacc 3
          | 14 ->  Pushacc 4
          | 15 ->  Pushacc 5
          | 16 ->  Pushacc 6
          | 17 ->  Pushacc 7
          | 18 ->  Pushacc (read ())
          | 19 ->  Pop (read ())
          | 20 ->  Assign (read ())
          | 21 ->  Envacc 1
          | 22 ->  Envacc 2
          | 23 ->  Envacc 3
          | 24 ->  Envacc 4
          | 25 ->  Envacc (read ())
          | 26 ->  Pushenvacc 1
          | 27 ->  Pushenvacc 2
          | 28 ->  Pushenvacc 3
          | 29 ->  Pushenvacc 4
          | 30 ->  Pushenvacc (read ())
          | 31 ->  Pushretaddr (read_ptr ())
          | 32 ->  Apply (read ())
          | 33 ->  Apply 1
          | 34 ->  Apply 2
          | 35 ->  Apply 3
          | 36 ->  let n = read () in let s = read () in Appterm (n, s)
          | 37 ->  Appterm ((read ()), 1)
          | 38 ->  Appterm ((read ()), 2)
          | 39 ->  Appterm ((read ()), 3)
          | 40 ->  Return (read ())
          | 41 ->  Restart
          | 42 ->  Grab (read ())
          | 43 ->  let n = read () in let ptr = read_ptr () in Closure (n, ptr)
          | 44 ->
            let f = read () in
            let v = read () in
            let o = read_ptr () in
            let t = if f = 1 then [||] else
                let t = Array.make (f - 1) (read_ptr ()) in
                for i = 1 to f - 2 do t.(i) <- read_ptr () done ; t
            in
            Closurerec (f, v, o, t)
          | 45 ->  Offsetclosurem2
          | 46 ->  Offsetclosure 0
          | 47 ->  Offsetclosure 2
          | 48 ->  Offsetclosure (read ())
          | 49 ->  Pushoffsetclosurem2
          | 50 ->  Pushoffsetclosure 0
          | 51 ->  Pushoffsetclosure 2
          | 52 ->  Pushoffsetclosure (read ())
          | 53 ->  Getglobal (read ())
          | 54 ->  Pushgetglobal (read ())
          | 55 ->  let n = read () in let p = read () in Getglobalfield (n, p)
          | 56 ->  let n = read () in let p = read () in
                                      Pushgetglobalfield (n, p)
          | 57 ->  Setglobal (read ())
          | 58 ->  Atom 0
          | 59 ->  Atom (read ())
          | 60 ->  Pushatom 0
          | 61 ->  Pushatom (read ())
          | 62 ->
            let n = read () in let t = read () in
                               if n <> 0 then Makeblock (n, t) else Atom t
          | 63 ->  Makeblock (1, read ())
          | 64 ->  Makeblock (2, read ())
          | 65 ->  Makeblock (3, read ())
          | 66 ->  Makefloatblock (read ())
          | 67 ->  Getfield 0
          | 68 ->  Getfield 1
          | 69 ->  Getfield 2
          | 70 ->  Getfield 3
          | 71 ->  Getfield (read ())
          | 72 ->  Getfloatfield (read ())
          | 73 ->  Setfield 0
          | 74 ->  Setfield 1
          | 75 ->  Setfield 2
          | 76 ->  Setfield 3
          | 77 ->  Setfield (read ())
          | 78 ->  Setfloatfield (read ())
          | 79 ->  Vectlength
          | 80 ->  Getvectitem
          | 81 ->  Setvectitem
          | 82 ->  Getstringchar
          | 83 ->  Setstringchar
          | 84 ->  Branch (read_ptr ())
          | 85 ->  Branchif (read_ptr ())
          | 86 ->  Branchifnot (read_ptr ())
          | 87 ->
            let n = read () in
            let size_tag = n lsr 16 in
            let size_long = n land (1 lsl 16 - 1) in
            let size = size_tag + size_long in
            let tab = Array.init size (fun _ -> read_ptr ()) in
            Switch (n, tab)
          | 88 ->  Boolnot
          | 89 ->  Pushtrap (read_ptr ())
          | 90 ->  Poptrap
          | 91 ->  Raise
          | 92 ->  Checksignals
          | 93 ->  Ccall (1, read_prim ())
          | 94 ->  Ccall (2, read_prim ())
          | 95 ->  Ccall (3, read_prim ())
          | 96 ->  Ccall (4, read_prim ())
          | 97 ->  Ccall (5, read_prim ())
          | 98 ->  let n = read () in let p = read_prim () in Ccall (n, p)
          | 99 ->  Const 0
          | 100 -> Const 1
          | 101 -> Const 2
          | 102 -> Const 3
          | 103 -> Const (read ())
          | 104 -> Pushconst 0
          | 105 -> Pushconst 1
          | 106 -> Pushconst 2
          | 107 -> Pushconst 3
          | 108 -> Pushconst (read ())
          | 109 -> Negint
          | 110 -> Addint
          | 111 -> Subint
          | 112 -> Mulint
          | 113 -> Divint
          | 114 -> Modint
          | 115 -> Andint
          | 116 -> Orint
          | 117 -> Xorint
          | 118 -> Lslint
          | 119 -> Lsrint
          | 120 -> Asrint
          | 121 -> Eq
          | 122 -> Neq
          | 123 -> Ltint
          | 124 -> Leint
          | 125 -> Gtint
          | 126 -> Geint
          | 127 -> Offsetint (read ())
          | 128 -> Offsetref (read ())
          | 129 -> Isint
          | 130 -> Getmethod
          | 131 -> let v = read () in let ptr = read_ptr () in Beq (v, ptr)
          | 132 -> let v = read () in let ptr = read_ptr () in Bneq (v, ptr)
          | 133 -> let v = read () in let ptr = read_ptr () in Blint (v, ptr)
          | 134 -> let v = read () in let ptr = read_ptr () in Bleint (v, ptr)
          | 135 -> let v = read () in let ptr = read_ptr () in Bgtint (v, ptr)
          | 136 -> let v = read () in let ptr = read_ptr () in Bgeint (v, ptr)
          | 137 -> Ultint
          | 138 -> Ugeint
          | 139 -> let v = read () in let ptr = read_ptr () in Bultint (v, ptr)
          | 140 -> let v = read () in let ptr = read_ptr () in Bugeint (v, ptr)
          | 141 -> let v = read () in let cch = read () in Getpubmet (v, cch)
          | 142 -> Getdynmet
          | 143 -> Stop
          | 144 -> Event
          | 145 -> Break
          | 146 ->  Reraise
          | 147 ->  Raisenotrace
          | _ -> failwith (Printf.sprintf "invalid opcode: %d" opcode)
      with End_of_file -> failwith "unexpected end of code section"
    in
    Some instrs
  with End_of_file -> None
;;

let opcode_of_bc bc =
  match bc with
    | Acc 0 -> 0
    | Acc 1 -> 1
    | Acc 2 -> 2
    | Acc 3 -> 3
    | Acc 4 -> 4
    | Acc 5 -> 5
    | Acc 6 -> 6
    | Acc 7 -> 7
    | Acc _ -> 8
    | Push -> 9
    | Pushacc 0 -> 10
    | Pushacc 1 -> 11
    | Pushacc 2 -> 12
    | Pushacc 3 -> 13
    | Pushacc 4 -> 14
    | Pushacc 5 -> 15
    | Pushacc 6 -> 16
    | Pushacc 7 -> 17
    | Pushacc _ -> 18
    | Pop _ -> 19
    | Assign _ -> 20
    | Envacc 1 -> 21
    | Envacc 2 -> 22
    | Envacc 3 -> 23
    | Envacc 4 -> 24
    | Envacc _ -> 25
    | Pushenvacc 1 -> 26
    | Pushenvacc 2 -> 27
    | Pushenvacc 3 -> 28
    | Pushenvacc 4 -> 29
    | Pushenvacc _ -> 30
    | Pushretaddr _ -> 31
    | Apply 1 -> 33
    | Apply 2 -> 34
    | Apply 3 -> 35
    | Apply _ -> 32
    | Appterm (_, 1) -> 37
    | Appterm (_, 2) -> 38
    | Appterm (_, 3) -> 39
    | Appterm (_, _) -> 36
    | Return _ -> 40
    | Restart -> 41
    | Grab _ -> 42
    | Closure (_, _)  -> 43
    | Closurerec (_, _, _, _) -> 44
    | Offsetclosurem2 -> 45
    | Offsetclosure 0 -> 46
    | Offsetclosure 2 -> 47
    | Offsetclosure _ -> 48
    | Pushoffsetclosurem2 -> 49
    | Pushoffsetclosure 0 -> 50
    | Pushoffsetclosure 2 -> 51
    | Pushoffsetclosure _ -> 52
    | Getglobal _ -> 53
    | Pushgetglobal _ -> 54
    | Getglobalfield (_, _) -> 55
    | Pushgetglobalfield (_, _) -> 56
    | Setglobal _ -> 57
    | Atom 0 -> 58
    | Atom _ -> 59
    | Pushatom 0 -> 60
    | Pushatom _ -> 61
    | Makeblock (1, _) -> 63
    | Makeblock (2, _) -> 64
    | Makeblock (3, _) -> 65
    | Makeblock (_, _) -> 62
    | Makefloatblock _ -> 66
    | Getfield 0 -> 67
    | Getfield 1 -> 68
    | Getfield 2 -> 69
    | Getfield 3 -> 70
    | Getfield _ -> 71
    | Getfloatfield _ -> 72
    | Setfield 0 -> 73
    | Setfield 1 -> 74
    | Setfield 2 -> 75
    | Setfield 3 -> 76
    | Setfield _ -> 77
    | Setfloatfield _ -> 78
    | Vectlength -> 79
    | Getvectitem -> 80
    | Setvectitem -> 81
    | Getstringchar -> 82
    | Setstringchar -> 83
    | Branch _ -> 84
    | Branchif _ -> 85
    | Branchifnot _ -> 86
    | Switch (_, _) -> 87
    | Boolnot -> 88
    | Pushtrap _ -> 89
    | Poptrap -> 90
    | Raise -> 91
    | Checksignals -> 92
    | Ccall (1, _) -> 93
    | Ccall (2, _) -> 94
    | Ccall (3, _) -> 95
    | Ccall (4, _) -> 96
    | Ccall (5, _) -> 97
    | Ccall (_, _) -> 98
    | Const 0 -> 99
    | Const 1 -> 100
    | Const 2 -> 101
    | Const 3 -> 102
    | Const _ -> 103
    | Pushconst 0 -> 104
    | Pushconst 1 -> 105
    | Pushconst 2 -> 106
    | Pushconst 3 -> 107
    | Pushconst _ -> 108
    | Negint -> 109
    | Addint -> 110
    | Subint -> 111
    | Mulint -> 112
    | Divint -> 113
    | Modint -> 114
    | Andint -> 115
    | Orint  -> 116
    | Xorint -> 117
    | Lslint -> 119
    | Lsrint -> 119
    | Asrint -> 120
    | Eq  -> 121
    | Neq -> 122
    | Ltint -> 123
    | Leint -> 124
    | Gtint -> 125
    | Geint -> 126
    | Offsetint _ -> 127
    | Offsetref _ -> 128
    | Isint -> 129
    | Getmethod -> 130
    | Beq (_, _) -> 131
    | Bneq (_, _) -> 132
    | Blint (_, _) -> 133
    | Bleint (_, _) -> 134
    | Bgtint (_, _) -> 135
    | Bgeint (_, _) -> 136
    | Ultint -> 137
    | Ugeint -> 138
    | Bultint (_, _) -> 139
    | Bugeint (_, _) -> 140
    | Getpubmet (_, _) -> 141
    | Getdynmet -> 142
    | Stop -> 143
    | Event -> 144
    | Break -> 145
    | Reraise -> 146
    | Raisenotrace -> 147
;;

let string_of_bc bc =
  match bc with
    | Acc n             -> Printf.sprintf "ACC %d" n
    | Push              -> Printf.sprintf "PUSH"
    | Pushacc n         -> Printf.sprintf "PUSHACC %d" n
    | Pop n             -> Printf.sprintf "POP %d" n
    | Assign n          -> Printf.sprintf "ASSIGN %d" n
    | Envacc n          -> Printf.sprintf "ENVACC %d" n
    | Pushenvacc n      -> Printf.sprintf "PUSHENVACC %d" n
    | Pushretaddr ptr   -> Printf.sprintf "PUSHRETADDR %d" ptr.pointed.new_addr
    | Apply n           -> Printf.sprintf "APPLY %d" n
    | Appterm (n,s)     -> Printf.sprintf "APPTERM %d %d" n s
    | Return n          -> Printf.sprintf "RETURN %d" n
    | Restart           -> Printf.sprintf "RESTART"
    | Grab n            -> Printf.sprintf "GRAB %d" n
    | Closure (n,ptr)   -> Printf.sprintf "CLOSURE %d %d" n ptr.pointed.new_addr
    | Closurerec (f,v,{pointed={new_addr=a;old_addr=_;bc=_};old_ofs=_},t) ->
      let b = Buffer.create 16 in
      Printf.bprintf b "CLOSUREREC %d %d %d [" f v a;
      Array.iter (fun p -> Printf.bprintf b " %d " p.pointed.new_addr) t;
      Printf.bprintf b "]";
      Buffer.contents b
    | Offsetclosurem2       -> Printf.sprintf "OFFSETCLOSUREM2"
    | Offsetclosure n       -> Printf.sprintf "OFFSETCLOSURE %d" n
    | Pushoffsetclosurem2   -> Printf.sprintf "PUSHOFFSETCLOSUREM2"
    | Pushoffsetclosure n   -> Printf.sprintf "PUSHOFFSETCLOSURE %d" n
    | Getglobal n           -> Printf.sprintf "GETGLOBAL %d" n
    | Pushgetglobal n       -> Printf.sprintf "PUSHGETGLOBAL %d" n
    | Getglobalfield (n,p)  -> Printf.sprintf "GETGLOBALFIELD %d %d"n p
    | Pushgetglobalfield (n,p) -> Printf.sprintf "PUSHGETGLOBALFIELD %d %d" n p
    | Setglobal n       -> Printf.sprintf "SETGLOBAL %d" n
    | Atom n            -> Printf.sprintf "ATOM %d" n
    | Pushatom n        -> Printf.sprintf "PUSHATOM %d" n
    | Makeblock (n,t)   -> Printf.sprintf "MAKEBLOCK %d %d" n t
    | Makefloatblock n  -> Printf.sprintf "MAKEFLOATBLOCK %d" n
    | Getfield n        -> Printf.sprintf "GETFIELD %d" n
    | Getfloatfield n   -> Printf.sprintf "GETFLOATFIELD %d" n
    | Setfield n        -> Printf.sprintf "SETFIELD %d" n
    | Setfloatfield n   -> Printf.sprintf "SETFLOATFIELD %d" n
    | Vectlength        -> Printf.sprintf "VECTLENGTH"
    | Getvectitem       -> Printf.sprintf "GETVECTITEM"
    | Setvectitem       -> Printf.sprintf "SETVECTITEM"
    | Getstringchar     -> Printf.sprintf "GETSTRINGCHAR"
    | Setstringchar     -> Printf.sprintf "SETSTRINGCHAR"
    | Branch ptr        -> Printf.sprintf "BRANCH %d" ptr.pointed.new_addr
    | Branchif ptr      -> Printf.sprintf "BRANCHIF %d" ptr.pointed.new_addr
    | Branchifnot ptr   -> Printf.sprintf "BRANCHIFNOT %d" ptr.pointed.new_addr
    | Switch (n, tab) ->
      let size_tag = n lsr 16 in
      let size_long = n land 0xFFFF in
      let b = Buffer.create 16 in
      Printf.bprintf b "SWITCH %d %d [" size_tag size_long;
      Array.iter
        (fun ptr -> Printf.bprintf b " %d " ptr.pointed.new_addr) tab;
      Printf.bprintf b "]";
      Buffer.contents b
    | Boolnot           -> Printf.sprintf "BOOLNOT"
    | Pushtrap ptr      -> Printf.sprintf "PUSHTRAP %d" ptr.pointed.new_addr
    | Poptrap           -> Printf.sprintf "POPTRAP"
    | Raise             -> Printf.sprintf "RAISE"
    | Reraise           -> Printf.sprintf "RERAISE"
    | Raisenotrace      -> Printf.sprintf "RAISENOTRACE"
    | Checksignals      -> Printf.sprintf "CHECKSIGNALS"
    | Ccall (n,{new_ind=ind;old_ind=_}) -> Printf.sprintf "CCALL %d %d" n ind
    | Const n           -> Printf.sprintf "CONST %d" n
    | Pushconst n       -> Printf.sprintf "PUSHCONST %d" n
    | Negint            -> Printf.sprintf "NEGINT"
    | Addint            -> Printf.sprintf "ADDINT"
    | Subint            -> Printf.sprintf "SUBINT"
    | Mulint            -> Printf.sprintf "MULINT"
    | Divint            -> Printf.sprintf "DIVINT"
    | Modint            -> Printf.sprintf "MODINT"
    | Andint            -> Printf.sprintf "ANDINT"
    | Orint             -> Printf.sprintf "ORINT"
    | Xorint            -> Printf.sprintf "XORINT"
    | Lslint            -> Printf.sprintf "LSLINT"
    | Lsrint            -> Printf.sprintf "LSRINT"
    | Asrint            -> Printf.sprintf "ASRINT"
    | Eq                -> Printf.sprintf "EQ"
    | Neq               -> Printf.sprintf "NEQ"
    | Ltint             -> Printf.sprintf "LTINT"
    | Leint             -> Printf.sprintf "LEINT"
    | Gtint             -> Printf.sprintf "GTINT"
    | Geint             -> Printf.sprintf "GEINT"
    | Offsetint ofs     -> Printf.sprintf "OFFSETINT %d" ofs
    | Offsetref ofs     -> Printf.sprintf "OFFSETREF %d" ofs
    | Isint             -> Printf.sprintf "ISINT"
    | Getmethod         -> Printf.sprintf "GETMETHOD"
    | Beq (v,ptr)       -> Printf.sprintf "BEQ %d %d" v ptr.pointed.new_addr
    | Bneq (v,ptr)      -> Printf.sprintf "BNEQ %d %d" v ptr.pointed.new_addr
    | Blint (v,ptr)     -> Printf.sprintf "BLINT %d %d" v ptr.pointed.new_addr
    | Bleint (v,ptr)    -> Printf.sprintf "BLEINT %d %d" v ptr.pointed.new_addr
    | Bgtint (v,ptr)    -> Printf.sprintf "BGTINT %d %d" v ptr.pointed.new_addr
    | Bgeint (v,ptr)    -> Printf.sprintf "BGEINT %d %d" v ptr.pointed.new_addr
    | Ultint            -> Printf.sprintf "ULTINT"
    | Ugeint            -> Printf.sprintf "UGEINT"
    | Bultint (v,ptr)   -> Printf.sprintf "BULTINT %d %d" v ptr.pointed.new_addr
    | Bugeint (v,ptr)   -> Printf.sprintf "BUGEINT %d %d" v ptr.pointed.new_addr
    | Getpubmet (v,cch) -> Printf.sprintf "GETPUBMET %d %d" v cch
    | Getdynmet         -> Printf.sprintf "GETDYNMET"
    | Stop              -> Printf.sprintf "STOP"
    | Event             -> Printf.sprintf "EVENT"
    | Break             -> Printf.sprintf "BREAK"
;;

let print_instr oc instr =
  Printf.fprintf oc "-@ %-4d +@ %-4d op %02x  %s\n" (instr.old_addr)
    (instr.new_addr) (opcode_of_bc instr.bc) (string_of_bc instr.bc);
;;

let check_bc bc =
  let check_range min n max =
    if n < min || n > max then
      let msg =
        Printf.sprintf "%d out of range [ %d ; %d ] in `%s'"
          n min max (string_of_bc bc)
      in
      failwith msg
  in
  match bc with
    | Acc n | Pushacc n | Pop n | Assign n -> check_range 0 n 0x7FFF;
    | Envacc n | Pushenvacc n -> check_range 0 n 0xFE;
    | Apply n -> check_range 0 n 0x7F;
    | Appterm (n, s) -> check_range 1 n 0x7F; check_range 0 s 0x7F;
    | Return n -> check_range 0 n 0x7F;
    | Grab n -> check_range 0 n 0x7C;
    | Closure (n, _) -> check_range 0 n 0x7F;
    | Closurerec (f, v, _, _) -> check_range 1 (2 * f - 1 + v) 0x7F;
    | Offsetclosure n | Pushoffsetclosure n ->
      check_range (-0xFE) n 0xFE;
      if n land 1 = 1 then
        failwith (Printf.sprintf "odd index in `%s'" (string_of_bc bc));
    | Getglobal n | Pushgetglobal n | Setglobal n -> check_range 0 n 0x7FFF;
    | Getglobalfield (n, p) | Pushgetglobalfield (n, p) ->
      check_range 0 n 0x7FFF; check_range 0 p 0xFE;
    | Atom t | Pushatom t ->
      check_range 0 t 0;
    | Makeblock (n, t) -> check_range 1 n 0xFF; check_range 0 t 0xFF;
    | Makefloatblock n -> check_range 1 n 0x7F;
    | Getfield n | Setfield n -> check_range 0 n 0xFE;
    | Getfloatfield n | Setfloatfield n -> check_range 0 n 0x7E;
    | Switch (n, _) -> check_range 0 (n land 0xFFFF) 0xFF;
    | Ccall (n, { new_ind = p ; old_ind = _ }) ->
      check_range 0 n 0xFF; check_range 0 p 0x7F;
    | Const n | Pushconst n -> check_range (-0x4000) n 0x7FFF;
    | Offsetint n | Offsetref n -> check_range (-0x4000) n 0x3FFF;
    | Beq (v, _) | Bneq (v, _) | Blint (v, _) | Bleint (v, _) | Bgtint (v, _)
    | Bgeint (v, _) | Bultint (v, _) | Bugeint (v, _) ->
      check_range (-0x4000) v 0x3FFF;
    | _ -> ()
;;

let export_bc put_byte bc =
  let write_op op = put_byte op in
  let write_byte arg = put_byte (arg land 0xFF) in
  let write_byte0 arg = put_byte ((arg lsl 1) land 0xFF) in
  let write_byte1 arg = put_byte (((arg lsl 1) lor 1) land 0xFF) in
  let write_int arg =
    put_byte (arg land 0xFF);
    put_byte ((arg lsr 8) land 0xFF);
  in
  let write_int0 arg =
    put_byte ((arg lsl 1) land 0xFF);
    put_byte ((arg lsr 7) land 0xFF);
  in
  let write_int1 arg =
    put_byte (((arg lsl 1) lor 1) land 0xFF);
    put_byte ((arg lsr 7) land 0xFF);
  in
  let write_ptr ptr = write_int ptr.pointed.new_addr in
  match bc with
    | Acc 0 ->               write_op 0;
    | Acc 1 ->               write_op 1;
    | Acc 2 ->               write_op 2;
    | Acc 3 ->               write_op 3;
    | Acc 4 ->               write_op 4;
    | Acc 5 ->               write_op 5;
    | Acc 6 ->               write_op 6;
    | Acc 7 ->               write_op 7;
    | Acc n ->               write_op 8;  write_int0 n;
    | Push ->                write_op 9;
    | Pushacc 0 ->           write_op 10;
    | Pushacc 1 ->           write_op 11;
    | Pushacc 2 ->           write_op 12;
    | Pushacc 3 ->           write_op 13;
    | Pushacc 4 ->           write_op 14;
    | Pushacc 5 ->           write_op 15;
    | Pushacc 6 ->           write_op 16;
    | Pushacc 7 ->           write_op 17;
    | Pushacc n ->           write_op 18; write_int0 n;
    | Pop n ->               write_op 19; write_int0 n;
    | Assign n ->            write_op 20; write_int0 n;
    | Envacc 1 ->            write_op 21;
    | Envacc 2 ->            write_op 22;
    | Envacc 3 ->            write_op 23;
    | Envacc 4 ->            write_op 24;
    | Envacc n ->            write_op 25; write_byte n;
    | Pushenvacc 1 ->        write_op 26;
    | Pushenvacc 2 ->        write_op 27;
    | Pushenvacc 3 ->        write_op 28;
    | Pushenvacc 4 ->        write_op 29;
    | Pushenvacc n ->        write_op 30; write_byte n;
    | Pushretaddr ptr ->     write_op 31; write_ptr ptr;
    | Apply 1 ->             write_op 33;
    | Apply 2 ->             write_op 34;
    | Apply 3 ->             write_op 35;
    | Apply n ->             write_op 32; write_byte1 (n - 1);
    | Appterm (n, 1) ->      write_op 37; write_byte0 n;
    | Appterm (n, 2) ->      write_op 38; write_byte0 n;
    | Appterm (n, 3) ->      write_op 39; write_byte0 n;
    | Appterm (n, s) ->      write_op 36; write_byte0 s; write_byte n;
    | Return n ->            write_op 40; write_byte0 n;
    | Restart ->             write_op 41;
    | Grab n ->              write_op 42; write_byte n;
    | Closure (n, ptr) ->    write_op 43; write_byte (n + 1); write_ptr ptr;
    | Closurerec (f, v, o, t) ->
      write_op 44;
      write_byte f;
      write_byte v;
      write_ptr o;
      Array.iter write_ptr t;
    | Offsetclosurem2 ->     write_op 45;
    | Offsetclosure 0 ->     write_op 46;
    | Offsetclosure 2 ->     write_op 47;
    | Offsetclosure n ->     write_op 48; write_byte (n lsr 1);
    | Pushoffsetclosurem2 -> write_op 49;
    | Pushoffsetclosure 0 -> write_op 50;
    | Pushoffsetclosure 2 -> write_op 51;
    | Pushoffsetclosure n -> write_op 52; write_byte (n lsr 1);
    | Getglobal n ->         write_op 53; write_int0 n;
    | Pushgetglobal n ->     write_op 54; write_int0 n;
    | Getglobalfield (n, p) ->     write_op 55; write_int0 n; write_byte p;
    | Pushgetglobalfield (n, p) -> write_op 56; write_int0 n; write_byte p;
    | Setglobal n ->         write_op 57; write_int0 n;
    | Atom 0 ->              write_op 58;
    | Atom _ ->              failwith "Creation of an atom with a tag <> 0"
    | Pushatom 0 ->          write_op 60;
    | Pushatom t ->          write_op 61; write_byte t;
    | Makeblock (1, t) ->    write_op 63; write_byte t;
    | Makeblock (2, t) ->    write_op 64; write_byte t;
    | Makeblock (3, t) ->    write_op 65; write_byte t;
    | Makeblock (n, t) ->    write_op 62; write_byte n; write_byte t;
    | Makefloatblock n ->    write_op 66; write_byte (2 * n);
    | Getfield 0 ->          write_op 67;
    | Getfield 1 ->          write_op 68;
    | Getfield 2 ->          write_op 69;
    | Getfield 3 ->          write_op 70;
    | Getfield n ->          write_op 71; write_byte n;
    | Getfloatfield n ->     write_op 72; write_byte (2 * n);
    | Setfield 0 ->          write_op 73;
    | Setfield 1 ->          write_op 74;
    | Setfield 2 ->          write_op 75;
    | Setfield 3 ->          write_op 76;
    | Setfield n ->          write_op 77; write_byte n;
    | Setfloatfield n ->     write_op 78; write_byte (2 * n);
    | Vectlength ->          write_op 79;
    | Getvectitem ->         write_op 80;
    | Setvectitem ->         write_op 81;
    | Getstringchar ->       write_op 82;
    | Setstringchar ->       write_op 83;
    | Branch ptr ->          write_op 84; write_ptr ptr;
    | Branchif ptr ->        write_op 85; write_ptr ptr;
    | Branchifnot ptr ->     write_op 86; write_ptr ptr;
    | Switch (n, tab) ->
      let size_long = n land 0xFFFF in
      write_op 87;
      write_byte size_long;
      Array.iter write_ptr tab;
    | Boolnot ->             write_op 88;
    | Pushtrap ptr ->        write_op 89; write_ptr ptr;
    | Poptrap ->             write_op 90;
    | Raise ->               write_op 91;
    | Reraise ->             write_op 91;
    | Raisenotrace ->        write_op 91;
    | Checksignals ->        () (* removed *)
    | Ccall (1, {new_ind=p;old_ind=_}) -> write_op 93; write_byte0 p;
    | Ccall (2, {new_ind=p;old_ind=_}) -> write_op 94; write_byte0 p;
    | Ccall (3, {new_ind=p;old_ind=_}) -> write_op 95; write_byte0 p;
    | Ccall (4, {new_ind=p;old_ind=_}) -> write_op 96; write_byte0 p;
    | Ccall (5, {new_ind=p;old_ind=_}) -> write_op 97; write_byte0 p;
    | Ccall (n, {new_ind=p;old_ind=_}) ->
      write_op 98; write_byte0 p; write_byte n;
    | Const 0 ->             write_op 99;
    | Const 1 ->             write_op 100;
    | Const 2 ->             write_op 101;
    | Const 3 ->             write_op 102;
    | Const n ->             write_op 103; write_int1 n;
    | Pushconst 0 ->         write_op 104;
    | Pushconst 1 ->         write_op 105;
    | Pushconst 2 ->         write_op 106;
    | Pushconst 3 ->         write_op 107;
    | Pushconst n ->         write_op 108; write_int1 n;
    | Negint ->              write_op 109;
    | Addint ->              write_op 110;
    | Subint ->              write_op 111;
    | Mulint ->              write_op 112;
    | Divint ->              write_op 113;
    | Modint ->              write_op 114;
    | Andint ->              write_op 115;
    | Orint  ->              write_op 116;
    | Xorint ->              write_op 117;
    | Lslint ->              write_op 118;
    | Lsrint ->              write_op 119;
    | Asrint ->              write_op 120;
    | Eq  ->                 write_op 121;
    | Neq ->                 write_op 122;
    | Ltint ->               write_op 123;
    | Leint ->               write_op 124;
    | Gtint ->               write_op 125;
    | Geint ->               write_op 126;
    | Offsetint n ->         write_op 127; write_int0 n;
    | Offsetref n ->         write_op 128; write_int0 n;
    | Isint ->               write_op 129;
    | Getmethod ->           write_op 130;
    | Beq (v, ptr) ->        write_op 131; write_int1 v; write_ptr ptr;
    | Bneq (v, ptr) ->       write_op 132; write_int1 v; write_ptr ptr;
    | Blint (v, ptr) ->      write_op 133; write_int1 v; write_ptr ptr;
    | Bleint (v, ptr) ->     write_op 134; write_int1 v; write_ptr ptr;
    | Bgtint (v, ptr) ->     write_op 135; write_int1 v; write_ptr ptr;
    | Bgeint (v, ptr) ->     write_op 136; write_int1 v; write_ptr ptr;
    | Ultint ->              write_op 137;
    | Ugeint ->              write_op 138;
    | Bultint (v, ptr) ->    write_op 139; write_int1 v; write_ptr ptr;
    | Bugeint (v, ptr) ->    write_op 140; write_int1 v; write_ptr ptr;
    | Getpubmet (tag, _) ->  write_op 141; write_int1 (tag land 0x7FFF);
    | Getdynmet ->           write_op 142;
    | Stop ->                write_op 143;
    | Event ->               () (* removed *)
    | Break ->               () (* removed *)
;;

let sizeof_bc bc =
  let cpt = ref 0 in
  export_bc (fun _ -> incr cpt) bc;
  !cpt
;;
