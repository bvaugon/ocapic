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

type bit = O | I

type t =
  | ADDWF of int * bit * bit
  | ADDWFC of int * bit * bit
  | ANDWF of int * bit * bit
  | CLRF of int * bit
  | COMF of int * bit * bit
  | CPFSEQ of int * bit
  | CPFSGT of int * bit
  | CPFSLT of int * bit
  | DECF of int * bit * bit
  | DECFSZ of int * bit * bit
  | DCFSNZ of int * bit * bit
  | INCF of int * bit * bit
  | INCFSZ of int * bit * bit
  | INFSNZ of int * bit * bit
  | IORWF of int * bit * bit
  | MOVF of int * bit * bit
  | MOVFF of int * int
  | MOVWF of int * bit
  | MULWF of int * bit
  | NEGF of int * bit
  | RLCF of int * bit * bit
  | RLNCF of int * bit * bit
  | RRCF of int * bit * bit
  | RRNCF of int * bit * bit
  | SETF of int * bit
  | SUBFWB of int * bit * bit
  | SUBWF of int * bit * bit
  | SUBWFB of int * bit * bit
  | SWAPF of int * bit * bit
  | TSTFSZ of int * bit
  | XORWF of int * bit * bit
  | BCF of int * int * bit
  | BSF of int * int * bit
  | BTFSC of int * int * bit
  | BTFSS of int * int * bit
  | BTG of int * int * bit
  | BC of int
  | BN of int
  | BNC of int
  | BNN of int
  | BNOV of int
  | BNZ of int
  | BOV of int
  | BRA of int
  | BZ of int
  | CALL of int * bit
  | CLRWDT
  | DAW
  | GOTO of int
  | NOP
  | POP
  | PUSH
  | RCALL of int
  | RESET
  | RETFIE of bit
  | RETLW of int
  | RETURN of bit
  | SLEEP
  | ADDLW of int
  | ANDLW of int
  | IORLW of int
  | LFSR of int * int
  | MOVLB of int
  | MOVLW of int
  | MULLW of int
  | SUBLW of int
  | XORLW of int
  | TBLRD
  | TBLRDPOSTINC
  | TBLRDPOSTDEC
  | TBLRDPREINC
  | TBLWT
  | TBLWTPOSTINC
  | TBLWTPOSTDEC
  | TBLWTPREINC
  | ADDFSR of int * int
  | ADDULNK of int
  | CALLW
  | MOVSF of int * int
  | MOVSS of int * int
  | PUSHL of int
  | SUBFSR of int * int
  | SUBULNK of int
  | Invalid of int
  | Undefined
;;

let parse msB lsB msB' lsB' =
  let invalid () = Invalid (msB * 256 + lsB) in
  let relative byte = if byte land 0x80 = 0 then byte else byte - 0x100 in
  let get_bit i byte = if (byte lsr i) land 1 = 1 then I else O in
  let list_of_byte byte =
    let rec f i acc =
      if i = 8 then acc
      else f (succ i) (get_bit i byte :: acc)
    in
    f 0 []
  in
  if msB = -1 || lsB = -1 then Undefined
  else match list_of_byte msB with
    | [O;O;I;O;O;I;d;a] -> ADDWF  (lsB, d, a)
    | [O;O;I;O;O;O;d;a] -> ADDWFC (lsB, d, a)
    | [O;O;O;I;O;I;d;a] -> ANDWF  (lsB, d, a)
    | [O;I;I;O;I;O;I;a] -> CLRF   (lsB, a)
    | [O;O;O;I;I;I;d;a] -> COMF   (lsB, d, a)
    | [O;I;I;O;O;O;I;a] -> CPFSEQ (lsB, a)
    | [O;I;I;O;O;I;O;a] -> CPFSGT (lsB, a)
    | [O;I;I;O;O;O;O;a] -> CPFSLT (lsB, a)
    | [O;O;O;O;O;I;d;a] -> DECF   (lsB, d, a)
    | [O;O;I;O;I;I;d;a] -> DECFSZ (lsB, d, a)
    | [O;I;O;O;I;I;d;a] -> DCFSNZ (lsB, d, a)
    | [O;O;I;O;I;O;d;a] -> INCF   (lsB, d, a)
    | [O;O;I;I;I;I;d;a] -> INCFSZ (lsB, d, a)
    | [O;I;O;O;I;O;d;a] -> INFSNZ (lsB, d, a)
    | [O;O;O;I;O;O;d;a] -> IORWF  (lsB, d, a)
    | [O;I;O;I;O;O;d;a] -> MOVF   (lsB, d, a)
    | [I;I;O;O;_;_;_;_] when msB' land 0b11110000 = 0b11110000 ->
      if msB' = -1 || lsB' = -1 then invalid ()
      else MOVFF (((msB land 0b1111) lsl 8) lor lsB,
                  ((msB' land 0b1111) lsl 8) lor lsB')
    | [O;I;I;O;I;I;I;a] -> MOVWF  (lsB, a)
    | [O;O;O;O;O;O;I;a] -> MULWF  (lsB, a)
    | [O;I;I;O;I;I;O;a] -> NEGF   (lsB, a)
    | [O;O;I;I;O;I;d;a] -> RLCF   (lsB, d, a)
    | [O;I;O;O;O;I;d;a] -> RLNCF  (lsB, d, a)
    | [O;O;I;I;O;O;d;a] -> RRCF   (lsB, d, a)
    | [O;I;O;O;O;O;d;a] -> RRNCF  (lsB, d, a)
    | [O;I;I;O;I;O;O;a] -> SETF   (lsB, a)
    | [O;I;O;I;O;I;d;a] -> SUBFWB (lsB, d, a)
    | [O;I;O;I;I;I;d;a] -> SUBWF  (lsB, d, a)
    | [O;I;O;I;I;O;d;a] -> SUBWFB (lsB, d, a)
    | [O;O;I;I;I;O;d;a] -> SWAPF  (lsB, d, a)
    | [O;I;I;O;O;I;I;a] -> TSTFSZ (lsB, a)
    | [O;O;O;I;I;O;d;a] -> XORWF  (lsB, d, a)
    | [I;O;O;I;_;_;_;a] -> BCF    (lsB, (msB lsr 1) land 0b111, a)
    | [I;O;O;O;_;_;_;a] -> BSF    (lsB, (msB lsr 1) land 0b111, a)
    | [I;O;I;I;_;_;_;a] -> BTFSC  (lsB, (msB lsr 1) land 0b111, a)
    | [I;O;I;O;_;_;_;a] -> BTFSS  (lsB, (msB lsr 1) land 0b111, a)
    | [O;I;I;I;_;_;_;a] -> BTG    (lsB, (msB lsr 1) land 0b111, a)
    | [I;I;I;O;O;O;I;O] -> BC     (relative lsB)
    | [I;I;I;O;O;I;I;O] -> BN     (relative lsB)
    | [I;I;I;O;O;O;I;I] -> BNC    (relative lsB)
    | [I;I;I;O;O;I;I;I] -> BNN    (relative lsB)
    | [I;I;I;O;O;I;O;I] -> BNOV   (relative lsB)
    | [I;I;I;O;O;O;O;I] -> BNZ    (relative lsB)
    | [I;I;I;O;O;I;O;O] -> BOV    (relative lsB)
    | [I;I;O;I;O;_;_;_] ->
      let n = ((msB land 0b111) lsl 8) lor lsB in
      if n land 0x400 = 0 then BRA n else BRA (n - 0x800)
    | [I;I;I;O;O;O;O;O] -> BZ     (relative lsB)
    | [I;I;I;O;I;I;O;s] when msB' land 0b11110000 = 0b11110000 ->
      if msB' = -1 || lsB' = -1 then invalid ()
      else CALL (((msB' land 0b1111) lsl 16) lor (lsB' lsl 8) lor lsB, s)
    | [I;I;I;O;I;I;I;I] when msB' land 0b11110000 = 0b11110000 ->
      if msB' = -1 || lsB' = -1 then invalid ()
      else GOTO (((msB' land 0b1111) lsl 16) lor (lsB' lsl 8) lor lsB)
    | [I;I;O;I;I;_;_;_] ->
      let n = ((msB land 0b111) lsl 8) lor lsB in
      if n land 0x400 = 0 then RCALL n else RCALL (n - 0x800)
    | [O;O;O;O;I;I;O;O] -> RETLW  (lsB)
    | [O;O;O;O;I;I;I;I] -> ADDLW  (lsB)
    | [O;O;O;O;I;O;I;I] -> ANDLW  (lsB)
    | [O;O;O;O;I;O;O;I] -> IORLW  (lsB)
    | [I;I;I;O;I;I;I;O]
        when msB' = 0b11110000 && lsB lsr 6 = 0 && lsB lsr 4 <> 0b11 ->
      if msB' = -1 || lsB' = -1 then invalid ()
      else LFSR (lsB lsr 4, ((lsB land 0b1111) lsl 8) lor lsB')
    | [O;O;O;O;O;O;O;I] when lsB lsr 4 = 0 -> MOVLB  (lsB)
    | [O;O;O;O;I;I;I;O] -> MOVLW  (lsB)
    | [O;O;O;O;I;I;O;I] -> MULLW  (lsB)
    | [O;O;O;O;I;O;O;O] -> SUBLW  (lsB)
    | [O;O;O;O;I;O;I;O] -> XORLW  (lsB)
    | [I;I;I;O;I;O;O;O] ->
      let f = lsB lsr 6 and k = lsB land 0b111111 in
      if f = 0b11 then ADDULNK k else ADDFSR (f, k)
    | [I;I;I;O;I;O;I;I] when msB' land 0b11110000 = 0b11110000 ->
      if msB' = -1 || lsB' = -1 then invalid ()
      else if lsB lsr 7 = 0 then
        MOVSF (lsB, ((msB' land 0b1111) lsl 8) lor lsB')
      else MOVSS (lsB land 0x7F, lsB' land 0x7F)
    | [I;I;I;O;I;O;I;O] -> PUSHL  (lsB)
    | [I;I;I;O;I;O;O;I] ->
      let f = lsB lsr 6 and k = lsB land 0b111111 in
      if f = 0b11 then SUBULNK k else SUBFSR (f, k)
    | [O;O;O;O;O;O;O;O] ->
      begin match list_of_byte lsB with
        | [O;O;O;O;O;I;O;O] -> CLRWDT
        | [O;O;O;O;O;I;I;I] -> DAW
        | [O;O;O;O;O;O;O;O] -> NOP
        | [O;O;O;O;O;I;I;O] -> POP
        | [O;O;O;O;O;I;O;I] -> PUSH
        | [I;I;I;I;I;I;I;I] -> RESET
        | [O;O;O;I;O;O;O;s] -> RETFIE (s)
        | [O;O;O;I;O;O;I;s] -> RETURN (s)
        | [O;O;O;O;O;O;I;I] -> SLEEP
        | [O;O;O;O;I;O;O;O] -> TBLRD
        | [O;O;O;O;I;O;O;I] -> TBLRDPOSTINC
        | [O;O;O;O;I;O;I;O] -> TBLRDPOSTDEC
        | [O;O;O;O;I;O;I;I] -> TBLRDPREINC
        | [O;O;O;O;I;I;O;O] -> TBLWT
        | [O;O;O;O;I;I;O;I] -> TBLWTPOSTINC
        | [O;O;O;O;I;I;I;O] -> TBLWTPOSTDEC
        | [O;O;O;O;I;I;I;I] -> TBLWTPREINC
        | [O;O;O;I;O;I;O;O] -> CALLW
        | _ -> invalid ()
      end
    | [I;I;I;I;_;_;_;_] -> NOP
    | _ -> invalid ()
;;

let print oc instr =
  let int_of_bit b = if b = O then 0 else 1 in
  let fda op f d a =
    Printf.fprintf oc "\t%s\t0x%x, 0x%x, 0x%x\n" op f (int_of_bit d)
      (int_of_bit a)
  and fba op f b a =
    Printf.fprintf oc "\t%s\t0x%x, 0x%x, 0x%x\n" op f b (int_of_bit a)
  and fa op f a =
    Printf.fprintf oc "\t%s\t0x%x, 0x%x\n" op f (int_of_bit a)
  and ff op f1 f2 =
    Printf.fprintf oc "\t%s\t0x%x, 0x%x\n" op f1 f2
  and fk op f k =
    Printf.fprintf oc "\t%s\t0x%x, 0x%x\n" op f k
  and pn op n =
    Printf.fprintf oc "\t%s\t0x%x\n" op n
  and pk op k =
    Printf.fprintf oc "\t%s\t0x%x\n" op k
  and ps op s =
    Printf.fprintf oc "\t%s\t0x%x\n" op (int_of_bit s)
  and zz op z1 z2 =
    Printf.fprintf oc "\t%s\t0x%x, 0x%x\n" op z1 z2
  and zf op z f =
    Printf.fprintf oc "\t%s\t0x%x, 0x%x\n" op z f
  and ks op k s =
    Printf.fprintf oc "\t%s\t0x%x, 0x%x\n" op k (int_of_bit s)
  and p op =
    Printf.fprintf oc "\t%s\n" op
  and invalid x =
    Printf.fprintf oc "\t-%04X-\n" x
  and undefined () =
    Printf.fprintf oc "\t------\n"
  in
  match instr with
    | ADDWF (f, d, a)  -> fda "addwf" f d a
    | ADDWFC (f, d, a) -> fda "addwfc" f d a
    | ANDWF (f, d, a)  -> fda "andwf" f d a
    | CLRF (f, a)      -> fa "clrf" f a
    | COMF (f, d, a)   -> fda "comf" f d a
    | CPFSEQ (f, a)    -> fa "cpfseq" f a
    | CPFSGT (f, a)    -> fa "cpfsgt" f a
    | CPFSLT (f, a)    -> fa "cpfslt" f a
    | DECF (f, d, a)   -> fda "decf" f d a
    | DECFSZ (f, d, a) -> fda "decfsz" f d a
    | DCFSNZ (f, d, a) -> fda "dcfsnz" f d a
    | INCF (f, d, a)   -> fda "incf" f d a
    | INCFSZ (f, d, a) -> fda "incfsz" f d a
    | INFSNZ (f, d, a) -> fda "infsnz" f d a
    | IORWF (f, d, a)  -> fda "iorwf" f d a
    | MOVF (f, d, a)   -> fda "movf" f d a
    | MOVFF (f1, f2)   -> ff "movff" f1 f2
    | MOVWF (f, a)     -> fa "movwf" f a
    | MULWF (f, a)     -> fa "mulwf" f a
    | NEGF (f, a)      -> fa "negf" f a
    | RLCF (f, d, a)   -> fda "rlcf" f d a
    | RLNCF (f, d, a)  -> fda "rlncf" f d a
    | RRCF (f, d, a)   -> fda "rrcf" f d a
    | RRNCF (f, d, a)  -> fda "rrncf" f d a
    | SETF (f, a)      -> fa "setf" f a
    | SUBFWB (f, d, a) -> fda "subfwb" f d a
    | SUBWF (f, d, a)  -> fda "subwf" f d a
    | SUBWFB (f, d, a) -> fda "subwfb" f d a
    | SWAPF (f, d, a)  -> fda "swapf" f d a
    | TSTFSZ (f, a)    -> fa "tstfsz" f a
    | XORWF (f, d, a)  -> fda "xorwf" f d a
    | BCF (f, b, a)    -> fba "bcf" f b a
    | BSF (f, b, a)    -> fba "bsf" f b a
    | BTFSC (f, b, a)  -> fba "btfsc" f b a
    | BTFSS (f, b, a)  -> fba "btfss" f b a
    | BTG (f, b, a)    -> fba "btg" f b a
    | BC n             -> pn "bc" n
    | BN n             -> pn "bn" n
    | BNC n            -> pn "bnc" n
    | BNN n            -> pn "bnn" n
    | BNOV n           -> pn "bnov" n
    | BNZ n            -> pn "bnz" n
    | BOV n            -> pn "bov" n
    | BRA n            -> pn "bra" n
    | BZ n             -> pn "bz" n
    | CALL (k, s)      -> ks "call" k s
    | CLRWDT           -> p "clrwdt"
    | DAW              -> p "daw"
    | GOTO n           -> pn "goto" n
    | NOP              -> p "nop"
    | POP              -> p "pop"
    | PUSH             -> p "push"
    | RCALL n          -> pn "rcall" n
    | RESET            -> p "reset"
    | RETFIE s         -> ps "retfie" s
    | RETLW k          -> pk "retlw" k
    | RETURN s         -> ps "retrun" s
    | SLEEP            -> p "sleep"
    | ADDLW k          -> pk "addlw" k
    | ANDLW k          -> pk "andlw" k
    | IORLW k          -> pk "iorlw" k
    | LFSR (f, k)      -> fk "lfsr" f k
    | MOVLB k          -> pk "movlb" k
    | MOVLW k          -> pk "movlw" k
    | MULLW k          -> pk "mullw" k
    | SUBLW k          -> pk "sublw" k
    | XORLW k          -> pk "xorlw" k
    | TBLRD            -> p "tblrd*"
    | TBLRDPOSTINC     -> p "tblrd*+"
    | TBLRDPOSTDEC     -> p "tblrd*-"
    | TBLRDPREINC      -> p "tblrd+*"
    | TBLWT            -> p "tblwt*"
    | TBLWTPOSTINC     -> p "tblwt*+"
    | TBLWTPOSTDEC     -> p "tblwt*-"
    | TBLWTPREINC      -> p "tblwt+*"
    | ADDFSR (f, k)    -> fk "addfsr" f k
    | ADDULNK k        -> pk "addulnk" k
    | CALLW            -> p "callw"
    | MOVSF (z, f)     -> zf "movsf" z f
    | MOVSS (z1, z2)   -> zz "movss" z1 z2
    | PUSHL k          -> pk "pushl" k
    | SUBFSR (f, k)    -> fk "subfsr" f k
    | SUBULNK k        -> pk "subulnk" k
    | Invalid x        -> invalid x
    | Undefined        -> undefined ()
;;
