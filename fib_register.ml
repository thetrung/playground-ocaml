type opcode =
  | HALT
  | MOV of int * int     (* dst, src *)
  | LDI of int * int   (* dst, immediate *)

  | ADD of int * int * int  (* src1, src2, dst *)
  | Sub of int * int * int
  | MUL of int * int * int
  | Div of int * int * int

  | LABEL of string       (* imm *)
  | JMP of int            (* imm *)
  | CMP of int * int * int(* src1, src2, dst *)
  | JEQ of int * int      (* imm, src1 *)

  | PRINT of int          (* reg *)

type vm = {
  mutable registers : int array;
  mutable ip : int;
  program : opcode array;
}

let create_vm program = {
  registers = Array.make 8 0;
  ip = 0;
  program;
}

let ax, bx, cx, dx, ex = 0,1,2,3,4

let run vm =
  let len = Array.length vm.program in
  Printf.printf "\nBEGIN (%d instructions)\n" len;
  let rec loop () =
    if vm.ip >= len then () else
    Printf.printf "OPCODE: %d\n" vm.ip;
    match vm.program.(vm.ip) with
    | HALT -> ()
    | MOV (dst, src) ->
        vm.registers.(dst) <- vm.registers.(src);
        vm.ip <- vm.ip + 1;
        loop ()
    | LDI (dst, imm) ->
        vm.registers.(dst) <- imm;
        vm.ip <- vm.ip + 1;
        loop ()
    | ADD (src1, src2, dst) ->
        vm.registers.(dst) <- vm.registers.(src1) + vm.registers.(src2);
        vm.ip <- vm.ip + 1;
        loop ()
    | Sub (src1, src2, dst) ->
        vm.registers.(dst) <- vm.registers.(src1) - vm.registers.(src2);
        vm.ip <- vm.ip + 1;
        loop ()
    | MUL (src1, src2, dst) ->
        vm.registers.(dst) <- vm.registers.(src1) * vm.registers.(src2);
        vm.ip <- vm.ip + 1;
        loop ()
    | Div (src1, src2, dst) ->
        if vm.registers.(src2) = 0 then failwith "Division by zero"
        else vm.registers.(dst) <- vm.registers.(src1) / vm.registers.(src2);
        vm.ip <- vm.ip + 1;
        loop ()
    | PRINT reg ->
        Printf.printf "PRINT: %d\n" vm.registers.(reg);
        vm.ip <- vm.ip + 1;
        loop ()
    | LABEL name ->
        Printf.printf "LABEL: %s @ %d\n" name vm.ip;
        vm.ip <- vm.ip + 1;
        loop ()
    | JMP ip -> 
        vm.ip <- ip;
        loop ()
    | JEQ (ip, reg) ->
        if vm.registers.(reg) == 0 
        then vm.ip <- ip
        else vm.ip <- vm.ip + 1;
        loop ()
    | CMP (src1, src2, dst) -> 
        let regs = vm.registers in
        Printf.printf "CMP (%d, %d)" regs.(src1) regs.(src2);
        regs.(dst) <- 
          if regs.(src1) == regs.(src2) then 0 else 
          if regs.(src1) > regs.(src2) then 1 else 
          if regs.(src1) < regs.(src2) then 2 else 
          failwith "Unsupported Method.";
        Printf.printf " => %d\n" vm.registers.(dx);
        vm.ip <- vm.ip + 1;
        loop ()
  in
  loop ();
  Printf.printf "END PROGRAM\n"

let () =
  let test_ADD_MUL = [|
    LDI (ax, 10);     (* $0x0 = 10 *)
    LDI (bx, 20);     (* $0x1 = 20 *)
    ADD (ax, bx, cx); (* exx0 + $0x1 = $0x2  *)
    LDI (dx, 2);      (* $0x3 = 2 *)
    MUL   (cx, dx, ex); (* $0x2 x $0x3 = $0x4 *)
    PRINT  ex;          (* PRINT $0x4 *)
    HALT;               (* Stop *)
  |] in let vm = create_vm test_ADD_MUL in run vm;

  let test_branching = [|
    LDI (ax, 0);
    LDI (bx, 1);
    LDI (cx, 5);
    LABEL("Entry");
    ADD   (ax, bx, ax);
    CMP   (ax, cx, dx);
    PRINT (ax);
    JEQ   (9, dx);  (* -> Exit *)
    JMP   (3);      (* -> Entry *)
    LABEL ("Exit");
    HALT;
  |] in let vm = create_vm test_branching in run vm;
