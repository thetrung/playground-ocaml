type opcode =
  | Halt
  | Mov of int * int     (* dst, src *)
  | LoadI of int * int   (* dst, immediate *)
  | Add of int * int * int  (* src1, src2, dst *)
  | Sub of int * int * int
  | Mul of int * int * int
  | Div of int * int * int
  | Print of int          (* reg *)

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

let run vm =
  let len = Array.length vm.program in
  let rec loop () =
    if vm.ip >= len then () else
    match vm.program.(vm.ip) with
    | Halt -> ()
    | Mov (dst, src) ->
        vm.registers.(dst) <- vm.registers.(src);
        vm.ip <- vm.ip + 1;
        loop ()
    | LoadI (dst, imm) ->
        vm.registers.(dst) <- imm;
        vm.ip <- vm.ip + 1;
        loop ()
    | Add (src1, src2, dst) ->
        vm.registers.(dst) <- vm.registers.(src1) + vm.registers.(src2);
        vm.ip <- vm.ip + 1;
        loop ()
    | Sub (src1, src2, dst) ->
        vm.registers.(dst) <- vm.registers.(src1) - vm.registers.(src2);
        vm.ip <- vm.ip + 1;
        loop ()
    | Mul (src1, src2, dst) ->
        vm.registers.(dst) <- vm.registers.(src1) * vm.registers.(src2);
        vm.ip <- vm.ip + 1;
        loop ()
    | Div (src1, src2, dst) ->
        if vm.registers.(src2) = 0 then failwith "Division by zero"
        else vm.registers.(dst) <- vm.registers.(src1) / vm.registers.(src2);
        vm.ip <- vm.ip + 1;
        loop ()
    | Print reg ->
        Printf.printf "%d\n" vm.registers.(reg);
        vm.ip <- vm.ip + 1;
        loop ()
  in
  loop ()
let r0, r1, r2, r3, r4 = 0,1,2,3,4
let () =
  let program = [|
    LoadI (r0, 10);     (* $0x0 = 10 *)
    LoadI (r1, 20);     (* $0x1 = 20 *)
    Add   (r0, r1, r2); (* $0x0 + $0x1 = $0x2  *)
    LoadI (r3, 2);      (* $0x3 = 2 *)
    Mul   (r2, r3, r4); (* $0x2 x $0x3 = $0x4 *)
    Print  r4;          (* PRINT $0x4 *)
    Halt;               (* Stop *)
  |] in
  let vm = create_vm program in
  run vm
