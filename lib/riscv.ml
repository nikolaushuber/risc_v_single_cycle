open Hardcaml
open Signal

module I = struct
  type 'a t = {
    clock : 'a; 
    reset : 'a; 
    instr : 'a [@bits 32];
    read_data : 'a [@bits 32];
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    pc : 'a [@bits 32];
    mem_write : 'a;
    alu_result : 'a [@bits 32];
    write_data : 'a [@bits 32];
  }
  [@@deriving hardcaml]
end

let create (scope : Scope.t) (i : _ I.t) = 
  let O.({ pc; mem_write; alu_result; write_data} as out) = O.Of_always.wire zero in 

  let alu_src = wire 1 in 
  let reg_write = wire 1 in 
  let jump = wire 1 in 
  let zero_ = wire 1 in 

  let 