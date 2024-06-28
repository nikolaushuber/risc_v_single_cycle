open Hardcaml
open Signal

module I = struct
  type 'a t = { clock : 'a; reset : 'a } [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    write_data : 'a; [@bits 32]
    data_addr : 'a; [@bits 32]
    mem_write : 'a;
    pc : 'a; [@bits 32]
  }
  [@@deriving hardcaml]
end

let create (scope : Scope.t) (i : _ I.t) =
  let write_data = wire 32 in
  let data_addr = wire 32 in
  let mem_write = wire 1 in

  let pc = wire 32 -- "pc" in
  let instr = wire 32 -- "instr" in
  let read_data = wire 32 in

  let rv =
    Riscv.hierarchical scope
      Riscv.I.{ clock = i.clock; reset = i.reset; instr; read_data }
  in

  pc <== rv.pc;
  mem_write <== rv.mem_write;
  write_data <== rv.write_data;
  data_addr <== rv.alu_result;

  let imem =
    Instruction_mem.hierarchical scope Instruction_mem.I.{ addr = pc }
  in

  instr <== imem.instr;

  let dmem =
    Data_memory.hierarchical scope
      Data_memory.I.
        {
          clock = i.clock;
          enable = mem_write;
          addr = data_addr.:[8, 0];
          wr_data = write_data;
        }
  in

  read_data <== dmem.rd_data;

  O.{ write_data; data_addr; mem_write; pc }
