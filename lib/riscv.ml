open Hardcaml
open Signal

module I = struct
  type 'a t = {
    clock : 'a;
    reset : 'a;
    instr : 'a; [@bits 32]
    read_data : 'a; [@bits 32]
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    pc : 'a; [@bits 32]
    mem_write : 'a;
    alu_result : 'a; [@bits 32]
    write_data : 'a; [@bits 32]
  }
  [@@deriving hardcaml]
end

let create (scope : Scope.t) (i : _ I.t) =
  let alu_src = wire 1 in
  let reg_write = wire 1 in
  let jump = wire 1 in
  let zero_ = wire 1 in
  let result_src = wire 2 in
  let imm_src = wire 2 in
  let alu_ctrl = wire 3 in
  let pc_src = wire 1 in

  let pc = wire 32 in
  let mem_write = wire 1 in
  let alu_result = wire 32 in
  let write_data = wire 32 in

  let c =
    Controller.hierarchical scope
      Controller.I.
        {
          op = i.instr.:[6, 0];
          funct3 = i.instr.:[14, 12];
          funct7b5 = bit i.instr 30;
          zero_;
        }
  in

  result_src <== c.result_src;
  mem_write <== c.mem_write;
  pc_src <== c.pc_src;
  alu_src <== c.alu_src;
  reg_write <== c.reg_write;
  jump <== c.jump;
  imm_src <== c.imm_src;
  alu_ctrl <== c.alu_ctrl;

  let dp =
    Datapath.hierarchical scope
      Datapath.I.
        {
          clock = i.clock;
          reset = i.reset;
          result_src;
          pc_src;
          alu_src;
          reg_write;
          imm_src;
          alu_ctrl;
          instruction = i.instr;
          read_data = i.read_data;
        }
  in

  zero_ <== dp.zero_;
  pc <== dp.pc;
  alu_result <== dp.alu_result;
  write_data <== dp.write_data;

  O.{ pc; mem_write; alu_result; write_data }

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"riscv" create input
