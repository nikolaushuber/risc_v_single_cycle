open Hardcaml
open Signal

module I = struct
  type 'a t = {
    clock : 'a;
    reset : 'a;
    result_src : 'a; [@bits 2]
    pc_src : 'a;
    alu_src : 'a;
    reg_write : 'a;
    imm_src : 'a; [@bits 2]
    alu_ctrl : 'a; [@bits 3]
    instruction : 'a; [@bits 32]
    read_data : 'a; [@bits 32]
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    zero_ : 'a;
    pc : 'a; [@bits 32]
    alu_result : 'a; [@bits 32]
    write_data : 'a; [@bits 32]
  }
  [@@deriving hardcaml]
end

let create (scope : Scope.t) (i : _ I.t) =
  let pc_next = wire 32 in
  let pc_plus_4 = wire 32 in
  let pc_target = wire 32 in
  let imm_ext = wire 32 in
  let src_a = wire 32 -- "src_a" in
  let src_b = wire 32 -- "src_b" in
  let result = wire 32 -- "result" in

  let spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in

  let zero_ = wire 1 in

  let pc = Always.Variable.reg ~width:32 spec in

  Always.(compile [
    if_ i.reset [
      pc <--. 0;
    ] [
      pc <-- pc_next;
    ]
  ]);
  let alu_result = wire 32 in
  let write_data = wire 32 in

  pc_plus_4 <== pc.value +:. 4;
  pc_target <== pc.value +: imm_ext;

  pc_next <== mux2 i.pc_src pc_target pc_plus_4 -- "pc_next";

  let regfile =
    Regfile.hierarchical scope
      Regfile.I.
        {
          clock = i.clock;
          wr_enable = (i.reg_write -- "reg_write");
          rs1_addr = (i.instruction.:[19, 15] -- "rs1_addr");
          rs2_addr = (i.instruction.:[24, 20] -- "rs2_addr");
          rt_addr = (i.instruction.:[11, 7] -- "rt_addr");
          rt_data = result;
        }
  in

  src_a <== regfile.rs1;
  write_data <== regfile.rs2;

  let extend =
    Extend.hierarchical scope
      Extend.I.{ instr = i.instruction; src = i.imm_src }
  in

  imm_ext <== extend.immext;

  src_b <== mux2 i.alu_src imm_ext write_data;

  let alu =
    Alu.hierarchical scope Alu.I.{ src_a; src_b; alu_ctrl = i.alu_ctrl }
  in

  alu_result <== alu.alu_res;
  zero_ <== alu.zero_;

  result <== mux i.result_src [ alu_result; i.read_data; pc_plus_4 ];

  O.{ zero_; pc = pc.value; alu_result; write_data }

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"datapath" create input
