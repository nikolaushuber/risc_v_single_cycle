open Hardcaml
open Signal

module I = struct
  type 'a t = { clock : 'a; clear : 'a } [@@deriving hardcaml]
end

let create (scope : Scope.t) (i : _ I.t) =
  let pc_next = wire 32 in
  let pc_enable = wire 1 in
  let instr_reg_enable = wire 1 in
  let memory_wr_enable = wire 1 in
  let immsrc = wire 2 in
  let regfile_wr_enable = wire 1 in
  let addr_src = wire 1 in
  let result_src = wire 2 in

  let addr_wire = wire 32 in
  let result_wire = wire 32 in

  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let pc_reg = reg spec ~enable:pc_enable pc_next in

  let memory_input =
    Memory.I.
      {
        clock = i.clock;
        enable = memory_wr_enable;
        addr = addr_wire;
        wr_data = result_wire;
      }
  in

  let Memory.O.{ rd_data } = Memory.hierarchical scope memory_input in

  let ir_reg = reg spec ~enable:instr_reg_enable rd_data in
  let data_reg = reg spec rd_data in

  let rs1_addr = ir_reg.:[19, 15] in

  let extender =
    Extend.hierarchical scope Extend.I.{ instr = ir_reg; src = immsrc }
  in

  let regfile_input =
    Regfile.I.
      {
        clock = i.clock;
        wr_enable = regfile_wr_enable;
        rs1_addr;
        rs2_addr = Signal.zero 5;
        rt_addr = Signal.zero 5;
        rt_data = Signal.zero 32;
      }
  in
  let regfile = Regfile.hierarchical scope regfile_input in

  let a_reg = reg spec regfile.rs1 in

  let alu =
    Alu.hierarchical scope
      Alu.I.{ src_a = a_reg; src_b = extender.immext; alu_ctrl = zero 3 }
  in

  let alu_out_reg = reg spec alu.alu_res in

  result_wire <== mux result_src [ alu_out_reg; data_reg; zero 32 ];

  addr_wire <== mux2 addr_src pc_reg result_wire;

  ignore memory
