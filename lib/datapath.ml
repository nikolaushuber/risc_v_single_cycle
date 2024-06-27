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

  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let pc_reg = reg spec ~enable:pc_enable pc_next in

  let memory_input =
    Memory.I.
      {
        clock = i.clock;
        enable = Signal.gnd;
        addr = Signal.zero 9;
        wr_data = Signal.zero 32;
      }
  in

  let Memory.O.{ rd_data } = Memory.hierarchical scope memory_input in

  let ir_reg = reg spec ~enable:instr_reg_enable rd_data in

  let rs1_addr = ir_reg.:[19, 15] in

  let Extend.O.{ immext } =
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

  let a_reg = reg spec ~enable:Signal.vdd regfile.rs1 in

  let alu = Alu.hierarchical scope Alu.I.{
    src_a = a_reg;
    src_b = zero 32;
    alu_ctrl = zero 3;
  } in

  ignore (pc_reg, instr_reg_enable, memory_wr_enable, memory, immext, alu)
