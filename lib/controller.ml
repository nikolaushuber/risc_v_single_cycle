open Hardcaml
open Signal

module I = struct
  type 'a t = {
    op : 'a; [@bits 7]
    funct3 : 'a; [@bits 3]
    funct7b5 : 'a;
    zero_ : 'a;
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    result_src : 'a; [@bits 2]
    mem_write : 'a;
    pc_src : 'a;
    alu_src : 'a;
    reg_write : 'a;
    jump : 'a;
    imm_src : 'a; [@bits 2]
    alu_ctrl : 'a; [@bits 3]
  }
  [@@deriving hardcaml]
end

let create (scope : Scope.t) (i : _ I.t) =
  let alu_op = wire 2 in
  let branch = wire 1 in

  let result_src = wire 2 in
  let mem_write = wire 1 in
  let pc_src = wire 1 -- "pc_src" in
  let alu_src = wire 1 in
  let reg_write = wire 1 in
  let jump = wire 1 in
  let imm_src = wire 2 in
  let alu_ctrl = wire 3 in

  let md = Maindec.hierarchical scope Maindec.I.{ op = i.op } in

  result_src <== md.result_src;
  mem_write <== md.mem_write;
  branch <== md.branch;
  alu_src <== md.alu_src;
  reg_write <== md.reg_write;
  jump <== md.jump;
  imm_src <== md.imm_src;
  alu_op <== md.alu_op;

  let ad =
    Aludec.hierarchical scope
      Aludec.I.
        { opb5 = bit i.op 5; funct3 = i.funct3; funct7b5 = i.funct7b5; alu_op }
  in

  alu_ctrl <== ad.alu_control;

  pc_src <== (branch &: i.zero_ |: jump);

  O.
    {
      result_src;
      mem_write;
      pc_src;
      alu_src;
      reg_write;
      jump;
      imm_src;
      alu_ctrl;
    }

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"controller" create input
