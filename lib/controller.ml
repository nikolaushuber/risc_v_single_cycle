open Hardcaml

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

let create _ _ = failwith ""

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"memory" create input
