open Hardcaml
open Signal

module I = struct
  type 'a t = { op : 'a [@bits 7] } [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    result_src : 'a; [@bits 2]
    mem_write : 'a;
    branch : 'a;
    alu_src : 'a;
    reg_write : 'a;
    jump : 'a;
    imm_src : 'a; [@bits 2]
    alu_op : 'a; [@bits 2]
  }
  [@@deriving hardcaml]
end

let create (_scope : Scope.t) (i : _ I.t) =
  let O.(
        {
          result_src;
          mem_write;
          branch;
          alu_src;
          reg_write;
          jump;
          imm_src;
          alu_op;
        } as out) =
    O.Of_always.wire zero
  in

  let case reg_write_ imm_src_ alu_src_ mem_write_ result_src_ branch_ alu_op_
      jump_ =
    Always.
      [
        reg_write <-- of_bit_string reg_write_;
        imm_src <-- of_bit_string imm_src_;
        alu_src <-- of_bit_string alu_src_;
        mem_write <-- of_bit_string mem_write_;
        result_src <-- of_bit_string result_src_;
        branch <-- of_bit_string branch_;
        alu_op <-- of_bit_string alu_op_;
        jump <-- of_bit_string jump_;
      ]
  in

  Always.(
    compile
      [
        switch i.op
          [
            (* lw *)
            (of_bit_string "0000011", case "1" "00" "1" "0" "01" "0" "00" "0");
            (* sw *)
            (of_bit_string "0100011", case "0" "01" "1" "1" "00" "0" "00" "0");
            (* r-type *)
            (of_bit_string "0110011", case "1" "00" "0" "0" "00" "0" "10" "0");
            (* beq *)
            (of_bit_string "1100011", case "0" "10" "0" "0" "00" "1" "01" "0");
            (* i-type alu *)
            (of_bit_string "0010011", case "1" "00" "1" "0" "00" "0" "10" "0");
            (* jal *)
            (of_bit_string "1101111", case "1" "11" "0" "0" "10" "0" "00" "1");
          ];
      ]);

  O.Of_always.value out

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"maindec" create input
