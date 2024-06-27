open Hardcaml
open Signal

module I = struct
  type 'a t = {
    opb5 : 'a;
    funct3 : 'a; [@bits 3]
    funct7b5 : 'a;
    alu_op : 'a; [@bits 2]
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { alu_control : 'a [@bits 3] } [@@deriving hardcaml]
end

let create (_scope : Scope.t) (i : _ I.t) =
  let O.({ alu_control } as out) = O.Of_always.wire zero in

  let r_type_sub = i.funct7b5 &: i.opb5 in

  Always.(
    compile
      [
        if_ (i.alu_op ==:. 0b00)
          [ alu_control <--. 0b000 ]
          (elif (i.alu_op ==:. 0b01)
             [ alu_control <--. 0b001 ]
             [
               switch i.funct3
                 [
                   ( of_bit_string "000",
                     [
                       if_ r_type_sub
                         [ alu_control <--. 0b001 ]
                         [ alu_control <--. 0b000 ];
                     ] );
                   (of_bit_string "010", [ alu_control <--. 0b101 ]);
                   (of_bit_string "110", [ alu_control <--. 0b011 ]);
                   (of_bit_string "111", [ alu_control <--. 0b010 ]);
                 ];
             ]);
      ]);

  O.Of_always.value out

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"memory" create input
