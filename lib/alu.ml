open Hardcaml
open Signal

module I = struct
  type 'a t = {
    src_a : 'a; [@bits 32]
    src_b : 'a; [@bits 32]
    alu_ctrl : 'a; [@bits 3]
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { alu_res : 'a [@bits 32] } [@@deriving hardcaml]
end

let create (_scope : Scope.t) (i : _ I.t) =
  let O.({ alu_res } as out) = O.Of_always.wire zero in

  Always.(
    compile
      [
        switch i.alu_ctrl
          [
            (of_bit_string "000", [ alu_res <-- i.src_a +: i.src_b ]);
            (of_bit_string "001", [ alu_res <-- i.src_a -: i.src_b ]);
            (of_bit_string "010", [ alu_res <-- (i.src_a &: i.src_b) ]);
            (of_bit_string "011", [ alu_res <-- (i.src_a |: i.src_b) ]);
            ( of_bit_string "101",
              [ alu_res <-- zero 31 @: (i.src_a <: i.src_b) ] );
          ];
      ]);

  O.Of_always.value out

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"alu" create input
