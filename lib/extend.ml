open Hardcaml
open Signal

module I = struct
  type 'a t = { instr : 'a; [@bits 32] src : 'a [@bits 2] }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { immext : 'a [@bits 32] } [@@deriving hardcaml]
end

let create (_scope : Scope.t) (i : _ I.t) =
  let O.({ immext } as out) = O.Of_always.wire zero in

  let imm_i = sresize i.instr.:[31, 20] 32 in
  let imm_s = sresize (i.instr.:[31, 25] @: i.instr.:[11, 7]) 32 in
  let imm_j =
    sresize
      (i.instr.:(31)
      @: i.instr.:[19, 12]
      @: i.instr.:(20)
      @: i.instr.:[30, 21]
      @: gnd)
      32
  in
  let imm_b =
    sresize
      (i.instr.:(31) @: i.instr.:(7)
      @: i.instr.:[30, 25]
      @: i.instr.:[11, 8]
      @: gnd)
      32
  in

  Always.(
    compile
      [
        switch i.src
          [
            (of_bit_string "00", [ immext <-- imm_i ]);
            (of_bit_string "01", [ immext <-- imm_s ]);
            (of_bit_string "10", [ immext <-- imm_b ]);
            (of_bit_string "11", [ immext <-- imm_j ]);
          ];
      ]);

  O.Of_always.value out

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"extend" create input
