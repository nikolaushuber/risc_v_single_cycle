open Hardcaml
open Signal

let program =
  [
    (0x00, 0x00500113);
    (0x04, 0x00C00193);
    (0x08, 0xFF718393);
    (0x0C, 0x0023E233);
    (0x10, 0x0041F2B3);
    (0x14, 0x004282B3);
    (0x18, 0x02728863);
    (0x1C, 0x0041A233);
    (0x20, 0x00020463);
    (0x24, 0x00000293);
    (0x28, 0x0023A233);
    (0x2C, 0x005203B3);
    (0x30, 0x402383B3);
    (0x34, 0x0471AA23);
    (0x38, 0x06002103);
    (0x3C, 0x005104B3);
    (0x40, 0x008001EF);
    (0x44, 0x00100113);
    (0x48, 0x00910133);
    (0x4C, 0x0221A023);
    (0x50, 0x00210063);
  ]

module I = struct
  type 'a t = { addr : 'a [@bits 32] } [@@deriving hardcaml]
end

module O = struct
  type 'a t = { instr : 'a [@bits 32] } [@@deriving hardcaml]
end

let ext = of_int ~width:32

let create (_scope : Scope.t) (i : _ I.t) =
  let O.({ instr } as out) = O.Of_always.wire zero in

  let program_cases =
    List.map
      (fun (addr, raw_instr) ->
        (ext addr, [ Always.( <-- ) instr (ext raw_instr) ]))
      program
  in

  Always.(compile [ switch i.addr program_cases ]);

  O.Of_always.value out

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"memory" create input
