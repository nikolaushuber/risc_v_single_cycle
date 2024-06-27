open Hardcaml
open Signal

module I = struct
  type 'a t = {
    clock : 'a;
    wr_enable : 'a;
    rs1_addr : 'a; [@bits 5]
    rs2_addr : 'a; [@bits 5]
    rt_addr : 'a; [@bits 5]
    rt_data : 'a; [@bits 32]
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { rs1 : 'a; [@bits 32] rs2 : 'a [@bits 32] } [@@deriving hardcaml]
end

let create (_scope : Scope.t) (i : _ I.t) =
  let wr_port =
    Write_port.
      {
        write_clock = i.clock;
        write_enable = i.wr_enable;
        write_address = i.rt_addr;
        write_data = i.rt_data;
      }
  in
  let regfile =
    multiport_memory 31 ~write_ports:[| wr_port |]
      ~read_addresses:[| i.rs1_addr; i.rs2_addr |]
  in
  let rs1 = mux2 (i.rs1_addr ==: Signal.zero 5) (Signal.zero 32) regfile.(0) in
  let rs2 = mux2 (i.rs2_addr ==: Signal.zero 5) (Signal.zero 32) regfile.(1) in
  O.{ rs1; rs2 }

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"regfile" create input
