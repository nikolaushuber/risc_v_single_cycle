open Hardcaml
open Signal

let mem_size = 512

module I = struct
  type 'a t = {
    clock : 'a;
    wr_enable : 'a;
    wr_addr : 'a; [@bits 9]
    wr_data : 'a; [@bits 32]
    rd_addr : 'a; [@bits 9]
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { rd_data : 'a [@bits 32] } [@@deriving hardcaml]
end

let create (_scope : Scope.t) (input : _ I.t) =
  let wr_port =
    Write_port.
      {
        write_clock = input.clock;
        write_enable = input.wr_enable;
        write_address = input.wr_addr;
        write_data = input.wr_data;
      }
  in
  let mem =
    multiport_memory mem_size ~write_ports:[| wr_port |]
      ~read_addresses:[| input.rd_addr |]
  in
  let rd_data = mem.(0) in
  O.{ rd_data }

let hierarchical scope input =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"memory" create input
