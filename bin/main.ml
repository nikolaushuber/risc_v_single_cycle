open Hardcaml
open Hardcaml_waveterm
open Risc_v

let create_sim () =
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface (Cpu.I) (Cpu.O) in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Cpu.create scope) in
  Waveform.create sim

let testbench () =
  let _, sim = create_sim () in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in

  let get_wire ?(signed = false) ?(hex = false) name = match Cyclesim.lookup_node_or_reg_by_name sim name with 
    | Some node -> Cyclesim.Node.to_bits node |> (if signed then Bits.to_sint else Bits.to_int) |> if hex then Printf.sprintf "%x" else string_of_int
    | None -> "Could not find internal signal " ^ name 
  in

  (* Reset the circuit *)
  i.reset := Bits.vdd;
  Cyclesim.cycle sim;
  i.reset := Bits.gnd;

  let rec aux () =
    Cyclesim.cycle_before_clock_edge sim;
    print_endline ("PC: " ^ get_wire ~hex:true "pc_0");
    print_endline ("- PCNext: " ^ get_wire ~hex:true "pc_next");
    print_endline ("- Instr: " ^ get_wire ~hex:true "instr");
    Cyclesim.cycle_at_clock_edge sim;
    Cyclesim.cycle_after_clock_edge sim;
    if Bits.is_vdd !(o.mem_write) && Bits.to_int !(o.data_addr) = 100 then
      if Bits.to_int !(o.write_data) = 25
      then print_endline "Simulation succeeded"
      else (
        print_endline "Simulation failed"; 
        print_endline ("Got: " ^ (Bits.to_int !(o.data_addr) |> string_of_int));
      )
    else aux ()
  in
  aux ()

let () = testbench ()
