open Hardcaml
open Hardcaml_waveterm
open Risc_v

let create_sim () =
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface (Regfile.I) (Regfile.O) in
  let sim = Sim.create (Regfile.create scope) in
  Waveform.create sim

type test_input = {
  rs1_addr : int;
  rs2_addr : int;
  rt_addr : int;
  rt_data : int;
  wr_enable : bool;
}

let step (sim : (_ Regfile.I.t, _ Regfile.O.t) Cyclesim.t) test =
  let i = Cyclesim.inputs sim in
  i.rs1_addr := Bits.of_int ~width:5 test.rs1_addr;
  i.rs2_addr := Bits.of_int ~width:5 test.rs2_addr;
  i.rt_addr := Bits.of_int ~width:5 test.rt_addr;
  i.rt_data := Bits.of_int ~width:32 test.rt_data;
  i.wr_enable := if test.wr_enable then Bits.vdd else Bits.gnd;
  Cyclesim.cycle sim

let mk_test rs1_addr rs2_addr rt_addr rt_data wr_enable =
  { rs1_addr; rs2_addr; rt_addr; rt_data; wr_enable }

let test_inputs =
  [
    mk_test 0 0 1 0xAAAAAAAA true;
    mk_test 0 1 0 0 false;
    mk_test 1 0 0 0 false;
    mk_test 0 0 0 0xDEADDEAD true;
    mk_test 0 0 0 0 false;
  ]

let testbench () =
  let waves, sim = create_sim () in
  List.iter (step sim) test_inputs;
  waves

let%expect_test "regfile test" =
  let waves = testbench () in
  Waveform.expect ~wave_width:4 ~display_height:23 waves;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌│
    │               ││     └────┘    └────┘    └────┘    └────┘    └────┘│
    │               ││────────────────────┬─────────┬─────────────────── │
    │rs1_addr       ││ 00                 │01       │00                  │
    │               ││────────────────────┴─────────┴─────────────────── │
    │               ││──────────┬─────────┬───────────────────────────── │
    │rs2_addr       ││ 00       │01       │00                            │
    │               ││──────────┴─────────┴───────────────────────────── │
    │               ││──────────┬─────────────────────────────────────── │
    │rt_addr        ││ 01       │00                                      │
    │               ││──────────┴─────────────────────────────────────── │
    │               ││──────────┬───────────────────┬─────────┬───────── │
    │rt_data        ││ AAAAAAAA │00000000           │DEADDEAD │00000000  │
    │               ││──────────┴───────────────────┴─────────┴───────── │
    │wr_enable      ││──────────┐                   ┌─────────┐          │
    │               ││          └───────────────────┘         └───────── │
    │               ││────────────────────┬─────────┬─────────────────── │
    │rs1            ││ 00000000           │AAAAAAAA │00000000            │
    │               ││────────────────────┴─────────┴─────────────────── │
    │               ││──────────┬─────────┬───────────────────────────── │
    │rs2            ││ 00000000 │AAAAAAAA │00000000                      │
    └───────────────┘└───────────────────────────────────────────────────┘
    e02207432043c165dbb25e2049cad2c0
    |}]
