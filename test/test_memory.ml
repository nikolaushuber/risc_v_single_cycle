open Hardcaml
open Hardcaml_waveterm
open Risc_v

let create_sim () =
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface (Memory.I) (Memory.O) in
  let sim = Sim.create (Memory.create scope) in
  Waveform.create sim

type test_input = {
  wr_enable : bool;
  wr_data : int;
  wr_addr : int;
  rd_addr : int;
}

let step (sim : ('a Memory.I.t, 'a Memory.O.t) Cyclesim.t) test =
  let inputs = Cyclesim.inputs sim in
  inputs.wr_enable := if test.wr_enable then Bits.vdd else Bits.gnd;
  inputs.wr_data := Bits.of_int ~width:32 test.wr_data;
  inputs.wr_addr := Bits.of_int ~width:9 test.wr_addr;
  inputs.rd_addr := Bits.of_int ~width:9 test.rd_addr;
  Cyclesim.cycle sim

let mk_test wr_enable wr_data wr_addr rd_addr =
  { wr_enable; wr_data; wr_addr; rd_addr }

let test_inputs =
  [
    mk_test true 0xDEAD 0 0;
    mk_test true 0xBEAF 4 0;
    mk_test false 0 0 0;
    mk_test false 0 0 4;
  ]

let testbench () =
  let waves, sim = create_sim () in
  List.iter (step sim) test_inputs;
  waves

let%expect_test "memory test" =
  let waves = testbench () in
  Waveform.expect ~wave_width:4 ~display_width:55 ~display_height:18 waves;
  [%expect
    {|
    ┌Signals────┐┌Waves───────────────────────────────────┐
    │clock      ││┌────┐    ┌────┐    ┌────┐    ┌────┐    │
    │           ││     └────┘    └────┘    └────┘    └────│
    │           ││──────────────────────────────┬─────────│
    │rd_addr    ││ 000                          │004      │
    │           ││──────────────────────────────┴─────────│
    │           ││──────────┬─────────┬───────────────────│
    │wr_addr    ││ 000      │004      │000                │
    │           ││──────────┴─────────┴───────────────────│
    │           ││──────────┬─────────┬───────────────────│
    │wr_data    ││ 0000DEAD │0000BEAF │00000000           │
    │           ││──────────┴─────────┴───────────────────│
    │wr_enable  ││────────────────────┐                   │
    │           ││                    └───────────────────│
    │           ││──────────┬───────────────────┬─────────│
    │rd_data    ││ 00000000 │0000DEAD           │0000BEAF │
    │           ││──────────┴───────────────────┴─────────│
    └───────────┘└────────────────────────────────────────┘
    4dccd95ad7ec948c1b34413e45e30f88
    |}]
