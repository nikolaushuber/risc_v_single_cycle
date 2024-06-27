open Hardcaml
open Hardcaml_waveterm
open Risc_v

let create_sim () =
  let scope = Scope.create ~flatten_design:true () in
  let module Sim = Cyclesim.With_interface (Memory.I) (Memory.O) in
  let sim = Sim.create (Memory.create scope) in
  Waveform.create sim

type test_input = { enable : bool; data : int; addr : int }

let step (sim : ('a Memory.I.t, 'a Memory.O.t) Cyclesim.t) test =
  let inputs = Cyclesim.inputs sim in
  inputs.enable := if test.enable then Bits.vdd else Bits.gnd;
  inputs.wr_data := Bits.of_int ~width:32 test.data;
  inputs.addr := Bits.of_int ~width:9 test.addr;
  Cyclesim.cycle sim

let mk_test enable data addr = { enable; data; addr }

let test_inputs =
  [
    mk_test true 0xDEAD 0;
    mk_test true 0xBEAF 4;
    mk_test false 0 0;
    mk_test false 0 4;
  ]

let testbench () =
  let waves, sim = create_sim () in
  List.iter (step sim) test_inputs;
  waves

let%expect_test "memory test" =
  let waves = testbench () in
  Waveform.expect ~wave_width:4 ~display_width:55 ~display_height:15 waves;
  [%expect
    {|
    ┌Signals────┐┌Waves───────────────────────────────────┐
    │clock      ││┌────┐    ┌────┐    ┌────┐    ┌────┐    │
    │           ││     └────┘    └────┘    └────┘    └────│
    │enable     ││────────────────────┐                   │
    │           ││                    └───────────────────│
    │           ││──────────┬─────────┬─────────┬─────────│
    │addr       ││ 000      │004      │000      │004      │
    │           ││──────────┴─────────┴─────────┴─────────│
    │           ││──────────┬─────────┬───────────────────│
    │wr_data    ││ 0000DEAD │0000BEAF │00000000           │
    │           ││──────────┴─────────┴───────────────────│
    │           ││────────────────────┬─────────┬─────────│
    │rd_data    ││ 00000000           │0000DEAD │0000BEAF │
    │           ││────────────────────┴─────────┴─────────│
    └───────────┘└────────────────────────────────────────┘
    bdbececb3fff500e80c7b2a7d46556b8
    |}]
