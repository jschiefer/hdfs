(*
  HDFS Digital Logic Hardware Design Utility Library (hdfslib.dll)
  Copyright (C) 2006 Andy Ray.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

(*
  Cosimulation with verilog.
  At the moment a PLI layer has been written for modelsim which works ok.
  It doesnt port to icarus verilog, unfortunately, which will require a VPI
  layer instead.
*)

open System
open System.Windows.Forms
open DigitalLogic
open Signal
open Simulator
open Cosimulation

let main() = 
  let tb_name = "cosim_tb" in
  let dut_name = "test_cosim" in
  let path = "output/" in

  (* create a circuit and write a verilog model *)
  let a = input "a" 4 in
  let b = input "b" 4 in
  let c = output "c" (regc enable (a + b)) in
  let d = output "d" (a - b) in
  let circuit = Circuit.create [ c; d ] in

  let sim = create circuit in    

  let gui = false in
  let cosim = Cosimulation.create_all_mti circuit clock reset "output/" "test_cosim" 
    ((if gui then "-gui" else "-c") ^ " -pli bin/cosim_mti.dll -quiet -do " ^ path ^ "sim.do") 
    [ "add wave -hex *"; "run 60"; "quit" ]in

  let a = sim.port "a" in
  let b = sim.port "b" in
  let enable = sim.port "enable" in
  let c = sim.port "c" in
  let d = sim.port "d" in
  
  let errors = ref 0 in
  let error_fn _ _ = errors := !errors + 1 in
  let sim = Simulator.combine sim cosim error_fn in
  let sim, data = sim.record in

  sim.reset;
  a.i <- 1;
  b.i <- 3;
  enable.i <- 1;
  sim.cycle;
  a.i <- 12;
  enable.i <- 0;
  sim.cycle;
  enable.i <- 1;
  sim.cycle;
  sim.cycle;
  sim.cycle;

  output_string stdout ("Detected " ^ string_of_int !errors ^ " errors during simulation\n");
  
  Waveform.draw2 data Waveform.IntFormat


[<STAThread>]    
do main()
