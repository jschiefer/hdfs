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

open System
open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D

open DigitalLogic
open Circuit
open Signal
open Numeric.Ops
open List
open Simulator

let main() = 

  (* build the circuit *)
  let ena = input "ena" 1 in
  let a = input "a" 4 in
  let b = input "b" 4 in  
  let counter_wire = wire 4 in
  let load = input "load" 1 in
  let init = input "init" 4 in
  let counter = reg clock reset empty ena (mux2 load init counter_wire) in
  let counter_plus1 = counter + (one 4) in
  counter_wire <== counter_plus1;
  let outputs = [ (output "counter" counter_plus1) ] in
  let circuit = Circuit.create outputs in
  
  (* create the simulator *)
  let sim = Simulator.create circuit in

  (* find inputs *)
  let load = sim.port "load" in
  let init = sim.port "init" in
  let ena = sim.port "ena" in

  (* create a waveform generator *)
  let sim, data = sim.record in

  (* simulate *)
  ena.i <- 1;
  init.i <- 15;
  load.i <- 0;
  sim.cycle;

  ena.i <- 0;
  load.i <- 0;
  sim.cycle;

  ena.i <- 1;
  load.i <- 0;
  sim.cycle;

  load.i <- 1;
  init.i <- 21;
  sim.cycle;

  load.i <- 0;
  sim.cycle;
  
  load.i <- 1;
  sim.cycle;

  load.i <- 0;
  sim.cycle;

  (* show data *)
  //Waveform.draw Simulator.simDataUInt32 { Waveform.wave_dark with font = new Font("", 10.0f, GraphicsUnit.Pixel); fontMargin=3; xSize=40 } (in_data @ out_data);
  //Waveform.draw_int { Waveform.wave_light with font = new Font("", 10.0f, GraphicsUnit.Pixel); fontMargin=3; xSize=40 } (in_data @ out_data) 
  Waveform.draw2 data Waveform.HexFormat
;;

[<STAThread>]    
do main()
