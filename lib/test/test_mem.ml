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

#light
open DigitalLogic
open Signal
open Simulator

let main() =
  let circuit = 
    let dbits = 32
    let abits = 9
    let r0 = input "r0" abits
    let w0 = input "w0" abits
    let d0 = input "d0" dbits
    let we0 = input "we0" 1 
    let re0 = input "re0" 1 
    //let q0 = memory (1 <<< abits) clock we0 w0 d0 r0
    let q0 = Circuits.Ram.ram_rbw (1 <<< abits) clock we0 w0 d0 re0 r0
    
    Circuit.create [ output "q0" q0; ] 

  Circuit.write_file Verilog.write "output/" "test_mem" ".v" circuit
  Circuit.write_file Vhdl.write "output/" "test_mem" ".vhd" circuit
  Circuit.write_file Fsharp.write "output/" "test_mem" ".fs" circuit
  Resources.report stdout circuit

  let sim = create circuit

  let r0 = sim.port "r0" 
  let w0 = sim.port "w0" 
  let d0 = sim.port "d0" 
  let we0 = sim.port "we0" 
  let re0 = sim.port "re0" 
  let sim, data = sim.record

  sim.cycle;
  
  re0.i <- 1;
  w0.i <- 3;
  r0.i <- 3;
  we0.i <- 1;
  d0.i <- 2;
  sim.cycle;
  
  d0.i <- 4;
  sim.cycle;
  
  r0.i <- 2;
  w0.i <- 4;
  d0.i <- 6;
  sim.cycle;
  
  r0.i <- 4;
  we0.i <- 0;
  sim.cycle;
  
  sim.cycle;
  
  sim.cycle;

  Waveform.draw2 data Waveform.IntFormat

[<System.STAThread>]    
do main()
