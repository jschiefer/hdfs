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
open Numeric.Ops
open Signal

let clk1 = input "clk1" 1 
let clk2 = input "clk2" 1 
let rst0 = input "rst0" 1 
let rst1 = input "rst1" 1 
let power_down = input "power_down" 1 
let clk1_gated = clk1 &: (~~~ power_down) 
let rst = rst0 |: rst1
let ena = input "ena" 1 
let d = input "d" 4 
let reg1 = reg clk1_gated rst (ones 4) ena d 
let reg2 = reg clk2 rst empty ena d 

let outputs = [ output "out1" reg1; output "out2" reg2 ] 

let circuit = Circuit.create outputs 

(* Gated clocks/resets are ok for netlists *)

Circuit.write_file Verilog.write "output/" "test_clks" ".v" circuit
Circuit.write_file Fsharp.write "output/" "test_clks" ".fs" circuit
Circuit.write_file Vhdl.write "output/" "test_clks" ".vhd" circuit

(* But not for simulation.  It wont fail (in some cases it makes sense 
   to continue) but the logic related to the clock and reset signals 
   will be ignored and a warning issued *)
let sim = Simulator.create circuit
sim.cycle;
sim.cycle;
