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
open Simulator

let main() = 
  let a0 = input "a0" 8
  let a1 = input "a1" 8
  let a2 = input "a2" 8
  let b = input "b" 3
  
  let circuit = 
    Circuit.create 
      [ 
        output "o0" (a0 >>: b);
        output "o1" (a1 >>+ b);
        output "o2" (a0 <<: b);
        output "o3" (barrel_shift_left a1 b);
        output "o4" (barrel_shift_right a2 b);
      ]
  let sim, data = (Simulator.create circuit).record in 
  
  let a0 = sim.port "a0"
  let a1 = sim.port "a1"
  let a2 = sim.port "a2"
  let b = sim.port "b"
  
  sim.reset
  a0.i <- 255;
  a1.i <- 136;
  a2.i <- 136;
  for i=0 to 7 do
    b.i <- i
    sim.cycle 
    
  Waveform.draw2 data Waveform.BinaryFormat

do main()

