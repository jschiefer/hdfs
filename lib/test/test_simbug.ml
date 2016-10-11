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
  let bits = 8
  let seq = regc_fb enable ((+) (one bits)) bits
  let seq = regc enable (one bits)
  let comb = seq + (one bits)
  let comb = comb + (one bits)
    
  let circuit = Circuit.create [ output "seq" seq; output "comb" comb ] 

  let sim = create circuit

  let enable = sim.port "enable" 
  let seq = sim.port "seq" 
  let comb = sim.port "comb" 
  let sim, data = sim.record

  enable.i <- 1
  sim.cycle ;
  printf "%i %i\n" seq.i comb.i
  sim.cycle ;
  printf "%i %i\n" seq.i comb.i
  sim.cycle ;
  printf "%i %i\n" seq.i comb.i
  
  Waveform.draw2 data Waveform.IntFormat

[<System.STAThread>]    
do main()
