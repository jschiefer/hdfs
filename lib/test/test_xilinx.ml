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
open Circuits
open Xilinx

let d = input "d" 1

let q0 = output "q0" (g_fdc (Some(g_bit 0)) clock reset d)
let q1 = output "q1" (g_fdc (Some(g_bit 1)) clock reset d)
let q2 = output "q2" (g_fdc None clock reset d)
let q3 = output "q3" (l_fdc ["INIT", (g_bit 0)] clock reset d)
let q4 = output "q4" (l_fdc ["INIT", (g_bit 1)] clock reset d)
let q5 = output "q5" (l_fdc [] clock reset d)
let q6 = output "q6" (fdc clock reset d)

let circuit = Circuit.create [ q0; q1; q2; q3; q4; q5; q6  ]

Circuit.write_file Verilog.write "output/" "test_xilinx" ".v" circuit
Circuit.write_file Vhdl.write "output/" "test_xilinx" ".vhd" circuit

