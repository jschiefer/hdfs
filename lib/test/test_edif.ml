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
open Circuits
open Xilinx

let a = input "a" 1 
let b = input "b" 1 
let c = g_lut2 (Some(g_vec "1000")) a b

let circuit = Circuit.create [ output "c" c ]

Circuit.write_file Verilog.write "output/" "test_edif" ".v" circuit
Circuit.write_file Vhdl.write "output/" "test_edif" ".vhd" circuit
Edif.write stdout "test_edif" circuit
