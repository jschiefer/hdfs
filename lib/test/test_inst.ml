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
open System.Globalization
open System.Text
open System.Threading
open System.Reflection
open System.Reflection.Emit
open System.IO

open DigitalLogic
open Numeric.Ops
open Circuit
open Signal
open Elaborate
open Util
open List

let _ = 
  let a = input "a_i" 4  in
  let b = input "b_i" 4  in
  let c = input "c_i" 4  in
  let d = input "d_i" 4  in
  let q0 = (wire 4) -- "q0" in
  let q1 = (wire 4) -- "q1" in
  let q2 = (wire 4) -- "q2" in
                        
  let simple2 ()= 
    let a = input "a_c" 4 in
    let b = input "b_c" 4 in
    [ (output "q_o" (a + b)) ]
  in

  inst "simple" 
    [ "a_c" ==> a; "b_c" ==> b ] 
    [ "q_o" ==> q0 ];
  inst "simple2" 
    [ "a_c" ==> b; "b_c" ==> d ] 
    [ "q_o" ==> q1 ];
  inst "simple3" 
    [ "a_c" ==> c; "b_c" ==> d ] 
    [ "q_o" ==> q2 ];

  let outputs = [ output "o_q0" q0; output "o_q1" q1; output "o_q2" q2 ] in

  (* simple2 is provided as a circuit and (if it's around) simple is taken from simple.dll *)
  elaborate outputs [ ("simple2",simple2()) ] [ "./" ];
//  elaborate outputs [] [ "" ];

  let circuit = Circuit.create outputs in
  
  write_file Fsharp.write "output/" "test_inst" ".fs" circuit;
  write_file Verilog.write "output/" "test_inst" ".v" circuit;
  write_file Vhdl.write "output/" "test_inst" ".vhd" circuit;
  Resources.report stdout circuit
