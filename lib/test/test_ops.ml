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

open DigitalLogic
open Numeric.Ops
open Signal
open List
open Simulator

let test_ops() = 

  let a1 , b1  =    constb "0",                     // 0
                    constb "1" in                   // 1
  let a2 , b2  =    constb "01",                    // 1
                    constb "10" in                  // 2
  let a3 , b3  =    constb "001",                   // 1
                    constb "100" in                 // 4
  let a4 , b4  =    constb "0101",                  // 5
                    constb "1010" in                // 10 (-6)
  let a10, b10 =    constb "1011001001",            // 713
                    constb "1111100111" in          // 999
  let a16, b16 =    constb "1000010101110101",      // 34165
                    constb "1111101000100011" in    // 64035

  let asserter (str,a,s) = output str (a ==: s), output (str ^ "_value") s in
    
  let asserts = [
    (* bitwise operations *)
    "and1", constb "0", a1 &: b1;
    "and4", constb "0000", a4 &: b4;
    "and10", constb "1011000001", a10 &: b10;

    "or1", constb "1", a1 |: b1;
    "or4", constb "1111", a4 |: b4;
    "or10", constb "1111101111", a10 |: b10;

    "xor1", constb "1", a1 ^: b1;
    "xor4", constb "1111", a4 ^: b4;
    "xor10", constb "0100101110", a10 ^: b10;

    (* addition/subtraction *)
    "add1", constb "1", a1 + b1;
    "add4", constb "1111", a4 + b4;
    "add10", constb "1010110000", a10 + b10;

    "sub1", constb "1", a1 - b1;
    "sub4", constb "1011", a4 - b4;
    "sub10", constb "1011100010", a10 - b10;

    (* multiplication *)
    "mul1_0", constb "00", a1 * b1;
    "mul1_1", constb "01", b1 * b1;
    "mul4_0", constb "00110010", a4 * b4;
    "mul4_1", constb "11100010", a4 *+ b4;

    (* shifting *)
    "lsl10", constb "1001001000", a10 <<: 3;
    "lsr10", constb "0001011001", a10 >>: 3;
    "asr10", constb "1111011001", a10 >>+ 3;
    
    (* concatentation *)
    "cat4_4", constb "01011010", a4 ++ b4;
    "cat10_4", constb "10110010011010", a10 ++ b4;
    "cat1_16", constb "11000010101110101", b1 ++ a16; 
    "cat16_1", constb "10000101011101011", a16 ++ b1; 

    (* selection *)
    "sel1", constb "1", select b1 0 0;
    "sel16_0", constb "111010001000", select b16 13 2; 
    "sel16_1", constb "0001", select b16 4 1;

    (* relational *)
    "eq0", constb "0", a1;
    "eq1", constb "1", b1;
    "eq4", constb "0101", a4;
    "neq4", constb "1", b4 /=: a4;
    "lt4_0", constb "1", a4 <: b4;
    "lt4_1", constb "1", b4 >: a4;
    "lt4_2", constb "0", b4 <: a4;
    "lt4_3", constb "1", a4 <=: a4;
    "lt4_4", constb "1", a4 >=: a4;
    "lt4_5", constb "1", b4 <+ a4;
    "lt4_6", constb "1", a4 >=+ b4;
    "lt4_7", constb "1", a4 >=+ a4;
    
    (* muxes *)
    "mux0", constb "0101", mux (constb "00") [ a4;b4;a4 ];
    "mux1", constb "1010", mux (constb "01") [ a4;b4;a4 ];
    "mux2", constb "0101", mux (constb "10") [ a4;b4;a4 ];
    "mux3", constb "1010", mux (constb "11") [ a4;b4 ];

  ] in
  
  let asserts, values = split (map asserter asserts) in
  let outputs = asserts@values in
  
  let circuit = Circuit.create outputs in

  Resources.report stdout circuit;
  Circuit.write_file Verilog.write "output/" "test_ops" ".v" circuit;
  Circuit.write_file Fsharp.write "output/" "test_ops" ".fs" circuit; 
  Circuit.write_file Vhdl.write "output/" "test_ops" ".vhd" circuit;
  Circuit.write_file C.write_c "output/" "test_ops" ".c" circuit

(*  let sim = Simulator.create_int outputs in
  let outputs = map (fun (n,s) -> n, find_output_data sim n, find_output_data sim (n ^ "_value")) asserts in 
  let report (n,d,v) = 
    if !d <> 1 then 
      output_string stdout ("assert failed: " ^ n ^ " = " ^ string_of_int !v ^ "\n") in
  cycle sim;
  iter report outputs 
*)

let _ = test_ops()
