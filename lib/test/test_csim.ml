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
open Circuit
open Signal
open List
open Simulator

let test_csim() = 

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
  let a32, b32 =    constb "10000000000000000000000000000000",      
                    constb "10000000000000000000000000000000" in    
  let a33, b33 =    constb "100000000000000000000000000000010",      
                    constb "100000000000000000000000000000011" in    
  let a64, b64 =    constb "0000000000000000000000000000000000000000000000000000000000000000",      
                    constb "0000000000000000000000000000000000000000000000000000000000000000" in    
                    
  let b_1 = b_regc enable 10 in
  let b_2 = b_wire0 20 in
  
  behave
    [
      b_if (a1) [
        b_1 $== consti 10 10;
      ] [];
      b_switch (a1) [
        b_case (a1) [
          b_2 $== consti 20 20;
          b_switch (a1) [
            b_case (a1) [
              b_2 $== consti 20 20;
            ];
            b_case (b1) [
              b_2 $== consti 20 30;
            ];
          ];
        ];
        b_case (b1) [
          b_2 $== consti 20 30;
          b_if (a1) [
            b_1 $== consti 10 10;
          ] [];
        ];
      ];
    ];

  let outputs = [ 
    output "o1" (~~~ a32);
    output "o2" (a33 <: b33); 
    output "o3" (a32 <: b32); 
    output "o4" (select a33 32 2);
    output "o5" (select a33 32 0);
    output "o6" (select a33 32 1);
    output "o7" (select a64 62 1);
    output "o8" (a64 ++ b64);
    output "o9" (a64 ++ b33);
    output "o10" (mux a3 [ a64; b64; a64; b64; a64; b64 ]);
    output "o11" (a64 * b64);
    output "o12" (a32 * b64);
    output "o13" (reg clock reset b33 enable a33);
    output "o14" (regc empty b2);
    output "o15" (memory 4 clock a1 a2 a33 b2);
    output "o16" (b_1.q);
    output "o17" (b_2.q);
    output "o18" (a33 * b33);
    output "o19" ((((input "i0" 2) *+ (input "i2" 2))) + (input "i3" 4) -- "hello");
  ] in
  
  let circuit = Circuit.create outputs in

  Resources.report stdout circuit;
  write_file Verilog.write "output/" "test_csim" ".v" circuit;
  write_file Fsharp.write "output/" "test_csim" ".fs" circuit; 
  write_file Vhdl.write "output/" "test_csim" ".vhd" circuit;
  (* c/c++ models *)
  write_file C.write_c "output/" "test_csim" ".c" circuit;
  write_file C.write_cpp "output/" "test_csim" ".cpp" circuit;
  (* compile the c# simulation dll (note: c# compiler only appears to work in the current directory) *)
  write_file C.write_cs "output/" "test_csim" ".cs" circuit;
  ()
  
let _ = test_csim()
