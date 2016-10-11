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
open Numeric.Ops
open Circuit
open Signal
open List

let form = 

  let ena = input "ena" 1 in
  let a = input "a" 4 in
  let b = input "b" 4 in

  let ops = map (fun op -> op a b) [ (+); (-); ( * ); ( *+ ); (&:); (|:); (^:) ] in
  let ops = (map (fun op -> op a 2) [ (<<:); (>>:); (>>+); ]) @ ops in
  let ops = (~~~ a) :: ops in
  let ops = (a ++ b) :: ops in
  let ops = (select a 1 0) :: ops in
  let ops = (mux2 (select a 3 3) a b) :: ops in
  let ops = (mux (select a 1 0) [ a; b; a ]) :: ops in
  let ops = (mux (select a 1 0) [ bit a 0; bit a 1; bit a 2; bit a 3 ]) :: ops in

  let ops = (reg clock reset (ones (width a)) ena a) :: ops in
  let ops = (reg clock reset empty ena (a++b)) :: ops in
  let ops = (reg clock empty empty ena a) :: ops in
  
  let ops = (constb "0101") :: ops in
  
  let ia = input "ia" 1 in
  let wa = wire 1 -- "wa" in
  let wb = wire 1 in
  let oa = reg clock empty empty empty wa in
  let ob = mux2 ena ia oa in
  let oc = reg clock reset empty (mux2 (bit b 0) (constb "0") ena) wb in
  let od = mux2 ena oc wa in
  wa <== ob;
  wb <== oc;

  let outputs = mapi (fun i s -> output ("o" ^ string_of_int i) s) (od :: ops) in

  let circuit = Circuit.create outputs in
  (write_file Vhdl.write "output/" "test_general" ".vhd" circuit);
  (write_file Verilog.write "output/" "test_general" ".v" circuit);
  (Resources.report stdout circuit);  
  (write_file Fsharp.write "output/" "test_general" ".fs" circuit); 

