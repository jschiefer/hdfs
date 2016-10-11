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

(** This is a testbench.  Needs to be turned into a generator. *)
open DigitalLogic
open DigitalLogic.Design
open DigitalLogic.Util
open DigitalLogic.Circuits.Fft
open Microsoft.FSharp.Math.Types
open List

let _ = 

  let inputs = [
    Complex.Create(-10.0,   0.0);
    Complex.Create(-20.0, -70.0);
    Complex.Create( 30.0,   0.0);
    Complex.Create(-20.0,  70.0);
  ] in
  
  let _ = iter (fun (x : Complex) -> printf "%.1f %.1f\n" x.RealPart x.ImaginaryPart) (fft_ref inputs (wn 4.0)) in

  let ibits = 16 in
  let wbits = 16 in
  let len = 4 in
  let inputs_hw = map (fun x -> 
      input ("ir" ^ (string_of_int x)) ibits, 
      input ("ii" ^ (string_of_int x)) ibits
  ) (range len) in

  let outputs = mapi (fun j (r,i) -> 
      output ("or" ^ (string_of_int j)) r,
      output ("oi" ^ (string_of_int j)) i
  ) (fft inputs_hw (wn (float_of_int len)) wbits) in
  let outr,outi = split outputs in

  let circuit = Circuit.create (outr @ outi) in
  write_file Verilog.write "output/fft_test" ".v" circuit;
  write_file Vhdl.write "output/fft_test" ".vhd" circuit;
  write_file Fsharp.write "output/fft_test" ".fs" circuit;
  