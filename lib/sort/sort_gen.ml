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
open Util
open List
open Printf

exception Sort_error of string

type globs_t = {
    mutable name : string;      (* name of circuit *)
    mutable path : string;      (* output path *)
    mutable ctyp : string;      (* "bi" for bitonic, "oe" for odd even *)
    mutable bits : int;         (* number of bits for inputs *)
    mutable num  : int;         (* log base 2 number of inputs *)
    mutable down : bool;        (* Sort down instead of up *)
    mutable sign : bool;        (* Signed inputs *)
    mutable pipe : bool;        (* Insert pipeline registers *)
    mutable vlog : bool;        (* generate verilog *)
    mutable vhdl : bool;        (* generate VHDL *)
    mutable fs   : bool;        (* generate F# *)
    mutable rp   : bool;        (* generate report *)
}

let globs = {
    name   = "sort";
    path = "";
    ctyp   = "oe";
    bits   = 8;
    num    = 8;
    down   = false;
    sign   = false;
    pipe   = false;
    vlog   = false;
    vhdl   = false;
    fs     = false;
    rp     = false;
}

let _ = Arg.parse [
    ("-n",    Arg.String (fun  x -> globs.name <- x),     "Circuit name");
    ("-w",    Arg.String (fun x -> globs.path <- x),      "Output path");
    ("-t",    Arg.String (fun  x -> globs.ctyp <- x),     "\"bi\" for bitonic, \"oet\" for odd even transposition sort, \"oem\" for odd even merge sort");
    ("-b",    Arg.Int    (fun  x -> globs.bits <- x),     "Input bit width");
    ("-i",    Arg.Int    (fun  x -> globs.num  <- x),     "number of inputs (power of two for bitonic sort)");
    ("-d",    Arg.Unit   (fun () -> globs.down <- true),  "Sort down instead of up");
    ("-s",    Arg.Unit   (fun () -> globs.sign <- true),  "Signed inputs (default unsigned)");
    ("-p",    Arg.Unit   (fun () -> globs.pipe <- true),  "Insert pipeline registers");
    ("-vlog", Arg.Unit   (fun () -> globs.vlog <- true),  "Generate verilog");
    ("-vhdl", Arg.Unit   (fun () -> globs.vhdl <- true),  "Generate vhdl");
    ("-fs",   Arg.Unit   (fun () -> globs.fs <- true),    "Generate F#");
    ("-rp",   Arg.Unit   (fun () -> globs.rp <- true),    "Generate report");
] (fun _ -> failwith ("Invalid command line")) "Sorting network generator"

let _ = 
    
    let bits = globs.bits in
    let num_inputs = globs.num in
    let num_list = range num_inputs in
    let circuit_name = globs.name in

    (* build the sorting network *)
    let ena = input "ena" 1 in
    let vld_in = input "vld_in" 1 in
    let inputs = map (fun x -> input ("i" ^ (string x)) bits) num_list in

    let sort_fn = match globs.down, globs.sign with
        | false, false -> (>~)
        | true , false -> (<~)
        | false, true  -> (>+)
        | true , true  -> (<+) in

    let pipelined = if globs.pipe then (reg clock reset Signal.empty ena) else (fun x -> x) in

    let outputs,vld_out = 
      match globs.ctyp with
      | "bi" -> Circuits.Sort.bitonic sort_fn pipelined inputs vld_in 
      | "oet" -> Circuits.Sort.odd_even_tranposition sort_fn pipelined inputs vld_in 
      | "oem" -> Circuits.Sort.odd_even_merge sort_fn pipelined inputs vld_in 
      | _ -> failwith "Unknown sorting algorithm" in

    let outputs = map2 (fun x y -> ("o" ^ (string y)), x) outputs num_list in
    let outputs = ("vld_out", vld_out) :: outputs in
    let outputs = map (fun (n,s) -> output n s) outputs in

    let circuit = Circuit.create outputs in
    
    (* output circuit *)
    try 
        if globs.vlog then write_file Verilog.write globs.path circuit_name ".v" circuit;
        if globs.fs then write_file Fsharp.write globs.path circuit_name ".fs" circuit;
        if globs.vhdl then write_file Vhdl.write globs.path circuit_name ".vhd" circuit;
        if globs.rp then Resources.report stdout circuit;
    with _ -> printf "Code generation failed\n"
