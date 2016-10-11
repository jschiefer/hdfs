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
open Signal
open Circuit
open Util
open Circuits.Rac
open List
open Printf

(****************************************************************************************************)
(* test implementations *)
(****************************************************************************************************)

(* in brief this is what the rac does *)
let eval_rac inputs coeffs = fold_left (+) 0 (map2 ( * ) inputs coeffs)

(* iterative rac for unsigned values *)
let eval_rac_iter_unsigned inputs input_bits coeffs coeff_bits = 
    let rom = build_rom coeffs in
    let rec eval idx acc =
        if idx = input_bits then acc
        else
            let bits = map (fun x -> (x >>> idx) &&& 1) inputs in
            let addr = fold_right (fun x a -> (a <<< 1) ||| x) bits 0 in
            let rom = (nth rom addr) * (pow2 input_bits) in
            let accn = rom + acc in
            printf "addr: %i acc: %i rom: %i accn: %i %i\n" addr acc rom accn (clog2 accn);
            eval (idx+1) (accn >>> 1) in
    eval 0 0

(* iterative rac for signed values *)
let eval_rac_iter_signed inputs input_bits coeffs coeff_bits = 
    let rom = build_rom coeffs in
    let rec eval idx acc =
        if idx = input_bits then acc
        else
            let bits = map (fun x -> (x >>> idx) &&& 1) inputs in
            let addr = fold_right (fun x a -> (a <<< 1) ||| x) bits 0 in
            let rom = (nth rom addr) * (pow2 input_bits) in
            let accn = if idx = (input_bits-1) then acc - rom else acc + rom in
            printf "addr: %i acc: %i rom: %i accn: %i %i\n" addr acc rom accn (clog2 accn);
            eval (idx+1) (accn >>> 1) in
    eval 0 0

(****************************************************************************************************)
(****************************************************************************************************)

type globs_t = {
    mutable name : string;      (* name of circuit *)
    mutable path : string;      (* output path *)
    mutable bits : int;         (* number of bits for inputs *)
    mutable uns  : bool;        (* build unsigned rac (inputs are treated as unsigned) *)
    mutable verb : bool;        (* verbose *)
    mutable vlog : bool;        (* generate verilog *)
    mutable vhdl : bool;        (* generate VHDL *)
    mutable fs   : bool;        (* generate f# *)
    mutable rp   : bool;        (* resource report *)
    mutable coeffs : int list;  (* coefficients (default arguments) *)
}

let globs = {
    name   = "rom_accumulator";
    path = "";
    bits   = 8;
    uns    = false;
    verb   = false;
    vlog   = false;
    vhdl   = false;
    fs     = false;
    rp     = false;
    coeffs = []
}

let _ = Arg.parse [
    ("-n",    Arg.String (fun x -> globs.name <- x),     "Circuit name");
    ("-w",    Arg.String (fun x -> globs.path <- x),     "Output path");
    ("-b",    Arg.Int    (fun x -> globs.bits <- x),     "Input bit width");
    ("-u",    Arg.Unit   (fun () -> globs.uns <- true),  "Inputs are unsigned");
    ("-v",    Arg.Unit   (fun () -> globs.verb <- true), "Verbose");
    ("-vlog", Arg.Unit   (fun () -> globs.vlog <- true), "Generate verilog");
    ("-vhdl", Arg.Unit   (fun () -> globs.vhdl <- true), "Generate vhdl");
    ("-fs",   Arg.Unit  (fun () -> globs.fs <- true),   "Generate f#");
    ("-rp",   Arg.Unit  (fun () -> globs.rp <- true),   "Resource report");
    ("--",    Arg.Rest (fun x  -> globs.coeffs <- (int x) :: globs.coeffs), "Coefficients (must follow all other arguments)");
] (fun _ -> raise (Rac_error ("Invalid command line"))) "Rom-accumulator circuit generator:\n\n> rac_gen -vhdl -- 0 2 -7 3\n"

let _ = 
    let print_coefficients s c = 
        printf "%s" s;
        iter (fun x -> printf "%i " x) c;
        printf "\n" in

    let input_bits = globs.bits in
    let coeffs = rev globs.coeffs in
    let unsigned = globs.uns in
    let num_inputs = length globs.coeffs in
    if num_inputs = 0 then raise (Rac_error ("No coefficients specified"));
    if unsigned then iter (fun x -> if x < 0 then raise (Rac_error ("Signed coeff found while working in unsigned mode"))) coeffs;
    let rom = build_rom coeffs in
    let max_coeff = fold_left (fun a x -> if a > x then a else x) (hd rom) rom in
    let min_coeff = fold_left (fun a x -> if a < x then a else x) (hd rom) rom in
    let coeff_bits = num_bits_range min_coeff max_coeff in
    (* in signed mode need to ensure sign bit is included even if all coeffs are positive *)
    let coeff_bits = 
        if globs.uns = false && min_coeff >= 0 then 
            coeff_bits+1 
        else coeff_bits in 

    let input_max = (pow2 (if unsigned then input_bits else (input_bits-1))) in

    let _ = 
        if globs.verb then
        (
            printf "Building %i input rac.\n" num_inputs;
            print_coefficients "Input coeffs: " coeffs;
            print_coefficients "Rom coeffs  : " rom;
            printf "Rom range   : %i -> %i\n" min_coeff max_coeff;
            printf "Rom bits    : %i\n" coeff_bits;
            printf "Max result  : %i\n" (eval_rac (map (fun x -> if x < 0 then -input_max else input_max) coeffs) coeffs)
        ) in

    let inputs = map (fun x -> input ("i" ^ (string x)) input_bits) (range num_inputs) in
    let acc,rdy = rac (not globs.uns) (input "clock" 1) (input "reset" 1) (input "ena" 1) (input "load" 1) inputs coeffs coeff_bits in
    
    let outputs = [ ("result",acc); ("ready", rdy) ] in
    let outputs = map (fun (n,s) -> output n s) outputs in
    let circuit = Circuit.create outputs in
    
    try 
        if globs.vlog then write_file Verilog.write globs.path globs.name ".v" circuit;
        if globs.vhdl then write_file Vhdl.write globs.path globs.name ".vhd" circuit;
        if globs.fs then write_file Fsharp.write globs.path globs.name ".fs" circuit;
        if globs.rp then Resources.report stdout circuit;
    with _ -> printf "Code generation failed\n"

