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
open Circuits.Cordic
open List
open Printf

(****************************************************************************************************)
(* command line *)
(****************************************************************************************************)

exception Arg_error of string

type globs_t = {
    mutable name : string;      (* name of circuit *)
    mutable path : string;      (* output path  *)
    mutable mode : string;      (* cordic mode (iter/comb/seq) *)
    mutable alg : string;       (* Cordic algorithm (0/1/2/mul/div/atan/sincos/magphase/polar_to_rect/atanh/sinhcosh/log/sqrt/exp) *)
    mutable iters : int;        (* number of iterations *)
    mutable bits : int;         (* number of bits in args and result *)
    mutable fix : int;          (* number of bits to left of point in fixed point representation *)
    mutable vlog : bool;        (* generate verilog *)
    mutable vhdl : bool;        (* generate VHDL *)
    mutable fs : bool;          (* generate f# *)
    mutable rp   : bool;        (* resource report *)
    mutable pre : int;          (* pre-rotation mode: 0 - none; 1/2 - different rotation modes *)
}

let globs = {
    name = "";
    path = "";
    mode = "seq";
    alg = "1";
    iters = 10;
    bits = 32;
    fix = 8;
    vlog = false;
    vhdl = false;
    fs = false;
    rp = false;
    pre = -1;
}

let _ = Arg.parse [
    ("-n",     Arg.String (fun x -> globs.name <- x), "Circuit name");
    ("-w",     Arg.String (fun x -> globs.path <- x), "Output path");
    ("-m",     Arg.String (fun x -> globs.mode <- x), "Cordic mode (iter/comb/seq)");
    ("-a",     Arg.String (fun x -> globs.alg <- x), "Cordic algorithm (0/1/2/mul/div/atan/sincos/magphase/polar_to_rect/atanh/sinhcosh/log/sqrt/exp)");
    ("-i",     Arg.Int (fun x -> globs.iters <- x), "Number of iterations");
    ("-b",     Arg.Int (fun x -> globs.bits <- x), "Number of bits in datapath");
    ("-f",     Arg.Int (fun x -> globs.fix <- x), "Fixed point representation:  <f bits>.<(b-f) bits>");
    ("-vlog",  Arg.Unit (fun () -> globs.vlog <- true), "Generate verilog");
    ("-vhdl",  Arg.Unit (fun () -> globs.vhdl <- true), "Generate vhdl");
    ("-fs",    Arg.Unit (fun () -> globs.fs <- true), "Generate f#");
    ("-rp",   Arg.Unit  (fun () -> globs.rp <- true),   "Resource report");
    ("-p",     Arg.Int (fun x -> globs.pre <- x), "pre-rotation mode: default - none; 0/1 - different rotation modes");
] (fun a -> raise (Arg_error ("Unexpected argument"))) "Cordic circuit generator"

(****************************************************************************************************)
(* Circuit builder *)
(****************************************************************************************************)

let _ = 

    let bits = globs.bits in
    let fix = globs.fix in
    let iters = globs.iters in
    let scale = 2.0 ** (float (bits-fix)) in
    let fconst f = consti bits (int (scale * f)) in
    let gain1 = Cordic_ref.invGain1 iters in
    let gain2 = Cordic_ref.invGain2 iters in
    let pre = globs.pre in
    let circuit_name = (if globs.name = "" then "cordic_" ^ globs.mode ^ "_" ^ globs.alg else globs.name) in

    let clock = input "clock" 1 in
    let reset = input "reset" 1 in
    let ena = input "ena" 1 in
    let vld_in = input "vld_in" 1 in

    let x, y, z, vecmode, cdc_mode = 
        match globs.alg with
        | "0" ->             input "x" bits, input "y" bits, input "z" bits, input "vecmode" bits, Cordic0
        | "1" ->             input "x" bits, input "y" bits, input "z" bits, input "vecmode" bits, Cordic1
        | "2" ->             input "x" bits, input "y" bits, input "z" bits, input "vecmode" bits, Cordic2
        | "mul" ->           input "x" bits, fconst 0.0,     input "y" bits, fconst 0.0,           Cordic0
        | "div" ->           input "y" bits, input "x" bits, fconst 0.0,     fconst 0.0,           Cordic0
        | "atan" ->          fconst 1.0,     input "x" bits, fconst 0.0,     fconst 0.0,           Cordic1
        | "sincos" ->        fconst gain1,   fconst 0.0,     input "x" bits, fconst (-1.0),        Cordic1
        | "magphase" ->      input "x" bits, input "y" bits, fconst 0.0,     fconst 0.0,           Cordic1
        | "polar_to_rect" -> input "x" bits, fconst 0.0,     input "y" bits, fconst (-1.0),        Cordic1
        | "sinhcosh" ->      fconst gain2,   fconst 0.0,     input "x" bits, fconst (-1.0),        Cordic2
        | "atanh" ->         fconst 1.0,     input "x" bits, fconst 0.0,     fconst 0.0,           Cordic2
        | "exp" ->           fconst gain2,   fconst 0.0,     input "x" bits, fconst (-1.0),        Cordic2
        | "log" ->           let x = input "x" bits in (x + fconst 1.00), x - (fconst 1.00), fconst 0.0, fconst 0.0, Cordic2
        | "sqrt" ->          let x = input "x" bits in (x + fconst 0.25), x - (fconst 0.25), fconst 0.0, fconst 0.0, Cordic2
        | _ -> raise (Arg_error ("Unknown cordic algorithm " ^ globs.alg)) in

    let cdc_alg = match globs.mode with
        | "comb" -> CordicComb(cdc_mode)
        | "seq" -> CordicSeq(cdc_mode)
        | "iter" -> CordicIter(cdc_mode) 
        | _ -> raise (Arg_error ("Unknown mode " ^ globs.mode)) in

    let xo, yo, zo, vld_out = cordic pre cdc_alg fix iters clock reset ena vld_in x y z vecmode in 

    (* hardware *)
    let outputs = match globs.alg with
        | "0" ->             [ "xo",   xo;  "yo", yo;  "zo", zo; ]
        | "1" ->             [ "xo",   xo;  "yo", yo;  "zo", zo; ]
        | "2" ->             [ "xo",   xo;  "yo", yo;  "zo", zo; ]
        | "mul" ->           [ "mul",  yo; ]
        | "div" ->           [ "div",  yo; ]
        | "atan" ->          [ "atan", zo; ]
        | "sincos" ->        [ "sin",  yo;  "cos", xo; ]
        | "magphase" ->      [ "mag",  xo;  "phase", zo; ]
        | "polar_to_rect" -> [ "xo",   yo;  "yo", xo; ]
        | "sinhcosh" ->      [ "sinh", yo;  "cosh", xo; ]
        | "atanh" ->         [ "atan", zo ]
        | "exp" ->           [ "exp", (xo + yo) ]
        | "log" ->           [ "log", (zo <<< 1) ]
        | "sqrt" ->          [ "sqrt", xo ]
        | _ -> raise (Arg_error ("Unknown cordic algorithm " ^ globs.alg)) in
    let outputs = ("vld_out", vld_out) :: outputs in
    
    let outputs = map (fun (n,s) -> output n s) outputs in
    let circuit = Circuit.create outputs in

    try 
        if globs.vlog then write_file Verilog.write globs.path circuit_name ".v" circuit;
        if globs.vhdl then write_file Vhdl.write globs.path circuit_name ".vhd" circuit;
        if globs.fs then write_file Fsharp.write globs.path circuit_name ".fs" circuit;
        if globs.rp then Resources.report stdout circuit;
    with _ -> printf "Code generation failed\n" 

