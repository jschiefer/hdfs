open Hdcaml
open Hdlib
open Design
open Hdutil
open Hdfixed
open Hdfloat

type globs_t = {
  mutable name : string;      (* name of circuit *)
  mutable exp  : int;         (* exponent bits *)
  mutable mant : int;         (* mantissa bits *)
  mutable denorm : bool;      (* denormal support *)
  mutable add  : bool;        (* build multiplier *)
  mutable mul  : bool;        (* build adder *)
  mutable vlog : bool;        (* generate verilog *)
  mutable vhdl : bool;        (* generate VHDL *)
  mutable sysc : bool;        (* generate systemc *)
}

let globs = {
  name = "float_circuit";
  exp = 8;
  mant = 23;
  denorm = false;
  add  = false;
  mul  = false;
  vlog = false;
  vhdl = false;
  sysc = false;
}

let _ = Arg.parse [
  ("-n",     Arg.String (fun x -> globs.name <- x), "Circuit name");
  ("-e",     Arg.Int (fun x -> globs.exp <- x), "exponent bits");
  ("-m",     Arg.Int (fun x -> globs.mant <- x), "mantissa bits");
  ("-d",     Arg.Unit (fun x -> globs.denorm <- true), "Denormal value support");
  ("-add",   Arg.Unit (fun () -> globs.add <- true), "Generate adder");
  ("-mul",   Arg.Unit (fun () -> globs.mul <- true), "Generate multiplier");
  ("-vlog",  Arg.Unit (fun () -> globs.vlog <- true), "Generate verilog");
  ("-vhdl",  Arg.Unit (fun () -> globs.vhdl <- true), "Generate vhdl");
  ("-c",     Arg.Unit (fun () -> globs.sysc <- true), "Generate systemc");
] (fun a -> failwith "Unexpected argument") "Floating point circuit generator"

let _ = 

  start_circuit "float_circuit";
        
  let module Float = 
	Float (
    struct 
      let exponent = globs.exp
      let mantissa = globs.mant
      let denormals = globs.denorm
    end
	)
  in
    
  let ena = const "1" in
  
  let a = regf ena (inputf "a" Float.num_bits Float.man) in
  let b = regf ena (inputf "b" Float.num_bits Float.man) in    
    
  if globs.add then
  begin
    let add = Float.add a b in
    outputf "add" add
  end;
      
  if globs.mul then
  begin
    let mul = Float.mul a b in
    outputf "mul" mul
  end;
    
  try 
    let circuit = get_circuit () in
    if globs.vlog then Verilog.output_netlist circuit;
    if globs.vhdl then Vhdl.output_netlist circuit;
    if globs.sysc then Systemc.output_model circuit;
  with _ -> failwith "Code generation failed" 

