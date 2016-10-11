(* binary to one hot circuit, VHDL/Verilog generator and simulator *)

#light
open DigitalLogic
open Numeric.Ops
open Signal

let argv = 
#if INTERACTIVE
  fsi.CommandLineArgs
#else
  Sys.argv
#endif

let gen_onehot_to_binary core_type hdl onehot_bits =

  (* build circuit *)
  //let buffer = regc enable
  let buffer x = x
  let onehot = input "onehot" onehot_bits
  let onehot_to_binary = 
    match core_type with 
    | "behave"  -> Util.one_hot_to_binary_b
    | "ortree4" -> Util.one_hot_to_binary_a 4
    | "ortree6" -> Util.one_hot_to_binary_a 6
    | "orflat"  -> Util.one_hot_to_binary_a 1
    | "clz" -> (fun x -> Util.count_leading_zeros (x |> Signal.bits_lsb |> Signal.concat_msb))
    | _ -> failwith "Invalid core type"
  let write,ext = 
    match hdl with 
    | "vhdl" -> Vhdl.write, ".vhd"
    | "verilog" -> Verilog.write, ".v"
    | _ -> failwith "Invalid hdl type"
  let circuit = [ (buffer (onehot_to_binary (buffer onehot))).output "binary" ] |> Circuit.create 
  
  (* write hdl *)
  Circuit.write_file write "" ("onehot_to_binary_" ^ core_type ^ "_" ^ string_of_int onehot_bits) ext circuit
  
  (* simulate *)
  let sim,data = (Simulator.create circuit).record
  sim.reset
  try sim.[enable].u <- 1 with _ -> ()
  let binary n m = [ for i in 0 .. n-1 -> if (n-i-1)=m then "1" else "0" ] |> String.concat "" 
  for i=0 to onehot_bits-1 do
    sim.[onehot].b <- binary onehot_bits i
    sim.cycle
  Waveform.draw2 data Waveform.HexFormat

do gen_onehot_to_binary argv.(1) argv.(2) (int_of_string argv.(3))
