#light 
open DigitalLogic
open Numeric.Ops
open Signal
open Util
open List
  
let fir_hw input coeffs = 
  let mul, _ = 
    fold_left (fun (taps,prev) cof -> 
      let tap = regc enable prev 
      taps @ [ regc enable (tap *+ cof) ], tap
    ) ([],input) coeffs
  binary_tree (fun d -> regc enable (se d)) (fun x y -> regc enable (x +:- y)) mul

let fir_sw coeffs inputs = 

  let fir_ref input taps coeffs = 
    let taps = lselect (input :: taps) 0 ((length coeffs) - 1)
    fold_left (fun acc (t,c) -> acc + (t * c)) 0 (combine taps coeffs), taps

  let rec calc i taps = 
    if i <> Array.length inputs then
      let a,taps = fir_ref inputs.(i) taps coeffs
      printf "%x\n" a
      calc (i+1) taps
    
  let taps = map (fun x -> 0) coeffs
  calc 0 taps

let fir_sim coeffs coeff_bits inputs input_bits = 
  
  let outputs = [ output "q" (fir_hw (input "d" input_bits) (map (consti coeff_bits) coeffs)) ]
  let circuit = Circuit.create outputs
  let sim = Simulator.create circuit
  let sim, data = sim.record
  
  let enable = sim.port "enable"
  let d = sim.port "d"
  let q = sim.port "q"

  sim.reset 
  enable.i <- 1
  for i=0 to Array.length inputs - 1 do
    d.i <- inputs.(i)
    sim.cycle 
  for i=0 to 4 do
    sim.cycle 
  
  Waveform.draw2 data Waveform.HexFormat
  
let coeffs = [ 1; 11; -20; 14; -5 ]
let coeff_bits = 6
let inputs = [| 10; -20; 30; 40 |]
let input_bits = 7
  
do fir_sw coeffs inputs
do fir_sim coeffs coeff_bits inputs input_bits
  
  