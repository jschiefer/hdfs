#light
open DigitalLogic
open Circuit
open Numeric.Ops
open Signal
open Simulator
open List
open Circuits.Multiply

let test() = 
  let a = input "a" 16
  let b = input "b" 16
  
  //let c = wallace a b
  let c = dadda a b
  let outputs = [ c -- "c" ]
  let circuit = Circuit.create outputs
  write_file Verilog.write "output/" "mul" ".v" circuit
  let sim = Simulator.create circuit
  let a = sim.port "a"
  let b = sim.port "b"
  let c = sim.port "c"
  
  sim.reset
  for i=0 to 15 do
    for j=0 to 15 do
      a.i <- i;
      b.i <- j;
      sim.cycle 
      assert (c.i = (i*j))
      printf "%i * %i = %i\n" a.i b.i c.i

  Resources.report stdout circuit

do test()
