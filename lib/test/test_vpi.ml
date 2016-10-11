#light
open DigitalLogic
open Numeric.Ops
open Signal

let test() = 
  printf "Creating circuit...\n"
  
  let a = input "a" 8
  let b = input "b" 8
  let c = a + b
  let c = (regc enable c).output "c"

  printf "Creating cosim...\n"

  let circuit = Circuit.create [ c ] 
  let sim = Cosimulation.create_all_vpi circuit clock reset "vvp" "output/" "vpitest" "-M bin -m cosim_vpi" []
  let sim,data = sim.record

  printf "Running cosim...\n"

  sim.reset
  sim.[enable].u <- 1
  sim.[a].u <- 1
  sim.[b].u <- 1
  sim.cycle
  sim.cycle
  sim.cycle
  
  printf "Wheres my waveform???\n"

  Waveform.draw2 data Waveform.IntFormat

  printf "Been and gone???\n"

do test()

