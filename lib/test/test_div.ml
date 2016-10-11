(* 17 / 9 bit const divider: Approx 80 luts, spartan3 50Mhz, VII 80 MHz, V4 120 MHz *)
#light
open DigitalLogic
open Numeric.Ops
open Signal
open Simulator
open List
open Circuits.Divide

let test_div_const() =  
  let divisor = 320
  
  (* construct circuit. *)
  let max_dividend = 76799
  let q, m = divide_const (input "dividend" 17) max_dividend divisor
  let outputs = [ q -- "quotient"; m -- "remainder" ]
  let circuit = Circuit.create outputs

  (* simulation *)
  let sim = Simulator.create circuit
  let dd = sim.port "dividend"
  let qt = sim.port "quotient"
  let rm = sim.port "remainder"

  (* random number generation *)
  let rand = 
    let rand = new System.Random(1)
    fun () -> Int32.to_int (rand.Next(0, max_dividend))
  
  (* perform a single calculation *)
  let calc dividend = 
    dd.i <- dividend
    sim.cycle 
    assert (qt.i = (dividend / divisor))
    assert (rm.i = (dividend % divisor))
    
  sim.reset 
  calc 0
  calc 1
  calc 2
  calc 319
  calc 320
  calc 321
  calc 639
  calc 640
  calc 641
  calc 76797
  calc 76798
  calc 76799
  for i = 0 to 100000 do
    calc (rand())

  (* write hdl *)
  Circuit.write_file Verilog.write "output/" "div" ".v" circuit
  Circuit.write_file Vhdl.write "output/" "div" ".vhd" circuit

let test_div () = 

  let dividend_bits = 3
  let dividend = input "dividend" dividend_bits
  let divisor_bits = 3
  let divisor = input "divisor" divisor_bits

  let quotient, remainder = divide dividend divisor
  let quotient = quotient.output "quotient"
  let remainder = remainder.output "remainder"
  let circuit = Circuit.create [ quotient; remainder ]
  let sim = Simulator.create circuit
  
  sim.reset
  for i=0 to (1 <<< dividend_bits) - 1 do
    for j=1 to (1 <<< divisor_bits) - 1 do
      sim.[dividend].u <- i
      sim.[divisor].u <- j
      sim.cycle
      if sim.[quotient].u <> (i/j) || sim.[remainder].u <> (i%j) then
        printf "%i / %i = %i %i (%i %i)\n" i j sim.[quotient].u sim.[remainder].u (i/j) (i%j)
  
do test_div()
