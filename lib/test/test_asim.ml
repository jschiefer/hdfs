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

open System
open System.Windows.Forms

open DigitalLogic
open Circuit
open Numeric
open Numeric.Ops
open ArrayBits
open Signal
open Cosimulation
open Simulator

let main() = 
  let os = output_string stdout in
  let path = "output/" in
  let tb_name = "testbench" in
  let dut_name = "test_sim_array" in

  let numTests, numCycles, maxInput = 30, 1000, 100 in (* long test *)
  //let numTests, numCycles, maxInput = 10, 10, 10 in (* short test *)

  (* create an array of random 32 bit numbers *)
  let rand = new Random() in
  let rand_array (a:ArrayBits) nBits = 
    let nWords = ((nBits+31)/32) in
    let rand_bytes = Bytearray.zero_create (nWords*4) in
    let r = rand.NextBytes(rand_bytes) in
    let b2i b = Byte.to_uint32 b in
    for i=0 to nWords-1 do
      let v = ((b2i rand_bytes.[(i*4)+0]) <<<  0) ||| ((b2i rand_bytes.[(i*4)+1]) <<<  8) |||
              ((b2i rand_bytes.[(i*4)+2]) <<< 16) ||| ((b2i rand_bytes.[(i*4)+3]) <<< 24) in
      a.data.(i) <- v;
    done;
    (ArrayBits.f_mask a)();
  in
  let print_array a =
    for i=(Array.length a) - 1 downto 0 do
      Printf.printf "%.8Ux" a.(i)
    done;
    Printf.printf "\n"
  in
  let rand_uint ()= 
    let a = ArrayBits.make 32 in
    rand_array a 32;
    a.data.(0)
  in
  let rand range = 
    let r = rand_uint() mod (UInt32.of_int range) in
    UInt32.to_int r
  in
      
  let inputs0 = List.map (fun i -> input ("i" ^ string_of_int (i+1)) (i+1)) (Util.range maxInput) in
  let inputs1 = List.mapi (fun i x -> input ("i" ^ string_of_int (i+1+maxInput)) (width x)) inputs0 in
  let inputs = inputs0 @ inputs1 in

  (* operators with different sized arguments *)
  let ops = [ ( * ); ( *+ ); (++) ] in
  let outputs0 = 
    List.map (fun op -> 
      List.map (fun i -> 
        let i0 = (List.nth inputs (rand (maxInput*2))) in
        let i1 = (List.nth inputs (rand (maxInput*2))) in
        (op i0 i1)
      ) (Util.range numTests) 
    ) ops in
  //let outputs0 = [] in

  (* operators which take same size args *)
  let ops = [ 
    (+); (-); 
    (&:); (|:); (^:); 
    (==:); (/=:); 
    (<:); (<=:); (>:); (>=:); 
    (<+); (<=+); (>+); (>=+) 
  ] in
  let outputs1 = 
    List.map (fun op -> 
      List.map (fun i -> 
        let num = rand maxInput in
        (op (List.nth inputs0 num) (List.nth inputs1 num))
      ) (Util.range numTests) 
    ) ops in
  //let outputs1 = [] in

  (* shift operators (essentially a limited select) *)
  let ops = [ (<<:); (>>:); (>>+) ] in
  let outputs2 = 
    List.map (fun op -> 
      List.map (fun i -> 
        let x = (List.nth inputs (rand (maxInput*2))) in
        (op x (rand (width x)))) (Util.range numTests) 
    ) ops in
  //let outputs2 = [] in
  
  (* select *)
  let outputs3 = 
    let x = List.nth inputs (rand (maxInput*2)) in
    let w = width x in
    let lo = rand w in
    let left = w - lo in
    let hi = lo + (rand left) in
    [ List.map (fun i -> (select x hi lo)) (Util.range numTests) ] in
  //let outputs3 = [] in
  
  let outputs = List.mapi (fun i x -> output ("o" ^ (string_of_int i)) x) (List.concat (outputs0 @ outputs1 @ outputs2 @ outputs3)) in
  let circuit = Circuit.create outputs in

  (* write the testbench, compile it and connect to the simulator *)  
  let mode = false in
  let c_errors = ref 0 in
  let f_errors = ref 0 in
  
  (* f#, c# and verilog simulation *)
  let sim_v = Cosimulation.create_all_mti circuit clock reset "output/" dut_name ("-pli bin/cosim_mti.dll -c -do output/sim.do +debug=0 +stop=1") [ "run 20000" ] in
  let sim_f = Simulator.create circuit in
  //let sim_c = C.create_all_csim circuit "" dut_name in
  
  let f_errors = ref 0 in
  let c_errors = ref 0 in
  let error_fn r (s0:Port) (s1:Port) = 
    printf "Expecting %s %i got %s %i\n" s0.name s0.width s1.name s1.width;
    r := !r + 1 in
  
  //let sim = Simulator.combine (Simulator.combine sim_v sim_f (error_fn f_errors)) sim_c (error_fn c_errors) in 
  let sim = Simulator.combine sim_v sim_f (error_fn f_errors) in 

  let set_inputs () = 
    List.iter (fun (p:Port) ->
      if p.name = "enable" then p.ul <- 1ul
      else if p.name <> "clock" && p.name <> "reset" then
        rand_array p.port_data p.width
    ) (sim.sim_inputs)
  in
  
  sim.reset;
  for i=0 to numCycles-1 do
    set_inputs();
    sim.cycle;
  done;

  output_string stdout ("Detected " ^ string_of_int !f_errors ^ " f# errors during simulation\n");
  output_string stdout ("Detected " ^ string_of_int !c_errors ^ " c# errors during simulation\n")


let debug () = 
  let os = output_string stdout in
  let path = "output/" in
  let tb_name = "testbench" in
  let dut_name = "test_sim_array" in
  //let numTests, numCycles, maxInput = 10, 100, 100 in (* short test *)
  let numCycles = 10 in
  let errors = ref 0 in
  
  // Perform the shift
  let hi, lo = 81, 49 in
  let bits = hi - lo + 1 in
  let a = Array.create ((hi + 32) / 32) 0 in
  let r = Array.create ((hi - lo + 32) / 32) 0 in
  let lsw = lo / 32 in
  let msw = hi / 32 in
  let lsb = lo % 32 in
  let msb = hi % 32 in
  let words = (bits+31)/32 in

  printf "array: a = %i, r = %i\n" (Array.length a) (Array.length r);
  printf "range: [%i:%i] = %i bits\n" hi lo bits;
  printf "words: msw = %i, lsw = %i, tot = %i\n" msw lsw words;
  printf "bits : %i / %i\n" msb lsb;
  
  let shift_right = 32 - lsb in
  let shift_left = 32 - shift_right in
  for i=0 to words-2 do  
    printf "%i = %i bits from %i and %i bits from %i\n" i (32-shift_right) (lsw+i) (32-shift_left) (lsw+i+1);
    r.(i) <- (a.(lsw + i) lsr shift_right) lor (a.(lsw + i + 1) lsl shift_left)
  done;
  printf "lsw + msw = %i\n" (lsw + msw);
  printf "lsw + msw + 1 = %i\n" (lsw + msw + 1);
  printf "%i = %i bits from %i and %i bits from %i\n" (lsw+msw-1) (32-shift_right) (lsw+msw) (32-shift_left) (lsw+msw);
  if (lsw + words) >= (Array.length a) then (* if number of bits left (32*(words-2) <= (32-shift_right) *)
    r.(words-1) <- (a.(lsw + words - 1) lsr shift_right)
  else
    r.(words-1) <- ((a.(lsw + words - 1) lsr shift_right) lor (a.(lsw + words) lsl shift_left));

(*
  let a = input "a" 34 in
  let b = a >>: 17 in
  let circuit = Circuit.create [ output "b" b ] in
  
  let sim_v = Cosimulation.create_all_mti_array circuit clock reset "output/" dut_name ("-pli bin/cosim_mti.dll -c -do sim.do +debug=0 +stop=1") in
  let sim_f = Simulator.create_array circuit in
  //let sim_c = C.create_all_csim_array circuit "" dut_name in
  let sim = Simulator.create_dual_sim_array sim_f sim_v errors in

  let sim, SimDataGen(in_data, wire_data, out_data) = 
    Simulator.generator_array sim numCycles in

  Simulator.sim_reset sim;
  for i=0 to numCycles-1 do
    //set_inputs();
    Simulator.sim_cycle sim; 
  done;
  output_string stdout ("Detected " ^ string_of_int !errors ^ " errors during simulation\n");
  Waveform.draw_bin simDataArray Waveform.wave_dark (in_data @ wire_data @ out_data)  
*)()

[<STAThread>]
//do debug()
do main()
