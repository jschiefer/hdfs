open DigitalLogic
open Design
open Simulator
open Circuits.Add

let adder x y = 
  let rec adder' x y carry_in = 
    match x,y with
    | [], [] -> []
    | x::xt, y::yt -> 
      let carry_out, sum = fa x y carry_in in
      let carry_out, sum = carry_out -- "carry_out", sum -- "sum" in
      sum :: adder' xt yt carry_out
    | _ -> failwith "args are not the same size"
  in
  concat_lsb (adder' (bits_lsb x) (bits_lsb y) gnd)

let main() =
  let bits = 8 in
  let circuit = Circuit.create [ output "c" (adder (input "a" bits) (input "b" bits)) ] in
  let sim = create_int circuit in
  let a = find_input_data sim "a" in
  let b = find_input_data sim "b" in
  let c = find_output_data sim "c" in

  sim_reset sim;
  let mutable errors = 0 in
  for i=0 to 100 do
      a := i;
      b := i*2;
      sim_cycle sim;
      if !c <> ((i + (i*2)) &&& 255) then (
        errors <- errors + 1;
        printf "cycle %i: expecting %i, got %i\n" i ((i + (i*2)) &&& 255) !c
      );
  done;
  printf "%i errors\n" errors

do main()
