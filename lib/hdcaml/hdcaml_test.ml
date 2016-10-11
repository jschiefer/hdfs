open Hdcaml
open Design

let test() = 

  let counter width enable =
    (* Because the counter has a feedback loop, we need to declare a signal to be assigned later. *)
    let next_count = signal "next_count" width in

    (* Instantiate a register, passing in the enable and next_count signals, and labeling the output. *)
    let count = "count" -- reg enable next_count in

    (* Assign next_count equal to count + 1. *)
    next_count <== count +: one width;

    (* Return the count signal. *)
    count
  in
  
  let counter_design width =
    (* Initialize the design database. *)
    start_circuit "counter_example";

    (* Define the top-level inputs. *)
    let enable = input "enable" 1 in

    (* Start a sub-circuit. *)
    circuit "counter";

    (* Instantiate the counter.  Label the enable inside the sub-circuit to aid in debugging. *)
    let count = counter width ("enable" -- enable) in

    (* Close the sub-circuit. *)
    endcircuit ();

    (* Define the top-level outputs. *)
    output "count" count;

    (* Fetch the circuit database. *)
    let circuit = get_circuit () in

    (* Generate a Verilog netlist. *)
    Verilog.output_netlist circuit;

    (* Generate a SystemC model. *)
    Systemc.output_model circuit;

    (* Open a VCD channel and simulate the counter. *)
    let vcd = open_out "counter_example.vcd" in
    let sim, inputs, outputs = Simulate.create circuit vcd in

    (* Fetch the enable input and set it high. *)
    let enable = List.assoc "enable" inputs in
    enable.(0) <- 1;

    (* Run a few cycles. *)
    Simulate.cycle sim;
    Simulate.cycle sim;
    Simulate.cycle sim;
    Simulate.cycle sim;

    (* Disable the counter. *)
    enable.(0) <- 0;
    Simulate.cycle sim;
    Simulate.cycle sim;

    (* Re-enable the counter. *)
    enable.(0) <- 1;
    Simulate.cycle sim;
    Simulate.cycle sim;

    (* Reset the circuit. *)
    Simulate.reset sim;
    Simulate.cycle sim;
    Simulate.cycle sim;

    (* Close the VCD channel. *)
    close_out vcd
  in
  
  (** Parametric FIR Filter:
    - any input precision
    - any coefficient precision
    - any output precision
    - any number of filter stages
    - any multiplier implementation
  *)
  let fir_filter output_width enable input coefficients ( *+ ) =
    let filter_stage accumulation coefficient =
      reg enable (accumulation +: select (input *+ coefficient) (output_width - 1) 0)
    in
    List.fold_left filter_stage (zero output_width) coefficients
  in
  
  (** Defines top level ports, instantiates fir_filter, and generates C and Verilog. *)
  let fir_filter_design input_width coeff_width output_width =
    start_circuit "fir_filter";
    let enable = input "enable" 1 in
    let filter_input = input "filter_input" input_width in
    let coefficients = [input "coeff_a" coeff_width; input "coeff_b" coeff_width; input "coeff_c" coeff_width] in
    output "filter_output" (fir_filter output_width enable filter_input coefficients ( *+ ));
    let circuit = get_circuit () in
    Verilog.output_netlist circuit;
    Systemc.output_model   circuit
  in

  let firstprogram width =
    start_circuit "firstprogram";
    let input1, input2, input3 =
      input "input1" width,
      input "input2" width,
      input "input3" width
    in
      let operate i1 i2 i3 =  
        (i1 +: i2) *: i3
      in
        output "result" (operate input1 input2 input3);
        let myfirstcircuit = get_circuit() in (* Closes the circuit and creates a circuit called "myfirstcircuit" *)
        Verilog.output_netlist myfirstcircuit (* Calls the Verilog generator with "myfirstcircuit" *)
  in

  let comparator length =
    start_circuit "comparator";
    (* Input definition *)
    let i1, i2 = (input "i1" length), (input "i2" length) in
    (* Comparison function, used recursively for signals wider than 1 bit *)
      let rec greater s1 s2 =
        if (width s1) == (width s2) then
          if
            (width s1) == 1
          then
            s1 &: (~: s2)      (* meaning s1 > s2 *)
          else 
            ((msb s1) &: (~: (msb s2))) |: ((~:((msb s1) ^: (msb s2))) &: (greater (lsbs s1) (lsbs s2)))
            (* meaning msb(s1) > msb(s2) or msb(s1) = msb(s2) and lsbs(s1) > lsbs (s2) *)
         else 
           failwith "Signals don't have same width"
         in
         (* Output definition *)
         output "GREATER" (greater i1 i2);
        let
          circuit = get_circuit ()
        in
          Hdcaml.Verilog.output_netlist circuit
  in

  let log_shifter control data =
    
    let rec shifter control data shift =
      let stage = mux2 (msb control) (data <<: shift) data in
      if width control = 1 
        then stage
        else shifter (lsbs control) stage (shift lsr 1)
    in
    
    if (width control) = 0 then raise (Invalid_argument "control");
    if (width data   ) = 0 then raise (Invalid_argument "data");

    shifter control data (1 lsl (width control - 1))
  in

  let log_shifter_design shift_width data_width =
    start_circuit "log_shifter";
    let
      control, data
    = 
      input "control" shift_width,
      input "data" data_width
    in  
      output "result" (log_shifter control data);   
      let
        circuit = get_circuit()
      in
        Verilog.output_netlist circuit
  in

  (* Circuit to calculate the parity bit of the signal *)
  let rec parity_calc data =
    match (width data) with
        1 -> ~: data
    | n -> (msb data) ^: parity_calc(lsbs data)
  in

  let shifter length =
    start_circuit "pts";
    let load = input "load" 1 in		
    let data = input "data" length in				
    let reg_in = (signal "in" (length+2)) in			
      let register = reg vdd reg_in in 
        reg_in <== mux2 load (vdd++data++parity_calc(data)) (register <<: 1);  
        output "result" (msb register);
          let circuit = get_circuit () in
            Verilog.output_netlist circuit
  in

  let simulation circuit =
    let vcd = open_out "simulator.vcd" in
      let sim, inputs, outputs = Simulate.create circuit vcd in
         let
           i1 = List.assoc "i1" inputs 
         and    
           i2 = List.assoc "i2" inputs
         in
           i1.(0) <- 2;
           i2.(0) <- 3;
           Simulate.cycle sim;
           let
             greater = List.assoc "GREATER" outputs
           and
             equal = List.assoc "EQUAL" outputs
           and
             less = List.assoc "LESS" outputs
           in
             Printf.printf "  I1=%d ; I2=%d\n"  i1.(0) i2.(0);
             Printf.printf "  GREATER=%d ; EQUAL=%d ; LESS=%d\n"  greater.(0) equal.(0) less.(0);
             close_out vcd
  in
  
  let compare length =
   
    start_circuit "compare";

    (* Inputs and outputs definition *)
    let i1, i2 = (input "i1" length), (input "i2" length) in
     output "GREATER" (i1 >: i2);
     output "EQUAL" (i1 ==: i2);		
     output "LESS" (i1 <: i2);		

     let
      circuit = get_circuit ()
     in
       Hdcaml.Verilog.output_netlist circuit; 
       simulation circuit
  in
           
  counter_design 4;
  fir_filter_design 8 16 24;
  firstprogram 8;
  comparator 5;
  log_shifter_design 8 16;
  shifter 8;
  compare 8

do test()
