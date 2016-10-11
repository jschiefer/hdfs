open DigitalLogic
open DigitalLogic.Design

let os = output_string stdout

(** Hardware design in HDFS is performed by constructing hardware "nodes" *)
(** from a set of basic operations predefined by the library.  For *)
(** example here is a function which will implement a (signed) complex multiplier. *)
let tutorial_0_complex_mul (ar,ai) (br,bi) = 
  (ar *+ br) -: (ai *+ bi), (ar *: bi) +: (ai *: br)
  
(** A circuit is built up by defining its inputs and constructing a list of its outputs. *)
(** Here we create a circuit from the complex multipler given tutorial 0 *)
let tutorial_1_inputs_and_outputs () =
  (* create the circuit inputs *)
  let ar = input "input_a_real" 4 in
  let ai = input "input_a_imag" 4 in
  let br = input "input_b_real" 4 in
  let bi = input "input_b_imag" 4 in
  (* the actual logic that gets created *)
  let cr, ci = tutorial_0_complex_mul (ar, ai) (br, bi) in
  (* Create the circuit output.  Note how it is placed into a list. *)
  let outputs = 
    [ 
      output "output_c_real" cr;
      output "output_c_imag" ci;
    ] in
  (* Given the circuits outputs, Design.create will parse the circuit 
     into a data structure which can be used for netlist generation 
     and simulation *)
  Circuit.create outputs 

(** Netlists can be generated in VHDL, Verilog and rewritten as F# *)
let tutorial_2_netlist () =
  let circuit = tutorial_1_inputs_and_outputs () in
  Design.write_file Verilog.write "tutorial_complex_mul" ".v" circuit;
  Design.write_file Vhdl.write "tutorial_complex_mul" ".vhd" circuit;
  Design.write_file Fsharp.write "tutorial_complex_mul" ".fs" circuit

let _ = tutorial_2_netlist()

(** Registers are used to hold state between clock cycles in a hardware design *)
(** They require a clock, reset, value to take at reset and enable signal. *)
(** The clock is generally a globally defined signal passed to each register (and memory) *)
(** in the design.  The reset is asserted asynchrously and sets all registers *)
(** in the design to a known value.  Enable controls whether the register loads *)
(** it's input value, or holds it's previous value. *)
let tutorial_3_registers d = 
  reg clock reset (zero (width d)) vdd d (* registers the signal d.  
                                            Enable == vdd which is always '1' (ie logic high).  
                                            Register will set itself to zero on reset *)

(** Wires are an important building block in circuit designs.  They allow *)
(** the output of a register to feedback to it's input.  On the otherhand *)
(** they also allow the generation of illegal circuit structures called *)
(** combinatorial loops which occur when a wire loops back to itself without *)
(** first passing through a register *)
let tutorial_4_feedback () =
  (* this circuit structure is illegal *)
  comb_check_on := true; (* normally these are off as they can severely affect performance *)
  let _ = 
    try
      let a = wire 1 in
      let b = ~: a in (* so far so good.  b is the logical negation of a *)
      a <== b;  (* oh dear.  a = ~b = ~a = ~b = ... chip goes boom (not really, but doesnt work) *)
    with 
    | Design_error s -> os ("Whoops - " ^ s ^ "\n")
    | _ -> failwith "Shouldnt get here..."
  in
  
  (* the same thing but with a register "in the way".  Now a = ~b = ~a etc on each clock cycle *)
  let a = wire 1 in
  let b = (~: a) in
  let r = tutorial_3_registers b in  
  a <== r; (* no problem this time. *)
  comb_check_on := false;
  r
  
(** The simulator can be used to test a circuit. *)
let tutorial_5_simulation() = 
  let r = tutorial_4_feedback () in
  let circuit = Circuit.create [ output "r" r ] in
  let sim = Simulator.create_int circuit in
  (* get the data for the circuits output port.  In this case there are no input ports to the circuit. *)
  let r = Simulator.find_output_data sim "r" in
  Simulator.cycle sim; 
  os ("output value = " ^ string_of_int !r ^ "\n");
  Simulator.cycle sim; 
  os ("output value = " ^ string_of_int !r ^ "\n");
  Simulator.cycle sim; 
  os ("output value = " ^ string_of_int !r ^ "\n");
  Simulator.cycle sim; 
  os ("output value = " ^ string_of_int !r ^ "\n")

let _ = tutorial_5_simulation()
