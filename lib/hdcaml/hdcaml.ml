#light

namespace Hdcaml 

  open DigitalLogic
  open Numeric.Ops
  open Signal

  module Design = begin

    (* Circuit types *)
    type signal = Signal
    type circuit = string * Circuit.Circuit

    (* Circuit data *)
    let circuit n = ()
    let endcircuit() = ()
    let add_sink _ = ()
    let next_id() = 0
    let path() = "none"
    let add_output, start_circuit, get_circuit = 
      let outputs = ref []
      let name = ref "some_circuit"
      (fun o -> outputs := o :: !outputs),
      (fun n -> name := n; outputs := []),
      (fun () -> 
        let o = !outputs
        signal_init()
        outputs := []
        !name, Circuit.create o
      )

    (* Signal Assignment and Annotation *)
    let signal n w = wire w -- n
    let (<==) a b = assign a b
    let (--) n (s : signal) = 
      if s.IsWire && (wire_name s) <> "" then s
      else s -- n

    (* Width Utilities *)
    let width = width
    let check_width w s = w = width s
    let check_width_bit s = width s = 1
    let check_width_nonzero s = width s > 0
    let check_width_same a b = width a = width b
    
    (* Top Level Ports *)
    let input n w = input n w
    let output n s = 
      let o = output n s
      add_output o
    
    (* Constants *)
    let const = constb
    let zero = zero
    let one = one
    let ones = ones
    let empty = empty
    let vdd = vdd
    let gnd = gnd

    (* Bit Manipulation *)
    let select = select
    let bit = bit
    let (++) = cat
    let repeat =  repeat
    let msb = msb
    let msbs = msbs
    let lsb = lsb
    let lsbs = lsbs
    let split = split
    let bits = bits
    
    (* Bitwise Logicals *)
    let (~:) = bnot
    let (&:) = band
    let (|:) = bor
    let (^:) = bxor
    
    (* Arithmetics *)
    let (+:) = add
    let (-:) = sub
    let ( *: ) = mulu
    let ( *+ ) = muls

    (* Shifting *)
    let (<<:) = sll
    let (>>:) = srl
    let (>>+) = sra
    
    (* Comparisons *)
    let (==:) = eq
    let (/=:) = neq
    let (<:)  = lsu
    let (>:)  = gtu
    let (<=:) = lsequ
    let (>=:) = gtequ
    let (<+)  = lss
    let (>+)  = gts
    let (<=+) = lseqs
    let (>=+) = gteqs

    (* Muxing *)
    let mux2 = mux2
    let mux = mux
    
    let reg enable data = reg clock reset empty enable data

  end

  (* XXX using ints with 32 bit precision rather than 31 bit.  This'll throw testbenches *)
  module Simulate = begin

    type port = string * int array     
    type simulator = (Simulator.Simulator * port list * port list) * port list * port list
    
    let create (name,circuit) ochan = 
      let sim = Simulator.create circuit
      let inputs = List.map (fun (p:Simulator.Port) -> p.name, Array.create ((p.width+30)/31) 0) (sim.sim_inputs)
      let outputs = List.map (fun (p:Simulator.Port) -> p.name, Array.create ((p.width+30)/31) 0) (sim.sim_outputs)
      ((sim, inputs, outputs), inputs, outputs)
      
    let cycle ((sim:Simulator.Simulator),inputs',outputs') = 
      let inputs = sim.sim_inputs
      let outputs = sim.sim_outputs
      List.iter2 (fun (p0:Simulator.Port) (n1,d1:uint32[]) -> 
        (* XXX convert 31 bit ints -> 32 bit uints *)
        for i=0 to Array.length p0.data - 1 do 
          p0.[i] <- uint32 d1.[0]
      ) inputs inputs'
      sim.cycle
      List.iter2 (fun (p0:Simulator.Port) (n1,d1:int32[]) -> 
        (* XXX convert 32 bit uints -> 31 bit ints *)
        for i=0 to Array.length p0.data - 1 do 
          d1.[i] <- int p0.[0]
      ) outputs outputs'
      
    let reset ((sim:Simulator.Simulator),_,_) = sim.reset 

  end

  module Systemc = begin
    let output_model (name,circuit) = Circuit.write_file (C.write C.GenC) "" name ".c" circuit
  end

  module Verilog = begin
    let output_netlist (name,circuit) = Circuit.write_file Verilog.write "" name ".v" circuit
  end

  module Vhdl = begin
    let output_netlist (name,circuit) = Circuit.write_file Vhdl.write "" name ".vhd" circuit
  end

  module Verify = begin
    type sequence = Sequence
    type property_ = Property (* property cannot be used in F# *)
    
    let rigid n w = (wire w) -- n
    
    let seq (s : Design.signal) = Sequence
    let ($) (s0 : sequence) (s1 : sequence) = Sequence
    let (-$) (s0 : sequence) (s1 : sequence) = Sequence
    let (|$) (s0 : sequence) (s1 : sequence) = Sequence
    let (&$) (s0 : sequence) (s1 : sequence) = Sequence
    let (&&$) (s0 : sequence) (s1 : sequence) = Sequence
    let within (s0 : sequence) (s1 : sequence) = Sequence
    let star = Sequence
    let plus = Sequence
    let any_n (i:int) = Sequence
    let any_n_to_m (i:int) (j:int) = Sequence
    let any_n_to_inf (i:int) = Sequence
    let repeat_star sequence = Sequence
    let repeat_plus sequence = Sequence
    let repeat_n sequence (i:int) = Sequence
    let repeat_n_to_m sequence (i:int) (j:int) = Sequence
    let repeat_n_to_inf sequence (i:int) = Sequence

    let prop (s : Design.signal) = Property
    let (!?) (p:property_) = Property
    let (&&?) (p0:property_) (p1:property_) = Property
    let (||?) (p0:property_) (p1:property_) = Property
    let (->?) (p0:property_) (p1:property_) = Property
    let (<->?) (p0:property_) (p1:property_) = Property
    let always (p:property_) = Property
    let never (p:property_) = Property
    let eventually (p:property_) = Property
    let propseq (s:sequence) = Property
    let propseq_w (s:sequence) = Property
    let until (p0:property_) (p1:property_) = Property
    let until_w (p0:property_) (p1:property_) = Property
    let (|->) (s:sequence) (p:property_) = Property
    let (|=>) (s:sequence) (p:property_) = Property

    let assertion (s:string) (p:property_) = ()
    let assert_always (s:string) (p:property_) = ()
    let assert_always_signal (s:string) (g:Design.signal) = ()
    let assume (s:string) (p:property_) = ()
    let assume_always (s:string) (p:property_) = ()
    let assume_always_signal (s:string) (g:Design.signal) = ()
    let cover (s:string) (q:sequence) = 0
    
  end

  (* Not implemented in hdfs *)
  module Waveform = begin

    type scope = Scope
    type name = string
    type value = string
    
    let open_vcd (ochan : System.IO.TextWriter) (name : string) = Scope
    let scope (sc : scope) (name : string) = sc
    let signal (sc : scope) (name : string) (width : int) (v : value) = sc
    let cycle (sc : scope) = ()

  end
  

