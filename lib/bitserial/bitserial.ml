(* Qeustion?  Is it possible to devise a data type which allows expressions like 
     (a+b)*c 
   to be implemented in bitserial arithmetic with implicit control signals? *)
#light
open DigitalLogic
open Circuit
open Simulator

let reg (d : Signal) ena = d.reg(Signal.Clock, Signal.Reset, 0, ena);

let bitserial_add ena (first : Signal) a b = 
  let carry_i = Signal.wire 1
  let carry_o, sum = Circuits.Add.fa a b carry_i
  let carry_r = reg carry_o ena
  carry_i <== first.mux2(0, carry_r)
  carry_r, carry_o, sum

let bitserial_sub ena (first : Signal) a b = 
  let borrow_i = Signal.wire 1
  let borrow_o, sum = Circuits.Add.fs a b borrow_i
  let borrow_r = reg borrow_o ena
  borrow_i <== first.mux2(0, borrow_r)
  borrow_r, borrow_o, sum

let serial_to_parallel ena w b = 
  let w = Signal.wire w
  let r = reg w ena 
  w <== b ++ r.msbs;
  r

type Bitserial =
  { 
    data : Signal; 
    first : Signal;
    i_ready : Signal;
    i_take : Signal;
    o_ready : Signal;
    o_take : Signal;
  }
  with

    (* parallel to serial *)
    static member (<==) ((a : Bitserial), (b : Bitserial)) = 0
    
    (* serial to parallel *)
    static member (==>) ((a : Bitserial), (b : Bitserial)) = 0
      
    (* add *)
    static member (+) ((a : Bitserial), (b : Bitserial)) = 
      let first = a.first ||| b.first
      let first_vld = a.first &&& b.first
      let ena_in = a.i_ready &&& a.i_ready &&& ((~~~ first) ||| first_vld)
      let _,_,s = bitserial_add ena_in first_vld a.data b.data
      {
        data = empty;
        first = empty;
        i_ready = empty;
        i_take = empty;
        o_ready = empty;
        o_take = empty;
      }
      
      
    (* subtract *)
    static member (-) ((a : Bitserial), (b : Bitserial)) = 0
      
      
    (* multiply *)
    static member ( * ) ((a : Bitserial), (b : Bitserial)) = 0
      
      
  end

//             /--------\
// <- itake  - |        | - otake  ->
// <- iready - |        | - oready ->
//             \--------/
type Handshake =
  {
    i_take : Signal
    i_ready : Signal
    
    o_take : Signal
    o_ready : Signal
    
    enable : Signal
  }
  with
    static member make = 
      let i_take = Signal.wire 1
      let i_ready = Signal.wire 1
      let o_take = Signal.wire 1
      let o_ready = Signal.wire 1
      {
        i_take = Signal.wire 1
        i_ready = Signal.wire 1
        o_take = Signal.wire 1
        o_ready = Signal.wire 1
      }
    member x.Transfer = o_ready
  end
  

let test() = 

  let first = Signal.input ("first", 1)
  let a = Signal.input ("a", 1)
  let b = Signal.input ("b", 1)
  let ci,co,s = bitserial_sub Signal.Enable first a b
  let p = serial_to_parallel Signal.Enable 5 s
  let circuit = Circuit.create [ ci.output "ci"; co.output "co"; s.output "s"; p.output "p" ]
  Circuit.write_file Verilog.write "" "bsadd" ".v" circuit

  let sim = Simulator.create_int circuit
  
  let fi = find_input_data sim 
  let fo = find_output_data sim 

  let first = fi "first"
  let ena = fi "enable"
  let a = fi "a"
  let b = fi "b"
  let ci = fo "ci"
  let co = fo "co"
  let s = fo "s"
  let p = fo "p"
  
  let a' = Array.rev [| 0; 1; 1; 0; 1 |]        //   13
  let b' = Array.rev [| 0; 0; 1; 1; 0 |]        // +  6
                                                // = 19 
  sim_reset sim
  first := 1
  ena := 1
  for i=0 to 4 do
    a := a'.(i)
    b := b'.(i)
    sim_cycle sim
    printf "s = %i + %i + %i = %i + %i\n" !a !b !ci !s !co
    first := 0
    
  sim_cycle sim
  printf "p = %i\n" !p


do test()

