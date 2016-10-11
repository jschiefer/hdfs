#light
open DigitalLogic
open Numeric.Ops
open Signal
open Fixed

let test_ops() = 


  let w0 = 16
  let w1 = 16
  let f0 = 4
  let f1 = 4
  
  let i0 = UFixed.to_UFixed(input "i0" w0, f0)
  let i1 = UFixed.to_UFixed(input "i1" w1, f1)
  
  //printf "%s\n" r.to_string
  //printf "%s\n" i.to_string
  
  //let clength r i = (r * r) + (i * i) + ((r * i) *$ 2)
  //let l = clength r i

  let add0 = i0 + i1
  let add1 = i0 + (i0.fixed_of_float 0.5)
  let add2 = i0 + 0.5

  let sub0 = i0 - i1
  let sub1 = i0 - (i0.fixed_of_float 0.5)
  
  let mul0 = i0 * i1
  let mul1 = i0 * (i0.fixed_of_float 0.5)
  
  let const0 = i0 * UFixed.consti(fix 2 1, 1)
  let const1 = i0 * UFixed.constf(fix 2 1, 0.5)
  
  let reint0 = mul0.SetFix 7
  let reint1 = mul0.SetFix 9
  
  let sel0 = mul0.[23,0].SetFix 0
  let sel1 = mul0.[22,-7].SetFix (-1)
  let sel2 = mul0.lsbs
  let sel3 = mul0.msbs
  let sel4 = mul0.lsb
  let sel5 = mul0.msb
  
  let less0 = (i0.constf 1.0) <: (i0.constf 0.5)
  let less1 = UFixed.constf(fix 8 4, 1.5) <: UFixed.constf(fix 6 2, 2.8)

  let eq0 = UFixed.constf(fix 2 1, 0.5) ==: UFixed.constf(fix 1 1, 0.5)
  let eq1 = UFixed.constf(fix 8 4, 0.5) ==: UFixed.constf(fix 6 2, 0.5)
  let neq0 = UFixed.constf(fix 2 1, 0.5) /=: UFixed.constf(fix 1 1, 0.5)
  let neq1 = UFixed.constf(fix 8 4, 0.5) /=: UFixed.constf(fix 6 2, 0.5)

  let reg0 = mul0.reg(clock, reset, UFixed.constf(mul0.range, 0.5), enable)
  let reg1 = i1.reg(clock, reset, i1.constf(1.0), enable)
  
  let bit0 = i0 &&& (i1 ||| i0)
  let bit1 = ~~~ (i0 ^^^ i1)
  
  let shl0 = i0 <<< 1
  let shl1 = i1 <<< 10

  let shr0 = i0 >>> 1
  let shr1 = i1 >>> 10
  
  let one0 = UFixed.consti(fix 1 0, 0).one
  let one1 = UFixed.consti(fix 1 1, 0).one
  let one2 = UFixed.consti(fix 1 (-1), 0).one
  let one3 = UFixed.consti(fix 2 1, 0).one

  let be0 = (UFixed.constf(fix 8 4, 1.0)).b_wire
  let be1 = (UFixed.constf(fix 8 4, 1.0)).b_wire
  behave 
    [
      be0 $== (be0.q.consti 1);
      be1 $== (be1.q.consti 1);
      (be0.q.signal <: 3).b_if ([ be1 $== (be1.q.constf 2.0) ], []);
      (be0.q /=: 4).b_if ([ be1 $== 3.0 ], []);
      (~~~ (be0.q)).b_if ([ be1 $== 4 ], []); 
      be0.q.b_switch
        [
          (be0.q.constf 0.0).b_case [ be1 $== be1.q.constf 1.0 ];
          (be0.q.constf 1.0).b_case [ be1 $== be1.q.constf 2.0 ];
          (be0.q.constf 2.0).signal.b_case [ be1 $== be1.q.constf 3.0 ];
        ]
    ]
  let be0 = be0.q
  let be1 = be1.q

  let out_list = 
      [
        "add0",   add0;   "add1",   add1;
        "sub0",   sub0;   "sub1",   sub1;
        "mul0",   mul0;   "mul1",   mul1; 
        "const0", const0; "const1", const1; 
        "reint0", reint0; "reint1", reint1; 
        "sel0",   sel0;   "sel1",   sel1; 
        "sel2",   sel0;   "sel3",   sel1; 
        "sel4",   sel0;   "sel5",   sel1; 
        "less0",  less0;  "less1",  less1; 
        "eq0",    eq0;    "eq1",    eq1; 
        "neq0",   neq0;   "neq1",   neq1; 
        "reg0",   reg0;   "reg1",   reg1; 
        "bit0",   bit0;   "bit1",   bit1; 
        "shl0",   shl0;   "shl1",   shl1; 
        "shr0",   shr0;   "shr1",   shr1; 
        "one0",   one0;   "one1",   one1; 
        "one2",   one2;   "one3",   one3; 
        "be0",    be0;    "be1",    be1; 
      ]

  let outputs = List.map (fun (x, (y : UFixed)) -> y.signal.output x) out_list
  
  let circuit = Circuit.create outputs
  Circuit.write_file Vhdl.write "output/" "fixed" ".vhd" circuit
  
  let sim = Simulator.create circuit
  let enable = sim.port "enable"
  let i0' = sim.port "i0"
  let i1' = sim.port "i1"

  let outputs = List.map (fun ((n:string),(s:UFixed)) -> (n,s,sim.port n)) out_list
  let print_fix (str, (signal : UFixed), (value:Simulator.Port)) = printf "%s = %f (%i) %s\n" str (signal.float_of_fixed value.u) value.u signal.to_string

  sim.reset 
  let cycle v0 v1 = 
    printf "*************************************\n"
    i0'.u <- i0.fixed_of_float v0
    i1'.u <- i1.fixed_of_float v1
    sim.cycle 
    print_fix ("i0", i0, i0')
    print_fix ("i1", i1, i1')
    List.iter print_fix outputs
  
  enable.u <- 1;
  cycle 2.5 2.5
  cycle 3.5 5.5

let test_ops_u() = 
  let a = UFixed.to_UFixed(input "a" 8, 4)
  let b = UFixed.to_UFixed(input "b" 4, 2)
  
  let c = ((a + b) + 4) + 0.5
  let c = UFixed.sub(10, UFixed.sub(2.0, ((a - b) - 4) - 0.5))
  let c = ((c * b) * 4) * 0.5
  let c = ((((UFixed.band(c, c) &&& UFixed.band(c, -1) &&& (UFixed.band(c, -2)) &: c) &: 34) &: 0.5))
  let c = ((((UFixed.bor(c, c) ||| UFixed.bor(c, -1) ||| (UFixed.bor(c, -2)) |: c) |: 34) &: 0.5))
  let c = ((((UFixed.bxor(c, c) ^^^ UFixed.bxor(c, -1) ^^^ (UFixed.bxor(c, -2)) |: c) ^: 34) &: 0.5))
  let c = ~~~ c
  let c = c <: c <: 1 <: 0.5
  let c = UFixed.lsu(c, UFixed.lsu(UFixed.lsu(1, UFixed.lsu(0.5, UFixed.lsu(c, 1))), 0.5))
  let c = c >: c >: 1 >: 0.5
  let c = UFixed.gtu(c, UFixed.gtu(UFixed.gtu(1, UFixed.gtu(0.5, UFixed.gtu(c, 1))), 0.5))
  let c = c <=: c <=: 1 <=: 0.5
  let c = UFixed.lsequ(c, UFixed.lsu(UFixed.lsequ(1, UFixed.lsequ(0.5, UFixed.lsequ(c, 1))), 0.5))
  let c = c >=: c >=: 1 >=: 0.5
  let c = UFixed.gtequ(c, UFixed.gtequ(UFixed.gtequ(1, UFixed.gtequ(0.5, UFixed.gtequ(c, 1))), 0.5))
  let c = c ==: c ==: 1 ==: 0.5
  let c = UFixed.eq(c, UFixed.eq(UFixed.eq(c, 1), 0.5))
  let c = c /=: c /=: 1 /=: 0.5
  let c = UFixed.neq(c, UFixed.neq(UFixed.neq(c, 1), 0.5))
  let c = c <<< 1 <<: 3 <<: b
  let c = UFixed.sll(c, UFixed.sll(c, b))
  let c = c >>> 1 >>: 3 >>: b
  let c = UFixed.srl(c, UFixed.srl(c, b))
  let c = (c ++ c ++ c).msbs.lsbs.lsb.msb
  
  Circuit.write_file Verilog.write "output/" "fixed_ops_u" ".v" (Circuit.create [ c.signal.output "c" ])

let test_ops_s() = 
  let a = SFixed.to_SFixed(input "a" 8, 4)
  let b = SFixed.to_SFixed(input "b" 4, 2)
  
  let c = ((a + b) + 4) + 0.5
  let c = SFixed.sub(10, SFixed.sub(2.0, ((a - b) - 4) - 0.5))
  let c = ((c * b) * 4) * 0.5
  let c = ((((SFixed.band(c, c) &&& SFixed.band(c, -1) &&& (SFixed.band(c, -2)) &: c) &: 34) &: 0.5))
  let c = ((((SFixed.bor(c, c) ||| SFixed.bor(c, -1) ||| (SFixed.bor(c, -2)) |: c) |: 34) &: 0.5))
  let c = ((((SFixed.bxor(c, c) ^^^ SFixed.bxor(c, -1) ^^^ (SFixed.bxor(c, -2)) |: c) ^: 34) &: 0.5))
  let c = ~~~ c
  let c = c <+ c <+ 1 <+ 0.5
  let c = SFixed.lss(c, SFixed.lss(SFixed.lss(1, SFixed.lss(0.5, SFixed.lss(c, 1))), 0.5))
  let c = c >+ c >+ 1 >+ 0.5
  let c = SFixed.gts(c, SFixed.gts(SFixed.gts(1, SFixed.gts(0.5, SFixed.gts(c, 1))), 0.5))
  let c = c <=+ c <=+ 1 <=+ 0.5
  let c = SFixed.lseqs(c, SFixed.lss(SFixed.lseqs(1, SFixed.lseqs(0.5, SFixed.lseqs(c, 1))), 0.5))
  let c = c >=+ c >=+ 1 >=+ 0.5
  let c = SFixed.gteqs(c, SFixed.gteqs(SFixed.gteqs(1, SFixed.gteqs(0.5, SFixed.gteqs(c, 1))), 0.5))
  let c = c ==: c ==: 1 ==: 0.5
  let c = SFixed.eq(c, SFixed.eq(SFixed.eq(c, 1), 0.5))
  let c = c /=: c /=: 1 /=: 0.5
  let c = SFixed.neq(c, SFixed.neq(SFixed.neq(c, 1), 0.5))
  let c = c <<< 1 <<: 3 <<: b
  let c = SFixed.sll(c, SFixed.sll(c, b))
  let c = c >>> 1 >>: 3 >>: b
  let c = SFixed.srl(c, SFixed.srl(c, b))
  let c = c >>+ 3 >>+ b
  let c = SFixed.sra(c, SFixed.sra(c, b))
  let c = (c ++ c ++ c).msbs.lsbs.lsb.msb
  
  Circuit.write_file Verilog.write "output/" "fixed_ops_s" ".v" (Circuit.create [ c.signal.output "c" ])

let to_bits i range = 
  let rec to_bits s (range:Range) i =
    let c = if i &&& 1 = 0 then '0' else '1' in
    let s = String.of_char c ^ (if range.low = 0 then "." else "") ^ s in
    if range.high = range.low then s
    else to_bits s {r_high=range.high; r_low=range.low+1} (i >>> 1)
  to_bits "" range i 

let test_resize_u i_width i_fix o_width o_fix =

  let resize (x : UFixed) (range:Range) = x.[range.high,range.low]
  //let resize (x : UFixed) (range:Range) = x.Truncate.Wrap.[range.high,range.low]

  let fix_in  = fix i_width i_fix
  let fix_out = fix o_width o_fix

  printf "input : [%i:%i] (%i.%i)\n" fix_in.high fix_in.low fix_in.width fix_in.fixedPoint
  printf "output: [%i:%i] (%i.%i)\n" fix_out.high fix_out.low fix_out.width fix_out.fixedPoint

  let inp = UFixed.to_UFixed(input "inp_u" fix_in.width, fix_in.fixedPoint)
  let range_out = fix_out.range
  let output_ranges = [ fix_out.range ]
  
  let outputs = List.mapi (fun i x -> "u" ^ string_of_int i, resize inp x) output_ranges
  let c_outputs = List.map (fun (n,(s : UFixed)) -> s.signal.output n) outputs
  
  let sim = Simulator.create (Circuit.create c_outputs)
  let inp' = sim.port "inp_u"
  let data = List.map (fun ((n:string),s) -> (n,s,sim.port n)) outputs
  sim.reset 
  List.iteri (fun cycle in_data ->
    inp'.u <- in_data
    sim.cycle 
    let p (f : UFixed) x = 
      to_bits x f.range ^ " (" ^ string_of_float (f.float_of_fixed x) ^ ")"
      //to_bits x f.range 
    //printf "  %s = %s\n" "t " (p inp in_data)
    List.iter (fun (n,s,(d:Simulator.Port)) -> 
      //printf "  %s = %s\n" n (p s !d)
      printf "%s\n" (p s d.u)
    ) data
  ) [ 
      for i in { 0 .. (1 <<< fix_in.width)-1} -> 
        let f = float_of_int i in
        let f = f / (2.0 ** float_of_int fix_in.fixedPoint) in
        UFixed.fixed_of_float' (true, inp.fixedPoint, f) 
    ]

let test_resize_s i_width i_fix o_width o_fix =

  let resize (x : SFixed) (range:Range) = x.[range.high,range.low]
  //let resize (x : SFixed) (range:Range) = x.Truncate.Wrap.[range.high,range.low]

  let fix_in  = fix i_width i_fix
  let fix_out = fix o_width o_fix

  printf "input : [%i:%i] (%i.%i)\n" fix_in.high fix_in.low fix_in.width fix_in.fixedPoint
  printf "output: [%i:%i] (%i.%i)\n" fix_out.high fix_out.low fix_out.width fix_out.fixedPoint

  let round_any (sel : Signal) (x : Signal) (xr : Range) low =
    let results, ranges = List.split [ round_inf_s x xr low; round_ninf_s x xr low; round_zero_s x xr low; round_away_zero_s x xr low; round_nearest_s x xr low; ]
    (* need to resize the results so they are all the same size so the mux will work *)
    let range = Range.union ranges
    let results = List.map (fun (x:Signal) -> (x.msb.repeat (range.width - x.width)) ++ x) results
    mux sel results, range
    
  let overflow_any (sel : Signal) (x : Signal) (xr : Range) high =
    (* no need to resize results as they "should" all be the same size (ie the target size) by noe *)
    let results, ranges = List.split [ wrap x xr high; saturate_s x xr high; ]
    mux sel results, Range.union ranges

  let sel_rnd = input "rnd" 3
  let sel_ovf = input "ovf" 3
  let inp = SFixed.to_SFixed(input "inp_s" fix_in.width, fix_in.fixedPoint).SetRoundingMode(round_any sel_rnd, overflow_any sel_ovf)
  let range_out = fix_out.range
  let output_ranges = [ fix_out.range ]
  
  let outputs = List.mapi (fun i x -> "s" ^ string_of_int i, resize inp x) output_ranges
  let c_outputs = List.map (fun (n,(s : SFixed)) -> s.signal.output n) outputs
  
  let se d w = if d &&& (1 <<< (w-1)) <> 0 then d ||| ((-1) <<< w) else d

  let sim = Simulator.create (Circuit.create c_outputs)
  let inp' = sim.port "inp_s" 
  let data = List.map (fun ((n:string),s) -> (n,s,sim.port n)) outputs
  let rnd = sim.port "rnd"
  let ovf = sim.port "ovf"
  
  sim.reset 
  
  List.iteri (fun cycle in_data ->
  
    let p (f : SFixed) x = 
      to_bits x f.range
      //to_bits x f.range ^ " (" ^ string_of_float (f.float_of_fixed x) ^ ")"
      //to_bits x f.range 
      
    inp'.i <- in_data
    ovf.u <- 0

    List.iter (fun i ->
      rnd.u <- i
      sim.cycle 
      List.iter (fun (n,s,(d:Simulator.Port)) -> printf "%s " (p s (se d.i s.width))) data
    ) [ 0 .. 4 ]
    
    printf " (%i)\n" cycle
    
    //List.iter (fun (n,s,d) -> printf "%s (%i)\n" (p s (se !d s.width)) cycle) data
  ) [ 
      for i in { 0 .. (1 <<< fix_in.width)-1} -> 
        let f = float_of_int (i - (1 <<< (fix_in.width-1))) in
        let f = f / (2.0 ** float_of_int fix_in.fixedPoint) in
        SFixed.fixed_of_float' (true, inp.fixedPoint, f) 
    ]

let test_sfixed() =
  let input n w f = SFixed.to_SFixed( input n w, f )
  
  let i0 = input "i0" 6 (+2)
  let i1 = input "i1" 6 (-1)
  let inputs = [ i0; i1 ]

  let outputs = 
    [
      (i0 + i1)
      (i0 * i1)
      (i0 - 1.0)
    ]

  let circuit = Circuit.create (List.mapi (fun i (x:SFixed) -> x.signal.output ("o" ^ string_of_int i)) outputs)
  let sim = Simulator.create circuit
    
  let i0' = sim.port "i0"
  let i1' = sim.port "i1"
  
  let map_outputs o = List.mapi (fun i (x:SFixed) -> sim.port ("o" ^ string_of_int i), x) outputs

  let outputs = map_outputs outputs
  
  sim.reset
  
  let se d w = if d &&& (1 <<< (w-1)) <> 0 then d ||| ((-1) <<< w) else d
  let sf (i : SFixed) i' = 
    let i' = i.float_of_fixed i'
    let s,v = if i' < 0.0 then "-",- i' else " ",i'
    s ^ (sprintf "%.8f" v)
    
  let sim_cycle () = 
    sim.cycle 
    List.iter (fun ((d:Simulator.Port),(s:SFixed)) -> 
      printf "%4i: %s\n" d.i (sf s d.i)
    ) outputs

  let data = 
    [
      ( 3.0, -4.0 );
      ( 3.0, 5.222 );
      ( 2.0, 5.222 );
    ]

  List.iter (fun (f0,f1) -> 
      i0'.i <- i0.fixed_of_float f0
      i1'.i <- i1.fixed_of_float f1
      printf "*****************************************************\n"
      printf "%4i: %s\n" i0'.i (sf i0 i0'.i)
      printf "%4i: %s\n" i1'.i (sf i1 i1'.i)
      printf "-----------------------------------------------------\n"
      sim_cycle ()
      printf "*****************************************************\n\n\n"
    ) data
  

//do test_ops_u()
//do test_ops_s()
//do test_resize_u (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2)) (int_of_string Sys.argv.(3)) (int_of_string Sys.argv.(4)) 
//do test_resize_s (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2)) (int_of_string Sys.argv.(3)) (int_of_string Sys.argv.(4)) 
do test_sfixed()

