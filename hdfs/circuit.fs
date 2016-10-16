#light "off"
(*
  HDFS Digital Logic Hardware Design (HDFS.dll)
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

/// <P>The circuit data type is constructed from from a list of circuit outputs (and optionally inouts)
/// and used by the netlist generators and simulator among other things.<P>
module DigitalLogic.Circuit

open System
open DigitalLogic.Numeric
open DigitalLogic.Signal

type BigInt = bigint

(** ***********************************************************)

(** get the string of the given binary operator (debug) *)
let string_of_binop = function
  | B_add -> "+:"
  | B_sub -> "-:"
  | B_mulu -> "*:"
  | B_muls -> "*+:"
  | B_and -> "&:"
  | B_or -> "|:"
  | B_xor -> "^:"
  | B_eq -> "==::"
  | B_lt -> "<::"
  | B_cat -> "++:"

(** get the string of the given unnary operator (debug) *)
let string_of_unop = function U_not -> "~:"

(** get the string of the given signal (debug) *)
let string_of_signal (x : Signal) = match x.signal with 
  | Signal_empty    -> "empty" 
  | Signal_const    (a,w,c) -> "const " ^ string a
  | Signal_binop    (a,w,op,s0,s1) -> string_of_binop op ^ " " ^ string a
  | Signal_unop     (a,w,op,s) -> string_of_unop op ^ " " ^ string a
  | Signal_wire     (a,w,n,d) -> "wire " ^ string a
  | Signal_mux      (a,w,sel,d) -> "mux " ^ string a
  | Signal_select   (a,hi,lo,s) -> "select " ^ string a
  | Signal_reg      (a,w,clk,rst,rstval,ena,d) -> "reg " ^ string a
  | Signal_mem      (a,dw,aw,size,clk,w,we,d,r) -> "mem " ^ string a
  | Signal_behave   (a,w,b,d) -> "behave " ^ string a
  | Signal_inst     (a,n,m,g,io,i,o) -> "instantiation " ^ string a
#if INST2
  | Signal_inst2    (a,s,g,io,i,o) -> "instantiation " ^ string a
#endif
  | Signal_tri      (a,w,d) -> "tristate " ^ string a

(* ************************************************************)

/// Given a signal recusively visits all it's dependants.  A set (of uid's) is used to ensure each node is visited 
/// only once (the signal tree is cyclic).  Functions with a user argument are called before and after the recursion 
let rec visit_signal set prefn postfn arg (signal : Signal) =
  let is_visited set (signal : Signal) = Set.contains (signal.uid) set in
  if is_visited set signal then (arg,set)
  else
    let set = Set.add (signal.uid) set in
    match signal.signal with
    | Signal_empty    -> 
      let arg = prefn arg signal in
      let arg = postfn arg signal in
      arg, set
    | Signal_const    (a,w,c) ->
      let arg = prefn arg signal in
      let arg = postfn arg signal in
      arg, set
    | Signal_binop    (a,w,op,s0,s1) -> 
      let arg = prefn arg signal in
      let arg,set = visit_signal set prefn postfn arg s0 in
      let arg,set = visit_signal set prefn postfn arg s1 in
      let arg = postfn arg signal in
      arg, set
    | Signal_unop     (a,w,op,s) -> 
      let arg = prefn arg signal in
      let arg,set = visit_signal set prefn postfn arg s in
      let arg = postfn arg signal in
      arg, set
    | Signal_wire     (a,w,n,d) -> 
      let arg = prefn arg signal in
      let arg,set = visit_signal set prefn postfn arg !d in
      let arg = postfn arg signal in
      arg, set
    | Signal_mux      (a,w,sel,d) -> 
      let arg = prefn arg signal in
      let arg,set = List.fold (fun (arg,set) s -> visit_signal set prefn postfn arg s) (arg,set) (sel::d) in
      let arg = postfn arg signal in
      arg, set
    | Signal_select   (a,hi,lo,s) -> 
      let arg = prefn arg signal in
      let arg,set = visit_signal set prefn postfn arg s in
      let arg = postfn arg signal in
      arg, set
    | Signal_reg      (a,w,clk,rst,rstval,ena,d) -> 
      let arg = prefn arg signal in
      let arg,set = List.fold (fun (arg,set) s -> visit_signal set prefn postfn arg s) (arg,set) [ clk; rst; rstval; ena; d ] in
      let arg = postfn arg signal in
      arg, set
    | Signal_mem      (a,dw,aw,size,clk,w,we,d,r) -> 
      let arg = prefn arg signal in
      let arg,set = List.fold (fun (arg,set) s -> visit_signal set prefn postfn arg s) (arg,set) [ clk; w; we; d; r ] in
      let arg = postfn arg signal in
      arg, set 
    | Signal_behave   (a,w,b,d) -> 
        let arg = prefn arg signal in
        let arg,set = List.fold (fun (arg,set) s -> visit_signal set prefn postfn arg s) (arg,set) d in
        let arg = postfn arg signal in
        arg, set 
    | Signal_inst     (a,n,m,g,io,i,o) -> 
        let arg = prefn arg signal in
        let arg,set = List.fold (fun (arg,set) s -> visit_signal set prefn postfn arg s) (arg,set) (List.map snd (i@io)) in
        let arg = postfn arg signal in
        arg, set 
#if INST2
    | Signal_inst2    (a,s,g,io,i,o) -> 
        let arg = prefn arg signal in
        let arg,set = fold_left (fun (arg,set) s -> visit_signal set prefn postfn arg s) (arg,set) (map snd (i@io)) in
        let arg = postfn arg signal ing
        arg, set 
#endif
    | Signal_tri      (a,w,d) -> 
      let arg = prefn arg signal in
      let arg,set = List.fold (fun (arg,set) s -> visit_signal set prefn postfn arg s) (arg,set) ((List.map fst d) @ (List.map snd d)) in
      let arg = postfn arg signal in
      arg, set

(** Visit all signals in the given list (typically a list of output signals).  The argument is "folded" between signals *)
let visit_signal_list prefn postfn arg signals =
  let arg, set = List.fold 
    (fun (arg,set) signal -> visit_signal set prefn postfn arg signal) (arg,Set.empty) signals in
  arg

(** Visit all signals in the given list (typically a list of output signals).  The argument is applied to each call. *)
let visit_signal_map prefn postfn arg_def signals =
  let args, _ = List.fold 
    (fun (args,set) signal -> 
      let arg,set = visit_signal set prefn postfn arg_def signal in
      (arg::args,set)
    ) ([],Set.empty) signals in
  List.rev args

(** default function argument for the visit_ functions.  Returns the argument. *)
let def_arg arg signal = arg 
(** default function argument for the visit_ functions.  Returns the signal. *)
let def_signal arg signal = signal

(* ************************************************************)

(** Debug.  Prints the behavioural assignment tree *)
let rec print_behave f i nodes = List.iter (print_behave_node f i) nodes
(** Debug.  Prints the behavioural assignment tree node *)
and print_behave_node (f:System.IO.TextWriter) i node = 
  let os (s:string) = f.Write(s) in
  match node with
  | B_if(cond, on_true, on_false) -> (
    os (i ^ "b_if (" ^ cond.name ^ ") [\n");
    print_behave f (i^" ") on_true;
    os (i ^ "] [\n");
    print_behave f (i^" ") on_false;
    os (i ^ "];\n")
  )
  | B_switch(cond, cases) -> (
    os (i ^ "b_switch (" ^ cond.name ^ ") [\n");
    List.iter (fun (idx, statements) -> 
      os (i ^ " b_case (constb \"" ^ string_of_const idx ^ "\") [\n");
      print_behave f (i^"  ") statements;
      os (i ^ " ];\n");
    ) cases;
    os (i ^ "];\n")
  )
  | B_assign(B_assign_tgt(l,r,d,hi,lo), expr) ->
    os (i ^ "(" ^ l.name ^ ", " ^ r.name ^ ", " ^ d.name ^ ")[" ^ string hi ^ ":" ^ string lo ^ "] |== " ^ expr.name ^ ";\n") 

(* *********************************************************** *)

/// Takes a circuit and creates a schedule based on the dependancies required. 
/// It will return a schedule for signals in the remaining list.  If there are 
/// signals in the computed list they will be used as part of the calculation 
/// but not returned
let scheduler dependants remaining computed =
  let debug = true in
  let set_add_list set l = List.fold (fun set (signal : Signal) -> Set.add (signal.uid) set) set l in

  let rec scheduler remaining computed computed_set = 
    let failed() = 
      if debug then (
        let os (s:string) = stdout.Write(s) in
        os "Remaining\n";
        List.iter (fun s -> 
          os (string_of_signal s ^ " - ");
          List.iter (fun s -> os (string_of_signal s ^ " ")) (dependants s);
          os "\n";
        ) remaining;
        os "Computed\n";
        List.iter (fun s -> os (string_of_signal s ^ "\n")) computed; 
      );
      failwith "No cells can be scheduled"
    in
    
    if remaining = [] then
      List.rev computed
    else
      let is_computed (signal : Signal) = Set.contains (signal.uid) computed_set in
      let is_ready (signal : Signal) = List.forall is_computed (List.filter ((<>) Signal.empty) (dependants signal)) in
      let ready, not_ready = List.partition is_ready remaining in
      if ready = [] then failed();
      scheduler not_ready (ready @ computed) (set_add_list computed_set ready)
  in
  
  scheduler remaining [] (set_add_list Set.empty computed)

/// A signal lets you easily discern which signals it requires as input (fanin).  
/// This function a generates map which tells you what it drives (fanout). 
let connected_nodes_map outputs = 
  let rec connect_node (set,map) (signal:Signal) = 
    let is_visited (signal : Signal) set = Set.contains (signal.uid) set in
    if is_visited signal set then set, map
    else
      let set = Set.add (signal.uid) set in
      let add from_uid to_uid map = 
        match Map.tryFind from_uid map with
        | Some(to_uid_set) -> Map.add from_uid (Set.add to_uid to_uid_set) map
        | None -> Map.add from_uid (Set.singleton to_uid) map
      in
      let map = List.fold (fun map (s:Signal) -> add s.uid signal.uid map) map signal.dependants in
      connect_nodes (set,map) signal.dependants 
  and connect_nodes (set,map) signals  = List.fold connect_node (set,map) signals in
  let set, map = connect_nodes (Set.empty, Map.empty) outputs in
  Map.map (fun _ s -> (Set.count s, Set.toList s)) map

(* *********************************************************** *)

(** Data type derived from a circuits outputs used for code generation and simulation *)
type Circuit = Circuit of 
  Signal list * Signal list * Signal list *             (* inputs, outputs, inouts *)
  Signal list * Signal list *                           (* wires, regs *)
  Signal list * Signal list * Signal list *             (* memories, logic nodes, constants *)
  Signal list *                                         (* instances *)
  Signal list *                                         (* instances2 *)
  Uid Set * Uid Set * Uid Set *                         (* inputs, outputs, inouts *)
  Map<Uid, Signal> *                                    (* map of uid's to signals *)
  Map<Uid, int * Uid list> *                            (* fanout map *)
  NBits                                                 (* misc: max bits *)
  with
  
    (** Gets a circuits inputs *)
    member c.Inputs              = match c with Circuit(x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x
    (** Gets a circuits outputs *)                                                 
    member c.Outputs             = match c with Circuit(_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x
    (** Gets a circuits inouts *)                                                  
    member c.Inouts              = match c with Circuit(_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x
    (** Gets a circuits wires *)                                                   
    member c.Wires               = match c with Circuit(_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) -> x
    (** Gets a circuits registers *)                                               
    member c.Regs                = match c with Circuit(_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) -> x
    (** Gets a circuits memories *)                                                
    member c.Memories            = match c with Circuit(_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) -> x
    (** Gets a circuits logic nodes (binops, unops, shifts etc) *)                                                     
    member c.Logic               = match c with Circuit(_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) -> x
    (** Gets a circuits constants *)                                                     
    member c.Constants           = match c with Circuit(_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) -> x
    (** Gets a circuits instantiations *)                                          
    member c.Inst                = match c with Circuit(_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) -> x
    (** Gets a circuits instantiations *)                                          
    member c.Inst2               = match c with Circuit(_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) -> x
    (** Set of inputs *)                                                
    member c.InputsSet           = match c with Circuit(_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) -> x
    (** Set of outputs *)                                               
    member c.OutputsSet          = match c with Circuit(_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) -> x
    (** Set of inouts *)                                                
    member c.InoutsSet           = match c with Circuit(_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) -> x
    (** Map of uids to signals *)                                                
    member c.Map                 = match c with Circuit(_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) -> x
    (** Map of uids to list of uids to which the signal connects *)                                          
    member c.FanoutMap           = match c with Circuit(_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) -> x
    (** Maximum signal width in a circuit *)                                              
    member c.MaxBitWidth         = match c with Circuit(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) -> x
                                                                             
    (** All signals in a circuit except clocks and resets *)                   
    member c.All                 = match c with Circuit(a,b,c,d,e,f,g,h,i,j,_,_,_,_,_,_) -> (a@b@c@d@e@f@g@h@i@j)
    (** all circuit input, output and inout ports *)                            
    member c.Io                  = (c.Inputs) @ (c.Outputs) @ (c.Inouts)

    (** Creates a circuit datatype given it's inouts and outputs.  The analysis performed here is used for code generation simulation and general validity checks. *)
    static member Create(inouts, outputs) = 
      (* outputs must be assigned wires with names *)
      List.iter (fun (x:Signal) -> 
        if not (x.IsWire) then failwith "Circuit outputs must be wires"
        else if wire_name x = "" then failwith "Circuit outputs must be named"
        else if (wire_connection x).IsEmpty then failwith ("Output " ^ wire_name x ^ " should be driven by something")
      ) (outputs @ inouts);
      let output_set       = List.fold (fun set (s : Signal) -> Set.add (s.uid) set) Set.empty outputs in
      let inout_set        = List.fold (fun set (s : Signal) -> Set.add (s.uid) set) Set.empty inouts in
      let is_output (signal : Signal) = Set.contains (signal.uid) output_set in
      let is_inout (signal : Signal)  = Set.contains (signal.uid) inout_set in

      (* record all signals except outputs and inouts *)
      let all_signals outputs = 
        let inputs,wires,regs,mems,nodes,consts,inst,inst2,bits = visit_signal_list 
        (fun (inps,wires,regs,mems,nodes,consts,inst,inst2,bits) (signal : Signal) -> 
          let max_bits() = if (signal.width) > bits then (signal.width) else bits in
          let dependants = signal.dependants in
          List.iter (fun x -> if (is_output x) then failwith "Circuit outputs may not be read by other logic") dependants;
          match signal.signal with
          | Signal_empty -> 
            (inps, wires, regs, mems, nodes, consts, inst, inst2, max_bits())
          | Signal_wire(q,w,n,d) -> 
            if (is_inout signal) || (is_output signal) then (inps, wires, regs, mems, nodes, consts, inst, inst2, max_bits())
            else if (!d).IsEmpty then (signal::inps, wires, regs, mems, nodes, consts, inst, inst2, max_bits())
            else (inps, signal::wires, regs, mems, nodes, consts, inst, inst2, max_bits())
          | Signal_reg(_,_,clk,rst,rstval,_,_) -> 
            (inps, wires, signal::regs, mems, nodes, consts, inst, inst2, max_bits())
          | Signal_mem(_,_,_,_,clk,_,_,_,_) -> 
            (inps, wires, regs, signal::mems, nodes, consts, inst, inst2, max_bits())
          | Signal_inst(a,n,m,g,io,i,o) ->
            let add_list l set = List.fold (fun set a -> Set.add a set) set l in
            (inps, wires, regs, mems, nodes, consts, signal::inst, inst2, max_bits())
#if INST2
          | Signal_inst2(a,n,g,io,i,o) ->
            let add_list l set = fold_left (fun set a -> Set.add a set) set l in
            (inps, wires, regs, mems, nodes, consts, inst, signal::inst2, max_bits())
#endif
          | Signal_const(_) ->
            (inps, wires, regs, mems, nodes, signal::consts, inst, inst2, max_bits())
          | _ -> 
            (inps, wires, regs, mems, signal::nodes, consts, inst, inst2, max_bits())
        ) def_arg ([],[],[],[],[],[],[],[],0) (List.map (fun o -> wire_connection o) outputs) in

        inputs, wires, regs, mems, nodes, consts, inst, inst2, bits
      in

      let signals = all_signals (outputs @ inouts) in
      let inputs, wires, regs, mems, nodes, consts, inst, inst2, max_bit_width = signals in

      (* inputs must be unassigned wires with names *)
      List.iter (fun (x:Signal) -> 
        if not (x.IsWire) then failwith "Circuit inputs must be wires"
        else if wire_name x = "" then failwith "Circuit inputs must be named"
        else if not (wire_connection x).IsEmpty then failwith ("Input " ^ wire_name x ^ " must not be connected to anything")
      ) inputs;

      let all_map = 
        List.fold (fun map (s : Signal list) ->
          List.fold (fun map (s:Signal) -> Map.add s.uid s map) map s
        ) Map.empty [ inputs; outputs; inouts; wires; regs; mems; nodes; consts; inst; inst2 ] in
        
      let uid (s:Signal) = s.uid in
      
      Circuit(
        List.sortWith (fun (s0 : Signal) (s1 : Signal) -> compare (s0.uid) (s1.uid)) inputs,
        List.sortWith (fun (s0 : Signal) (s1 : Signal) -> compare (s0.uid) (s1.uid)) outputs,
        List.sortWith (fun (s0 : Signal) (s1 : Signal) -> compare (s0.uid) (s1.uid)) inouts,
        List.sortWith (fun (s0 : Signal) (s1 : Signal) -> compare (s0.uid) (s1.uid)) wires,
        regs,mems,nodes,consts,inst,inst2,
        Set.ofList (List.map uid inputs),
        Set.ofList (List.map uid outputs),
        Set.ofList (List.map uid inouts),
        all_map,
        connected_nodes_map (outputs @ inouts),
        max_bit_width)

    (* Is the uid or signal in the circuit *)

    member c.mem uid = Map.containsKey uid c.Map
    member c.mem (s:Signal) = Map.containsKey s.uid c.Map

    (* Get a signal based on it's uid *)
    member c.find uid = Map.find uid c.Map

    (* is the signal an input *)
    member c.IsInput uid = Set.contains uid c.InputsSet
    member c.IsInput (s:Signal) = Set.contains s.uid c.InputsSet

    (* is the signal an output *)
    member c.IsOutput uid = Set.contains uid c.OutputsSet
    member c.IsOutput (s:Signal) = Set.contains s.uid c.OutputsSet

    (* is the signal an inout *)
    member c.IsInout uid = Set.contains uid c.InoutsSet
    member c.IsInout (s:Signal) = Set.contains s.uid c.InoutsSet

  end

(** Creates a circuit datatype given it's inouts and outputs.  The analysis performed here is used for code generation simulation and general validity checks. *)
let create_io inouts outputs = Circuit.Create(inouts,outputs)

(** Creates a circuit datatype given it's outputs.  The analysis performed here is used for code generation simulation and general validity checks. *)
let create outputs = Circuit.Create([], outputs)

(** Find the inputs of a circuit given it's outputs *)
let find_inputs outputs = 
  visit_signal_list 
    (fun inputs signal -> 
      match signal.signal with
      | Signal_wire(q,w,n,d) -> 
        if (!d).IsEmpty
        then signal::inputs
        else inputs
      | _ -> inputs
    ) def_arg [] (List.map (fun o -> wire_connection o) outputs) 

(** Run a circuit generator to create a file. *)
let write_file fn path n ext outputs = 
  let path = 
    match path with
    | "" -> ""
    | _ -> 
      if (path.[path.Length-1] <> '/') && (path.[path.Length-1] <> '\\') then 
        path ^ "/"
      else 
        path
  in
  let f = System.IO.File.CreateText(path ^ n ^ ext) in
  fn f n outputs;
  f.Close()
