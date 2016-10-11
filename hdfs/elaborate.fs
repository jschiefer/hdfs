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

(** Replace instantiations with circuits. *)
module DigitalLogic.Elaborate

open DigitalLogic
open Circuit
open Signal
open System.Reflection
open List

exception Dynload_method of MethodInfo
exception Elaborate_error of string

(** Raises the exception Elaborate_error *)
let failwith s = raise (Elaborate_error s)

(** Loads a circuit from the given dll using reflection *)
let load_circuit_from_dll path name = 
  try
    let assembly = Assembly.LoadFrom(path ^ name ^ ".dll") in
    let typ = assembly.GetType(String.capitalize name) in
    let methods = typ.GetMethods() in
    Some (Array.find (fun (m : MethodInfo) -> m.Name = name) methods)
  with 
  | _ -> None


(** Takes a circuit with instantiation nodes and replaces them with circuits loaded from the given dll's, or from the list of circuits provided to the function *)
let rec elaborate circuit circuits paths = 
  let os (s:string) = stdout.Write(s) in
  
  (* Copy a circuit, renumber it's nodes and create new references.  (This is WAY harder than I thought it would be but you have to be careful here) *)
  let rec renumber_node set signal = 
    let is_visited set (signal : Signal) = Set.mem (signal.uid) set in
    if is_visited set signal then (signal,set)
    else
      let set = Set.add (signal.uid) set in
      let su x = { su = x } in
      match signal.signal with
      | Signal_empty                                -> 
        Signal.empty, set
      | Signal_const    (a,w,c)                     -> 
        signal, set (* no point in renumbering constants *)
      | Signal_binop    (a,w,op,s0,s1)              -> 
        let a = signal_incr_uid() in 
        let s0, set = renumber_node set s0 in
        let s1, set = renumber_node set s1 in
        su (Signal_binop(a,w,op,s0,s1)), set
      | Signal_unop     (a,w,op,s)                  -> 
        let a = signal_incr_uid() in 
        let s, set = renumber_node set s in
        su (Signal_unop(a,w,op,s)), set
      | Signal_wire     (a,w,n,d)                   -> 
        let a = signal_incr_uid() in 
        let d, set = renumber_node set !d in
        su (Signal_wire(a,w,ref !n,ref d)), set
      | Signal_mux      (a,w,sel,d)                 -> 
        let a = signal_incr_uid() in 
        let sel, set = renumber_node set sel in
        let d, set = renumber_nodes set d in
        su (Signal_mux(a,w,sel,d)), set
      | Signal_select   (a,hi,lo,s)                 -> 
        let a = signal_incr_uid() in 
        let s, set = renumber_node set s in
        su (Signal_select(a,hi,lo,s)), set
      | Signal_reg      (a,w,clk,rst,rstval,ena,d)  -> 
        let a = signal_incr_uid() in 
        let ena, set = renumber_node set ena in                                 (****** how should clocks be renumbered ? ******)
        let d, set = renumber_node set d in
        su (Signal_reg(a,w,clk,rst,rstval,ena,d)), set
      | Signal_mem      (a,dw,aw,size,clk,w,we,d,r) -> 
        let a = signal_incr_uid() in 
        let w, set = renumber_node set w in
        let we, set = renumber_node set we in
        let d, set = renumber_node set d in
        let r, set = renumber_node set r in
        su (Signal_mem(a,dw,aw,size,clk,w,we,d,r)), set
      | Signal_behave   (a,w,b,d)                   -> 
        let a = signal_incr_uid() in 
        let d, set = renumber_nodes set d in
        su (Signal_behave(a,w,b,d)), set
      | Signal_inst     (a,n,m,g,io,i,o)             ->                          (* not sure about io here *)
        let a = signal_incr_uid() in 
        let ii, set = renumber_nodes set (map snd i) in
        let ioo, set = renumber_nodes set (map snd io) in
        su (Signal_inst(a,n,m,g, map2 (fun i ii -> fst i, ii) io ioo, map2 (fun i ii -> fst i, ii) i ii, o)), set
    | Signal_tri      (a,w,d) -> 
        let a = signal_incr_uid() in 
        let oe = map fst d in
        let dd = map snd d in
        let oe, set = renumber_nodes set oe in
        let dd, set = renumber_nodes set dd in
        let d = map2 (fun x y -> (x,y)) oe dd in
        su (Signal_tri(a,w,d)), set

  and renumber_nodes set signals = 
    let signals, set = fold_left 
      (fun (signals,set) signal -> 
        let signal,set = renumber_node set signal in
        (signal::signals,set)
      ) ([],Set.empty) signals in
    rev signals, set
  in
  
  let renumber_nodes signals = 
    let signals,set = renumber_nodes Set.empty signals in
    signals
  in
  
  (* look through the list of circuits for a match *)
  let find_circuit name = 
    try
      let c = List.assoc name circuits in
      Some(renumber_nodes c)
    with _ -> None
  in

  (* find dll from a list of paths *)
  let rec find_dll paths name = 
    match paths with
    | [] -> None
    | path :: paths -> (
      match load_circuit_from_dll path name with
      | None -> find_dll paths name 
      | Some x -> Some (unbox (x.Invoke(null,null))) (* should really do more error checking here *)
    )
  in

  (* remove the name from the input and output wires of the subcircuit as it may well clash *)
  let clear_name (w : Signal) = 
    match w.signal with
    | Signal_wire(_,_,n,_) -> n := ""; w
    | _ -> failwith "Expecting a wire"
  in

  (* Same as the <== operator except allows reassignment of wires.  For internal use only. *)
  let (|<==|) a b =
    Signal.check_same [a;b];
    match a.signal with
    | Signal_wire (_,_,_,d) -> 
      check_comb a b;
      d := b
    | _ -> (failwith "Must only assign to wires")
  in

  let connect_inputs inst_inputs inputs = 
    iter (fun wire ->
      let input_name = wire_name wire in
      let inst_input = 
        try find (fun x -> (fst x) = input_name) inst_inputs
        with _ -> failwith ("Could not find input connection for " ^ input_name) in
      (clear_name wire) |<==| (snd inst_input)        
    ) inputs
  in

  let connect_outputs inst_outputs outputs = 
    iter (fun wire ->
      let output_name = wire_name wire in
      let inst_output = 
        try find (fun x -> (fst x) = output_name) inst_outputs
        with _ -> failwith ("Could not find output connection for " ^ output_name) in
      (snd inst_output) |<==| (clear_name wire)        
    ) outputs
  in

  (* find the instantiation in either the list of circuits (by preference) or from a dll *)
  let rec find_inst name = 
    match find_circuit name with
    | Some x -> 
      elab x;
      Some x
    | None -> (
      match find_dll paths name with
      | Some x -> 
        elab x;
        Some x
      | None -> None
    )

  (* Visit nodes in the circuit and as we find an instantiation replace 
     it with the appropraite circuit.  This will alter the data structure 
     on the fly as it is searched as we update the wire nodes. *)
  and elab outputs = 
    visit_signal_list
      (fun arg signal ->
        match signal.signal with
        | Signal_inst(a,n,m,g,io,i,o) -> (
          let replace = find_inst n in
          if g <> [] then failwith "Generics are not supported for elaboration";
          match replace with
          | None -> ()
          | Some(outputs) ->
            let inputs = find_inputs outputs in
            (* connect inputs and outputs *)
            connect_inputs i inputs;
            connect_outputs o outputs 
        )
        | _ -> ()
      ) def_arg () outputs
  in
  
  ignore (elab circuit)
