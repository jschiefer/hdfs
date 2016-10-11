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

namespace DigitalLogic

open System.Diagnostics
open DigitalLogic.Numeric
open DigitalLogic.Numeric.Conversions
open DigitalLogic.Numeric.ArrayBits
open DigitalLogic.Numeric.ResizingArray
open DigitalLogic.Circuit
open DigitalLogic.Signal
open List

(*--------------------------------------------------------------------------------------------*)
(*--------------------------------------------------------------------------------------------*)

(** Simulation of synchronous, single clock circuits *)
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Simulator =
begin

  exception Sim_error of string

  (** raise the exception Sim_error *)
  let failwith s = raise (Sim_error s) 

  (** simulator task *)
  type Task = unit -> unit

  (** simulator port (input, output or wire) *)
  type Port = 
    { 
      port_uid : Uid;
      port_name : string; 
      port_data : ArrayBits;
    }
    with
      (** Name of port *)
      member x.name = x.port_name
      (** Uid of port *)
      member x.uid = x.port_uid
      (** Underlying uint32 array *)
      member x.data = x.port_data.data
      (** Width of port in bits *)
      member x.width = x.port_data.width
      (** Width of port in words (32 bit) *)
      member x.words = x.port_data.words

      (** Provides a.[i] style item access to the uint32 array representing the port value *)
      member x.Item 
        with get((idx : int)) = x.data.[idx]
        and set (idx : int) (v : uint32) = x.data.[idx] <- v

      (* Map property accessors from ArrayBits to the prot *)

      (* Clear the port *)
      member x.clear = x.port_data.clear
      
      (* sign extend port set with a signed 8, 16 or 32 bit property *)
      member x.sign_extend width = x.port_data.sign_extend width

      /// int access property with signed get
      member x.i
        with get() = x.port_data.i
        and set(v : int) = x.port_data.i <- v

      /// int access property with unsigned get (32 bit values will turn out signed though)
      member x.u
        with get() = x.port_data.u
        and set(v : int) = x.port_data.u <- v

      /// sbyte access property      
      member x.y
        with get() = x.port_data.y
        and set(v : sbyte) = x.port_data.y <- v
        
      /// byte access property       
      member x.uy
        with get() = x.port_data.uy
        and set(v : byte) = x.port_data.uy <- v
        
      /// int16 access property      
      member x.s
        with get() = x.port_data.s
        and set(v : int16) = x.port_data.s <- v
        
      /// uint16 access property     
      member x.us
        with get() = x.port_data.us
        and set(v : uint16) = x.port_data.us <- v
        
      /// int32 access property      
      member x.l
        with get() = x.port_data.l
        and set(v : int32) = x.port_data.l <- v
        
      /// uint32 access property     
      member x.ul 
        with get() = x.port_data.ul
        and set(v : uint32) = x.port_data.ul <- v

      /// uint64 access property     
      member x.UL
        with get() = x.port_data.UL
        and set(v : uint64) = x.port_data.UL <- v

      /// int64 access property      
      member x.L
        with get() = x.port_data.L
        and set(v : int64) = x.port_data.L <- v

      // TODO: BigInt's (need to convert numeric based stuff to F# BigInt rather than Big_int so we can use the nice operator based API)

      // Using binary strings for access
      member x.b
        with get() = x.port_data.b
        and set(v : string) = x.port_data.b <- v

      // Using hex strings for access
      member x.hs
        with get() = x.port_data.hs
        and set(v : string) = x.port_data.hs <- v

      member x.hu
        with get() = x.port_data.hu
        and set(v : string) = x.port_data.hu <- v

      member x.a
        with get() = x.port_data
        and set(v:Numeric.ArrayBits.ArrayBits) = x.port_data.a <- v

    end

  (** Simulator data type *)
  type Simulator = 
    {
      sim_circuit  : Circuit;                       (* circuit simulator was built from *)
      sim_reset    : Task;                          (* reset *)
      sim_cycle    : Task;                          (* cycle *)
      sim_inputs   : Port list;                     (* inputs *)
      sim_wires    : Port list;                     (* wires *)
      sim_outputs  : Port list;                     (* outputs *)
      sim_port_map : Map<Uid,Port>;                 (* Map of port Uid's to Ports *)
      sim_name_map : Map<string,Uid>;               (* Map of port names to Ports *)
      sim_data_map : Map<Uid,ArrayBits>;            (* Internal scheduled data nodes *)
      sim_reg_map  : Map<Uid,ArrayBits>;            (* Internal register nodes *)
      sim_mem_map  : Map<Uid,ArrayBits->ArrayBits>; (* Internal memory *)
    }
    with
      (** Resets the simulator *)
      member x.reset = x.sim_reset()
      (** Performs a simulation cycle *)
      member x.cycle = x.sim_cycle()
      
      member x.find_port s (l:Port list) = 
        match l with 
        | [] -> failwith ("No port with name " ^ s ^ " could be found")
        | hd::tl -> if hd.name = s then hd else x.find_port s tl
      
      (** List of input ports *)
      member x.inputs = x.sim_inputs
      (** List of output ports *)
      member x.outputs = x.sim_outputs
      (** List of named wire ports *)
      member x.wires = x.sim_wires
      (** List of all ports *)
      member x.ports = (x.inputs @ x.outputs @ x.wires)

      (** Find a port given a Uid *)
      [<OverloadID("sim_port_uid")>]
      member x.port (uid:Uid) = match Map.tryfind uid x.sim_port_map with Some(x) -> x | _ -> failwith ("Could not find port " ^ string uid)
      (** Find a port given a Signal *)
      [<OverloadID("sim_port_signal")>]
      member x.port (signal:Signal) = x.port signal.uid
      (** Find a port given a string name *)
      [<OverloadID("sim_port_string")>]
      member x.port (str:string) = try x.port (Map.find str x.sim_name_map) with _ -> failwith ("Could not find port " ^ str)

      (** Does the given name represent an input *)
      member x.is_input str = try x.find_port str x.sim_inputs |> ignore; true with _ -> false
      (** Does the given name represent an output *)
      member x.is_output str = try x.find_port str x.sim_outputs |> ignore; true with _ -> false
      (** Does the given name represent a named wire *)
      member x.is_wire str = try x.find_port str x.sim_wires |> ignore; true with _ -> false

      (** Lookup a port given a Signal *)
      [<OverloadID("sim_item_signal")>]
      member x.Item with get(idx : Signal) = x.port idx

    end

  (** Creates a simulator given a circuit and an option value which if Some(file_handle) *)
  (** will generate a trace log file. *)
  let create' (trace_file:System.IO.TextWriter option) (circuit : Circuit) = 
    let trace, trace_file = match trace_file with None -> false, stdout | Some(x) -> true, x in
    let os (s:string) = trace_file.Write(s) in
    
    let inputs = circuit.Inputs in
    let outputs = circuit.Outputs in
    let wires = circuit.Wires in
    let regs = circuit.Regs in
    let mems = circuit.Memories in
    let logic = circuit.Logic @ circuit.Constants in
    
    let words_of_signal s = ((width s) + 31) / 32 in

    let dependants (signal : Signal) =
      match signal.signal with
      | Signal_reg _ -> []
      | Signal_mem(_,_,_,_,_,w,we,data,r) -> [r] (* Reads are sheduled along with combinatorial logic.  Thus only the read address is needed as a dependant.
                                                    Writes update at the end of a simulator cycle. *)
      | Signal_wire(_,_,_,d) -> if !d = Signal.empty then [] else [!d] 
      | _ -> signal.dependants in

    let schedule = scheduler dependants (logic @ wires @ mems) (inputs @ regs) in

    (* Check that clocks and resets are simple wire (or chains thereof) or empty.  If not write warning that the circuit may not simulate as expected *)
    let rec check_clk_rst (s : Signal) = 
      if s.IsEmpty then true
      else if s.IsWire then
        check_clk_rst (wire_connection s)
      else false
    in
    let check_clk (s : Signal) = 
      if not (check_clk_rst s) then
        os ("*** WARNING: The clock signal " ^ s.name ^ " is driven by logic.\nThis is not supported by the simulator so simulation may not be correct.\n")
    in
    let check_rst (s : Signal) = 
      if not (check_clk_rst s) then
        os ("*** WARNING: The reset signal " ^ s.name ^ " is driven by logic.\nThis is not supported by the simulator so simulation may not be correct.\n")
    in
      
    (* test to see is the array data is all zero.  Assumes any unused tops bits are zero *)
    let is_zero (data:ArrayBits) = 
      let data = data.data in
      let len = Array.length data in
      let rec is_zero' i =
        if len = i then true
        else if data.[i] <> 0ul then false
        else is_zero' (i+1)
      in
      is_zero' 0
    in

    let make_data_map map (signal : Signal) = 
      match signal.signal with
      | Signal_empty    -> 
        map
      | Signal_const    (a,w,c) -> 
        let data = ArrayBits.of_string c in 
        Map.add signal.uid data map
      | Signal_binop    (_,w,_,_,_)  
      | Signal_unop     (_,w,_,_) 
      | Signal_wire     (_,w,_,_) 
      | Signal_mux      (_,w,_,_) 
      | Signal_behave   (_,w,_,_) -> 
        let data = ArrayBits.make w in
        Map.add signal.uid data map
      | Signal_select   (_,hi,lo,_) -> 
        let data = ArrayBits.make (hi-lo+1) in
        Map.add signal.uid data map
      | Signal_reg      (_,w,clk,rst,_,_,_) -> 
        let data = ArrayBits.make w in
        check_clk clk;
        check_rst rst;
        Map.add signal.uid data map
      | Signal_mem      (a,dw,aw,size,clk,w,we,data,r) -> 
        let data = ArrayBits.make dw in
        check_clk clk;
        Map.add signal.uid data map
      | Signal_inst     _ ->
        failwith "Instantiation not supported in simulation"
      | Signal_tri     _ ->
        failwith "Tristates not supported in simulation"
    in

    (* Configure the construction of memories *)
    let log_array_max_size = 12 in    (* Up to 4K rams are built with arrays *)
    let log_block_array_size = 8 in   (* After which they are split into 256 element chunks and stored in a map *)
    (* Build a map of memory arrays *)
    let make_mem_map map (signal : Signal) = 
      match signal.signal with
      | Signal_mem      (a,dw,aw,size,clk,w,we,data,r) -> 
        let lookup = f_mem log_array_max_size log_block_array_size dw aw in
        Map.add signal.uid lookup map
      | _ -> failwith "Expecting a memory"
    in

    let data_map = fold_left make_data_map Map.empty (inputs @ outputs @ wires @ regs @ mems @ logic) in
    let reg_map = fold_left make_data_map Map.empty regs in
    let mem_map = fold_left make_mem_map Map.empty mems in

    let find (signal : Signal) = Map.find signal.uid data_map in
    let find_reg (signal : Signal) = Map.find signal.uid reg_map in
    let find_mem (signal : Signal) = Map.find signal.uid mem_map in
    
    (* create functions which map behavioural code to assigns guarded by if and switch statements. *)
    let rec behave_tasks (tgt_data : ArrayBits) code = 
      let code = map (behave_task tgt_data) code in
      (fun () -> iter (fun x -> x()) code) 
    and behave_task tgt_data code = 
      match code with
      | B_if(cond, on_true, on_false) -> 
        let cond = find cond in
        let eval_true = behave_tasks tgt_data on_true in
        let eval_false = behave_tasks tgt_data on_false in
        (fun () -> 
          if is_zero cond then eval_false()
          else eval_true()
        )
        
      | B_switch(cond, cases) -> 
        let width_of_cond = width cond in
        let data_of_cond = find cond in
        (* map of case index constants to the tasks to execute. *)
        let case_tasks_map = fold_left (fun map (cond_match, exprs) -> 
          let data_of_cond_match = ArrayBits.of_string (string_of_const cond_match) in
          let code_of_case = behave_tasks tgt_data exprs in
          Map.add data_of_cond_match code_of_case map
        ) Map.empty cases in
        (fun () -> 
          match Map.tryfind data_of_cond case_tasks_map with
          | None -> ()
          | Some(x) -> x()
        )
        
      | B_assign(B_assign_tgt(_,_,_,_,_), expr) -> 
        let words = words_of_signal expr in
        let expr = (find expr).data in
        let tgt = tgt_data.data in
        (fun () -> 
          for i = 0 to (words-1) do
            tgt.[i] <- expr.[i]
          done)
    in

    let trace_log str (s : Signal) = 
      let fmt (s:Signal) = 
        let n = s.name in
        let d = find s in
        "[" ^ n ^ " - " ^ d.to_string ^ "]"
      in
      if trace then (fun () -> (str ^ " " ^ fmt s ^ " : " ^ (fold_strings " " (map fmt s.dependants))))
      else (fun () -> "")
    in
    
    let compile (signal : Signal) = 
      let trace str = trace_log str signal in
      match signal.signal with
      | Signal_empty 
      | Signal_const(_) -> None
        
      | Signal_binop    (a,w,op,s0,s1) -> 
      begin
        let r = find signal in
        let a = find s0 in
        let b = find s1 in
        match op with
        | B_add  -> Some(trace "add", ArrayBits.f_add r a b)
        | B_sub  -> Some(trace "sub", ArrayBits.f_sub r a b)
        | B_mulu -> Some(trace "mulu", ArrayBits.f_mulu r a b)
        | B_muls -> Some(trace "muls", ArrayBits.f_muls r a b)
        | B_and  -> Some(trace "and", ArrayBits.f_band r a b)
        | B_or   -> Some(trace "or", ArrayBits.f_bor r a b)
        | B_xor  -> Some(trace "xor", ArrayBits.f_bxor r a b)
        | B_eq   -> Some(trace "eq", ArrayBits.f_eq r a b)
        | B_lt   -> Some(trace "lsu", ArrayBits.f_lsu r a b)
        | B_cat  -> Some(trace "cat", ArrayBits.f_cat r a b)
      end

      (* Bitwise not. *)
      | Signal_unop     (a,w,op,s) -> Some(trace "not", ArrayBits.f_bnot (find signal) (find s))
        
      (* Wires.  Differentiate between input wires and internal wires. *)
      | Signal_wire     (a,w,n,data) -> 
        let tgt_data = find signal in
        if !data = Signal.empty then 
          (* should be an input - mask the data so the user cant overflow numbers *)
          Some(trace "input", ArrayBits.f_mask tgt_data)
        else
          (* copy data - no need to mask as the source will already have the correct masking applied *)
          Some(trace "wire", ArrayBits.f_copy tgt_data (find !data))
      
      (* Multiplexer. *)
      | Signal_mux      (a,w,sel,dlist) ->
        let max_mux = 31 in (* mux's are built with arrays if select.width < max_mux, otherwise with a map *)
        let target = (find signal) in
        let select = (find sel) in
        let lookup = f_mux max_mux select.width (map (fun d -> find d) dlist) in
        Some(trace "mux", (fun () -> (f_copy target (lookup select))() ))
        
      (* Selection.  Make some effort to identify special cases here as this is a common operation *)
      | Signal_select   (a,hi,lo,s) -> 
        Some(trace "select", f_select (find signal) (find s) hi lo)

      (* Registers must update in two parts.  First we copy the data to the reg_map.  
         Later we'll copy to the actual value (which is read by other tasks).
         This is because registers which read other registers are not scheduled.  *)
      | Signal_reg      (a,w,clk,rst,rstval,ena,data) -> 
        let tgt_data = find_reg signal in
        let src_data = find data in
        if ena = Signal.empty then Some(trace "reg", f_copy tgt_data src_data)
        else
          let ena_data = find ena in 
          Some(trace "reg", f_copy_ena tgt_data ena_data src_data)

      (* Memory reads *)
      | Signal_mem      (a,dw,aw,size,clk,w,we,data,r) -> 
        let q = find signal in
        let r = find r in
        let lookup = find_mem signal in
        Some(trace "mem_read", (fun () -> (f_copy q (lookup r))() ))
        
      (* Behavioural tasks *)
      | Signal_behave   (a,w,b,dl) -> 
        let task = behave_tasks (find signal) b in
        Some(trace "behave", fun () -> task())
        
      (* Stuff that is not supported by the simulator *)
      | Signal_inst     (a,n,m,g,io,i,o) ->
        failwith "Instantiation nodes not supported in simulation"
        
      | Signal_tri     _ ->
        failwith "Tristate nodes not supported in simulation"
        
    in
    
    let compile_reg_update (signal : Signal) = 
      let trace str = trace_log str signal in
      match signal.signal with
      | Signal_reg      (_) ->
        let q0 = find_reg signal in
        let q1 = find signal in
        Some(trace "reg_update", f_copy q1 q0)
      | _ -> failwith "Expecting register or memory"
    in
    
    (* Reset both sets of register data values *)
    let rec compile_reg_reset (signal : Signal) = 
      let trace str = trace_log str signal in
      match signal.signal with
      | Signal_reg(a,w,clk,rst,rstval,ena,data) -> 
        let wid = width signal in
        let q = find signal in
        let q_s = find_reg signal in
        if rst = Signal.empty then None (* do nothing *)
        else if rstval = Signal.empty then Some(trace "reset", (fun () -> (f_zero q)() ; (f_zero q_s)() )) (* reset to zero *)
        else Some(trace "reset", (fun () -> (f_copy q (find rstval))(); (f_copy q_s (find rstval))() )) (* reset with given value *)
      | _ -> failwith "Expecting a register"
    in

    let compile_mem_write (signal : Signal) = 
      let trace str = trace_log str signal in
      match signal.signal with
      | Signal_mem      (a,dw,aw,size,clk,w,we,data,r) -> 
        let we = find we in
        let d = find data in
        let w = find w in
        let lookup = find_mem signal in
        Some(trace "mem_write", (fun () -> if we.data.[0] <> 0ul then (f_copy (lookup w) d)() ))
      | _ -> failwith "Expecting register or memory"
    in
    
    let rec make_tasks fn l = 
      match l with
      | [] -> []
      | hd :: tl -> 
        match fn hd with
        | None -> make_tasks fn tl
        | Some(n,x) -> 
          let x = 
            if trace then
              (fun () ->
                x();
                printf "%s\n" (n())
              )
            else x in
          x :: make_tasks fn tl
    in

    let scheduled_tasks    = make_tasks compile schedule in
    let input_tasks        = make_tasks compile inputs in
    let output_tasks       = make_tasks compile outputs in
    let reg_tasks          = make_tasks compile regs in
    let reg_update_tasks   = make_tasks compile_reg_update regs in
    let mem_write_tasks    = make_tasks compile_mem_write mems in
    let reg_resets         = make_tasks compile_reg_reset regs in

    (*
    printf "scheduled_tasks    %i\n" (List.length scheduled_tasks    );
    printf "input_tasks        %i\n" (List.length input_tasks        );
    printf "output_tasks       %i\n" (List.length output_tasks       );
    printf "reg_tasks          %i\n" (List.length reg_tasks          );
    printf "reg_finalize_tasks %i\n" (List.length reg_finalize_tasks );
    printf "reg_resets         %i\n" (List.length reg_resets         );
    *)
    
    let reset = 
      let f = (fun () -> iter (fun x -> x()) reg_resets) in
      if trace then
        (fun () -> printf "********* RESET\n"; f())
      else f
    in
    
    let cycle = 
      let f = (fun () -> iter (fun x -> x()) 
        (input_tasks @ scheduled_tasks @ output_tasks @    (* Combinatorial logic *)
         mem_write_tasks @ reg_tasks @ reg_update_tasks))  (* Sequential logic *)
      in
      if trace then
        let cycle = ref 0 in
        (fun () -> printf "********* CYCLE %i\n" !cycle; cycle := !cycle + 1; f())
      else f
    in
    
    let mk_port (s:Signal) = { port_uid = s.uid; port_name = s.name; port_data = find s } in
    let inputs  = map mk_port inputs in
    let wires = (filter (fun s -> (wire_connection s <> Signal.empty) && (wire_name s <> "")) wires) in
    let wires   = map mk_port wires in
    let outputs = map mk_port outputs in
    {
      sim_circuit  = circuit;
      sim_reset    = reset;
      sim_cycle    = cycle;
      sim_inputs   = inputs;
      sim_wires    = wires;
      sim_outputs  = outputs;
      sim_port_map = fold_left (fun map (s:Port) -> Map.add s.uid s map) Map.empty (inputs @ wires @ outputs);
      sim_name_map = fold_left (fun map (s:Port) -> Map.add s.name s.uid map) Map.empty (inputs @ wires @ outputs);
      sim_data_map = data_map;
      sim_reg_map  = reg_map;
      sim_mem_map  = mem_map;
    }

  (** Creates a simulator given a list of inputs. *)
  let create = create' None
    
  (** Combine 2 simulators into a single simulator and record differences.  *)
  (** The combination is made with regard to port names which requires some care *)
  (** with the use of signal_init() depending on how the circuits are derived. *) 
  let combine (s0:Simulator) (s1:Simulator) sim_error_fn = 
    {
      s0 with
      sim_reset    = (fun () -> s0.reset; s1.reset);
      sim_cycle    = 
        (fun () -> 
          (* copy inputs from s0 to s1 *)
          iter (fun (s:Port) -> try (f_copy (s1.port s.name).port_data s.port_data)() with _ -> ()) s0.inputs;
          s0.cycle; 
          s1.cycle;
          (* Compare s0 wire with s1 *)
          let different s0 s1 = (ArrayBits.eq s0.port_data s1.port_data).data.[0] = 0ul in
          iter (fun (s0:Port) -> 
            try 
              let s1 = s1.port s0.name in
              if different s0 s1 then sim_error_fn s0 s1 
            with _ -> ()) s0.wires;
          (* Compare s0 outputs with s1 *)
          iter (fun (s0:Port) -> 
            try 
              let s1 = s1.port s0.name in
              if different s0 s1 then sim_error_fn s0 s1 
            with _ -> ()) s0.outputs
        );
    }

  (** Record simulation data in a resize array.  Returns arrays (which are updated as the simulation cycles) *)
  (** for inputs, named wires and outputs *)
  let record_ports (s0:Simulator) (ports:Port list) enable = 
    let cycle = ref 0 in
    let mk_ra (p:Port) = ResizingArray.create 32 (ArrayBits.make p.width), p in
    let ra_ports = map mk_ra ports in
    let cycle () = 
      let record ((ra:ResizingArray<ArrayBits>), (p:Port)) = ra.[!cycle] <- ArrayBits.copy p.port_data in
      (* cycle *)
      s0.cycle;
      if !enable then
      begin
        (* record ports *)
        iter record ra_ports;
        (* Increment cycle *)
        cycle := !cycle + 1
      end
    in
    { s0 with sim_cycle = cycle }, ra_ports
      
  type Simulator
    with
        (** Record simulation data *)
        member x.record = record_ports x (x.inputs @ x.wires @ x.outputs) (ref true)
        (** Record simulation data for inputs and output only *)
        member x.record_io = record_ports x (x.inputs @ x.outputs) (ref true)
        (** Record simulation data for inputs and output only *)
        member x.record_en en = record_ports x (x.inputs @ x.wires @ x.outputs) en
        
        (* access to internal nodes (this is largely provided to access internal memorys if needed) *)

        [<OverloadID("data_node_0")>]
        member x.data_node uid = Map.find uid x.sim_data_map 
        [<OverloadID("data_node_1")>]
        member x.data_node (signal:Signal) = x.data_node signal.uid
        
        [<OverloadID("reg_node_0")>]
        member x.reg_node uid = Map.find uid x.sim_reg_map 
        [<OverloadID("reg_node_1")>]
        member x.reg_node (signal:Signal) = x.reg_node signal.uid
        
        [<OverloadID("mem_lookup_0")>]
        member x.mem_lookup uid = Map.find uid x.sim_mem_map 
        [<OverloadID("mem_lookup_1")>]
        member x.mem_lookup (signal:Signal) = x.mem_lookup signal.uid
        
        [<OverloadID("mem_addr_0")>]
        member x.mem_addr ((uid:Uid),(addr:ArrayBits)) = (x.mem_lookup uid) addr
        [<OverloadID("mem_addr_1")>]
        member x.mem_addr ((signal:Signal),(addr:ArrayBits)) = x.mem_addr(signal.uid, addr)

        [<OverloadID("mem_addr_2")>]
        member x.mem_addr (uid,(addr:int)) = 
          match (x.sim_circuit.find uid).signal with
          | Signal_mem(a,dw,aw,size,clk,w,we,data,r) -> x.mem_addr (uid, ArrayBits.of_int aw addr)
          | _ -> failwith "Expecting a memory"
        [<OverloadID("mem_addr_3")>]
        member x.mem_addr ((signal:Signal),(addr:int)) = x.mem_addr(signal.uid, addr)

    end

  (* **************************************************** *)
  (* running external tools *)
  (* this is used by the cosim and csim modules *)
  (* **************************************************** *)

  (** run an external (shell) command and wait for it to complete.  Optionally display its stdout. *)
  let execute_command' show_output exe_name args =
    let p = new Process() in
    p.StartInfo.UseShellExecute <- false;
    p.StartInfo.RedirectStandardOutput <- true;
    p.StartInfo.FileName <- exe_name;
    p.StartInfo.Arguments <- args;
    let _ = p.Start() in
    let output = p.StandardOutput.ReadToEnd() in
    p.WaitForExit(); 
    if show_output then stdout.Write(output)
    else ()

  (** run an external (shell) and wait for it to complete.  Dont display its stdout. *)
  let execute_command = execute_command' false

  (** run an external (shell) command and return the process handle without waiting *)
  let run_command exe_name args = 
    let p = new Process() in
    p.StartInfo.UseShellExecute <- false;
    //p.StartInfo.RedirectStandardOutput <- true;
    p.StartInfo.FileName <- exe_name;
    p.StartInfo.Arguments <- args;
    let _ = p.Start() in
    p


end

