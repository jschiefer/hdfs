(** THIS MODULE IS NOT YET COMPLETED *)
module DigitalLogic.Edif

#nowarn "62"    // Using ^ for string concatenation
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

open DigitalLogic.Circuit
open DigitalLogic.Signal

type PortRef = 
  | InstRefIn of string * Signal            (* Name of (input) port, and instance signal *)
  | InstRefOut of string * string * Signal  (* Name of (output) port, name of instance, and output wire *)
  | NetRef of Signal                        (* (output) wire *)
  with
    member x.signal = match x with InstRefIn(_,s) | InstRefOut(_,_,s) | NetRef(s) -> s
    member x.Ref = 
      match x with 
      | NetRef(s) -> ("(portRef " ^ s.name ^ ")")
      | InstRefIn(p,s) -> ("(portRef " ^ p ^ " (instanceRef " ^ s.name ^ "))")
      | InstRefOut(p,n,s) -> ("(o_portRef " ^ p ^ " (instanceRef " ^ n ^ "))")
  end

(* The edif writer should output structural netlists.  That is only instantiation and wires are supported.  
   Circuit must be transformed into this format (ie synthesized) prior to writing as edif.
   Eventually this can become generic, however, we'll need to study various vendor specific edif netlists
   to see exactly what parameters are required.  At the moment we're aiming for Xilinx Virtex parts. *)
let write (f:System.IO.TextWriter) name (circuit : Circuit) = 
  let os (s:string) = f.Write(s) in
  let date = System.DateTime.Now in

  List.iter (fun x -> if x <> [] then failwith "Edif only supports instantiations and wires") [ circuit.Regs; circuit.Memories; circuit.Logic; circuit.Constants ];

  let insts = circuit.Inst in
  let date_string = fold_strings " " (List.map string [ date.Year; date.Month; date.Day; date.Hour; date.Minute; date.Second ]) in
  
  (* write the header *)
  os (
"(edif " ^ name ^ "
  (edifVersion 2 0 0)
  (edifLevel 0) 
  (keywordMap (keywordLevel 0))
  (status
    (written
      (timeStamp " ^ date_string ^ ")
      (author \"HDFS\")
      (program \"hdfs.dll\" (version \"" ^ Signal.hdfs_version ^ "\"))
    )
  )
  (library VIRTEX
    (edifLevel 0)
    (technology (numberDefinition))
");

  (* cell definitions *)
  let write_cells comp_set (signal : Signal) = 
    match signal.signal with
    | Signal_inst(a,n,m,g,io,i,o) ->
      if Set.contains n comp_set then comp_set
      else (
        os ("    (cell " ^ n ^ " (cellType GENERIC)\n");
        os ("      (view PRIM (viewType netlist)\n");
        os ("        (interface\n");
        List.iter (fun w -> 
          os ("          (port " ^ fst w ^ " (direction INOUT))\n");
        ) io;
        List.iter (fun w -> 
          os ("          (port " ^ fst w ^ " (direction INPUT))\n");
        ) i;
        List.iter (fun w -> 
          os ("          (port " ^ fst w ^ " (direction OUTPUT))\n");
        ) o;
        os ("        )\n");
        os ("      )\n");
        os ("    )\n");
        Set.add n comp_set
      )
    | _ -> failwith "Expecting instantiation"
  in
  ignore (List.fold (fun set i -> write_cells set i) Set.empty circuit.Inst);
  os ("  )\n");
  
  (* top level circuit *)
  os (
"  (library work
    (edifLevel 0)
    (technology (numberDefinition))
    (cell " ^ name ^ "(cellType GENERIC))
      (view hdfs (viewType NETLIST)
        (interface
");

  (* inputs/outputs *)
  List.iter (fun s ->
    os ("          (port " ^ wire_name s ^ " (direction INOUT)\n")
  ) (circuit.Inouts);
  List.iter (fun s ->
    os ("          (port " ^ wire_name s ^ " (direction INPUT)\n")
  ) (circuit.Inputs);
  List.iter (fun s ->
    os ("          (port " ^ wire_name s ^ " (direction OUTPUT)\n")
  ) (circuit.Outputs);
  os ("        )\n");
  os ("      (contents\n");
  
  (* submodules *)
  let inst_name (x : Signal) = match x.signal with Signal_inst(_,n,m,_,_,_,_) -> n | _ -> "" in
  List.iter (fun (s : Signal) ->
    os ("        (instance " ^ s.name ^ " (viewRef PRIM (cellRef " ^ inst_name s ^ " (libraryRef VIRTEX)))\n") // properties???
  ) (circuit.Inst);
  
  let find_inst_port_name uid ports = 
    let found, name = 
      List.fold (fun (found,n) (port_name, (port_signal:Signal)) -> 
        if found then found, n
        else if port_signal.uid = uid then (true, port_name)
        else (false, n)
      ) (false, "") ports
    in
    if not found then failwith ("Couldnt associate signal, with uid " ^ (string uid) ^ ", with port name");
    name
  in
  
  (**************** this is an awful hack ... that doesnt work either. ******************)

  (* get all signals driven by the list of signals.  Passes through wires. *)
  let rec resolve_signals u s = 
    List.concat (
      List.map (fun (x:Signal) ->
        printf "RESOLVE: %s\n" x.name;
        match x.signal with
        | Signal_wire(_) ->
        begin
          match Map.tryFind x.uid circuit.FanoutMap with
          | None -> [ NetRef(x) ] (* an output *)
          | Some(_,f) -> resolve_signals x.uid (List.map (fun x -> circuit.find x) f)
        end
        | Signal_inst(a,inst_name,m,g,io,i,o) ->
          [ InstRefIn(find_inst_port_name u i, x) ]
        | _ ->
          failwith "Unsupported edif construct (or cat/sel which is TODO)\n"
      ) s
    ) in
  let resolve_signal (s:Signal) = resolve_signals s.uid [s] in

  (* write a net connection. *)
  let rec write_net set (port : PortRef) = 
    printf "WRITE_NET %s\n" port.signal.name;
    if circuit.IsOutput port.signal && not (Set.contains port.signal.uid set) then 
      set
    else
      let set = Set.add port.signal.uid set in
      let targets = resolve_signal port.signal in
      printf "Length: %i\n" (List.length targets);
      os ("        (net hdfs_net_" ^ port.signal.name ^ "\n");
      os ("          (joined\n");
      os ("            " ^ port.Ref ^ "\n");
      List.iter (fun (port : PortRef) -> os ("            " ^ port.Ref ^ "\n")) targets;
      os ("          )\n");
      os ("        )\n");
      let set = write_nets set targets in
      set
    
  and write_nets set s = 
    List.fold (fun set (s:PortRef) -> 
      match s with
      | NetRef _ | InstRefOut _ -> write_net set s
      | InstRefIn(n',s') -> 
        match s'.signal with
        | Signal_inst(_,_,_,_,_,_,o) -> write_nets set (List.map (fun (n,s) -> InstRefOut(n,s'.name,s)) o)
        | _ -> failwith "Expecting instantiation"
    ) set s in

  ignore (write_nets Set.empty (List.map (fun x -> NetRef(x)) circuit.Inputs))

