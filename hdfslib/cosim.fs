#light "off"
(*
  HDFS Digital Logic Hardware Design Utility Library (hdfslib.dll)
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

/// <P>Module for doing cosimulation with verilog.  
/// A key bit is a class written in c# that I grabbed from here:</P>
/// <P><A HREF="http://www.codeproject.com/cs/threads/dotnetnamedpipespart1.asp">http://www.codeproject.com/cs/threads/dotnetnamedpipespart1.asp</A></P>
/// <P>which provides interprocess communication using pipes. 
/// The other key bit is the PLI binding for modelsim.
/// I was hoping that would also work with icarus verilog,
/// but it seems that it only supports VPI (bummer, but
/// with a bit of effort it should be doable).<P>
module DigitalLogic.Cosimulation

open DigitalLogic
open Numeric
open Circuit
open Signal
open Simulator
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

exception Cosimulation_error of string
let failwith s = raise (Cosimulation_error s)

type HANDLE = nativeint

let PIPE_ACCESS_DUPLEX    = 0x00000003ul
let PIPE_READMODE_MESSAGE = 0x00000002ul
let PIPE_TYPE_MESSAGE     = 0x00000004ul
let GENERIC_READ          = 0x80000000ul
let GENERIC_WRITE         = 0x40000000ul
let OPEN_EXISTING         = 0x00000003ul

[<DllImport("kernel32", SetLastError=true, CharSet=CharSet.Auto)>]
extern HANDLE CreateFile (string lpFileName, 
                          uint32 dwDesiredAccess, 
                          uint32 dwShareMode,
                          HANDLE lpSecurityAttributes, 
                          uint32 dwCreationDisposition,
                          uint32 dwFlagsAndAttributes, 
                          HANDLE hTemplateFile)

[<DllImport("kernel32", SetLastError=true, CharSet=CharSet.Auto)>]
extern HANDLE CreateNamedPipe(
    string lpName,									  // pipe name
    uint32 dwOpenMode,								// pipe open mode
    uint32 dwPipeMode,								// pipe-specific modes
    uint32 nMaxInstances,							// maximum number of instances
    uint32 nOutBufferSize,						// output buffer size
    uint32 nInBufferSize,							// input buffer size
    uint32 nDefaultTimeOut,						// time-out interval
    HANDLE pipeSecurityDescriptor		  // SD
    );
    
[<DllImport("kernel32", SetLastError=true, CharSet=CharSet.Auto)>]
extern bool ConnectNamedPipe(
    HANDLE hHandle,									  // handle to named pipe
    HANDLE lpOverlapped					      // overlapped structure
    );

[<DllImport("kernel32", SetLastError=true, CharSet=CharSet.Auto)>]
extern bool ReadFile(
    HANDLE hHandle,									  // handle to file
    byte[] lpBuffer,								  // data buffer
    uint32 nNumberOfBytesToRead,			// number of bytes to read
    byte[] lpNumberOfBytesRead,			  // number of bytes read
    uint32 lpOverlapped								// overlapped buffer
    );

[<DllImport("kernel32", SetLastError=true, CharSet=CharSet.Auto)>]
extern bool WriteFile(
    HANDLE hHandle,									  // handle to file
    byte[] lpBuffer,							    // data buffer
    uint32 nNumberOfBytesToWrite,			// number of bytes to write
    byte[] lpNumberOfBytesWritten,	  // number of bytes written
    uint32 lpOverlapped								// overlapped buffer
    );

[<DllImport("kernel32", SetLastError=true, CharSet=CharSet.Auto)>]
extern bool CloseHandle(HANDLE hHandle);

(******************************************************)
(******************************************************)
open System
open System.IO
open System.Runtime.InteropServices

(** initialises the named pipe server *)
let server_create_pipe name = 
  let flags1 = PIPE_ACCESS_DUPLEX in
  let flags2 = PIPE_TYPE_MESSAGE + PIPE_READMODE_MESSAGE in  (* this may not be right as we always write fixed size messages *)
  CreateNamedPipe("\\\\.\\pipe\\" ^ name, flags1, flags2, 1ul, 1024ul, 1024ul, 0ul, Int32.to_nativeint 0l)

let nill = IntPtr.Zero

(** waits for a connection on the pipe *)
let wait_connect_pipe hPipe = 
  ConnectNamedPipe(hPipe, nill)  (* could really do with timing out here if possible *)

(** reads data from the pipe *)
let read_pipe hPipe max_message_size = 
  let buffer = Bytearray.zero_create max_message_size in
  let num_read = Bytearray.zero_create 4 in
  if ReadFile(hPipe, buffer, UInt32.of_int max_message_size, num_read, 0ul) 
  then Bytearray.ascii_to_string buffer
  else failwith "f#: Read from pipe failed\n"

(** writes data to the pipe *)
let write_pipe hPipe message = 
  let num_written = Bytearray.zero_create 4 in
  let buffer = Bytearray.string_to_ascii message in
  if not (WriteFile(hPipe, buffer, UInt32.of_int (String.length message), num_written, 0ul)) 
  then failwith "f#: Write to pipe failed\n"
 
(** creates a pipe client *)
let client_create_pipe name = (* not really for the f# side of things - used for testing api *)
    let flags1 = GENERIC_READ + GENERIC_WRITE in
    let flags2 = OPEN_EXISTING in
    CreateFile("\\\\.\\pipe\\" ^ name, flags1, 0ul, nill, flags2, 0ul, nill)

(** closes the pipe *)
let close_pipe hPipe = CloseHandle(hPipe)

(******************************************************)
(******************************************************)

/// <P>Writes the testbench for the dut.  Note that the cosimulation mechanism requires a clock 
/// signal to be given whether or not one actually exists in the design (in general one will).
/// In that case just supply the default clock.</P>
/// <P>A further limitation is that only one clock can be provided to the design.  This could be
/// circumvented with a much more elaborate pli module.  However, I would suggest in those
/// where cases multiple clocks are needed it's time to do a full testbench in verilog.</P>
let write_testbench (circuit : Circuit) clock path test_name dut_name clock_half_period = 
  let f = open_out (path ^ test_name ^ ".v") in
  let os = output_string f in
  let hasclock = match tryfind ((=) clock) (circuit.Inputs) with None -> false | _ -> true in
  let inputs = filter ((<>) clock) (circuit.Inputs) in
  let outputs = circuit.Outputs in
  
  let range_of_signal s = 
    let w = width s in
    if w <> 1 then ("[" ^ string_of_int (w-1) ^ ":0] ")
    else "" 
  in
  
  os ("module " ^ test_name ^ "();\n");
  os (" reg " ^ wire_name clock ^ ";\n");
  iter (fun x -> os (" reg " ^ range_of_signal x ^ wire_name x ^ ", " ^ apply_prefix (wire_name x) ^ ";\n")) inputs;
  iter (fun x -> os (" wire " ^ range_of_signal x ^ wire_name x ^ ";\n")) outputs;
  os ("\n always begin\n  " ^ 
    wire_name clock ^ " <= 0; #" ^ string_of_int clock_half_period ^ ";\n  " ^ 
    wire_name clock ^ " <= 1; #" ^ string_of_int clock_half_period ^ ";\n" ^ " end\n\n");
  os (" initial $hdfs_drive_signals(" ^ string_of_int (length inputs) ^ ", " ^ wire_name clock ^ ", " ^
    (fold_strings ", " (map (fun x -> apply_prefix (wire_name x)) inputs)) ^ ");\n");
  os (" initial $hdfs_monitor_signals(" ^ string_of_int (length outputs) ^ ", " ^ wire_name clock ^ ", " ^
    (fold_strings ", " (map wire_name outputs)) ^ ");\n\n");
  os (" always @(posedge " ^ wire_name clock ^ ") begin\n");
  iter (fun x -> os ("  " ^ wire_name x ^ " <= " ^ apply_prefix (wire_name x) ^ ";\n")) inputs;
  os (" end\n\n");
  os (" " ^ dut_name ^ " the_" ^ dut_name ^ " (" ^
    (fold_strings ", " (map (fun x -> "." ^ wire_name x ^ "(" ^ wire_name x ^ ")") 
      (if hasclock then (clock::inputs@outputs) else inputs@outputs))) ^ ");\n");
  os ("\nendmodule\n");
  close_out f

(******************************************************)
(* modelsim helpers *)
(******************************************************)

let mti_path = ref ""

(** calls the modelsim verilog compiler *)
let vlog args fnames = execute_command (!mti_path ^ "vlog") (args ^ " " ^ fold_strings " " fnames)
(** calls the modelsim vlib tool *)
let vlib name = execute_command (!mti_path ^ "vlib") name
(** calls the modelsim vmap tool *)
let vmap lib path = execute_command (!mti_path ^ "vlib") (lib ^ " " ^ path)
(** constructs a modelsim .do file *)
let dofile name commands = 
  let f = open_out name in
  output_string f (fold_strings "\n" commands);
  close_out f

(******************************************************)
(* iverilog helpers *)
(******************************************************)

let ivlog_path = ref ""

let iverilog args fnames = 
  let files = args ^ " " ^ fold_strings " " fnames in
  printf "iverilog compiling: %s %s\n" (!ivlog_path ^ "iverilog") files;
  let p = new System.Diagnostics.Process() in
  p.StartInfo.UseShellExecute <- true;
  p.StartInfo.RedirectStandardOutput <- false;
  p.StartInfo.FileName <- (!ivlog_path ^ "iverilog");
  p.StartInfo.Arguments <- files;
  let _ = p.Start() in
  p.WaitForExit()
  
let vvp args testbench = 
  printf "vvp: %s %s\n" args testbench;
  let p = new System.Diagnostics.Process() in
  p.StartInfo.UseShellExecute <- true;
  p.StartInfo.RedirectStandardOutput <- false;
  p.StartInfo.FileName <- (!ivlog_path ^ "vvp");
  p.StartInfo.Arguments <- args ^ " " ^ testbench;
  p.Start() |> ignore;
  p

(*
let create_ivlog_int circuit clock reset test_name vsim_args = 
  let inputs = filter ((<>) clock) (circuit_inputs circuit) in
  let inputs = map (fun x -> x, ref 0) inputs in
  let _,reset = try find (fun (x,_) -> x = reset) inputs with _ -> (DigitalLogic.Design.gnd, ref 0) in
  let clock = ref 0 in
  let outputs = map (fun x -> x, ref 0) (circuit_outputs circuit) in
  let proc = run_command (!ivlog_path ^ "vvp") ("-M . -m cosim_ivlog " ^ vsim_args ^ " " ^ test_name) in
  let pipe = server_create_pipe "hdfs_cosim" in
  let _ = wait_connect_pipe pipe in
  let bytes_read = fold_left (fun acc (s,_) -> acc + (((width s) + 3)/4)) 0 outputs in
  Cosim(circuit, proc, pipe, bytes_read, clock, reset, inputs, outputs)
*)

(******************************************************)
(* cosimulation interface *)
(******************************************************)

(** Creates a modelsim verilog cosimulator.  Expects the testbench and dut to be written and compiled *)
let create_mti (circuit : Circuit) clock reset test_name vsim_args = 
  let new_port (s:Signal) = { port_uid = s.uid; port_name = s.name; port_data = ArrayBits.make s.width } in
  let inputs = filter ((<>) clock) circuit.Inputs in
  let inputs = map (fun s -> new_port s) inputs in
  let reset = try find (fun (p:Port) -> p.name = wire_name reset) inputs with _ -> new_port reset in
  let outputs = map (fun s -> new_port s) (circuit.Outputs) in
  let proc = run_command (!mti_path ^ "vsim") (vsim_args ^ " " ^ test_name) in
  let pipe = server_create_pipe "hdfs_cosim" in
  let _ = wait_connect_pipe pipe in
  let bytes_read = fold_left (fun acc (p:Port) -> acc + ((p.width + 3) / 4)) 0 outputs in
    
  let update_read_data read_data outputs = 
    ignore (fold_left (fun pos (p:Port) ->
        let chars = (p.width + 3) / 4 in
        let str = (String.sub read_data pos chars) in
        p.hu <- String.map (fun x -> if x = 'X' || x = 'x' || x = 'Z' || x = 'z' then '0' else x) str;  // XXX: possible bug here
        pos + chars
      ) 0 outputs)
  in
  let cycle () = 
    try
      let drive_data = fold_left (fun str (p:Port) -> str ^ p.hu) "" inputs in  // XXX: possible bug here
      write_pipe pipe drive_data; 
      let monitor_data = read_pipe pipe bytes_read in
      update_read_data monitor_data outputs
    with _ -> output_string stdout "Sim cycle failed.\n"
  in
  let reset () = 
    try
      reset.i <- 1;
      cycle ();
      reset.i <- 0;
    with _ -> output_string stdout "Sim reset failed.\n"
  in
  {
    sim_circuit  = circuit;
    sim_reset    = reset;
    sim_cycle    = cycle;
    sim_inputs   = inputs;
    sim_wires    = [];
    sim_outputs  = outputs;
    sim_port_map = fold_left (fun map (s:Port) -> Map.add s.uid s map) Map.empty (inputs @ outputs);
    sim_name_map = fold_left (fun map (s:Port) -> Map.add s.name s.uid map) Map.empty (inputs @ outputs);
    sim_data_map = Map.empty;
    sim_reg_map  = Map.empty;
    sim_mem_map  = Map.empty;
  } : Simulator

(** Set up a verilog simulation, including writing the testbench and compiling the verilog sources *)
let create_all_mti' clock_half_period circuit clock reset path dut_name vsim_args do_args = 
  (* prepare the verilog files *)
  write_file Verilog.write path dut_name ".v" circuit;
  write_testbench circuit clock path (dut_name ^ "_tb") dut_name clock_half_period;
  //vlib "work"; // this should be set up externally now.
  vlog "" [ path ^ dut_name ^ ".v"; path ^ dut_name ^ "_tb" ^ ".v" ];
  dofile (path ^ "sim.do") do_args;

  (* build the simulator *)
  create_mti circuit clock reset (dut_name ^ "_tb") vsim_args

let create_all_mti = create_all_mti' 5

(******************************************************)
(* New VPI based cosimulator *)
(******************************************************)

let write_testbench_vpi (circuit : Circuit) clock path test_name dut_name = 
  let f = open_out (path ^ test_name ^ ".v") in
  let os = output_string f in
  let hasclock = match tryfind ((=) clock) (circuit.Inputs) with None -> false | _ -> true in
  let inputs = filter ((<>) clock) (circuit.Inputs) in
  let outputs = circuit.Outputs in
  
  let range_of_signal s = 
    let w = width s in
    if w <> 1 then ("[" ^ string_of_int (w-1) ^ ":0] ")
    else "" 
  in
  
  os ("module " ^ test_name ^ "();\n");
  os (" reg " ^ wire_name clock ^ ";\n");
  iter (fun x -> os (" reg " ^ range_of_signal x ^ wire_name x ^ ", " ^ apply_prefix (wire_name x) ^ ";\n")) inputs;
  iter (fun x -> os (" wire " ^ range_of_signal x ^ wire_name x ^ ";\n")) outputs;
  os ("\n always begin\n  " ^ 
    wire_name clock ^ " <= 1; #5;\n  " ^ 
    wire_name clock ^ " <= 0; #5;\n" ^ " end\n\n");
  os (" always @(posedge " ^ wire_name clock ^ ") $hdfs_drive(" ^
    (fold_strings ", " (map (fun x -> apply_prefix (wire_name x)) inputs)) ^ ");\n");
  os (" always @(negedge " ^ wire_name clock ^ ") $hdfs_monitor(" ^ 
    (fold_strings ", " (map wire_name outputs)) ^ ");\n\n");
  os (" " ^ dut_name ^ " the_" ^ dut_name ^ " (" ^
    (fold_strings ", " (map (fun x -> "." ^ wire_name x ^ "(" ^ wire_name x ^ ")") 
      (if hasclock then (clock::inputs@outputs) else inputs@outputs))) ^ ");\n");
  os ("\nendmodule\n");
  close_out f

(** Creates a modelsim verilog cosimulator.  Expects the testbench and dut to be written and compiled *)
let create_vpi (circuit : Circuit) clock reset simulator test_name sim_args = 
  let new_port (s:Signal) = { port_uid = s.uid; port_name = s.name; port_data = ArrayBits.make s.width } in
  let inputs = filter ((<>) clock) circuit.Inputs in
  let inputs = map (fun s -> new_port s) inputs in
  let reset = try find (fun (p:Port) -> p.name = wire_name reset) inputs with _ -> new_port reset in
  let outputs = map (fun s -> new_port s) (circuit.Outputs) in
  let pipe = server_create_pipe "hdfs_cosim" in
  //let proc = run_command simulator (sim_args ^ test_name) in
  let proc = vvp sim_args "a.out" in
  let _ = wait_connect_pipe pipe in
  let bytes_read = fold_left (fun acc (p:Port) -> acc + ((p.width + 3) / 4)) 0 outputs in

  let update_read_data read_data outputs = 
    ignore (fold_left (fun pos (p:Port) ->
        let chars = (p.width + 3) / 4 in
        let str = (String.sub read_data pos chars) in
        p.hu <- String.map (fun x -> if x = 'X' || x = 'x' then '0' else x) str;  // XXX: possible bug here
        pos + chars
      ) 0 outputs)
  in
  let cycle () = 
    try
      let drive_data = fold_left (fun str (p:Port) -> str ^ p.hu) "" inputs in  // XXX: possible bug here
      printf "writing data to simulator ... \n";
      write_pipe pipe drive_data; 
      printf "%s\n" drive_data;
      printf "reading data from  simulator...\n";
      let monitor_data = read_pipe pipe bytes_read in
      printf "%s\n" monitor_data;
      update_read_data monitor_data outputs
    with _ -> output_string stdout "Sim cycle failed.\n"
  in
  let reset () = 
    try
      reset.i <- 1;
      cycle ();
      reset.i <- 0;
    with _ -> output_string stdout "Sim reset failed.\n"
  in
  {
    sim_circuit  = circuit;
    sim_reset    = reset;
    sim_cycle    = cycle;
    sim_inputs   = inputs;
    sim_wires    = [];
    sim_outputs  = outputs;
    sim_port_map = fold_left (fun map (s:Port) -> Map.add s.uid s map) Map.empty (inputs @ outputs);
    sim_name_map = fold_left (fun map (s:Port) -> Map.add s.name s.uid map) Map.empty (inputs @ outputs);
    sim_data_map = Map.empty;
    sim_reg_map  = Map.empty;
    sim_mem_map  = Map.empty;
  } : Simulator

(** Set up a verilog simulation, including writing the testbench and compiling the verilog sources *)
let create_all_vpi circuit clock reset simulator path dut_name sim_args do_args = 
  (* prepare the verilog files *)
  write_file Verilog.write path dut_name ".v" circuit;
  write_testbench_vpi circuit clock path (dut_name ^ "_tb") dut_name;
  //vlib "work";
  //vlog "" [ path ^ dut_name ^ ".v"; path ^ dut_name ^ "_tb" ^ ".v" ];
  printf "Compiling files...\n";
  iverilog "" [ path ^ dut_name ^ ".v"; path ^ dut_name ^ "_tb" ^ ".v" ];
  if do_args <> [] then
    dofile (path ^ "sim.do") do_args;

  (* build the simulator *)
  printf "Creating vpi sim...\n";
  create_vpi circuit clock reset simulator (dut_name ^ "_tb") sim_args

(******************************************************)
(* test *)
(******************************************************)

let test_pipes() = 
  let run_server() = 
    let pipe = server_create_pipe "test" in
    let _ = wait_connect_pipe pipe in
    write_pipe pipe "Hello from server";
    let resp = read_pipe pipe 1024 in
    output_string stdout resp
  in
    
  let run_client() = 
    let pipe = client_create_pipe "test" in
    let resp = read_pipe pipe 1024 in
    output_string stdout resp;
    write_pipe pipe "Hello from client"
  in
  
  let run_sim() = 
    try 
      let pipe = server_create_pipe "hdfs_cosim" in
      let _ = wait_connect_pipe pipe in
      (* the first thing that'll happen is we receive a message from the simulator with data *)
      let temp = Bytearray.create 1 in
      for i=0 to 500 do
        write_pipe pipe "12345";
        let s = read_pipe pipe 6 in
        output_string stdout ("Read " ^ s ^ "\n");
      done;
      output_string stdout "Sending terminate...\n";
      write_pipe pipe "q"
    with _ ->
      output_string stdout "Failed....\n"
  in
  
  if Sys.argv.(1) = "server" then run_server()
  else if Sys.argv.(1) = "client" then run_client()
  else if Sys.argv.(1) = "sim" then run_sim()
  else failwith "Dont know how to run..!"
