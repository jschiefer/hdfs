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
module DigitalLogic.CosimulationV01

open AppModule.NamedPipes
open DigitalLogic
open Circuit
open Signal
open SimulatorV01
open List

exception Cosimulation_error of string
let failwith s = raise (Cosimulation_error s)

(*
open System.Runtime.InteropServices;

  // This is the API used for cosimulation.  It would be nice to convert it to the F# [<DllImport>]  syntax ...
  // ... but this C# binding seems to do all sort of nice conversions behind the scenes ...

  public const uint PIPE_ACCESS_DUPLEX = 0x00000003;
  public const uint PIPE_READMODE_MESSAGE = 0x00000002;
  public const uint PIPE_TYPE_MESSAGE = 0x00000004;
  
  public const uint GENERIC_READ = (0x80000000);
  public const uint GENERIC_WRITE = (0x40000000);
  public const uint OPEN_EXISTING     = 3;
  
  
  [DllImport("kernel32.dll", SetLastError=true)]
  public static extern IntPtr CreateNamedPipe(
      String lpName,									// pipe name
      uint dwOpenMode,								// pipe open mode
      uint dwPipeMode,								// pipe-specific modes
      uint nMaxInstances,							// maximum number of instances
      uint nOutBufferSize,						// output buffer size
      uint nInBufferSize,							// input buffer size
      uint nDefaultTimeOut,						// time-out interval
      IntPtr pipeSecurityDescriptor		// SD
      );

  [DllImport("kernel32.dll", SetLastError=true)]
  public static extern bool ConnectNamedPipe(
      IntPtr hHandle,									// handle to named pipe
      Overlapped lpOverlapped					// overlapped structure
      );

  [DllImport("kernel32.dll", SetLastError=true)]
  public static extern bool ReadFile(
      IntPtr hHandle,											// handle to file
      byte[] lpBuffer,								// data buffer
      uint nNumberOfBytesToRead,			// number of bytes to read
      byte[] lpNumberOfBytesRead,			// number of bytes read
      uint lpOverlapped								// overlapped buffer
      );

  [DllImport("kernel32.dll", SetLastError=true)]
  public static extern bool WriteFile(
      IntPtr hHandle,											// handle to file
      byte[] lpBuffer,							  // data buffer
      uint nNumberOfBytesToWrite,			// number of bytes to write
      byte[] lpNumberOfBytesWritten,	// number of bytes written
      uint lpOverlapped								// overlapped buffer
      );

  [DllImport("kernel32.dll", SetLastError=true)]
  public static extern IntPtr CreateFile(
      String lpFileName,						  // file name
      uint dwDesiredAccess,					  // access mode
      uint dwShareMode,								// share mode
      SecurityAttributes attr,				// SD
      uint dwCreationDisposition,			// how to create
      uint dwFlagsAndAttributes,			// file attributes
      uint hTemplateFile);					  // handle to template file

  [DllImport("kernel32.dll", SetLastError=true)]
  public static extern bool CloseHandle(IntPtr hHandle);
*)

(******************************************************)
(******************************************************)

(** initialises the named pipe server *)
let server_create_pipe name = 
  let flags1 = NamedPipeNative.PIPE_ACCESS_DUPLEX in
  let flags2 = NamedPipeNative.PIPE_TYPE_MESSAGE + NamedPipeNative.PIPE_READMODE_MESSAGE in  (* this may not be right as we always write fixed size messages *)
  NamedPipeNative.CreateNamedPipe("\\\\.\\pipe\\" ^ name, flags1, flags2, 1ul, 1024ul, 1024ul, 0ul, Int32.to_nativeint 0l)

(** waits for a connection on the pipe *)
let wait_connect_pipe hPipe = 
  NamedPipeNative.ConnectNamedPipe(hPipe, null)  (* could really do with timing out here if possible *)

(** reads data from the pipe *)
let read_pipe hPipe max_message_size = 
  let buffer = Bytearray.zero_create max_message_size in
  let num_read = Bytearray.zero_create 4 in
  if NamedPipeNative.ReadFile(hPipe, buffer, UInt32.of_int max_message_size, num_read, 0ul) 
  then Bytearray.ascii_to_string buffer
  else failwith "f#: Read from pipe failed\n"

(** writes data to the pipe *)
let write_pipe hPipe message = 
  let num_written = Bytearray.zero_create 4 in
  let buffer = Bytearray.string_to_ascii message in
  if not (NamedPipeNative.WriteFile(hPipe, buffer, UInt32.of_int (String.length message), num_written, 0ul)) 
  then failwith "f#: Write to pipe failed\n"
 
(** creates a pipe client *)
let client_create_pipe name = (* not really for the f# side of things - used for testing api *)
    let flags1 = NamedPipeNative.GENERIC_READ + NamedPipeNative.GENERIC_WRITE in
    let flags2 = NamedPipeNative.OPEN_EXISTING in
    NamedPipeNative.CreateFile("\\\\.\\pipe\\" ^ name, flags1, 0ul, null, flags2, 0ul, 0ul)

(** closes the pipe *)
let close_pipe hPipe = NamedPipeNative.CloseHandle(hPipe)

(******************************************************)
(******************************************************)

/// <P>Writes the testbench for the dut.  Note that the cosimulation mechanism requires a clock 
/// signal to be given whether or not one actually exists in the design (in general one will).
/// In that case just supply the default clock.</P>
/// <P>A further limitation is that only one clock can be provided to the design.  This could be
/// circumvented with a much more elaborate pli module.  However, I would suggest in those
/// where cases multiple clocks are needed it's time to do a full testbench in verilog.</P>
let write_testbench (circuit : Circuit) clock path test_name dut_name = 
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
    wire_name clock ^ " <= 0; #5;\n  " ^ 
    wire_name clock ^ " <= 1; #5;\n" ^ " end\n\n");
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
(** constructs a modelsim .do file *)
let dofile name commands = 
  let f = open_out name in
  output_string f (fold_strings "\n" commands);
  close_out f

(******************************************************)
(* iverilog helpers *)
(******************************************************)

let ivlog_path = ref "c:\\dev\\tools\\iverilog\\bin\\"

let iverilog args fnames = execute_command (!ivlog_path ^ "iverilog") (args ^ " " ^ fold_strings " " (map (fun x -> x ^ ".v") fnames))

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

(** Creates a modelsim verilog coSimulatorV01.  Expects the testbench and dut to be written and compiled *)
let create_mti (dcfg : 'a SimulatorV01.sim_data_t) (circuit : Circuit) clock reset test_name vsim_args = 
  let inputs = filter ((<>) clock) circuit.Inputs in
  let inputs = map (fun s -> wire_name s, width s, dcfg.create (width s)) inputs in
  let _,_,reset = try find (fun (n,_,_) -> n = wire_name reset) inputs with _ -> ("", 1, dcfg.create 1) in
  let clock = dcfg.create 1 in
  let outputs = map (fun s -> wire_name s, width s, dcfg.create (width s)) (circuit.Outputs) in
  let proc = run_command (!mti_path ^ "vsim") (vsim_args ^ " " ^ test_name) in
  let pipe = server_create_pipe "hdfs_cosim" in
  let _ = wait_connect_pipe pipe in
  let bytes_read = fold_left (fun acc (_,w,_) -> acc + ((w + 3) / 4)) 0 outputs in
    
  let update_read_data (dcfg : 'a SimulatorV01.sim_data_t) read_data outputs = 
    ignore (fold_left (fun pos (n,w,v) ->
        let chars = (w + 3) / 4 in
        dcfg.of_hex_str v (String.sub read_data pos chars) w;
        pos + chars
      ) 0 outputs)
  in
  let cycle () = 
    try
      let drive_data = fold_left (fun str (n,w,v) -> str ^ (dcfg.to_hex_str v w)) "" inputs in
      write_pipe pipe drive_data; 
      let monitor_data = read_pipe pipe bytes_read in
      update_read_data dcfg monitor_data outputs
    with _ -> output_string stdout "Sim cycle failed.\n"
  in
  let reset () = 
    try
      dcfg.set_const reset "1";
      cycle ();
      dcfg.set_zero reset 1;
    with _ -> output_string stdout "Sim reset failed"
  in
  
  Simulator(reset, cycle, inputs, [], outputs)

(** modelsim verilog cosimulator for int simulations *)
let create_mti_int = create_mti SimulatorV01.simDataInt
(** modelsim verilog cosimulator for uint32 simulations *)
let create_mti_uint32 = create_mti SimulatorV01.simDataUInt32
(** modelsim verilog cosimulator for uint64 simulations *)
let create_mti_uint64 = create_mti SimulatorV01.simDataUInt64
(** modelsim verilog cosimulator for array simulations *)
let create_mti_array = create_mti SimulatorV01.simDataArray

(** Set up a verilog simulation, including writing the testbench and compiling the verilog sources *)
let create_all_mti (dcfg : 'a SimulatorV01.sim_data_t) circuit clock reset path dut_name vsim_args do_args = 
  (* prepare the verilog files *)
  write_file Verilog.write path dut_name ".v" circuit;
  write_testbench circuit clock path (dut_name ^ "_tb") dut_name;
  vlib "work";
  vlog "" [ path ^ dut_name ^ ".v"; path ^ dut_name ^ "_tb" ^ ".v" ];
  dofile (path ^ "sim.do") do_args;

  (* build the simulator *)
  create_mti dcfg circuit clock reset (dut_name ^ "_tb") vsim_args

(** Sets up verilog cosimulator for int simulations *)
let create_all_mti_int = create_all_mti SimulatorV01.simDataInt
(** Sets up verilog cosimulator for uint32 simulations *)
let create_all_mti_uint32 = create_all_mti SimulatorV01.simDataUInt32
(** Sets up verilog cosimulator for uint64 simulations *)
let create_all_mti_uint64 = create_all_mti SimulatorV01.simDataUInt64
(** Sets up verilog cosimulator for array simulations *)
let create_all_mti_array = create_all_mti SimulatorV01.simDataArray

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
