//#light 
open Xilinx_types
open List

let argv = 
#if INTERACTIVE
  fsi.CommandLineArgs
#else
  Sys.argv
#endif

let os, close_output = 
  //let f = open_out argv.[2] in
  let f = System.IO.File.CreateText(argv.[2]) in
  (fun (x:string) -> f.Write(x)), (fun () -> f.Close())
;;

let load_xilinx_components () =
  let open_file() = 
    try open_in_bin argv.[1]
    with _ -> failwith ("Couldnt open unisim.vhd") in
  let fin = open_file() in
  //let fin = stdin in
  let lexbuf = Lexing.from_channel fin in
  let _ = Xilinx_lexer.init_linenum () in
  let components = 
    try Xilinx_parser.main Xilinx_lexer.token lexbuf 
    with 
    | _ -> failwith ("Failed to read unisim.vhd at line " ^ (string !Xilinx_lexer.lineNum)) in
  let _ = close_in fin in
  components
;;

(*******************************************************************************)

let fold_strings inner = fold_left (fun x a -> if x = "" then a else x ^ inner ^ a) "" 
;;

let string_of_dir = function
  | In -> "in"
  | Out -> "out"
  | Inout -> "inout"
;;

let string_of_type = function
  | T_std_logic -> "std_logic"
  | T_std_logic_vector (l,r) -> 
    if l<0 
    then "std_logic_vector"  
    else if l >= r 
    then "std_logic_vector(" ^ string l ^ " downto " ^ string r ^ ")"
    else "std_logic_vector(" ^ string l ^ " to " ^ string r ^ ")"
  | T_real -> "real"
  | T_integer -> "integer"
  | T_bit -> "bit" (* can these be unified in to std_logic? *)
  | T_bit_vector(l,r) -> 
    if l<0 
    then "bit_vector"  
    else if l >= r 
    then "bit_vector(" ^ string l ^ " downto " ^ string r ^ ")"
    else "bit_vector(" ^ string l ^ " to " ^ string r ^ ")"
  | T_boolean -> "boolean"
  | T_string -> "string"
  | T_time -> "time"
;;

let generic_string p = 
  let someof x = match x with Some(x) -> x | _ -> failwith "Nothing to return" in
  match p with 
  Port(n, d, t, v) -> ("  " ^ n ^ " : " ^ string_of_type t ^ (if v = None then "" else " := " ^ someof v))
;;
  
let port_string p = 
  let someof x = match x with Some(x) -> x | _ -> failwith "Nothing to return" in
  match p with 
  Port(n, d, t, v) -> (" " ^ n ^ " : " ^ string_of_dir d ^ " " ^ string_of_type t ^ (if v = None then "" else " := " ^ someof v))
;;

let write_component_vhdl c = 
  match c with
  Component(n, g, p) ->
    os ("component " ^ n ^ "\n");
    if g <> [] then (
      os " generic (\n";
      os (fold_strings ";\n" (map generic_string g));
      os "\n );\n"
    );
    if p <> [] then (
      os " port (\n";
      os (fold_strings ";\n" (map port_string p));
      os "\n );\n"
    );
    os "end component;\n"
;;

let write_vhdl_package components = 
  os "
library IEEE;
use IEEE.STD_LOGIC_1164.all;
package VCOMPONENTS is

";
  iter write_component_vhdl components;
  os "
  
end VCOMPONENTS;
"
;;

(*******************************************************************************)

let list_components components =
  iter (fun x -> 
      match x with 
      | Component(n, g, p) -> 
        os ("Read component " ^ n ^ "\n");
    ) components
;;

(*******************************************************************************)

let map_port_sizes comp = 
  let resolve_size s = 
    let len = String.length s in
    if len = 0 then failwith "String has 0 length";
    if String.get s 0 = 'X' then
      ((String.length s - 3)*4)-1,0
    else if String.get s 0 = '"' then (
      String.iter (fun x -> if x <> '0' && x <> '1' && x <> '"' then failwith "Dont understand string") s;
      String.length s - 3,0
    ) else 
      failwith "Dont understand string"
  in
  let rec map_port_sizes ports = 
    match ports with
    | [] -> []
    | hd :: tl -> 
      let hd = (
        match hd with
        | Port(n, d, t, v) -> 
          match t with
          | T_std_logic_vector(l, r) ->
            if l < 0 then
              match v with
              | None -> failwith ("Cannot resolve length of " ^ n)
              | Some x -> 
                let l,r = resolve_size x in
                Port(n, d, T_std_logic_vector(l, r), v)
            else hd
          | T_bit_vector(l, r) ->
            if l < 0 then
              match v with
              | None -> failwith ("Cannot resolve length of " ^ n)
              | Some x -> 
                let l,r = resolve_size x in
                Port(n, d, T_bit_vector(l, r), v)
            else hd
          | _ -> hd
        )
      in
        hd :: (map_port_sizes tl)
  in
  let rec check_ports ports = 
    match ports with
    | [] -> ()
    | hd :: tl -> (
      match hd with
      | Port(n, d, t, v) ->
        match t with
        | T_std_logic | T_std_logic_vector _ -> ()
        | T_bit_vector _ | T_bit | T_real | T_integer | T_boolean | T_string | T_time -> failwith "Types in ports should be std_logic"
    )
  in
  let (Component(n, g, p)) = comp in
  check_ports p;
  Component(n, map_port_sizes g, map_port_sizes p)
;;

(*******************************************************************************)

let width_of_type = function
  | T_std_logic -> 1
  | T_std_logic_vector (l,r) -> (abs (l-r)) + 1 
  | T_bit_vector (l,r) -> (abs (l-r)) + 1 
  | T_bit -> 1
  | T_real -> 0
  | T_integer -> 32
  | T_boolean -> 1
  | T_string -> 0
  | T_time -> 0
;;

let rec to_chars s = 
  match s with
  | "" -> []
  | _ ->
    let hd = String.get s 0 in
    let tl = String.sub s 1 ((String.length s)-1) in
    hd :: (to_chars tl) 
;;

let escape_string s =
  let chars = to_chars s in
  let rec escape_char_list c = 
    match c with
    | [] -> ""
    | '\"' :: tl -> "\\\"" ^ (escape_char_list tl)
    | hd :: tl -> (String.of_char hd) ^ (escape_char_list tl)
  in
  escape_char_list (to_chars s)
;;

let hex_to_bin s =
  let chars = to_chars s in
  let hex_to_bin s = 
    match s with 
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'a' -> "1010"
    | 'b' -> "1011"
    | 'c' -> "1100"
    | 'd' -> "1101"
    | 'e' -> "1110"
    | 'f' -> "1111"
    | 'A' -> "1010"
    | 'B' -> "1011"
    | 'C' -> "1100"
    | 'D' -> "1101"
    | 'E' -> "1110"
    | 'F' -> "1111"
    | _ -> failwith "Unknown hex char"
  in
  let rec to_bin s = 
    match s with 
    | [] -> "" 
    | hd::tl -> (hex_to_bin hd) ^ (to_bin tl)
  in
  to_bin chars
;;
  
let ml_string_of_type typ data = 
  let string_of_sl s = match s with "'0'" -> "0" | "'1'" -> "1" | _ -> failwith "Bad sl" in
  let string_of_slv s = 
    match String.get s 0 with 
    | 'X'  -> "\"" ^ (hex_to_bin (String.sub s 2 ((String.length s)-3))) ^ "\""
    | '\"' -> "" ^ s ^ ""
    | _    -> failwith "Bad slv"
  in
  let string_of_bool = String.lowercase in
  let string_of s = s in
  let string_of_time s = "\"" ^ s ^ "\"" in
  let some fn d = match d with None -> "None" | Some s -> "Some(" ^ fn s ^ ")" in
  match typ with
  | T_std_logic -> "Gd_Sl(" ^ (some string_of_sl data) ^ ")"
  | T_std_logic_vector (l,r) ->  "Gd_Slv(" ^ string l ^ ", " ^ string r ^ ", " ^ some string_of_slv data ^ ")"
  | T_bit_vector (l,r) ->  "Gd_Bv(" ^ string l ^ ", " ^ string r ^ ", " ^ (some string_of_slv data) ^ ")"
  | T_bit ->  "Gd_Bit(" ^ (some string_of_sl data) ^ ")"
  | T_real ->  "Gd_Float(" ^ (some string_of data) ^ ")"
  | T_integer ->  "Gd_Int(" ^ (some string_of data) ^ ")"
  | T_boolean ->  "Gd_Bool(" ^ (some string_of_bool data) ^ ")"
  | T_string -> "Gd_String(" ^ (some string_of data) ^ ")"
  | T_time ->  "Gd_Time(" ^ (some string_of_time data) ^ ")"
;;

(* how do we deal with:
   inouts; generics; time; real
*)
let write_ml_instatiation c = 
  match c with 
  Component(name, g, p) ->
    let inputs = List.filter (fun p -> match p with Port(n, d, t, v) -> if d = In then true else false) p  in
    let outputs = List.filter (fun p -> match p with Port(n, d, t, v) -> if d = Out then true else false) p  in
    let inouts = List.filter (fun p -> match p with Port(n, d, t, v) -> if d = Inout then true else false) p  in
    
    (* Write the "generic" version of the function which requires all generics to be specified *)
    
    os ("let g_" ^ name ^ " \n");
    (* generics *)
    iter (fun g -> match g with Port(n, d, t, v) -> os (" generic_" ^ n ^ " (* generic -" ^ generic_string g ^ " *)\n")) g;
    (* ports *)
    iter (fun p -> match p with Port(n, d, t, v) -> os (" inout_" ^ n ^ " (* inout -" ^ port_string p ^ " *)\n")) inouts;
    iter (fun p -> match p with Port(n, d, t, v) -> os (" input_" ^ n ^ " (* input -" ^ port_string p ^ " *)\n")) inputs;
    os ("=\n");
    (* output wires - dont name them (need the mangler...) *)
    iter (fun p -> match p with Port(n, d, t, v) -> os (" let output_" ^ n ^ " = wire " ^ string (width_of_type t) ^ " in\n")) outputs;
    (* check inputs *)
    iter (fun p -> match p with Port(n, d, t, v) -> os (" check_width \"" ^ name ^ "\" \"" ^ n ^ "\" input_" ^ n ^ " " ^ string (width_of_type t) ^ ";\n")) inputs;
    (* instance *)
    os (" instgio \"" ^ String.uppercase name ^ "\" [\n");
    (* generics *)
    iter (fun p -> match p with Port(n, d, t, v) -> os ("  (\"" ^ String.uppercase n ^ "\", " ^ ml_string_of_type t v ^ ", generic_" ^ n ^ ");\n")) g;
    os (" ] [\n");
    (* inouts *)
    iter (fun p -> match p with Port(n, d, t, v) -> os ("  \"" ^ String.uppercase n ^ "\" ==> inout_" ^ n ^ ";\n")) inouts;
    os (" ] [\n");
    (* inputs *)
    iter (fun p -> match p with Port(n, d, t, v) -> os ("  \"" ^ String.uppercase n ^ "\" ==> input_" ^ n ^ ";\n")) inputs;
    os (" ] [\n");
    (* outputs *)
    iter (fun p -> match p with Port(n, d, t, v) -> os ("  \"" ^ String.uppercase n ^ "\" ==> output_" ^ n ^ ";\n")) outputs;
    os (" ];\n (\n");
    os (fold_strings ",\n" (map (fun p -> match p with Port(n, d, t, v) -> "  output_" ^ n ^ " (* output -" ^ port_string p ^ " *)") outputs));
    os ("\n )\n\n");
    
    (* write the "list" version of the function with which arbitrary generics need to be specified *)
    os ("let l_" ^ name ^ " generics_list \n");
    (* ports *)
    iter (fun p -> match p with Port(n, d, t, v) -> os (" inout_" ^ n ^ "\n")) inouts;
    iter (fun p -> match p with Port(n, d, t, v) -> os (" input_" ^ n ^ "\n")) inputs;
    os ("=\n");
    (* find the generics *)
    iter (fun g -> match g with Port(n, d, t, v) -> os (" let generic_" ^ n ^ " = if generics_list = [] then None else List.try_assoc \"" ^ String.uppercase n ^ "\" generics_list in\n")) g;
    (* call the base function *)
    os (" g_" ^ name ^ "\n" ^ 
        "  " ^ (fold_strings " " (map (fun g -> match g with Port(n, d, t, v) -> "generic_" ^ n) g)) ^ 
        "  " ^ (fold_strings " " (map (fun g -> match g with Port(n, d, t, v) -> "inout_" ^ n) inouts)) ^ 
        "  " ^ (fold_strings " " (map (fun g -> match g with Port(n, d, t, v) -> "input_" ^ n) inputs)) ^ "\n\n");
    
    (* write the "non-generic" version of the function which takes no generics.  This one will take the "proper" name as I suspect it will get most use *)
    os ("let " ^ name ^ " \n");
    (* ports *)
    iter (fun p -> match p with Port(n, d, t, v) -> os (" inout_" ^ n ^ "\n")) inouts;
    iter (fun p -> match p with Port(n, d, t, v) -> os (" input_" ^ n ^ "\n")) inputs;
    os ("=\n");
    (* call the base function *)
    os (" l_" ^ name ^ " []\n" ^ 
        "  " ^ (fold_strings " " (map (fun g -> match g with Port(n, d, t, v) -> "inout_" ^ n) inouts)) ^ 
        "  " ^ (fold_strings " " (map (fun g -> match g with Port(n, d, t, v) -> "input_" ^ n) (inputs)) ^ "\n\n"))
;;

let write_ml_instatiations components = 
  os ("(** Xilinx instantiation primitives using new syntax *)
module Xilinx = 
begin

exception Xilinx_error of string
let failwith s = raise (Xilinx_error s)

let check_width module_name signal_name signal n = 
  if (width signal) <> n then 
  failwith (\"module \" ^ module_name ^ \" : port \" ^ signal_name ^ \" : expecting \" ^ 
            string n ^ \" bit signal, got \" ^ string (width signal) ^ \" bit signal\")

");
  iter write_ml_instatiation components;
  os ("
end
")
;;
(*******************************************************************************)

let write_ml_instatiation_inst c = 
  match c with 
  Component(name, g, p) ->
    let inputs = List.filter (fun p -> match p with Port(n, d, t, v) -> if d = In then true else false) p  in
    let outputs = List.filter (fun p -> match p with Port(n, d, t, v) -> if d = Out then true else false) p  in
    let inouts = List.filter (fun p -> match p with Port(n, d, t, v) -> if d = Inout then true else false) p  in
    
    (* Write the "generic" version of the function which requires all generics to be specified *)
    
    os ("let g_" ^ name ^ "\n");
    (* generics *)
    iter (fun g -> match g with Port(n, d, t, v) -> os (" generic_" ^ n ^ " (* generic -" ^ generic_string g ^ " *)\n")) g;
    (* ports *)
    iter (fun p -> match p with Port(n, d, t, v) -> os (" input_" ^ n ^ " (* input -" ^ port_string p ^ " *)\n")) inputs;
    os ("=\n");
    (* inout/output sizes *)
    iter (fun p -> match p with Port(n, d, t, v) -> os (" let inout_" ^ n ^ " = " ^ string (width_of_type t) ^ " in\n")) inouts;
    iter (fun p -> match p with Port(n, d, t, v) -> os (" let output_" ^ n ^ " = " ^ string (width_of_type t) ^ " in\n")) outputs;
    (* check inputs *)
    iter (fun p -> match p with Port(n, d, t, v) -> os (" check_width \"" ^ name ^ "\" \"" ^ n ^ "\" input_" ^ n ^ " " ^ string (width_of_type t) ^ ";\n")) inputs;
    (* instance *)
    os (" let inst = Inst.makegio \"" ^ String.uppercase name ^ "\" [\n");
    (* generics *)
    iter (fun p -> match p with Port(n, d, t, v) -> os ("  (\"" ^ String.uppercase n ^ "\", " ^ ml_string_of_type t v ^ ", generic_" ^ n ^ ");\n")) g;
    os (" ] [\n");
    (* inouts *)
    iter (fun p -> match p with Port(n, d, t, v) -> os ("  \"" ^ String.uppercase n ^ "\" ==> inout_" ^ n ^ ";\n")) inouts;
    os (" ] [\n");
    (* inputs *)
    iter (fun p -> match p with Port(n, d, t, v) -> os ("  \"" ^ String.uppercase n ^ "\" ==> input_" ^ n ^ ";\n")) inputs;
    os (" ] [\n");
    (* outputs *)
    iter (fun p -> match p with Port(n, d, t, v) -> os ("  \"" ^ String.uppercase n ^ "\" ==> output_" ^ n ^ ";\n")) outputs;
    os (" ]in\n\n");
    os (" inst\n\n\n");
    
    (* write the "list" version of the function with which arbitrary generics need to be specified *)
    os ("let l_" ^ name ^ " generics_list \n");
    (* ports *)
    iter (fun p -> match p with Port(n, d, t, v) -> os (" input_" ^ n ^ "\n")) inputs;
    os ("=\n");
    (* find the generics *)
    iter (fun g -> match g with Port(n, d, t, v) -> os (" let generic_" ^ n ^ " = if generics_list = [] then None else List.try_assoc \"" ^ String.uppercase n ^ "\" generics_list in\n")) g;
    (* call the base function *)
    os (" g_" ^ name ^ "\n" ^ 
        "  " ^ (fold_strings " " (map (fun g -> match g with Port(n, d, t, v) -> "generic_" ^ n) g)) ^ 
        "  " ^ (fold_strings " " (map (fun g -> match g with Port(n, d, t, v) -> "input_" ^ n) inputs)) ^ "\n\n");
    
    (* write the "non-generic" version of the function which takes no generics.  This one will take the "proper" name as I suspect it will get most use *)
    os ("let " ^ name ^ " \n");
    (* ports *)
    iter (fun p -> match p with Port(n, d, t, v) -> os (" input_" ^ n ^ "\n")) inputs;
    os ("=\n");
    (* call the base function *)
    os (" l_" ^ name ^ " []\n" ^ 
        "  " ^ (fold_strings " " (map (fun g -> match g with Port(n, d, t, v) -> "input_" ^ n) (inputs)) ^ "\n\n"))
;;

let write_ml_instatiations_inst components = 
  os ("(** Xilinx instantiation primitives using new syntax *)
module Xilinxi = 
begin

exception Xilinx_error of string
let failwith s = raise (Xilinx_error s)

let check_width module_name signal_name signal n = 
  if (width signal) <> n then 
  failwith (\"module \" ^ module_name ^ \" : port \" ^ signal_name ^ \" : expecting \" ^ 
            string n ^ \" bit signal, got \" ^ string (width signal) ^ \" bit signal\")

");
  iter write_ml_instatiation_inst components;
  os ("
end
")
;;

(*******************************************************************************)

(* load the components from the vhdl unisim file and figure out missing port sizes *)
let components = map map_port_sizes (load_xilinx_components())
;;

let _ = 
  os ("(** Xilinx instantiation primitives using new syntax *)
namespace DigitalLogic.Circuits
open DigitalLogic
open Circuit
open Signal

")
;;

(* write_vhdl_package components *)
write_ml_instatiations components
;;

(* write_vhdl_package components - new stnax *)
write_ml_instatiations_inst components
;;

close_output()
;;
