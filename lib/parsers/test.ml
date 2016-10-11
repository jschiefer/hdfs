open Verilog_parser

let os = output_string stdout 

let load_verilog name =
  let open_file name = 
    try open_in_bin name
    with _ -> failwith ("Couldnt open " ^ name) in
  let fin = open_file name in
  let lexbuf = Lexing.from_channel fin in
  let components = 
    try 
      Verilog_parser.source_text Verilog_lexer.token lexbuf 
    with 
    | x -> 
      let sp (p:Lexing.Position) = "[l:" ^ string_of_int (p.Line + 1) ^ " c:" ^ string_of_int (p.Column + 1) ^ "]" in
      output_string stdout ("Failed: file " ^ name ^ " " ^ sp lexbuf.StartPos ^ " to " ^ sp lexbuf.EndPos ^ "\n");
      raise x
  in
  let _ = close_in fin in
  components

let print_verilog m = 
  match m with
  | Module n -> 
    printf "module %s\n" n
  | MacroModule -> 
    printf "macromodule...skipping (???)\n"
  | Udp -> 
    printf "Udp...skipping (???)\n"

let _ = 
  try 
    let comps = load_verilog Sys.argv.(1) in
    printf "SUCCESS: (%i modules)\n" (List.length comps);
    List.iter (fun i -> print_verilog i) comps
  with _ -> ()



