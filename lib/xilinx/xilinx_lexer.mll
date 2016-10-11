{

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

open Xilinx_parser
open Lexing

let keyword_table = Hashtbl.create 10
do List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
        [     
            "component",            P_component;
            "is",                   P_is;
            "end",                  P_end;
            "attribute",            P_attribute;
            "of",                   P_of;
            "port",                 P_port;
            "generic",              P_generic;
            "downto",               P_downto;
            "to",                   P_to;

            "ps",                   P_ps;
            "ns",                   P_ns;

            "in",                   P_in;
            "inout",                P_inout;
            "out",                  P_out;

            "STD_LOGIC",            P_std_logic_type;
            "std_logic",            P_std_logic_type;
            "STD_ULOGIC",           P_std_logic_type;
            "std_ulogic",           P_std_logic_type;
            "STD_LOGIC_VECTOR",     P_std_logic_vector_type;
            "std_logic_vector",     P_std_logic_vector_type;
            "INTEGER",              P_integer_type;
            "integer",              P_integer_type;
            "REAL",                 P_real_type;
            "real",                 P_real_type;
            "BOOLEAN",              P_bool_type;
            "boolean",              P_bool_type;
            "STRING",               P_string_type;
            "string",               P_string_type;
            "BIT",                  P_bit_type;
            "bit",                  P_bit_type;
            "BIT_VECTOR",           P_bit_vector_type;
            "bit_vector",           P_bit_vector_type;
            "TIME",                 P_time_type;
            "time",                 P_time_type;

            "library",              P_library;
            "use",                  P_use;
            "signal",               P_signal;
            "package",              P_package;
        ]

let lineNum = ref 1
let init_linenum () = lineNum := 1
let incr_linenum () = lineNum := !lineNum + 1

}

let newline = ('\n' | '\r' '\n')
let not_newline = [^ '\r' '\n' ]
let digit = ['0'-'9']

rule token = parse
  
  | [' ' '\t']              { token lexbuf }                      (* skip blanks *)
  | '-''-'not_newline*newline
                            { incr_linenum (); token lexbuf }     (* skip single line comments *)
  | newline                 { incr_linenum (); token lexbuf }
  | ';'                     { P_semi }
  | ':'                     { P_colon }
  | '('                     { P_obracket }
  | ')'                     { P_cbracket }
  | '='                     { P_equals }
  | '.'                     { P_dot }
  
  | '-'?['0'-'9']+          { let lxm = lexeme lexbuf in P_int lxm }

  | '\'' ['A'-'Z' 'a'-'z' '0'-'9' ] * '\'' 
                            { let lxm = lexeme lexbuf in P_char lxm }

  | '"' ['A'-'Z' 'a'-'z' '0'-'9' '_' '/' '\\' '.'] * '"' 
                            { let lxm = lexeme lexbuf in P_string lxm }
  | 'X''"' ['A'-'Z' 'a'-'z' '0'-'9' '_' '/' '\\' '.'] * '"' 
                            { let lxm = lexeme lexbuf in P_string lxm }

  | ['-']?digit+('.'digit+)?(['e''E']digit+)?   
                            { let lxm = lexeme lexbuf in P_string lxm }

  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * 
                            { let id = String.lowercase (lexeme lexbuf) in 
                              try let tok = Hashtbl.find keyword_table id in tok
                              with Not_found -> P_ident id }
                              
  | eof                     { P_endfile }
