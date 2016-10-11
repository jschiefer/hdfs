{

open Lexing
open Verilog_parser

let keyword_table = Hashtbl.create 10
  let _ = List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
  [     
    "module",            T_module;
    "endmodule",         T_endmodule;
    "macromodule",       T_macromodule;
    "udp",               T_udp;
    "parameter",         T_parameter;
  ]

(*
let lineNum = ref 1 
let init_linenum () = lineNum := 1
let incr_linenum () = 
    lineNum := !lineNum + 1

let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        Lexing.pos_bol = pos.Lexing.pos_cnum }
*)

(*
let incr_linenum (lexbuf:lexbuf) = 
  let inc_lnum bol pos = 
    let lnum = pos.Lexing.pos_lnum in 
    {pos with Lexing.pos_lnum =  lnum+1; Lexing.pos_bol = bol }
  in
  lexbuf.EndPos <- inc_lnum (Lexing.lexeme_end lexbuf) (Lexing.lexeme_end_p lexbuf)
*)

let incr_linenum (lexbuf:lexbuf) = lexbuf.EndPos <- lexbuf.EndPos.NextLine

let failwith s = failwith ("LEX ERROR: " ^ s)

let char_list_of_string s = 
    let rec ss i = 
        if i < String.length s then s.[i] :: ss (i+1)
        else [] in
    ss 0

let string_of_char_list s = 
    List.fold_right (fun c s -> String.of_char c ^ s) s ""

let string_filter chars str = 
    string_of_char_list 
        (List.filter (fun c -> not (String.contains chars c)) 
            (char_list_of_string str))

}

let newline = ('\n' | '\r' '\n')
let not_newline = [^ '\r' '\n' ]
let digit = ['0'-'9']
let unsigned_number = ['0'-'9' '_']+
let number = ['+' '-']* unsigned_number

rule token = parse
  
  (* skip blanks *)
  | [' ' '\t'] { token lexbuf }                      
  
  (* skip single line comments *)
  | '/''/'not_newline*newline { incr_linenum lexbuf; token lexbuf }     

  | newline { incr_linenum lexbuf; token lexbuf }

  | '/''*' { end_comment lexbuf }

  (* operator characters *)
  | ';' { T_semi }
  | ',' { T_comma }
  | ':' { T_colon }
  | '!' { T_question_mark }
  | '.' { T_dot }
  | '(' { T_ob }
  | ')' { T_cb }
  | '{' { T_ocb }
  | '}' { T_ccb }
  | '[' { T_osb }
  | ']' { T_csb }
  | '+' { T_add }
  | '-' { T_sub }
  | '*' { T_mul }
  | '/' { T_div }
  | '^' { T_hat }
  | '~' { T_not }
  | '!' { T_exclam }
  | '&' { T_and }
  | '|' { T_or }
  | '%' { T_mod }
  | '=' { T_eq }
  | '<' { T_less }
  | '>' { T_great }

  (* number handling *)
  
  (* simple decimal number *)
  | number {
        let id = lexeme lexbuf in
        T_number(BaseDecimal, id)
    }

  (* number with base specification *)
  | unsigned_number? '\'' [ 'b' 'B' 'o' 'O' 'd' 'D' 'h' 'H' ] ['0'-'9' 'A'-'F' 'a'-'f' '_' 'x' 'X' 'z' 'Z']+ {
        let id = lexeme lexbuf in
        try
            let tick_pos = String.index id '\'' in
            let base = match id.[tick_pos+1] with
            | 'b' | 'B' -> BaseBinary
            | 'o' | 'O' -> BaseOctal
            | 'd' | 'D' -> BaseDecimal
            | 'h' | 'H' -> BaseHex
            | _ -> failwith ("Invalid base " ^ String.of_char id.[tick_pos]) in
            T_number(base, String.sub id (tick_pos+1) (String.length id - tick_pos - 1))
        with _ ->
            failwith ("Cannot get format for number " ^ id)
    }

  (* Floating point number *)
  | number '.' unsigned_number
  | number ('.' unsigned_number)? ['e' 'E'] number {
        let id = lexeme lexbuf in
        T_number(BaseFloat, id)
    }

  (* identifiers *)
  | ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '$']* { 
        // Not implemented: escaped identifiers
        let id = lexeme lexbuf in 
        try let tok = Hashtbl.find keyword_table id in tok
        with Not_found -> 
            //printf "IDENT: %s\n" id;
            T_ident id 
    }

  | '$' ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '$']* { 
        // Not implemented: escaped identifiers
        let id = lexeme lexbuf in
        T_system_ident id 
    }

  | '"' [^ '\r' '\n' '"' ]* '"' { let str = lexeme lexbuf in T_string str }


  | eof { T_endfile }

and end_comment = parse
    
    '*''/' { token lexbuf }
  | newline { incr_linenum lexbuf; end_comment lexbuf }
  | eof { T_endfile }
  | _ { end_comment lexbuf }
  
