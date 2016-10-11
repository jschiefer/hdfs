%{

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

open Xilinx_types

%}

%token P_component
%token P_is
%token P_end
%token <string> P_ident
%token P_attribute 
%token P_of
%token P_ns
%token P_ps
%token P_in
%token P_inout
%token P_out
%token P_port
%token P_equals
%token P_generic
%token P_downto
%token P_to
%token P_semi
%token P_colon
%token P_obracket
%token P_cbracket
%token P_dot

%token <string> P_int
%token <string> P_char
%token <string> P_string

%token P_std_logic_type
%token P_std_logic_vector_type
%token P_bit_type
%token P_bit_vector_type
%token P_integer_type
%token P_real_type
%token P_bool_type
%token P_string_type
%token P_time_type

%token P_library
%token P_use
%token P_signal
%token P_package

%token P_endfile

%type <Xilinx_types.xilinx_comp_t list> main
%start main

%% 

main: 
    header xcomponent_list P_end P_ident P_semi
                                            { $2 }
                                            
header:
    P_library P_ident P_semi P_use P_ident P_dot P_ident P_dot P_ident P_semi P_package P_ident P_is 
    P_attribute P_ident P_colon P_string_type P_semi signal_list
                                            { }

signal_list:
    signal                                  { }
  | signal_list signal                      { }
  
signal:
    P_signal P_ident P_colon vhdtype P_semi { }
  | P_signal P_ident P_colon vhdtype P_colon P_equals data P_semi
                                            { }

xcomponent_list:
    xcomponent                              { [$1] }
  | xcomponent_list xcomponent              { $1 @ [$2] }
  
xcomponent: 
    P_component P_ident generics ports P_end P_component P_semi
    P_attribute P_ident P_of P_ident P_colon P_component P_is P_string P_semi
                                            { Component ($2, $3, $4) }

generics:
                                            { [] }
  | P_generic P_obracket ports_list P_cbracket P_semi
                                            { $3 }

ports:
                                            { [] }
  | P_port P_obracket ports_list P_cbracket P_semi
                                            { $3 }
                                            
ports_list:
    port_decl                               { [$1] }
  | ports_list P_semi port_decl             { $1 @ [$3] }
  
port_decl:
    P_ident P_colon dirn vhdtype            { Port($1, $3, $4, None) }
  | P_ident P_colon dirn vhdtype P_colon P_equals data timespec
                                            { Port($1, $3, $4, Some($7 ^ $8)) }

vhdtype: 
    P_std_logic_type                        { T_std_logic }
  | P_std_logic_vector_type range           { let l,r = $2 in T_std_logic_vector(l,r) }
  | P_bit_type                              { T_bit }
  | P_bit_vector_type range                 { let l,r = $2 in T_bit_vector(l,r) }
  | P_integer_type                          { T_integer }
  | P_real_type                             { T_real }
  | P_bool_type                             { T_boolean }
  | P_string_type                           { T_string }
  | P_time_type                             { T_time }

data:
    P_char                                  { $1 }
  | P_int                                   { $1 }
  | P_ident                                 { $1 }
  | P_string                                { $1 }
  
timespec:
                                            { "" }
  | P_ps                                    { " ps" }
  | P_ns                                    { " ns" }

range:
                                            { (-1,-1) }
  | P_obracket P_int P_downto P_int P_cbracket        
                                            { (int $2, int $4) }
  | P_obracket P_int P_to P_int P_cbracket        
                                            { (int $2, int $4) }

dirn:
                                            { In }
  | P_in                                    { In }
  | P_inout                                 { Inout }
  | P_out                                   { Out }
  
