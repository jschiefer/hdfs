%{

type module_t = string 
type ident_t = string list
type base_t = BaseBinary | BaseOctal | BaseDecimal | BaseHex | BaseFloat
type number_t = base_t * string
type hdl_t = 
    | Module of module_t
    | MacroModule
    | Udp

%}

%token T_module
%token T_macromodule
%token T_endmodule
%token T_udp
%token T_parameter
%token <string> T_ident
%token <string> T_system_ident
%token <number_t> T_number
%token <base_t> T_base
%token T_semi
%token T_comma
%token T_colon
%token T_question_mark
%token T_dot
%token T_ob
%token T_cb
%token T_ocb
%token T_ccb
%token T_osb
%token T_csb
%token T_add
%token T_sub
%token T_mul
%token T_div
%token T_hat
%token T_not
%token T_exclam
%token T_and
%token T_or
%token T_mod
%token T_eq
%token T_less
%token T_great
%token <string> T_string

%token T_endfile

%start source_text
%type <hdl_t list> source_text

%% 

/*************************************** 1. SOURCE TEXT ****************************************/

source_text:
    description_list { $1 }

description_list:
    { [] }
  | description description_list { $1 :: $2 }

description:
	p_module { $1 }
  | udp { $1 }

p_module: 
    T_module name_of_module opt_list_of_ports T_semi module_items T_endmodule { Module($2) }
  | T_macromodule name_of_module opt_list_of_ports T_semi module_items T_endmodule { MacroModule }

udp: 
    T_udp { Udp }

name_of_module: 
    T_ident { $1 }

opt_list_of_ports:
    { [] }
  | T_ob comma_ports T_cb { $2 }

comma_ports:
    port { [$1] }
  | port T_comma comma_ports { $1 :: $3 }

port:
    {}
  | port_expression {} 
  | T_dot name_of_port T_ob port_expression T_cb {}

port_expression:
    port_reference {}
  | T_ocb port_reference_list T_ccb {}

port_reference_list:
    port_reference { [$1] }
  | port_reference T_comma port_reference_list { $1 :: $3 }

port_reference:
    name_of_variable { $1 }
  | name_of_variable T_osb constant_expression T_csb { $1 }
  | name_of_variable T_osb constant_expression T_colon constant_expression T_csb { $1 }

name_of_port:
    T_ident { $1 }

name_of_variable:
    T_ident { $1 }

module_items:
    module_item {[]}
  | module_item module_items {[]}

module_item:
    {}
  | parameter_declaration {}
  | input_declaration {}
  | output_declaration {}
  | inout_declaration {}
/*  | net_declaration {}
  | reg_declaration {}
  | time_declaration {}
  | integer_declaration {}
  | real_declaration {}
  | event_declaration {}
  | gate_declaration {}
  | UDP_instantiation {}
  | module_instantiation {}
  | parameter_override {}
  | continuous_assign {}
  | specify_block {}
  | initial_statement {}
  | always_statement {}
  | task {}
  | function {}
*/

/*************************************** 2. DECALRATIONS ****************************************/

parameter_declaration:
	T_parameter list_of_param_assignments T_semi {}

list_of_param_assignments:
    param_assignment { [$1] }
  | param_assignment T_comma list_of_param_assignments { $1::$3 }

param_assignment: T_ident T_eq constant_expression {}

input_declaration
	T_input <range>? <list_of_variables> ;

output_declaration
	T_output <range>? <list_of_variables> ;

inout_declaration
	T_inout <range>? <list_of_variables> ;

list_of_variables:
	||= nettype <drive_strength>? <expandrange>? <delay>? <list_of_assignments> ;
    
list_of_assignments:
    assignment {}
  | assignment T_comma list_of_assignments {}

nettype: wire  tri  tri1  supply0  wand  triand  tri0  supply1  wor  trior  trireg

/*************************************** 7. EXPRESSIONS ****************************************/

lvalue:
    identifier {}
  | identifier T_osb expression T_csb {}
  | identifier T_osb constant_expression T_colon constant_expression T_csb {}
  | concatenation {}

constant_expression:
    expression {}

mintypmax_expression:
    expression {}
  | expression T_colon expression T_colon expression {}

expression:
    primary {}
  | unary_operator primary {}
  | expression binary_operator expression {}
  | expression T_question_mark expression T_colon expression {}
  | string {}

unary_operator:
    T_add {}
  | T_sub {}
  | T_exclam {}
  | T_not {}
  | T_and {}
  | T_not T_and {}
  | T_or {}
  | T_hat T_or {}
  | T_hat {}
  | T_not T_hat {}

binary_operator:
    T_add {}
  | T_sub {}
  | T_mul {}
  | T_div {}
  | T_mod {}
  | T_eq T_eq {}
  | T_exclam T_eq {}
  | T_eq T_eq T_eq {}
  | T_exclam T_eq T_eq {}
  | T_and T_and {}
  | T_or T_or {}
  | T_less {}
  | T_less T_eq {}
  | T_great  {}
  | T_great T_eq {}
  | T_and {}
  | T_or {}
  | T_hat {}
  | T_hat T_not {}
  | T_great T_great {}
  | T_less T_less {}

string: 
    T_string { $1 }

concatenation:
    T_ocb concatenation_list T_ccb {}

concatenation_list:
    expression { [$1] }
  | expression T_comma concatenation_list { $1 :: $3 }

multiple_concatenation:
	T_ocb expression concatenation T_ccb {}

primary:
    number {}
  | identifier {}
  | identifier T_osb expression T_csb {}
  | identifier T_osb constant_expression T_colon constant_expression T_csb {}
  | concatenation {}
  | multiple_concatenation {}
  | function_call {} 
  | T_ob mintypmax_expression T_cb {}

number:
    T_number { $1 }

function_call:
    name_of_function T_ob expression_comma_list T_cb {}
  | name_of_system_function T_ob expression_comma_list T_cb {}
  | name_of_system_function {}

expression_comma_list:
    expression { }
  | expression T_comma expression_comma_list { }

name_of_function:
	T_ident { $1 }

name_of_system_function:
	T_system_ident { $1 }

/*************************************** 8. GENERAL ****************************************/

identifier:
    T_ident { [$1] }
  | T_ident T_dot identifier { $1 :: $3 }




