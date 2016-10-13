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

(** Circuit resource reporting *)
module DigitalLogic.Resources

open DigitalLogic
open Circuit
open Signal

(** Writes a report of the hardware primitives used in the circuit to the given file *)
let report f (circuit : Circuit) =
  let t0 = System.DateTime.Now in
  let timing s t0 t1 = System.Console.WriteLine("{0}: {1}", s, t1-t0) in

  let os (f:System.IO.TextWriter) (s:string) = f.Write(s) in
  let os = os f in
  
  let b_add = ref [] in
  let b_sub = ref [] in
  let b_mulu = ref [] in
  let b_muls = ref [] in
  let b_and = ref [] in
  let b_or = ref [] in
  let b_xor = ref [] in
  let b_eq = ref [] in
  let b_lt = ref [] in
  let b_cat = ref [] in
  let u_not = ref [] in
  let signal_empty = ref [] in
  let signal_const = ref [] in
  let signal_binop = ref [] in
  let signal_unop  = ref [] in
  let signal_wire  = ref [] in
  let signal_mux   = ref [] in
  let signal_select= ref [] in
  let signal_reg   = ref [] in
  let signal_mem   = ref [] in
  let signal_behave= ref [] in
  let signal_inst  = ref [] in
  let signal_tri   = ref [] in

  let incr op arg = op := arg :: !op in

  let incr_binop arg = function
  | B_add -> incr b_add arg
  | B_sub -> incr b_sub arg
  | B_mulu -> incr b_mulu arg
  | B_muls -> incr b_muls arg
  | B_and -> incr b_and arg
  | B_or -> incr b_or arg
  | B_xor -> incr b_xor arg
  | B_eq -> incr b_eq arg
  | B_lt -> incr b_lt arg
  | B_cat -> incr b_cat [] in

  let incr_unop arg = function
  | U_not -> incr u_not arg in

  let incr_signal (x : Signal) = match x.signal with
  | Signal_empty                    -> incr signal_empty  []
  | Signal_const    (a,w,c)         -> incr signal_const  []
  | Signal_binop    (a,w,op,s0,s1)  -> incr signal_binop  []; incr_binop [width s0; width s1] op 
  | Signal_unop     (a,w,op,s)      -> incr signal_unop   []; incr_unop [w] op
  | Signal_wire     (a,w,n,d)       -> incr signal_wire   []
  | Signal_mux      (a,w,sel,d)     -> incr signal_mux    [1 <<< (width sel); w]
  | Signal_select   (a,hi,lo,s)     -> incr signal_select []
  | Signal_reg      (a,w,clk,rst,rstval,ena,d) -> incr signal_reg [width d]
  | Signal_mem      (a,dw,aw,size,clk,w,we,d,r)-> incr signal_mem [size; dw] 
  | Signal_behave   (a,w,b,d)       -> incr signal_behave [] 
  | Signal_inst     (a,n,m,g,io,i,o)  -> incr signal_inst   [] 
  | Signal_tri      (a,w,d)         -> incr signal_tri    [List.length d; w] in (* we should probably also be interested in the number of drivers *)

  List.iter incr_signal circuit.All;

  let rep_fn arg name = 
    let l = !arg in
    let num = List.length l in
    match num with
    | 0 -> ()
    | _ -> (
      os (name ^ ": " ^ string num ^ "\n");
      let parts = List.length (List.head l) in
      match parts with
      | 0 -> ()
      | n -> (
        let rec build l = 
          match l with
          | [] -> []
          | hd :: tl ->
            let a = List.filter ((=) hd) tl in
            let b = List.filter ((<>) hd) tl in
            (hd, (List.length a)+1) :: build b in
        let l = build l in
        let s l = fold_strings " x " (List.map string l) in
        List.iter (fun (v,c) -> os ("  " ^ string c ^ ": " ^ s v ^ " bits \n")) l
      )
    ) in
  
  let logic_data = [
    b_add        ,"add",    rep_fn;
    b_sub        ,"sub",    rep_fn; 
    b_mulu       ,"mulu",   rep_fn;
    b_muls       ,"muls",   rep_fn;
    b_and        ,"and",    rep_fn;
    b_or         ,"or",     rep_fn;
    b_xor        ,"xor",    rep_fn;
    b_eq         ,"eq",     rep_fn;
    b_lt         ,"lt",     rep_fn;
    u_not        ,"not",    rep_fn;
    signal_mux   ,"mux",    rep_fn;
    signal_reg   ,"reg",    rep_fn;
    signal_mem   ,"mem",    rep_fn;
    signal_tri   ,"tri",    rep_fn;
  ] in
  let language_data = [
    b_cat        ,"cat",    rep_fn;
    signal_select,"select", rep_fn;
    signal_const ,"const",  rep_fn;
    signal_wire  ,"wire",   rep_fn;
    signal_binop ,"binop",  rep_fn;
    signal_unop  ,"unop",   rep_fn;
    signal_empty ,"empty",  rep_fn;
    signal_behave,"behave", rep_fn;
    signal_inst  ,"inst",   rep_fn;
  ] in
   
  os ("*****\nLogic\n*****\n");
  List.iter (fun (arg,name,fn) -> fn arg name) logic_data;
  os ("********\nLanguage\n********\n");
  List.iter (fun (arg,name,fn) -> fn arg name) language_data;
  os ("***********************\nInstantiated components\n***********************\n");
  List.iter 
    (fun x ->
      match x.su with
      | Signal_inst(a,n,m,g,io,i,o) ->
        printf " %s\n" n
      | _ -> ()
    ) circuit.All;

  //timing "Wrote report in" t0 System.DateTime.Now
