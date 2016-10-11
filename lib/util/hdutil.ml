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

(** General utilities useful for hardware design. *)
module DigitalLogic.Util

open DigitalLogic.Numeric.Ops
open DigitalLogic.Circuit
open DigitalLogic.Signal

exception Reduce_error of string

(*****************************************************************************)
(*****************************************************************************)

(** given a (positive) value calculates the required bit width ie ceil(log2(x)) *)
let rec clog2 x = match x with 0 | 1 -> 1 | x -> 1 + (clog2 (x/2)) 

(** 2 ^ n in integer arithmetic *)
let rec pow2 n = 2 * (if n<=1 then 1 else pow2 (n-1))

(** creates the list [ 0; 1; 2; .. ; (n-1) ] (note that the new list comprehension syntax in F# can perform this task) *)
let range n = 
    let rec r0 n i = if n = i then [] else i :: (r0 n (i+1)) in
    r0 n 0 

(** select a range from the list *)
let lselect l lo hi = 
    let rec ls l idx lo hi = 
        if idx > hi then []
        else if idx < lo then ls (List.tl l) (idx+1) lo hi
        else (List.hd l) :: (ls (List.tl l) (idx+1) lo hi) in 
    ls l 0 lo hi 

(** Selects the even elements from a list *)
let leven l = 
  let rec r l n = 
    match l with
    | [] -> []
    | hd :: tl ->
      if (n &&& 1) = 0 
      then hd :: (r tl (n+1))
      else (r tl (n+1)) in
  r l 0
      
(** Selects the odd elements from a list *)
let lodd l = 
  let rec r l n = 
    match l with
    | [] -> []
    | hd :: tl ->
      if (n &&& 1) = 1 
      then hd :: (r tl (n+1))
      else (r tl (n+1)) in
  r l 0


(*****************************************************************************)
(* vector operations *)
(*****************************************************************************)

(** number of bits required to represent the given value, which may be signed *)
let num_bits v = 
    if v < 0 then (clog2 (abs (v+1))) + 1
    else clog2 v

(** absolute value *)
let abs a = mux2 (msb a) (- a) a 

(** pad vector (at right handside ie below lsb) up to a power of two with the given constant (which should be 1 bit) *)
let pad_right_pow2 v c =
  let len = width v in
  let log = clog2 len in
  let llen = pow2 log in
  if llen - len > 0 then
    v ++ (repeat c (llen-len))
  else v

(* should these be moved to design.ml? *)

(** unsigned addition.  Output is 1 bit larger than parameters so cannot overflow *)
let (+~+) a b = (gnd ++ a) + ((gnd ++ b) : Signal)

(** signed addition.  Output is 1 bit larger than parameters so cannot overflow *)
let (+~-) a b = ((msb a) ++ a) + (((msb b) ++ b) : Signal)     

(** unsigned subtaction.  Output is 1 bit larger than parameters so cannot overflow *)
let (-~+) a b = (gnd ++ a) - ((gnd ++ b) : Signal)                

(** signed subtaction.  Output is 1 bit larger than parameters so cannot overflow *)
let (-~-) a b = ((msb a) ++ a) - (((msb b) ++ b) : Signal)

(** Counts from 0 to (max-1) then from zero again.  If max == 1<<n, then the comparator is not generated and overflow arithmetic used instead *)
let mod_counter max c = 
  let w = width c in
  let lmax = 1 <<< w in
  if lmax = (max + 1) 
  then c + 1
  else mux2 (c ==~ max) (zero w) (c + 1)

(** creates a tree of operations.  The arity of the operator is configurable *)
let rec tree arity ops l =
  let split l n =
     let (lh,ll,_) = List.fold_left (fun (l0,l1,m) e ->
       if m < n then ((e::l0),l1,m+1) else (l0,e::l1,m+1)) ([],[],0) l in
     (List.rev lh, List.rev ll) in
  let rec t0 l =
    let l0,l1 = split l arity in
    if l1 = [] then [ ops l0 ]
    else (ops l0) :: (t0 l1) in
  match l with
  | [] -> failwith "Invalid list given to tree"
  | [a] -> a
  | _ -> tree arity ops (t0 l) 

(** creates a binary tree of operations *)
let binary_tree uop bop = tree 2 
  (function [a] -> uop a | [a;b] -> bop a b | _ -> failwith "Binary tree expects no more than two arguments per operation")

(*****************************************************************************)
(*****************************************************************************)

(** Builds a pipeline of registers. *)
let rec pipeline n clock reset reset_val enable d = 
  if n=0 then d
  else reg clock reset reset_val enable (pipeline (n-1) clock reset reset_val enable d)

let pipelinec n enable d = pipeline n clock reset Signal.empty enable d

(** Same as pipeline except that reset is not specified.  This allows it to be inferred as SRL16 in Virtex FPGAs. *)
let rec pipeline_srl n clock enable d = 
  if n=0 then d
  else reg clock Signal.empty Signal.empty enable (pipeline_srl (n-1) clock enable d)

let pipelinec_srl n enable d = pipeline_srl n clock enable d 

(*****************************************************************************)
(*****************************************************************************)

(** binary encoding to one hot encoding.  N cases where N is <= (2 ** width s).  Unused cases driven to 0 *)
let binary_to_one_hot addr n = 
  if n <= 0 then Signal.empty
  else (
    assert (n <= (1 <<< (width addr)));
    let wid = width addr in
    let one_hot = b_wire0 n in
    behave [
      b_switch (addr) (
        List.map 
          (fun i -> b_case (consti wid i) [ one_hot $== const1h n i; ])
            [ 0 .. (n-1) ]
      )
    ];
    one_hot.q
  )

(** convert one hot vector to binary.  Behavioural implementation using a case statement.
    If the one hot vector is invalid  (zero or more than one bit is set) then the output is set to zero *)
let one_hot_to_binary_b onehot = 
  let owidth = width onehot in
  let bwidth = clog2 (owidth-1) in
  let binary = b_wire0 bwidth in
  behave 
    [
      b_switch onehot
        [ for i in { 0 .. owidth-1 } -> b_case (const1h owidth i) [ binary $== consti bwidth i ] ]
    ];
  binary.q

(** convert one hot vector to binary.  Or based implementation.
    Two versions are possible, with the or's composed linearly, or as a tree. *)
let one_hot_to_binary_a arity onehot = 
  let owidth = width onehot in
  let bwidth = clog2 (owidth-1) in
  let leaves = [ for i in 0 .. owidth-1 -> onehot.[i].mux2(consti bwidth i, 0) ] in
  if arity <= 1 then reduce_1 (|||) leaves
  else tree arity (reduce_1 (|||)) leaves

(* The or-tree version with an arity of 4 produces the smallest circuit, 
   so that's the default.  Just or'ing everything together without a tree 
   structure (arity <= 1), however, produces the fastest circuit.  *)
let one_hot_to_binary = one_hot_to_binary_a 4

(*****************************************************************************)
(*****************************************************************************)

(* counts the number of leading zeros in the input vector.  No idea how it really works *)
let count_leading_zeros vector = 

  let llen = clog2 ((width vector)-1) in
  if (pow2 llen) <> (width vector) then 
    failwith "count_leading_zeros: input vector must be a power of two in length";

  (* the magic cells... *)
  let cell2 a b = [ (a &~ (~~~ b)); (a &~ b) ] in
  let celln ca a b = [ a |~ (ca &~ b) ] in
  let rec build_cell msb al bl = (* al, bl same length, start from lsb *)
    match List.length al with
    | 1 -> cell2 (List.hd al) (List.hd bl)
    | _ -> 
      let a,b = List.hd al, List.hd bl in
      (celln msb a b) @ (build_cell msb (List.tl al) (List.tl bl)) in
  let build_cell al bl = List.rev (build_cell (List.hd al) (List.rev al) (List.rev bl)) in

  (* [a;b;c;d;...] -> [(a,b);(c;d);...] *)
  let rec pair_list l = 
    match l with
    | [] -> []
    | [a] -> failwith "pair_of_lists: input list must be of even length"
    | a :: b :: tl -> (a,b) :: (pair_list tl) in

  (* pair the list, apply the cells which do their magic, which gives a list which is then paired and the process repeated.
     Basically a tree is contructed in which pairs are taken from the list which start off as one bit each.  This yields a 
     list of 2 bit results.  At the next level two bit pairs yield 3 bit results, and so on until there is one result. *)
  let rec build_stage bits = 
    match bits with
    | [(al,bl)] -> Signal.concat (build_cell al bl)
    | _ -> build_stage (pair_list (List.map (fun (al,bl) -> build_cell al bl) bits)) in

  build_stage 
    (pair_list (List.map (fun x -> [x]) (bits (~~~ vector))))
      
(*****************************************************************************)
(*****************************************************************************)

(* Build roms based on the given function operating on 1 or 2 arguments *)

let op_rom' fn in_bits out_bits = 
  let rec aux i = 
    if i = (1 <<< in_bits) - 1 then []
    else consti out_bits (fn i) :: aux (i+1)
  in
  aux 0

let op_rom_1 fn in_bits out_bits = 
  fun addr -> mux addr (op_rom' fn in_bits out_bits)

let op_rom_2 fn in_bits_0 in_bits_1 out_bits = 
  let rec aux i = 
    if i = (1 <<< in_bits_0) - 1 then []
    else op_rom' (fn i) in_bits_1 out_bits @ aux (i+1)
  in
  fun (addr0:Signal) (addr1:Signal) -> mux (addr0 ++ addr1) (aux 0)

let rom_add (a:Signal) (b:Signal) = op_rom_2 (+) a.width b.width (max a.width b.width)
let rom_sub (a:Signal) (b:Signal) = op_rom_2 (-) a.width b.width (max a.width b.width)
let rom_mul (a:Signal) (b:Signal) = op_rom_2 ( * ) a.width b.width (a.width + b.width)

(*****************************************************************************)
(*****************************************************************************)

let async_pulse_handshake clock_src clock_dst reset start =

  let reg_s = reg clock_src reset Signal.empty Signal.empty in
  let reg_d = reg clock_dst reset Signal.empty Signal.empty in
  
  let fb reg fn = 
    let d = wire 1 in
    d <== (reg (fn d));
    d
  in

  let take = wire 1 -- "async_handshake_take" in

  let take1 = fb reg_s (fun (d:Signal) -> (start ^^^ d)) in
  let got1 = fb reg_d (fun (d:Signal) -> (take ^^^ d)) in
  
  let got2 = reg_s got1 in
  let got3 = reg_s got2 in
  let got4 = reg_s got3 in
  let take2 = reg_d take1 in
  let take3 = reg_d take2 in
  let take4 = reg_d take3 in
  
  take <== (take3 ^^^ take4);
  
  take, (got3 ^^^ got4) -- "async_handshake_got"

(*****************************************************************************)
(*****************************************************************************)

(** Binary reflected gray code *)
let make_gray_code_strings l = 
  let gray1 = [ "0"; "1" ] in
  if l <= 0 then failwith "Gray code with less than 1 bit not possible"
  else
    let rec build_gray_code n codes = 
      if n=l then codes
      else
        build_gray_code (n+1) 
          ((List.map ((^) "0") codes) @ (List.map ((^) "1") (List.rev codes))) in
    build_gray_code 1 gray1

(** Constructs a statemachine, where each state has a string name.  The state signal, and a function to look up a state value given it's name is returned. *)
let make_states clock reset enable states = 
  let num_states = List.length states in
  let log_states = clog2 (num_states-1) in
  let states = List.mapi (fun i x -> (i,x)) states in
  let map = List.fold_left (fun map (idx,str) -> Map.add str (consti log_states idx) map) Map.empty states in
  b_reg clock reset Signal.empty enable log_states, (fun str -> Map.find str map)

(** Constructs a statemachine using a one-hot encoding *)
let make_states_onehot clock reset enable states = 
  let num_states = List.length states in
  let states = List.mapi (fun i x -> (i,x)) states in
  let map = List.fold_left (fun map (idx,str) -> Map.add str (const1h num_states idx) map) Map.empty states in
  b_reg clock reset (const1h num_states 0) enable num_states, (fun str -> Map.find str map)


(** Constructs a statemachine using a gray encoding *)
let make_states_gray clock reset enable states = 
  let num_states = List.length states in
  let log_states = clog2 (num_states-1) in
  let gray_strings = lselect (make_gray_code_strings log_states) 0 (num_states-1) in
  let states = List.map2 (fun x g -> (x,g)) states gray_strings in
  let map = List.fold_left (fun map (str,gray) -> Map.add str (constb gray) map) Map.empty states in
  b_reg clock reset Signal.empty enable log_states, (fun str -> Map.find str map)

(*****************************************************************************)
(*****************************************************************************)

type StateEncoding = SECount | SEOneHot | SEGray

(* Encoding of state machine enumerations, up to 32 bits, for use with behavioural blocks *)
(* Note: might be sensible to allow multiple state variables be generated - this makes it more like a type *)
type StateEnum = 
  {
    states : (string * Signal) list;
    state_var : BehaveAssignTarget;
  }
  with
  
    static member make clock reset enable encoding states = 
      let num_states = List.length states in
      let log_num_states = ubits (num_states-1) in
      let codes, state_width = 
        match encoding with
        | SECount -> [ for i in { 0 .. num_states - 1 } -> consti log_num_states i ], log_num_states
        | SEOneHot -> [ for i in { 0 .. num_states - 1 } -> const1h num_states i ], num_states
        | SEGray -> List.map constb (lselect (make_gray_code_strings log_num_states) 0 (num_states-1)), log_num_states
      in
      {
        states = List.zip states codes;
        state_var = b_reg clock reset (List.hd codes) enable state_width;
      }
    
    member x.signal_of_state str = 
      try List.assoc str x.states 
      with _ -> failwith ("Couldn't look up state " ^ str)

    member x.Item with get(state:string) = x.signal_of_state state
    
    (* given the state value encoded as a binary string, return the state name *)
    member x.state_of_binary_string str = 
      let rec find s = 
        match s with
        | [] -> failwith ("Couldnt look up state with value " ^ str)
        | (name,signal)::tl ->
          if str = string_of_const signal then name
          else find tl
      in
      find x.states
      
    member x.v = x.state_var
    member x.q = x.v.q
    
    static member (==~) ((state:StateEnum), (s:string)) = (==~) state.q (state.[s] : Signal)
    static member (/=~) ((state:StateEnum), (s:string)) = (/=~) state.q (state.[s] : Signal)
    static member ($==) ((state:StateEnum), (s:string)) = ($==) state.v (state.[s] : Signal)

  end

(*****************************************************************************)
(*****************************************************************************)

(* Allows simple behave code involving 1 reg or wire to be written as an expression *)
let ber clock reset reset_val enable bits fn = 
  let f = b_reg clock reset reset_val enable bits in
  behave (fn f);
  f.q

let berc = ber clock reset Signal.empty

let bew def fn = 
  let f = b_wire def in
  behave (fn f);
  f.q

let bew0 bits fn = bew (zero bits) fn

