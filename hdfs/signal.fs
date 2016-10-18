namespace DigitalLogic

#nowarn "62"    // Using ^ for string concatenation
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


open System
open System.Collections
open DigitalLogic.Numeric
open DigitalLogic.Numeric.Ops
open DigitalLogic.Numeric.Conversions

(** <P>Main circuit design type.</P> *)
(** <P>Signals represent generated hardware and various functions and *)
(** operators are designed to operate on signals to define hardware functionality</P> *)
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Signal = begin

  (* ************************************************************)
  (* ************************************************************)

  (** Type used for bit widths, bit positions etc. *)
  type NBits = int

  (** Each signal node requires a unique id which is encoded with this type.  It's not unfeasible that a very large design might need a 64 bit id. *)
  type Uid = int

  (** Binary operators *)
  type BinaryOperator = 
      B_add
    | B_sub
    | B_mulu
    | B_muls
    | B_and
    | B_or
    | B_xor
    | B_eq
    | B_lt
    | B_cat

  (** Unary operator (currently only logical not implemented) *)
  type UnaryOperator = U_not

  (* Logic signal used to build digital hardware circuits. *)
  [<System.Diagnostics.DebuggerDisplay("bits={width} name={name} type={string_of_node_type}")>]
  type Signal = { su : SignalUnion }
    with
      /// Get the signal union value
      member x.signal = x.su

      /// Returns the width of the signal.  This will be >= 1 unless the signal is empty (or an instatiation node) in which case it will be 0.
      member s.width = match s.signal with
        | Signal_empty    -> 0
        | Signal_const    (a,w,c) -> w
        | Signal_binop    (a,w,op,s0,s1) -> w
        | Signal_unop     (a,w,op,s) -> w
        | Signal_wire     (a,w,n,d) -> w
        | Signal_mux      (a,w,sel,d) -> w
        | Signal_select   (a,hi,lo,s) -> hi-lo+1
        | Signal_reg      (a,w,clk,rst,rstval,ena,d) -> w
        | Signal_mem      (a,dw,aw,size,clk,w,we,d,r) -> dw
        | Signal_behave   (a,w,b,d) -> w
        | Signal_inst     (a,n,m,g,io,i,o) -> 0 (* ??? *)
#if INST2
        | Signal_inst2    (a,s,g,io,i,o) -> 0 (* ??? *)
#endif
        | Signal_tri      (a,w,d) -> w

      /// Returns the unique identifier of the signal
      member x.uid = match x.signal with
        | Signal_empty    -> 0
        | Signal_const    (a,w,c) -> a
        | Signal_binop    (a,w,op,s0,s1) -> a
        | Signal_unop     (a,w,op,s) -> a
        | Signal_wire     (a,w,n,d) -> a
        | Signal_mux      (a,w,sel,d) -> a
        | Signal_select   (a,hi,lo,s) -> a
        | Signal_reg      (a,w,clk,rst,rstval,ena,d) -> a
        | Signal_mem      (a,dw,aw,size,clk,w,we,d,r) -> a
        | Signal_behave   (a,w,b,d) -> a
        | Signal_inst     (a,n,m,g,io,i,o) -> a
#if INST2
        | Signal_inst2    (a,s,g,io,i,o) -> a
#endif
        | Signal_tri      (a,w,d) -> a
        
      /// Returns the dependants of the signal
      member x.dependants = match x.signal with
        | Signal_empty    -> []
        | Signal_const    (a,w,c) -> []
        | Signal_binop    (a,w,op,s0,s1) -> [s0;s1]
        | Signal_unop     (a,w,op,s) -> [s]
        | Signal_wire     (a,w,n,d) -> if (!d).IsEmpty then [] else [!d]
        | Signal_mux      (a,w,sel,d) -> sel :: d
        | Signal_select   (a,hi,lo,s) -> [s]
        | Signal_reg      (a,w,clk,rst,rstval,ena,d) -> [clk;rst;rstval;ena;d]
        | Signal_mem      (a,dw,aw,size,clk,w,we,d,r) -> [clk;w;we;d;r]
        | Signal_behave   (a,w,b,d) -> d
        | Signal_inst     (a,n,m,g,io,i,o) -> List.map snd (io@i)
#if INST2
        | Signal_inst2    (a,n,g,io,i,o) -> List.map snd (io@i)
#endif
        | Signal_tri      (a,w,d) -> (List.map fst d) @ (List.map snd d)

      /// Returns true if the signal is the empty signal
      member x.IsEmpty = x.signal = Signal_empty
      /// Returns true if the signal is a wire
      member x.IsWire = match x.signal with Signal_wire _ -> true | _ -> false
      /// Returns true if the signal is a constant
      member x.IsConst = match x.signal with Signal_const _ -> true | _ -> false
      /// Returns true if the signal is behavioral code
      member x.IsBehave = match x.signal with Signal_behave _ -> true | _ -> false 
      /// Returns true if the signal is a memory
      member x.IsMem = match x.signal with Signal_mem _ -> true | _ -> false
      /// Returns true if the signal is a register
      member x.IsReg = match x.signal with Signal_reg _ -> true | _ -> false
      /// Returns true if the signal is a register or memory
      member x.IsSequential = match x.signal with Signal_reg _ | Signal_mem _ -> true | _ -> false
      /// Returns true if the signal is an instantiation
#if INST2
      member x.IsInst = match x.signal with Signal_inst _ | Signal_inst2 _ -> true | _ -> false
#else
      member x.IsInst = match x.signal with Signal_inst _ -> true | _ -> false
#endif
      /// Returns true if the signal is a tristate
      member x.IsTristate = match x.signal with Signal_tri _ -> true | _ -> false
      
      /// Optimisation utility function:  if the signal is a constant convert it to a StringBits type
      member x.to_stringbits = match x.signal with Signal_const(_,_,s) -> StringBits.of_string s | _ -> failwith "Cannot convert non-constant value to StringBits"
      
    end
  (* Internal discrimiated union representation the hardware AST. *)
  and SignalUnion = 
    | Signal_empty    
    | Signal_const    of Uid * NBits * string
    | Signal_binop    of Uid * NBits * BinaryOperator * Signal * Signal
    | Signal_unop     of Uid * NBits * UnaryOperator * Signal
    | Signal_wire     of Uid * NBits * string ref * Signal ref
    | Signal_mux      of Uid * NBits * Signal * Signal list
    | Signal_select   of Uid * NBits * NBits * Signal
    | Signal_reg      of Uid * NBits * Signal * Signal * Signal * Signal * Signal         (* clk, rst, rst_value, ena, d *)
    | Signal_mem      of Uid * NBits * NBits * NBits *                                    (* data width, addr width, size *)
                         Signal *                                                         (* clk *)
                         Signal * Signal * Signal *                                       (* w, we, d *)
                         Signal                                                           (* r *)
    | Signal_behave   of Uid * NBits * Behave list * Signal list                          (* behavioral code *)
    | Signal_inst     of Uid * string * string ref * Generic list *
                         InstConnect list * InstConnect list * InstConnect list           (* instantiation - id, comp name, inst name, generics, inouts, inputs, outputs *)
    | Signal_tri      of Uid * NBits * (Signal * Signal) list
    
#if INST2
    (* ... in testing ... *)
    | Signal_inst2    of Uid * InstanceSpec * (string * InstGeneric) list * (string * Signal) list * (string * Signal) list * (string * Signal) list 
#endif

  and Behave =
    | B_assign of BehaveAssignTarget * Signal
    | B_if of Signal * Behave list * Behave list
    | B_switch of Signal * (Signal * Behave list) list

  ///<p>Datatype representing wires and registers assigned to within behavioral code.  
  ///   It should be constructed using the Signal.b_reg and Signal.b_wire methods.</p>
  and BehaveAssignTarget = B_assign_tgt of 
      Signal *    (* lhs of assignment *)
      Signal *    (* rhs of assignment *)
      Signal *    (* default value if unassigned *)
      int *       (* left index *)
      int         (* right index *)

  (** Connections used for instantiating hardware blocks (will probably be removed) *)
  and InstConnect = string * Signal

  (* Generics.  These are used only with external instantiated components. *)

  (** Description of a generic with an optional default value *)
  and GenericSpec = 
    | Gd_Bit    of int option
    | Gd_Bv     of int * int * string option
    | Gd_Sl     of int option
    | Gd_Slv    of int * int * string option
    | Gd_Int    of int option
    | Gd_Nat    of int option
    | Gd_Pos    of int option
    | Gd_Float  of float option
    | Gd_Time   of string option
    | Gd_String of string option
    | Gd_Bool   of bool option

  (** for setting a generic parameter in an instantiation *)
  and GenericData = 
    | G_Bit of int 
    | G_Vec of string
    | G_Int of int 
    | G_String of string
    | G_Float of float
    | G_Bool of bool

  (** generic name, type, (optional) default value, and (optional) supplied value *)
  and Generic = string * GenericSpec * (GenericData option) 
  
  (* note - how about std_ulogic ??? *)

  (* A new specification data type for instantiation *)
  and InstanceSpec = 
    {
      name : string;
      generics : (string * InstGeneric) list;
      inouts : (string * InstPort) list;
      inputs : (string * InstPort) list;
      outputs : (string * InstPort) list;
    }
  (* Ways in which vector defaults can be specified *)
  and InstVectorInitData = 
    | BitString         of string
    | HexString         of string
  (* Supported generic types *)
  and InstGeneric = 
    | GenericBit    of int option
    | GenericSl     of int option
    | GenericInt    of int option
    | GenericNat    of int option
    | GenericPos    of int option
    | GenericFloat  of float option
    | GenericTime   of string option
    | GenericString of string option
    | GenericBool   of bool option
    | GenericBv     of (int * int) option * InstVectorInitData option
    | GenericSlv    of (int * int) option * InstVectorInitData option
    | GenericUns    of (int * int) option * InstVectorInitData option
    | GenericSgn    of (int * int) option * InstVectorInitData option
  (* for ports we support bit, bit_vector, std_logic, std_logic_vector, signed and unsigned (numeric_std only).
     Vector ranges are optional.  If enabled then HDFS will check the vector connections.  Otherwise it's up to
     the programmer to ensure they are correct to avoid potential simulation/synthesis issues. *)
  and InstPort = 
    | PortBit    of int option
    | PortSl     of int option
    | PortBv     of (int * int) option * InstVectorInitData option
    | PortSlv    of (int * int) option * InstVectorInitData option
    | PortUns    of (int * int) option * InstVectorInitData option
    | PortSgn    of (int * int) option * InstVectorInitData option
   
  (** Library version number *)
  let hdfs_version = "0.3"

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** The empty (null) signal. *)
  let empty = { su = Signal_empty }
  (** Logic '0' *)
  let gnd = { su = Signal_const(1, 1, "0") }
  (** Logic '1' *)
  let vdd = { su = Signal_const(2, 1, "1") }
  (** Default clock signal (may be removed) *)
  let clock = { su = Signal_wire(3, 1, ref "clock", ref empty) }
  (** Default asynchronous reset signal (may be removed) *)
  let reset = { su = Signal_wire(4, 1, ref "reset", ref empty) }
  (** Default register enable signal *)
  let enable = { su = Signal_wire(5, 1, ref "enable", ref empty) }

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Internal.  Applys the given function to the width of each signal in the given list *)
  let check_width(s, fn, str) = List.iter (fun (s : Signal) -> if not (fn (s.width)) then (failwith str)) s
  (** Internal.  Checks that the given list of signals all have the same width *)
  let check_same (s : Signal list) = 
    let tw = (List.head s).width in
    List.iter (fun (s : Signal) -> 
      if (tw <> (s.width)) then (failwith ("Expecting width " ^ string tw ^ " but got width " ^ string (s.width)))
    ) s
  (** Internal.  Checks that the given list of signals are all 1 bit wide *)
  let check_one s = check_width(s, (fun w -> w=1), "Expecting width to be 1")
  (** Internal.  Checks that the given list of signals are all at least 1 bit wide *)
  let check_ok s = check_width(s, (fun w -> w>0), "Expecting width of at least 1")
  (** Internal.  Ensures the given signal is suitable or a clock *)
  let check_clock clk = check_width([clk], (fun x -> x=0 || x=1), "clock should be a 1 bit")
  (** Internal.  Ensures the given signal is suitable or a reset *)
  let check_reset rst = check_width([rst], (fun x -> x=0 || x=1), "reset should be empty or 1 bit")
  (** Internal.  Ensures the given signal is suitable or an enable *)
  let check_enable ena = check_width([ena], (fun x -> x=0 || x=1), "enable should be emtpy or 1 bit")

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Functions to increment the circuit id, reset the id's, apply signal name prefix, and do name mangling *)
  let signal_init, signal_incr_uid, apply_prefix, signal_mangle_name = 
    let cnt = ref 100 in (* "global" signals use ids < 100.  Avoids potential clashes if reset *)
    let prefix = "hdfs_" in
    let prefix_len = prefix.Length in
    let h = Hashtable(128) in
    (fun() -> cnt := 100; h.Clear()), 
    (fun() -> cnt := !cnt+1; !cnt),
    (fun n -> prefix ^ n),
    (fun (n:string) ->
      (* ... string should be made uppercase first, as VHDL is not case sensitive *)
      if n.Length >= prefix_len then (
        if (n.Substring(0, prefix_len)) = prefix then failwith ("The prefix " ^ prefix ^ " is reserved");        
      );
      let rec mangle n = 
        let cnt = 
          try h.[n] :?> int
          with _ -> 0 in
        match cnt with
        | 0 -> h.Add(n, 1); n
        | x -> 
          let nn = n ^ "_" ^ (string x) in
          h.Remove(n);
          h.Add(n, (x + 1));
          mangle nn in  (* it could be that this name also needs mangling, so try again.  This'll produce some funny names, but it's relatively simple *)
      mangle n
    )

  (** Gets the name of a signal.  If it's a named wire then returns the name, otherwise the standard prefix is applied to the uid and returned. *)
  let name (s:Signal) = 
    match s.signal with
    | Signal_wire(a,w,n,d) when !n <> "" -> !n (* should be mangled *)
    | _ -> apply_prefix (string (s.uid))

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)
    
  (** Enable or disable constant propogation optimisations while building the signal graph *)
  (* !!! This is new, turn it off if there are problems *)
  let constant_propogation = ref true

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Number of bits required to represent the value as an unsigned number *)
  let rec ubits x = 
    assert (x >= 0);
    match x with 0 | 1 -> 1 | x -> 1 + (ubits (x/2))
  
  (** Number of bits required to represent the value as an signed number *)
  let sbits x = 
    if x >= 0 then ubits x + 1
    else ubits (-x-1) + 1

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Width of a signal *)
  let width (x:Signal) = x.width
  (** dependants of a signal *)
  let dependants (x:Signal) = x.dependants

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Removes duplicate values from the list (should be done much more efficiently) *)
  let rec unique_list l = 
    match l with
    | [] -> []
    | hd :: tl ->
      hd :: (unique_list (List.filter ((<>) hd) tl)) 
      
  (** Takes a list of signals and returns a list of where each signal is unique based on it's uid *)
  let unique_signal_list signals = 
    let map = (List.fold (fun set (signal : Signal) -> Map.add (signal.uid) signal set) Map.empty signals) in
    let l = Map.foldBack (fun k d l -> d :: l) map [] in
    l

  (** Get the connection for this wire, or empty *)
  let wire_connection (x : Signal) = match x.signal with Signal_wire(_,_,_,d) -> !d | _ -> empty
  (** Get the name of this wire, or "" *)
  let wire_name (x : Signal) = match x.signal with Signal_wire(_,_,n,_) -> !n | _ -> ""

  (** Places the given string between each string in the given list *)
  let fold_strings inner = List.fold (fun x a -> if x = "" then a else x ^ inner ^ a) "" 

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Constant from a string in binary format *)
  let constb s = 
    String.iter (fun x -> if x <> '0' && x <> '1' then failwith "Constants must only contain 0's or 1's") s;
    if s = "" then (failwith "Binary constant has zero width")
    else if s = "0" then gnd
    else if s = "1" then vdd
    else { su = Signal_const(signal_incr_uid(), String.length s, s) }

  (** Constant from a width and integer value *)
  let consti l v = constb (bin_str_of_int l v)

  (** Constant from a string in hex format *)
  let consthu l v = constb (bin_str_of_hex_str Unsigned l v)

  (** Constant from a string in hex format *)
  let consths l v = constb (bin_str_of_hex_str Signed l v)

  (** Constant from a string in decimal format *)
  let constd l v = constb (bin_str_of_dec_str l v)

  (** Constant from a Big_int *)
  let constbi l v = constb (bin_str_of_big_int l v)

  (** Onehot constant *)
  let const1h l v = 
    if v >= l || v < 0 then failwith "Invalid one hot bit";
    let rec build_onehot n =
      if n = l then ""
      else if n = v then ((build_onehot (n+1)) ^ "1")
      else ((build_onehot (n+1)) ^ "0")
    in
    constb (build_onehot 0)

  (** Constant from a string using verilog style constant formatting ie 32'd12 *)
  let constv (s:string) : Signal = 
    let slen, sval = match s.Split('\'') with [|s0;s1|] -> s0,s1 | _ -> failwith ("Invalid verilog style constant: " ^ s) in
    let len = int slen in
    let ctrl = sval.[0] in
    let sval = sval.Substring(1, (String.length sval - 1)) in
    match ctrl with
    | 'd' -> constd len sval
    | 'x' | 'h' -> consthu len sval
    | 'X' | 'H' -> consths len sval
    | 'b' -> 
      let slen = String.length sval in
      if slen < len then constb ((String('0', (len-slen))) ^ sval)
      else if slen > len then constb (sval.Substring((slen-len), len))
      else constb sval
    | 'B' ->
      let slen = String.length sval in
      if slen < len then constb ((String(sval.[0], (len-slen))) ^ sval)
      else if slen > len then constb (sval.Substring((slen-len), len))
      else constb sval
    | _ -> failwith ("Invalid verilog style constant" ^ s ^ " - bad control character")

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Return binary string value of signal, assuming it is a constant. Otherwise return an empty string. *)
  let string_of_const (x:Signal) = match x.signal with Signal_const(_,_,c) -> c | _ -> ""

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Zero with given width *)
  let zero n = 
    let rec zeros n = 
      if n=0 then ""
      else "0" ^ zeros (n-1) in
    constb (zeros n)
    
  (** One with given width *)
  let one n = 
    let rec zeros n = 
      if n=0 then ""
      else "0" ^ zeros (n-1) in
    constb ((zeros (n-1)) ^ "1")

  (** Ones with given width (ie -1) *)
  let ones n = 
    let rec ones n = 
      if n=0 then ""
      else "1" ^ ones (n-1) in
    constb (ones n)

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Select range of bits from signal *)
  let select (s:Signal) hi lo = 
    let is_select  = function Signal_select(_) -> true | _ -> false in
    if lo > hi then failwith ("Invalid select range: " ^ (string hi) ^ " " ^ (string lo));
    if lo < 0 then failwith ("Select lo = " ^ string lo ^ " is negative");
    if hi >= s.width then failwith ("Select hi = " ^ string hi ^ " is >= width of target = " ^ string s.width);
    if hi = s.width-1 && lo = 0 then s
    (* If the select is into a constant, turn it into a new constant *)
    else if !constant_propogation && s.IsConst then constb (StringBits.select s.to_stringbits hi lo).to_string
    (* If the argument to the select is another select, re-index the argument *)
    else if !constant_propogation && is_select s.su then
      match s.su with
      | Signal_select(_,_,lo',s') -> { su = Signal_select(signal_incr_uid(),hi+lo',lo+lo',s') }
      | _ -> failwith "Expecting selection"
    else { su = Signal_select(signal_incr_uid(),hi,lo,s) }
  
  (** Select a single bit from signal *)
  let bit s n = select s n n 

  (** Concatentate two signals *)
  let cat (a:Signal) (b : Signal) =
    if a.IsEmpty then b
    else if b.IsEmpty then a
    else if !constant_propogation && a.IsConst && b.IsConst then constb (StringBits.cat a.to_stringbits b.to_stringbits).to_string
    else { su = Signal_binop(signal_incr_uid(), a.width + b.width, B_cat, a, b) }

  (** Concatenate a list of signals.  Head of list aligns with MSB of result *)
  let rec concat s = 
    match s with
    | [] -> empty
    | hd :: tl -> cat hd (concat tl) 

  (** Concatenate a list of signals.  Head of list aligns with MSB of result *)
  let concat_msb s = concat s
  (** Concatenate a list of signals.  Head of list aligns with LSB of result *)
  let concat_lsb s = concat (List.rev s)

  (** Convert signal to a list of bits.  MSB at head of resulting list *)
  let bits (s:Signal) = 
    let rec bits' n s = 
      if n < 0 then []
      else (bit s n) :: bits' (n-1) s in
    bits' (s.width-1) s  

  (** Convert signal to a list of bits.  MSB at head of resulting list *)
  let bits_msb = bits
  (** Convert signal to a list of bits.  LSB at head of resulting list *)
  let bits_lsb s = List.rev (bits s)

  (** Split a signal into two as evenly as possible.  If the signals width is odd the left *)
  (** hand returned signal is 1 bit larger than the right hand one *)
  let split s = 
    let w = width s in
    if w = 1 then s, empty
    else select s (w-1) (w/2), select s ((w/2)-1) 0

  (** Repeat a signal "n" times *)
  let rec repeat s n = 
    if n = 0 then empty 
    else if n = 1 then s
    else if (n%2) = 1 then cat s (repeat (cat s s) (n/2))
    else repeat (cat s s) (n/2)

  (** Select the most significant (left most) bit. *)
  let msb s = bit s (s.width-1)
  
  (** Select the most significant (left most) bits. *)
  let msbs s = select s (s.width-1) 1
  
  (** Select the least significant (right most) bit. *)
  let lsb s = bit s 0
  
  (** Select the least significant (right most) bits. *)
  let lsbs s = select s (s.width-2) 0
  
  (** Sign extend by one bit *)
  let se s = concat [ msb s; s ]
  
  (** Unsigned extend by one bit *)
  let ue s = concat [ gnd; s ]

  (** resize vec depending on sign *)
  let resize sign vec newwid = 
    let oldwid = width vec in
    let extend vec = cat (repeat (if sign = Signed then msb vec else gnd) (newwid-oldwid)) vec in
    let shrink vec = select vec (newwid-1) 0 in
    if oldwid = newwid then vec
    else if oldwid < newwid then extend vec
    else shrink vec

  (** resize vec adding 0's at the msb as required *)
  let uresize = resize Unsigned

  (** resize vec repeating the msb to preserve sign *)
  let sresize = resize Signed

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Apply the given operator between each element of the given list. Requires at least two signals. *)
  let reduce op s = 
    match List.length s with
    | 0 | 1 -> failwith ("Cant reduce 0 or 1 bits")
    | _ -> List.fold (fun acc x -> op acc x) (List.head s) (List.tail s)

  (** Lets the single reduce case pass by just returning the data. *)
  let reduce_1 op s = 
    match List.length s with
    | 1 -> List.head s 
    | _ -> reduce op s

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** N-input mux *)
  let mux (s:Signal) (d : Signal list) = 
    let len = List.length d in
    if (1 <<< (s.width)) < len then failwith ("Mux select not big enough for data: " ^ string (s.width) ^ " bits " ^ string len ^ " for elements");
    check_ok d;
    check_same d;
    match len with
    | 0 -> (failwith "Mux must have at least one input")
    | 1 -> List.head d (* with one data item it is always selected (needed for the generators) *)
    | _ -> 
      if !constant_propogation && s.IsConst && len < (1 <<< 30) && s.width < 32 then
        (* if the select signal is a constant, and also within the range of an integer, replace the mux with the selected value from the mux data list *)
        let idx = int_of_bin_str (string_of_const s) in
        if idx < len then List.item idx d
        else List.item (len-1) d
      else { su = Signal_mux (signal_incr_uid(), (List.head d).width, s, d) }

  (** 2-input mux *)
  let mux2 s hi lo = 
    check_one [s];
    mux s [lo;hi] 

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Logical shift left by a constant *)
  let sll (a:Signal) shift = 
    if shift < 0 then failwith ("Expecting positive shift amount, got " ^ string shift ^ ".");
    if shift = 0 then a
    else if shift >= a.width then zero a.width
    else cat (select a (a.width - 1 - shift) 0) (zero shift)
      
  (** Logical shift right by a constant *)
  let srl (a:Signal) shift = 
    if shift < 0 then failwith ("Expecting positive shift amount, got " ^ string shift ^ ".");
    if shift = 0 then a
    else if shift >= a.width then zero a.width
    else cat (zero shift) (select a (a.width - 1) shift)

  (** Arithemtic shift right by a constant *)
  let sra (a:Signal) shift = 
      if shift < 0 then failwith ("Expecting positive shift amount, got " ^ string shift ^ ".");
      if shift = 0 then a
      else if shift >= a.width then repeat (msb a) a.width
      else cat (repeat (msb a) shift) (select a (a.width - 1) shift)
      
  (** Log shifter *)
  let log_shift (op : Signal->int->Signal) (a : Signal) (b : Signal) = 
    let rec sft (a : Signal) (n : int) = 
      if n = b.width then a
      else 
        let s = mux2 (bit b n) (op a (1 <<< n)) a in
        sft s (n+1)
    in
    sft a 0
      
  (** Arithmetic variable right shift. *)
  let shift_right_arithmetic data shift = log_shift sra data shift

  (** Logical variable right shift. *)
  let shift_right_logical data shift = log_shift srl data shift
      
  (** Variable left shift. *)
  let shift_left data shift = log_shift sll data shift

  (** right barrel shift. *)
  let barrel_shift_right (data : Signal) (shift : Signal) =
    let w = data.width in
    let bsr a n = 
      match n with
      | 0 -> a
      | x -> cat (select a (x-1) 0) (select a (w-1) x) in
    log_shift bsr data shift

  (** left barrel shift. *)
  let barrel_shift_left (data : Signal) (shift : Signal) =
    let w = data.width in
    let bsl a n = 
      match n with
      | 0 -> a
      | x -> cat (select a (w-1-x) 0) (select a (w-1) (w-x)) in
    log_shift bsl data shift
      
  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Addition.  Both arguments and the result are the same width *)
  let add (a : Signal) (b : Signal) = 
    check_ok [a; b];
    check_same [a; b];
    if !constant_propogation && a.IsConst && b.IsConst then constb (a.to_stringbits + b.to_stringbits).to_string
    else { su = Signal_binop(signal_incr_uid(), a.width, B_add, a, b) }

  (** Subtraction.  Both arguments and the result are the same width *)
  let sub (a : Signal) (b : Signal) = 
    check_ok [a; b];
    check_same [a; b];
    if !constant_propogation && a.IsConst && b.IsConst then constb (a.to_stringbits - b.to_stringbits).to_string
    else { su = Signal_binop(signal_incr_uid(), a.width, B_sub, a, b) }

  (** Negation.  Argument and the result are the same width *)
  let negate (a : Signal) = sub (zero (width a)) a

  (** Unsigned multiplication.  Results width is the sum of the widths of the arguments. *)
  let mulu (a : Signal) (b : Signal) =
    check_ok [a; b];
    if !constant_propogation && a.IsConst && b.IsConst then constb (a.to_stringbits * b.to_stringbits).to_string
    else { su = Signal_binop(signal_incr_uid(), (a.width) + (b.width), B_mulu, a, b) }

  (** Signed multiplication.  Results width is the sum of the widths of the arguments. *)
  let muls (a : Signal) (b : Signal) = 
    check_ok [a; b];
    if !constant_propogation && a.IsConst && b.IsConst then constb (a.to_stringbits *+ b.to_stringbits).to_string
    else { su = Signal_binop(signal_incr_uid(), (a.width) + (b.width), B_muls, a, b) }

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Logical AND.  Both arguments and the result are the same width *)
  let band a b =
    check_ok [a; b];
    check_same [a; b];
    if !constant_propogation && a.IsConst && b.IsConst then constb (a.to_stringbits &&& b.to_stringbits).to_string
    else { su = Signal_binop(signal_incr_uid(), a.width, B_and, a, b) }

  (** Logical OR.  Both arguments and the result are the same width *)
  let bor (a : Signal) (b : Signal) =
    check_ok [a; b];
    check_same [a; b];
    if !constant_propogation && a.IsConst && b.IsConst then constb (a.to_stringbits ||| b.to_stringbits).to_string
    else { su = Signal_binop(signal_incr_uid(), a.width, B_or, a, b) }

  (** Logical XOR.  Both arguments and the result are the same width *)
  let bxor (a : Signal) (b : Signal) =
    check_ok [a; b];
    check_same [a; b];
    if !constant_propogation && a.IsConst && b.IsConst then constb (a.to_stringbits ^^^ b.to_stringbits).to_string
    else { su = Signal_binop(signal_incr_uid(), a.width, B_xor, a, b) }

  (** Logical NOT.  Argument and the result are the same width *)
  let bnot a =
    check_ok [a];
    if !constant_propogation && a.IsConst then constb (~~~ a.to_stringbits).to_string
    else { su = Signal_unop(signal_incr_uid(), a.width, U_not, a) }

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Equality.  Both arguments are the same width.  Result is 1 bit. *)
  let eq (a : Signal) (b : Signal) =
    check_ok [a; b];
    check_same [a; b];
    if !constant_propogation && a.IsConst && b.IsConst then bnot (reduce_1 bor (bits (bxor a b)))
    else { su = Signal_binop(signal_incr_uid(), 1, B_eq, a, b) }

  (** Inequality.  Both arguments are the same width.  Result is 1 bit. *)
  let neq (a : Signal) (b : Signal) = bnot (eq a b)

  (** Unsigned less than.  Both arguments are the same width.  Result is 1 bit. *)
  let lsu (a : Signal) (b : Signal) =
    check_ok [a; b];
    check_same [a; b];
    if !constant_propogation && a.IsConst && b.IsConst then constb (a.to_stringbits <~ b.to_stringbits).to_string
    else { su = Signal_binop(signal_incr_uid(), 1, B_lt, a, b) }

  (** Unsigned greater than.  Both arguments are the same width.  Result is 1 bit. *)
  let gtu (a : Signal) (b : Signal) = lsu b a
  
  (** Unsigned less than or equal to.  Both arguments are the same width.  Result is 1 bit. *)
  let lsequ (a : Signal) (b : Signal) = bnot (gtu a b)
  
  (** Unsigned greater than or equal to.  Both arguments are the same width.  Result is 1 bit. *)
  let gtequ (a : Signal) (b : Signal) = bnot (lsu a b)

  (** Signed less than.  Both arguments are the same width.  Result is 1 bit. *)
  let lss (a : Signal) (b : Signal) =
    if a.width = 1 && b.width = 1 
    then (band a (bnot b)) (* 00=0, 01=0, 10=1, 11=0 *) 
    else lsu (cat (bnot (msb a)) (lsbs a)) (cat (bnot (msb b)) (lsbs b))
    
  (** Signed greater than.  Both arguments are the same width.  Result is 1 bit. *)
  let gts (a : Signal) (b : Signal) = 
    if a.width = 1 && b.width = 1 
    then (band b (bnot a)) (* 00=0, 01=1, 10=0, 11=0 *) 
    else gtu (cat (bnot (msb a)) (lsbs a)) (cat (bnot (msb b)) (lsbs b))      
    
  (** Signed less than or equal to.  Both arguments are the same width.  Result is 1 bit. *)
  let lseqs (a : Signal) (b : Signal) = 
    if a.width = 1 && b.width = 1 
    then (bnot (gts a b)) (* 00=1, 01=0, 10=1, 11=1 *) 
    else lsequ (cat (bnot (msb a)) (lsbs a)) (cat (bnot (msb b)) (lsbs b))      

  (** Signed greater than or equal to.  Both arguments are the same width.  Result is 1 bit. *)
  let gteqs (a : Signal) (b : Signal) = 
    if a.width = 1 && b.width = 1 
    then (bnot (lss a b)) (* 00=1, 01=1, 10=0, 11=1 *) 
    else gtequ (cat (bnot (msb a)) (lsbs a)) (cat (bnot (msb b)) (lsbs b))

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Enables combinatorial loop checks when wires are assigned to with <==.  Can be very slow *)
  let comb_check_on = ref false   (* This can have a dramatic and very bad effect on performance, 
                                     so I suggest it's only turned on once in a while to check 
                                     for combinatorial loops *)

  (** Check if there is a combinatorial path from the first argument to the second argument (ie without passing through a register) *)
  let rec check_comb (a : Signal) (b : Signal) = 
    if !comb_check_on then (
      if a = b then failwith ("Combinatorial loop found when assigning to wire " ^ name a) 
      else 
        if not (b.IsSequential) then
          List.iter (fun b -> (check_comb a b)) b.dependants
    )

  (** Dangling wire assignment *)
  let assign (a : Signal) (b : Signal) =
    if not (b.IsInst) then check_same [a;b]; (* instantiations are different because they have widths depending on which output they are connected to *)
    match a.signal with
    | Signal_wire (_,_,_,d) -> 
      check_comb a b;
      if not (!d).IsEmpty then failwith "Wire has already been assigned to";
      d := b
    | _ -> (failwith "Must only assign to wires")

  (** Creates a wire of the given width which may be assigned to with <==. *)
  let wire w = 
    if w < 1 then failwith ("Cannot create a wire of width " ^ string w)
    else { su = Signal_wire(signal_incr_uid(), w, ref "", ref empty) }

  (** Set the name of a signal *)
  let rec set_name (s:Signal) name =
    match s.signal with
    | Signal_inst(a,n,m,g,io,i,o) ->
      if !m <> "" then failwith "Instances must only be named once"
      else
        m := signal_mangle_name name;
        s
    | Signal_wire (a,w,n,d) 
        when !n = ""  (* allows a wire which already has a name to get another one *)
        -> 
      if !n <> "" then failwith ("Wire name has already been set.  Old name = " ^ !n ^ ", New name = " ^ name);
      n := signal_mangle_name name; s
    | _ ->
      let w = wire s.width in
      assign w s;
      set_name w name

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Create a register *)
  let reg clk rst (rst_val:Signal) ena d = 
    check_clock clk;
    check_reset rst;
    check_enable ena;
    check_ok [d];
    if not rst.IsEmpty then (
      if not rst_val.IsEmpty then (
        check_same [rst_val; d];
        if not (rst_val.IsConst) then failwith "Register reset values must be a constant or empty";
      )
    );
    { su = Signal_reg(signal_incr_uid(), d.width, clk, rst, rst_val, ena, d) }
    
  (** Create a register whose value is driven according to the supplied callback function *)
  let reg_fb clk rst rst_val ena fn w =
    let w = wire w in
    let r = reg clk rst rst_val ena w in
    assign w (fn r);
    r
    
  (** Create a register with default clock and reset *)
  let regc ena d = reg clock reset (zero (width d)) ena d
  (** Create a register, with default clock and reset, whose value is driven according to the supplied callback function *)
  let regc_fb ena fn w = reg_fb clock reset (zero w) ena fn w

  (** Create an asynchronous read, synchronous write memory *)
  let memory size clk we w d r = 
    check_clock clk;
    check_one [we];
    check_ok [d];
    check_same [w;r];
    let q = { su = Signal_mem(signal_incr_uid(), d.width, r.width, size, clk, w, we, d, r) } in 
    ignore (signal_incr_uid()); (* reserve an id for the memory array *)
    q

  (** Create a tristate *)
  let tristate d = 
    let len = List.length d in
    if len <= 0 then failwith "Tristate must have at least one driver";
    List.iter (fun ((oe : Signal),_) -> if (oe.width) <> 1 then failwith "Tristate enable must be one bit") d;
    check_same (List.map snd d);
    { su = Signal_tri(signal_incr_uid(), (snd (List.head d)).width, d) }
      
  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Input signal.  A named wire which is not connected to anything else *)
  let input n w = set_name (wire w) n

  (** Output signal.  A wire is created and given the supplied name. *)
  let output n s = 
    try
      let w = set_name (wire (width s)) n in
      assign w s;
      w
    with 
    | Failure x -> failwith ("\n  Could not create output " ^ n ^ ":\n  " ^ x)
    | _ -> failwith ("\n  Could not create output " ^ n)

  (** Inout signal. *)
  let inout n w = set_name (wire w) n

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** Behavioral if/else. *)
  let b_if (cond:Signal) on_true on_false = 
    let w = cond.width in (* control condition must be 1 bit (a vhdl thing )*)
    let cond = if w <> 1 then neq (zero w) cond else cond in
    B_if(cond, on_true, on_false) 
    
  (** b_if (cond) [ code ] [] *)
  let b_when cond on_true = b_if cond on_true []
  (** b_if (cond) [ ] [ code ] *)
  let b_unless cond on_false = b_if cond [] on_false

  (** Behavioral switch *)
  let b_switch (cond:Signal) cases = 
    // Only the C generator (current not fully working anyway) should be affected by > 32
    //if cond.width > 32 then failwith "switch conditions must currently be 32 bits or less";
    B_switch(cond, cases)

  (** Behavioral case *)
  let b_case (cond:Signal) case = 
    if not cond.IsConst then failwith "Case conditions must be constants";
    (cond,case)

  (** For loop (experimental) *)
  let b_for range fn arg = 
    let code, arg = Seq.fold (fun (codes, arg) i -> let code, arg = fn i arg in codes @ code, arg) ([],arg) range in
    b_when (vdd) code
    
  (** Wire to which combinatorial behavioral assignment can be made *)
  let b_wire (defval:Signal) = 
    assert (defval.width > 0);
    let width = defval.width in
    let w = wire width in
    check_same [w; defval];
    B_assign_tgt(w, w, defval, width-1, 0)
    
  (** Behavioral wire with default value of zero *)
  let b_wire0 w = b_wire (zero w)

  (** Register to which sequential behavioral assignment can be made.  Holds its previous value if no assignment is made. *)
  let b_reg clock reset (rstval:Signal) enable width = 
    let d = wire width in
    let q = reg clock reset rstval enable d in
    B_assign_tgt(q, d, q, width-1, 0)
    
  (** Register to which sequential behavioral assignment can be made.  Holds its previous value if no assignment is made.  Uses default clock and reset. *)
  let b_regc enable width = b_reg clock reset (zero width) enable width

  (** Processes behavioral assignments into a format that can be put in the signal graph. *)
  let behave code = 

    (* XXX BUG: if you put mutliple cases with the same constant value the simulation get's all confused.  Goodness knows what happens with the netlists XXX *)

    (* not used...
    (* return true if complete, false if incomplete. Fail if multiple cases found *)      
    let check_cases cond cases = 
      let idx_set cases = fold_left (fun set (idx, cond) -> 
          let idx_str = string_of_const idx in
          if Set.contains idx_str set then failwith "Duplicate case index found";
          Set.add (bin_str_of_const idx) set) 
        Set.empty cases in
      let num = length cases in
      let max_num = 1 <<< (width cond) in
      if max_num < num 
      then failwith ("There are more cases (" ^ string num ^ ") than are representable with " ^ string (width cond) ^ " bits")
      else 
        ignore (idx_set cases)
    in *)

    (* find all assigned wires *)
    let rec behave_targets targets nodes = List.fold behave_target targets nodes
    and behave_target targets node = 
      match node with
      | B_if(cond, on_true, on_false) -> behave_targets targets (on_true @ on_false)
      | B_switch(cond, cases) -> 
        List.iter (fun (x : Signal) -> 
          if not x.IsConst then 
            failwith "Case indexes must be constants (no expressions - even if logically constant - allowed)\n";
          if width x <> width cond then
            failwith "Width of case index must equal width of select condition\n") (List.map fst cases);
        behave_targets targets (List.concat (List.map snd cases))
      | B_assign(B_assign_tgt(qtarget,target,def,hi,lo), expr) ->
        match target.signal with
        | Signal_wire(_,_,_,d) when !d = empty -> B_assign_tgt(qtarget,target,def,hi,lo) :: targets
        | _ -> failwith "Targets in behavioral expressions must be unassigned wires"
    in
    let targets = unique_list (behave_targets [] code) in     // .... unique, but doesnt account for the index range properly
    
    (* Apply the default assignments *)
    let default_code = List.map (fun t -> match t with B_assign_tgt(qtarget,target,def,hi,lo) -> B_assign(B_assign_tgt(qtarget,target,def,hi,lo), def)) targets in
    let targets = List.map (fun t -> match t with B_assign_tgt(qtarget,target,def,hi,lo) -> target) targets in
    let code = default_code @ code in  
    
    (* split by targets *)
    let rec behave_split_list target nodes = 
      let match_assign_to_target target = function
      | B_assign(B_assign_tgt(_,x,_,_,_),_) when x = target -> true
      | B_assign(_) -> false
      | _ -> true in
      List.map (behave_split target) (List.filter (match_assign_to_target target) nodes)
    and behave_split target node = 
      match node with
      | B_if(cond, on_true, on_false) -> B_if(cond, behave_split_list target on_true, behave_split_list target on_false)
      | B_switch(cond, cases) -> B_switch(cond, List.map (fun (i, cases) -> (i, (behave_split_list target cases))) cases)
      | B_assign(target, expr) -> node
    in
    let codes = List.map (fun target -> behave_split_list target code) targets in

    (* return true if cases are complete, false if incomplete. *)      
    let case_complete cond cases = (1 <<< (width cond)) >= (List.length cases) in
    
    (* remove empty case and if statements, identify assignments which cannot occur.
       return optimised tree and boolean indication of whether a transformation was performed.
       For more advanced optimisations you would need to analyse the expressions themselves *)
    let rec optimise_list opt nodes = 
      (* do the optimisations *)
      let is_assign = function B_assign(_) -> true | _ -> false in
      let is_empty = function
      | B_if(_,a,b) when a = [] && b = [] -> true
      | B_switch(_,c) when c = [] -> true
      | _ -> false in
      (* filter empty statements *)
      let nodes, opt = List.fold (fun (l,b) node -> if is_empty node then (l,true) else (node::l,b)) ([],opt) nodes in
      let nodes = List.rev nodes in
      (* When "something" is followed by an assignment the "something" can never contribute to the final value, so it can be removed *)
      let rec assigns (nl,opt) l = 
        match l with
        | [] -> nl,opt
        | [a] -> a::nl,opt
        | a :: tl -> 
          (* is_assign should really mean is complete.
             a "complete" term is one which all execution branches assign to the target variable.
             Ananysis for if's is easy.  Analysis for switches is harder as we first need to 
             error check the supplied case indexes against the condition. *)
          (*let rec is_complete node = 
            let fold_and = fold_left (fun b n -> b && (is_complete n)) true in
            match node with 
            | B_if(cond, on_true, on_false) when on_true <> [] && on_false <> [] -> (fold_and on_true) && (fold_and on_false)
            | B_switch(cond, cases) when case_complete cond cases -> fold_left (fun b (i, cases) -> b && fold_and cases) true cases
            | B_assign(target, expr) -> true
            | _ -> false in
          let aggressive = false in (* decide whether to perform full analysis (ie turn off if bugs are seen as might be here - XXX not yet working as I would expect) *)
          let is_complete = if aggressive then is_complete else is_assign in  (* is assign looks 1 assign ahead.  it should look more.  Definately need to revisit this optimisation *)
          *)
          let is_complete = is_assign in
          if is_complete (List.head tl) 
          then assigns (nl,true) tl 
          else assigns (a::nl,opt) tl 
      in
      let nodes, opt = assigns ([],opt) nodes in
      let nodes = List.rev nodes in
      let nodes, opts = List.unzip (List.map (optimise opt) nodes) in
      nodes, List.fold (||) opt opts
      
    and optimise opt node = 
      match node with
      | B_if(cond, on_true, on_false) -> 
        (* recursively optimise *)
        let on_true, opt = optimise_list opt on_true in
        let on_false, opt = optimise_list opt on_false in
        B_if(cond, on_true, on_false), opt
      | B_switch(cond, cases) -> 
        (* filter empty cases *)
        let cases, opt = List.fold (fun (cur,opt) case -> match case with _,[] -> (cur,true) | _ -> (case::cur,opt)) ([],opt) cases in
        let cases = List.rev cases in
        (* recursively optimise *)
        let cases, opts = List.unzip (List.map (fun (i,case) -> let n,o = optimise_list opt case in (i,n),o) cases) in 
        B_switch(cond, cases), List.fold (||) opt opts
      | B_assign(target, expr) -> 
        node, opt
    in
    
    let run_optimiser code = 
      let rec run i code = 
        let code, opt = optimise_list false code in
        if opt 
        then run (i+1) code
        else code, i
      in
      run 1 code 
    in
    
    (* find remaining dependants for each signal *)
    let rec dependants_list deps nodes = List.fold dependants deps nodes
    and dependants deps node = 
      match node with
      | B_if(cond, on_true, on_false) -> dependants_list (cond::deps) (on_true @ on_false)
      | B_switch(cond, cases) ->
        (*let deps = cond :: ((map fst cases) @ deps) in // this adds the constants to the dependants map which is not really what we want *)
        dependants_list (cond::deps) (List.concat (List.map snd cases)) 
      //| B_assign(target, expr) -> expr::deps
      (* note: if the default assignment is optimised then we lose the fact 
         that the default is also a dependant which breaks the f# generator. 
         On the other hand I'm not sure what effect this will have on the
         scheduler. *)
      | B_assign(B_assign_tgt(qtarget, target, def, high, low), expr) -> def::expr::deps 
    in

    (* now wire up the signals *)
    List.iter2 (fun code target ->
      let code, opt = run_optimiser code in 
      let dependants = unique_signal_list (dependants_list [] code) in
      let behave_signal = { su = Signal_behave(signal_incr_uid(), width target, code, dependants) } in
      assign target behave_signal;
    ) codes targets

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (** bit generic *)
  let g_bit d  = (G_Bit(d))
  (** vector generic *)
  let g_vec d  = (G_Vec(d))
  (** integer generic *)
  let g_int d  = (G_Int(d))
  (** real generic *)
  let g_flt d  = (G_Float(d))
  (** string generic *)
  let g_str d  = (G_String(d))
  (** boolean generic *)
  let g_bool d = (G_Bool(d))

  (** Returns true if the given generic specification is a bit *)
  let g_bit_type  = function Gd_Bit _    | Gd_Sl _   -> true | _ -> false
  (** Returns true if the given generic specification is a vector *)
  let g_vec_type  = function Gd_Bv _     | Gd_Slv _  -> true | _ -> false
  (** Returns true if the given generic specification is an integer *)
  let g_int_type  = function Gd_Int _    | Gd_Nat _  | Gd_Pos _  -> true | _ -> false
  (** Returns true if the given generic specification is a string *)
  let g_str_type  = function Gd_String _ | Gd_Time _ -> true | _ -> false
  (** Returns true if the given generic specification is a real *)
  let g_flt_type  = function Gd_Float  _ -> true | _ -> false
  (** Returns true if the given generic specification is a boolean *)
  let g_bool_type = function Gd_Bool   _ -> true | _ -> false
  
  let generic_int name value = (name, Gd_Int(None), Some(G_Int(value)))
  let generic_string name value = (name, Gd_String(None), Some(G_String(value)))

  (** Associates a name with a signal in instantiations *)
  let (==>) name signal = (name, signal)

  (** Creates an instantiation of a sub module with generics and inouts.  Not supported for simulation. *)
  let instgio name generics inouts inputs outputs = 
    check_ok (List.map snd inputs);
    check_ok (List.map snd outputs);
    let inst = { su = Signal_inst(signal_incr_uid(), name, ref "", generics, inouts, inputs, outputs) } in
    List.iter (fun (x : string * Signal) -> 
      if not (snd x).IsWire 
      then failwith "Output signals of instantions must be wires"
      else assign (snd x) inst) (outputs@inouts)


  (** Creates an instantiation of a sub module.  Not supported for simulation. *)
  let inst name inputs outputs = instgio name [] [] inputs outputs

  type Signal with

    (** Gets the name of a signal.  If it's a named wire then returns the name, otherwise the standard prefix is applied to the uid and returned. *)
    member s.name = 
      match s.signal with
      | Signal_wire(a,w,n,d) when !n <> "" -> !n (* should be mangled *)
      | Signal_inst(a,n,m,g,io,i,o) when !m <> "" -> !m (* should be mangled *)
      | _ -> apply_prefix (string (s.uid))

    member s.string_of_node_type = 
      match s.signal with
      | Signal_empty    -> "empty"
      | Signal_const    (a,w,c) -> "const " ^ c
      | Signal_binop    (a,w,op,s0,s1) -> 
      begin
        match op with
          B_add -> "+"
        | B_sub -> "-"
        | B_mulu -> "*"
        | B_muls -> "*+"
        | B_and -> "&&&"
        | B_or -> "|||"
        | B_xor -> "^^^"
        | B_eq -> "==~"
        | B_lt -> "<"
        | B_cat -> "++"
      end
      | Signal_unop     (a,w,op,s) -> "~~~"
      | Signal_wire     (a,w,n,d) -> "wire"
      | Signal_mux      (a,w,sel,d) -> "mux"
      | Signal_select   (a,hi,lo,s) -> "[" ^ string hi ^ ":" ^ string lo ^ "]"
      | Signal_reg      (a,w,clk,rst,rstval,ena,d) -> "register"
      | Signal_mem      (a,dw,aw,size,clk,w,we,d,r) -> "memory"
      | Signal_behave   (a,w,b,d) -> "behave"
      | Signal_inst     (a,n,m,g,io,i,o) -> "inst"
#if INST2
      | Signal_inst2    (a,s,g,io,i,o) -> "inst2"
#endif
      | Signal_tri      (a,w,d) -> "tristate"

    (** Constant from an integer value.  Width taken from member argument. *)
    member x.consti(v) = consti x.width v

    (** Constant from a string using verilog style constant formatting ie 32'd12 *)
    member x.constv (s:string) = 
      match s.[0] with
      | '\'' -> constv (string x.width ^ s)
      | _ -> constv s

#if OLD_SLICE_SYNTAX
    member s.Item 
      with 
      (** Selection of a range of bits from a signal using the syntax signal.[hi,lo] *)
      get((hi : int),(lo : int)) = select s hi lo
#else
    member s.Item
      with
      (** Selection of a single bit from a signal using the syntax signal.[bit] *)
      get((b : int)) = bit s b
    member s.GetSlice(hi:int Option,lo:int Option) = 
      match hi,lo with
      | None,None -> s (* select whole vector, pretty useless case really, but we let it pass *)
      | Some(hi),Some(lo) -> (* select given range *)
        select s hi lo
      | None,Some(lo) -> (* select from top downto lo *)
        select s ((width s)-1) lo
      | Some(len),None -> (* select top len bits *)
        if len <= 0 then failwith "select.[len..] syntax requires len > 0";
        select s ((width s)-1) ((width s)-len)
#endif//OLD_SLICE_SYNTAX

    (** Concatentate two signals *)
    static member (++) ((a : Signal), (b : Signal)) = cat a b

    (** Select a single bit from signal *)
    member s.bit n = bit s n
    (** Convert signal to a list of bits.  MSB at head of resulting list *)
    member s.bits = bits s
    (** Convert signal to a list of bits.  LSB at head of resulting list *)
    member s.bits_lsb = bits_lsb s
    (** Convert signal to a list of bits.  MSB at head of resulting list *)
    member s.bits_msb = bits_msb s
    (** Split a signal into two as evenly as possible.  If the signals width is odd the left *)
    (** hand returned signal is 1 bit larger than the right hand one *)
    member s.split = split s
    (** Repeat a signal "n" times *)
    member s.repeat n = repeat s n
    (** Select the most significant (left most) bit. *)
    member s.msb = msb s
    (** Select the most significant (left most) bits. *)
    member s.msbs = msbs s
    (** Select the least significant (right most) bit. *)
    member s.lsb = lsb s
    (** Select the least significant (right most) bits. *)
    member s.lsbs = lsbs s
    (** Sign extend by one bit *)
    member s.se = se s
    (** Unsigned extend by one bit *)
    member s.ue = ue s

    (** resize vec depending on sign *)
    member s.resize(sign, w) = resize sign s w
    (** resize vec adding 0's at the msb as required *)
    member s.uresize w = s.resize(Unsigned, w)
    (** resize vec repeating the msb to preserve sign *)
    member s.sresize w = s.resize(Signed, w)

    (** Apply the given operator between each bit of the signal. Requires at least two signals. *)
    member s.reduce op = reduce op (bits s)
    (** Lets the single reduce case pass by just returning the data. *)
    member s.reduce_1 op = reduce_1 op (bits s)
    
    (** N-input mux *)
    member s.mux (d : Signal list) = mux s d

    (** 2-input mux *)
    member s.mux2((hi:Signal), (lo:Signal)) = mux2 s hi lo
    (** 2-input mux.  high argument is an integer *)
    member s.mux2((hi:int), (lo:Signal)) = s.mux2(lo.consti hi,lo)
    (** 2-input mux.  low argument is an integer *)
    member s.mux2((hi:Signal), (lo:int)) = s.mux2(hi,hi.consti lo)

    (** Addition.  Both arguments and the result are the same width *)
    static member (+) ((a : Signal), (b : Signal)) = add a b
    (** Addition.  Right argument is an integer. *)
    static member (+) ((a : Signal), (b : int)) = a + a.consti b
    (** Addition.  Right argument is a string. *)
    static member (+) ((a : Signal), (b : string)) = a + (a.constv b)

    (** Subtraction.  Both arguments and the result are the same width *)
    static member (-) ((a : Signal), (b : Signal)) = sub a b
    (** Subtraction.  Right argument is an integer. *)
    static member (-) ((a : Signal), (b : int)) = a - a.consti b
    (** Subtraction.  Right argument is a string. *)
    static member (-) ((a : Signal), (b : string)) = a - a.constv b

    (** Negation.  Argument and the result are the same width *)
    static member (~-) (a : Signal) = negate a

    (** Unsigned multiplication.  Results width is the sum of the widths of the arguments. *)
    static member ( * ) ((a : Signal), (b : Signal)) = mulu a b
    (** Unsigned multiplication.  Right argument is an integer. *)
    static member ( * ) ((a : Signal), (b : int)) = 
      assert (b >= 0);
      (* optimise if b=0, b=1 or b is a power of two *)
      match b with
      | 0 -> zero a.width
      | 1 -> a
      | x -> 
        let log = ubits b in
        let log1 = ubits (b-1) in
        let bb = 1 <<< log1 in
        if bb = b then a ++ (zero log1)
        else a * (consti log b)
    (** Unsigned multiplication.  Right argument is a string. *)
    static member ( * ) ((a : Signal), (b : string)) = a * a.constv b

    (** Signed multiplication.  Results width is the sum of the widths of the arguments. *)
    static member ( *+ ) ((a : Signal), (b : Signal)) = muls a b
    (** Signed multiplication.  Right argument is an integer. *)
    static member ( *+ ) ((a : Signal), (b : int)) = 
      match b with
      | 0 -> zero a.width
      | 1 -> a
      | x -> a *+ (consti (sbits b) b)
    (** Signed multiplication.  Right argument is a string. *)
    static member ( *+ ) ((a : Signal), (b : string)) = a *+ a.constv b

    (** Logical shift left by a constant. *)
    static member (<<<) ((a : Signal), (shift : int)) = sll a shift
    (** Logical shift left by a constant. *)
    static member (<<~) ((a : Signal), (shift : int)) = a <<< shift
    (** Logical shift left by a variable amount. *)
    static member (<<~) ((a : Signal), (shift : Signal)) = shift_left a shift

    (** Logical shift right by a constant. *)
    static member (>>>) ((a : Signal), (shift : int)) = srl a shift
    (** Logical shift right by a constant. *)
    static member (>>~) ((a : Signal), (shift : int)) = a >>> shift
    (** Logical shift right by a variable amount. *)
    static member (>>~) ((a : Signal), (shift : Signal)) = shift_right_logical a shift

    (** Arithmetic shift right by a constant. *)
    static member (>>+) ((a : Signal), (shift : int)) = sra a shift
    (** Arithmetic shift right by a variable amount. *)
    static member (>>+) ((a : Signal), (shift : Signal)) = shift_right_arithmetic a shift

    (** Logical and.  Both arguments and the returned signal have the same width. *)
    static member (&&&) (a, b) = band a b
    (** Logical and.  Both arguments and the returned signal have the same width. *)
    static member (&~) ((a : Signal), (b : Signal)) = a &&& b
    (** Logical and.  Right hand argument is an integer. *)
    static member (&~) ((a : Signal), (b : int)) = a &&& a.consti b
    (** Logical and.  Right hand argument is a string. *)
    static member (&~) ((a : Signal), (b : string)) = a &&& a.constv b

    (** Logical or.  Both arguments and the returned signal have the same width. *)
    static member (|||) ((a : Signal), (b : Signal)) = bor a b
    (** Logical or.  Both arguments and the returned signal have the same width. *)
    static member (|~) ((a : Signal), (b : Signal)) = a ||| b
    (** Logical or.  Right hand argument is an integer. *)
    static member (|~) ((a : Signal), (b : int)) = a ||| a.consti b
    (** Logical or.  Right hand argument is a string. *)
    static member (|~) ((a : Signal), (b : string)) = a ||| a.constv b

    (** Logical xor.  Both arguments and the returned signal have the same width. *)
    static member (^^^) ((a : Signal), (b : Signal)) = bxor a b
    (** Logical xor.  Both arguments and the returned signal have the same width. *)
    static member (^~) ((a : Signal), (b : Signal)) = a ^^^ b
    (** Logical xor.  Right hand argument is an integer. *)
    static member (^~) ((a : Signal), (b : int)) = a ^^^ a.consti b
    (** Logical xor.  Right hand argument is a string. *)
    static member (^~) ((a : Signal), (b : string)) = a ^^^ a.constv b

    (** Logical NOT.  Argument and result have the same width *)
    static member (~~~) a = bnot a

    (** Equality.  The arguments must have the same width. The returned signal is 1 bit wide. *)
    static member (==~) ((a : Signal), (b : Signal)) = eq a b
    (** Equality.  Right hand argument is an integer. *)
    static member (==~) ((a : Signal), (b : int)) = a ==~ a.consti b
    (** Equality.  Right hand argument is a string. *)
    static member (==~) ((a : Signal), (b : string)) = a ==~ a.constv b

    (** Inequality.  The arguments must have the same width. The returned signal is 1 bit wide. *)
    static member (/=~) ((a : Signal), (b : Signal)) = ~~~ (a ==~ b)
    (** Inequality.  Right hand argument is an integer. *)
    static member (/=~) ((a : Signal), (b : int)) = ~~~ (a ==~ b)
    (** Inequality.  Right hand argument is a string. *)
    static member (/=~) ((a : Signal), (b : string)) = ~~~ (a ==~ b)

    (** Unsigned less than.  The arguments must have the same width. The returned signal is 1 bit wide. *)
    static member (<~) ((a : Signal), (b : Signal)) = lsu a b
    (** Unsigned less than.  Right hand argument is an integer. *)
    static member (<~) ((a : Signal), (b : int)) = a <~ a.consti b
    (** Unsigned less than.  Right hand argument is a string. *)
    static member (<~) ((a : Signal), (b : string)) = a <~ a.constv b
    
    (** Unsigned greater than.  The arguments must have the same width. The returned signal is 1 bit wide. *)
    static member (>~) ((a : Signal), (b : Signal)) = gtu a b
    (** Unsigned greater than.  Right hand argument is an integer. *)
    static member (>~) ((a : Signal), (b : int)) = a >~ a.consti b
    (** Unsigned greater than.  Right hand argument is a string. *)
    static member (>~) ((a : Signal), (b : string)) = a >~ a.constv b

    (** Unsigned less than or equal.  The arguments must have the same width. The returned signal is 1 bit wide. *)
    static member (<=~) ((a : Signal), (b : Signal)) = lsequ a b
    (** Unsigned less than or equal.  Right hand argument is an integer. *)
    static member (<=~) ((a : Signal), (b : int)) = a <=~ a.consti b
    (** Unsigned less than or equal.  Right hand argument is a string. *)
    static member (<=~) ((a : Signal), (b : string)) = a <=~ a.constv b

    (** Unsigned greater than or equal.  The arguments must have the same width. The returned signal is 1 bit wide. *)
    static member (>=~) ((a : Signal), (b : Signal)) = gtequ a b
    (** Unsigned greater than or equal.  Right hand argument is an integer. *)
    static member (>=~) ((a : Signal), (b : int)) = a >=~ a.consti b
    (** Unsigned greater than or equal.  Right hand argument is a string. *)
    static member (>=~) ((a : Signal), (b : string)) = a >=~ a.constv b

    (* Signed comparison operators.  For width == 1, cannot select lsbs, so it's dealt with specially with combinatorial expressions *)

    (** Signed less than.  The arguments must have the same width. The returned signal is 1 bit wide. *)
    static member (<+) ((a : Signal), (b : Signal)) = lss a b
    (** Signed less than.  Right hand argument is an integer. *)
    static member (<+) ((a : Signal), (b : int)) = a <+ a.consti b
    (** Signed less than.  Right hand argument is a string. *)
    static member (<+) ((a : Signal), (b : string)) = a <+ a.constv b
    
    (** Signed greater than.  The arguments must have the same width. The returned signal is 1 bit wide. *)
    static member (>+) ((a : Signal), (b : Signal)) = gts a b
    (** Signed greater than.  Right hand argument is an integer. *)
    static member (>+) ((a : Signal), (b : int)) = a >+ a.consti b
    (** Signed greater than.  Right hand argument is a string. *)
    static member (>+) ((a : Signal), (b : string)) = a >+ a.constv b

    (** Signed less than or equal.  The arguments must have the same width. The returned signal is 1 bit wide. *)
    static member (<=+) ((a : Signal), (b : Signal)) = lseqs a b
    (** Signed less than or equal.  Right hand argument is an integer. *)
    static member (<=+) ((a : Signal), (b : int)) = a <=+ a.consti b
    (** Signed less than or equal.  Right hand argument is a string. *)
    static member (<=+) ((a : Signal), (b : string)) = a <=+ a.constv b

    (** Signed greater than or equal.  The arguments must have the same width. The returned signal is 1 bit wide. *)
    static member (>=+) ((a : Signal), (b : Signal)) = gteqs a b
    (** Signed greater than or equal.  Right hand argument is an integer. *)
    static member (>=+) ((a : Signal), (b : int)) = a >=+ a.consti b
    (** Signed greater than or equal.  Right hand argument is a string. *)
    static member (>=+) ((a : Signal), (b : string)) = a >=+ a.constv b

    (** Signal naming *)
    static member (--) ((s : Signal), n) = set_name s n
    
    (** Dangling wire assignment. *)
    static member (<==) ((a : Signal), (b : Signal)) = assign a b

    (** Creates a register *)
    member d.reg(clock, reset, (reset_val:Signal), enable) = reg clock reset reset_val enable d
    (** Creates a register where reset value is specified as an integer *)
    member d.reg(clock, reset, (reset_val:int), enable) = reg clock reset (d.consti reset_val) enable d
    
    (** Creates a register with default clock and reset *)
    member d.regc(enable) = regc enable d

    (** Creates a memory *)
    member d.memory(size, clk, we, w, r) = memory size clk we w d r 

    (** Creates an output *)
    member s.output n = output n s

    (** Behavioral if/else *)
    member cond.b_if (on_true, on_false) = b_if cond on_true on_false
    (** if (cond) [ code ] [ ]  *)
    member cond.b_when on_true = b_if cond on_true []
    (** if (cond) [ ] [ code ]  *)
    member cond.b_unless on_false = b_if cond [] on_false
    
    (** Behavioral if/else.  This overloaded version comes in curried form since writing the uncurried form requires awkward syntax. *)
    // member cond.b_if on_true on_false = b_if cond on_true on_false
    (** Behavioral switch *)
    member cond.b_switch cases = b_switch cond cases
    (** Behavioral case *)
    member cond.b_case case = b_case cond case
    (** Wire to which combinatorial behavioral assignment can be made *)
    member defval.b_wire = b_wire defval
    (** Register to which sequential behavioral assignment can be made.  Holds its previous value if no assignment is made. *)
    member rstval.b_reg(clock, reset, enable) = b_reg clock reset rstval enable rstval.width


  end

  type BehaveAssignTarget with

      (** Accesses underlying signal value of behavioral register or wire *)
      member x.q = let (B_assign_tgt(q,_,_,_,_)) = x in q

      (** Behavioral assignment *)
      static member (|==) ((target : BehaveAssignTarget), (expr : Signal)) = 
        check_same [target.q; expr];
        B_assign(target, expr)

      (** Behavioral assignment with integer value *)
      static member (|==) ((target : BehaveAssignTarget), (expr : int)) = target |== target.q.consti expr
      
      (** Behavioral assignment with sring value *)
      static member (|==) ((target : BehaveAssignTarget), (expr : string)) = target |== target.q.constv expr
      
      (** Sets name of behavioral register or wire *)
      member x.set_name name = 
        let (B_assign_tgt(a,b,c,d,e)) = x in
        B_assign_tgt(set_name a name,b,c,d,e)
        
      (** Sets name of behavioral register or wire *)
      static member (--) ((s : BehaveAssignTarget), n) = s.set_name n

#if BEHAVE_TEST
      member s.Item 
        with 
        (** NOT COMPLETE: designed to allow part selection on left hand side of behavioral assignments ... how can we ensure it doesnt occur on right hand side??? *)
        get((hi : int),(lo : int)) = 
          let massert x = if x then () else failwith "BehaveAssignTarget selection failed" in
          let B_assign_tgt(a,b,c,_,_) = s in
          massert (hi>=lo);
          massert (hi>=0);
          massert (lo>=0);
          massert (hi<=(a.width-1));
          B_assign_tgt(a,b,c,hi,lo)
#endif
        
  end
      
  type Inst = 
    { inst : Signal }
    with

      // PROBLEM.
      // Need to assign IO signals both externally, and internally to the instance.
      // For this we'll need to rethink the way this is working.    
      static member makegio comp_name generics inouts inputs outputs = 
        let outputs = List.map (fun (n,w) -> n, wire w) outputs in
        let inouts = List.map (fun (n,w) -> n, wire w) inouts in
        let inst = { su = Signal_inst(signal_incr_uid(), comp_name, ref "", generics, inouts, inputs, outputs) } in
        List.iter (fun (x : string * Signal) -> 
          if not (snd x).IsWire 
          then failwith "Output/inout signals of instantions must be wires"
          else assign (snd x) inst) (outputs@inouts);
        { inst = inst }
        
      static member makeg comp_name generics inputs outputs = Inst.makegio comp_name generics [] inputs outputs 
      static member makeio comp_name inouts inputs outputs = Inst.makegio comp_name [] inouts inputs outputs 
      static member make comp_name inputs outputs = Inst.makegio comp_name [] [] inputs outputs 
    
      member x.Inputs = 
        match x.inst.signal with
        | Signal_inst(a,n,m,g,io,i,o) -> i
        | _ -> failwith "Unexpected signal type in Instance"
      member x.Outputs = 
        match x.inst.signal with
        | Signal_inst(a,n,m,g,io,i,o) -> o
        | _ -> failwith "Unexpected signal type in Instance"
      member x.Inouts = 
        match x.inst.signal with
        | Signal_inst(a,n,m,g,io,i,o) -> io
        | _ -> failwith "Unexpected signal type in Instance"
      
      member x.Item
        with get(n:string) = 
          let assoc k = List.tryFind(fst >> (=) k) in
          let ports = x.Inputs @ x.Outputs @ x.Inouts in
          assoc n ports;
          
      (** Sets name of component instantiation *)
      static member (--) ((s : Inst), n) = 
        s.inst -- n |> ignore;  // Either returns the same signal, or throws an exception
        s
      
    end

end

