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

(** HDFS release 0.1 version of the simulator.  *)
module DigitalLogic.SimulatorV01

open System.Diagnostics;
open DigitalLogic.Circuit
open DigitalLogic.Signal

(****************************************************************)
(****************************************************************)

exception Sim_error of string

(** raise the exception Sim_error *)
let failwith s = raise (Sim_error s)

(** simulator task *)
type task_t = unit -> unit

(** simulator input or output port *)
type 'a port_t = string * int * 'a

(** Simulator data type *)
type 'a Simulator = Simulator of 
  task_t *                          (* reset *)
  task_t *                          (* cycle *)
  'a port_t list *                  (* inputs *)
  'a port_t list *                  (* wires *)
  'a port_t list                    (* outputs *)

(* data arrays returned by the simulator generator *)
type 'a gen_sim_data_t = SimDataGen of 'a list * 'a list * 'a list 

(****************************************************************)
(* implementations of data operations for various simulator types. *)
(* currently implemented - int, uint32, uint64, array based uint32 *)
(****************************************************************)

(** Type which parameterises the simulator so it can be used with integer or array based datatypes *)
type 'a sim_data_t = 
{
  (* For operations like copy, binop etc the mask function will be applied after the operator is run.
     At the moment this occurs after pretty much every call.  However, certain functions make certain 
     garuntees, so this can be relaxed for all implementations within the compilation function. *)
  create : int -> 'a;
  create_array : int -> int -> 'a array;
  set_zero : 'a -> int -> unit;
  set_const : 'a -> string -> unit;
  to_int : 'a -> int;
  copy : 'a -> 'a -> int -> unit;
  mask : 'a -> int -> unit;
  sign_extend : 'a -> int -> unit;
  unop : UnaryOperator -> 'a -> 'a -> int -> unit;
  binop : BinaryOperator -> 'a -> 'a -> 'a -> int -> int -> unit;
  sel : 'a -> 'a -> int -> int -> unit;
  eq : 'a -> 'a -> int -> bool;
  to_hex_str : 'a -> int -> string;
  of_hex_str : 'a -> string -> int -> unit;
  to_bin_str : 'a -> int -> string;
}

let hex_of_int i = 
  match i with
  | 0  -> "0"  | 1  -> "1" | 2  -> "2" | 3  -> "3"
  | 4  -> "4"  | 5  -> "5" | 6  -> "6" | 7  -> "7"
  | 8  -> "8"  | 9  -> "9" | 10 -> "a" | 11 -> "b"
  | 12 -> "c"  | 13 -> "d" | 14 -> "e" | 15 -> "f"
  | _ -> failwith ("hex_of_int: Invalid hex char: " ^ (string i))
  
let int_of_hex h = 
  match h with
  | '0'  -> 0  | '1' -> 1  | '2'  -> 2  | '3' -> 3 
  | '4'  -> 4  | '5' -> 5  | '6'  -> 6  | '7' -> 7 
  | '8'  -> 8  | '9' -> 9  | 'a'  -> 10 | 'b' -> 11
  | 'c'  -> 12 | 'd' -> 13 | 'e'  -> 14 | 'f' -> 15
  | 'A'  -> 10 | 'B' -> 11 | 'C'  -> 12 | 'D' -> 13 
  | 'E'  -> 14 | 'F' -> 15
  | _ -> failwith ("int_of_hex: Invalid hex char: " ^ (String.of_char h))
  
(** Integer simulator datatype implementation *)
let simDataInt = 
  let mask = ~~~ 0 in
  let create1() = ref 0 in
  let sign_extend a w = if ((w % 32) <> 0) && (0 <> (a &&& (1 <<< (w-1)))) then a ||| (mask <<< w) else a in
  {
    create = (fun n -> create1());
    create_array = (fun wid len -> 
        let a = Array.create len (create1()) in
        for i=1 to (len-1) do a.[i] <- create1() done;
        a
      );
    set_zero = (fun a n -> a := 0);
    set_const = (fun data s ->
      let len = String.length s in
      data := 0;
      for i = 0 to len - 1 do
        data := (!data <<< 1) + (if s.[i] = '1' then 1 else 0)
      done
    );
    to_int = (fun a -> !a);
    copy = (fun a b w -> a := !b);
    mask = (fun a w -> if w < 32 then a := !a &&& (~~~ (mask <<< w)));
    sign_extend = (fun a w -> a := sign_extend !a w);
    unop = (fun op r a w -> r := ~~~ !a);
    binop = (fun op r a b wa wb -> 
      r := match op with
        | B_add -> !a + !b; 
        | B_sub -> !a - !b
        | B_mulu -> !a * !b
        | B_muls -> 
          let a = (sign_extend !a wa) in
          let b = (sign_extend !b wb) in
          a * b
        | B_and -> !a &&& !b
        | B_or -> !a ||| !b
        | B_xor -> !a ^^^ !b
        | B_eq -> if !a = !b then 1 else 0
        | B_lt -> if !a < !b then 1 else 0
        | B_cat -> !b ||| (!a <<< wb)); 
    sel = (fun r a hi lo -> r :=  UInt32.to_int ((UInt32.of_int !a) >>> lo)); 
    eq = (fun a b w -> !a = !b);
    to_hex_str = (fun d bits -> 
        let b4 = (bits + 3) / 4 in
        let rec build v b =
          if b4 = b then ""
          else (build (UInt32.to_int ((UInt32.of_int v) >>> 4)) (b+1)) ^ (hex_of_int (v &&& 15)) in
        build !d 0
      );
    of_hex_str = (fun d str w -> 
        let len = String.length str in
        let idx i = len - i - 1 in
        let width = (w+3)/4 in
        d := 0;
        for i=0 to width-1 do
          d := !d ||| ((int_of_hex str.[idx i]) <<< (4*i));
        done;
      );
    to_bin_str = (fun d bits ->
        let rec build v b =
          if b = bits then ""
          else (build (v >>> 1) (b+1)) ^ (if (v &&& 1) = 1 then "1" else "0")
        in 
        build !d 0
      );
  }

(** Unsigned integer simulator datatype implementation *)
let simDataUInt32 = 
  let mask = ~~~ 0ul in
  let create1() = ref 0ul in
  let sign_extend a w = if ((w % 32) <> 0) && (0ul <> (a &&& (1ul <<< (w-1)))) then a ||| (mask <<< w) else a in
  {
    create = (fun n -> create1());
    create_array = (fun wid len -> 
        let a = Array.create len (create1()) in
        for i=1 to (len-1) do a.[i] <- create1() done;
        a
      );
    set_zero = (fun a n -> a := 0ul);
    set_const = (fun data s ->
      data := 0ul;
      for i = 0 to String.length s - 1 do
        data := (!data <<< 1) + (if s.[i] = '1' then 1ul else 0ul)
      done);
    to_int = (fun a -> UInt32.to_int !a);
    copy = (fun a b w -> a := !b);
    mask = (fun a w -> if w < 32 then a := !a &&& (~~~ (mask <<< w)));
    sign_extend = (fun a w -> a := sign_extend !a w);
    unop = (fun op r a w -> r := ~~~ !a);
    binop = (fun op r a b wa wb -> 
      r := match op with
        | B_add -> !a + !b
        | B_sub -> !a - !b
        | B_mulu -> !a * !b
        | B_muls -> 
          let se a w = if 0ul = (a &&& (1ul <<< (w-1))) then a ||| (mask <<< w) else a in
          (se !a wa) * (se !b wb)
        | B_and -> !a &&& !b
        | B_or -> !a ||| !b
        | B_xor -> !a ^^^ !b
        | B_eq -> if !a <> 0ul then 1ul else 0ul
        | B_lt -> if !a < !b then 1ul else 0ul
        | B_cat -> !b ||| (!a <<< wb)); 
    sel = (fun r a hi lo -> r := (!a >>> lo)); 
    eq = (fun a b w -> !a = !b);
    to_hex_str = (fun d bits -> 
        let b4 = (bits + 3) / 4 in
        let rec build v b =
          if b4 = b then ""
          else (build (v >>> 4) (b+1)) ^ (hex_of_int (UInt32.to_int (v &&& 15ul))) in
        build !d 0
      );
    of_hex_str = (fun d str w -> 
        let len = String.length str in
        let idx i = len - i - 1 in
        let width = (w+3)/4 in
        d := 0ul;
        for i=0 to width-1 do
          d := !d ||| (UInt32.of_int ((int_of_hex str.[idx i]) <<< (4*i)));
        done;
      );
    to_bin_str = (fun d bits ->
        let rec build v b =
          if b = bits then ""
          else (build (v >>> 1) (b+1)) ^ (if (v &&& 1ul) = 1ul then "1" else "0")
        in 
        build !d 0
      );
  }

(** 64 bit unsigned integer simulator datatype implementation *)
let simDataUInt64 = 
  let mask = ~~~ 0UL in
  let create1() = ref 0UL in
  let sign_extend a w = if ((w % 64) <> 0) && (0UL <> (a &&& (1UL <<< (w-1)))) then a ||| (mask <<< w) else a in
  {
    create = (fun n -> ref 0UL);
    create_array = (fun wid len -> 
        let a = Array.create len (create1()) in
        for i=1 to (len-1) do a.[i] <- create1() done;
        a
      );
    set_zero = (fun a n -> a := 0UL);
    set_const = (fun data s ->
      data := 0UL;
      for i = 0 to String.length s - 1 do
        data := (!data <<< 1) + (if s.[i] = '1' then 1UL else 0UL)
      done);
    to_int = (fun a -> UInt64.to_int !a);
    copy = (fun a b w -> a := !b);
    sign_extend = (fun a w -> a := sign_extend !a w);
    mask = (fun a w -> if w < 64 then a := !a &&& (~~~ (mask <<< w)));
    unop = (fun op r a w -> r := ~~~ !a);
    binop = (fun op r a b wa wb -> 
      r := match op with
        | B_add -> !a + !b
        | B_sub -> !a - !b
        | B_mulu -> !a * !b
        | B_muls -> 
          let se a w = if 0UL = (a &&& (1UL <<< (w-1))) then a ||| (mask <<< w) else a in
          (se !a wa) * (se !b wb)
        | B_and -> !a &&& !b
        | B_or -> !a ||| !b
        | B_xor -> !a ^^^ !b
        | B_eq -> if !a <> 0UL then 1UL else 0UL
        | B_lt -> if !a < !b then 1UL else 0UL
        | B_cat -> !b ||| (!a <<< wb)); 
    sel = (fun r a hi lo -> r := (!a >>> lo)); 
    eq = (fun a b w -> !a = !b);
    to_hex_str = (fun d bits -> 
        let b4 = (bits + 3) / 4 in
        let rec build v b =
          if b4 = b then ""
          else (build (v >>> 4) (b+1)) ^ (hex_of_int (UInt64.to_int (v &&& 15UL))) in
        build !d 0
      );
    of_hex_str = (fun d str w -> 
        let len = String.length str in
        let idx i = len - i - 1 in
        let width = (w+3)/4 in
        d := 0UL;
        for i=0 to width-1 do
          d := !d ||| (UInt64.of_int ((int_of_hex str.[idx i]) <<< (4*i)));
        done;
      );
    to_bin_str = (fun d bits ->
        let rec build v b =
          if b = bits then ""
          else (build (v >>> 1) (b+1)) ^ (if (v &&& 1UL) = 1UL then "1" else "0")
        in 
        build !d 0
      );
  }

/// r = a * b.  a and b must be sign extended (if appropriate) to 32 bits prior to this call
let array_mul words_r sign_a sign_b words_a words_b r a b = 
  if words_r = 1 then (
    r.[0] <- a.[0] * b.[0]
  ) else (
    let access a words sign i =
      if i >= words then (if sign then 0xFFFFFFFFul else 0ul)
      else a.[i] in
    let access_a = access a words_a sign_a in
    let access_b = access b words_b sign_b in
    let mask = 0xFFFFFFFFUL in
    for i = 0 to words_r - 1 do r.[i] <- 0ul done;
    for i = 0 to words_r - 1 do 
      let ib = ref 0 in
      for ia = i downto 0 do 
        let tmp = ref ((UInt32.to_uint64 (access_a ia)) * (UInt32.to_uint64 (access_b !ib))) in
        for ic = i to words_r - 1 do
          tmp := !tmp + (UInt32.to_uint64 r.[ic]);
          r.[ic] <- UInt64.to_uint32 (!tmp &&& mask);
          tmp := !tmp >>> 32;
        done;
        ib := !ib + 1;
      done;
    done
  )

(** Array based simulator datatype implementation *)
let simDataArray = 
  let mask = ~~~ 0ul in
  let sign_bit a w = (* gets the sign bit *)
    let w = w - 1 in
    let word = w / 32 in
    let bit = w % 32 in
    if 0ul <> (a.[word] &&& (1ul <<< bit)) then true else false 
  in
  let sign_extend a w = (* sign extends top word, if required (not 32 bits, is signed) *)
    let sb = sign_bit a w in
    if w % 32 <> 0 then (
      let word = w / 32 in
      let bit = w % 32 in
      let sign_extend a w = a ||| (mask <<< w) in
      if sb then a.[word] <- sign_extend a.[word] bit
    );
    sb
  in
  let mask_data a w = (* masks the top word *)
    let words = (w + 31) / 32 in
    let bits = w % 32 in
    if bits <> 0 then
      a.[words-1] <- a.[words-1] &&& (~~~ (mask <<< bits)) 
  in
  let create1 n = Array.create ((n + 31) / 32) 0ul in
  {
    create = (fun n -> create1 n);
    create_array = (fun wid len -> 
        let a = Array.create len (create1 wid) in
        for i=1 to (len-1) do a.[i] <- create1 wid done;
        a
      );
    set_zero = (fun a n -> for i=0 to Array.length a - 1 do a.[i] <- 0ul done);
    set_const = (fun data s ->
      let bits = String.length s in
      let words = (bits + 31) / 32 in
      for i = 0 to words - 1 do data.[i] <- 0ul; done;
      for i = 0 to bits - 1 do
        let word, bit = ((i / 32), (i % 32)) in
        if s.[bits-i-1] = '1' then 
          data.[word] <- data.[word] ||| (1ul <<< bit);
      done);
      
    to_int = (fun a -> UInt32.to_int a.[0]);
    copy = (fun a b w -> 
      let words = (w + 31) / 32 in
      for i = 0 to words - 1 do a.[i] <- b.[i] done);
    mask = mask_data;
    sign_extend = (fun a w -> ignore (sign_extend a w));
    unop = (fun op r a w -> 
      let words = (w + 31) / 32 in
      for i = 0 to words - 1 do r.[i] <- ~~~ a.[i] done);
    binop = (fun op r a b wa wb -> 
      let words_a = (wa + 31) / 32 in
      let words_b = (wb + 31) / 32 in
      match op with
      (* addition *)
      | B_add -> 
        let tmp = ref 0UL in
        let mask = 0xFFFFFFFFUL in
        for i=0 to words_a - 1 do
          tmp := (UInt32.to_uint64 a.[i]) + (UInt32.to_uint64 b.[i]) + !tmp;
          r.[i] <- UInt64.to_uint32 (!tmp &&& mask);
          tmp := (!tmp >>> 32) &&& 1UL;
        done;
      | B_sub -> 
        let tmp = ref 0UL in
        let mask = 0xFFFFFFFFUL in
        for i=0 to words_a - 1 do
          tmp := (UInt32.to_uint64 a.[i]) - (UInt32.to_uint64 b.[i]) - !tmp;
          r.[i] <- UInt64.to_uint32 (!tmp &&& mask);
          tmp := (!tmp >>> 32) &&& 1UL;
        done;
      (* multiplication *)
      | B_mulu -> array_mul ((wa+wb+31)/32) false false words_a words_b r a b
      | B_muls -> (
        let sign_a = sign_extend a wa in (* add sign *)
        let sign_b = sign_extend b wb in
        array_mul ((wa+wb+31)/32) sign_a sign_b words_a words_b r a b;
        if sign_a then mask_data a wa; (* restore *)
        if sign_b then mask_data b wb;
      )
      (* logical *)
      | B_and -> for i=0 to words_a - 1 do r.[i] <- a.[i] &&& b.[i] done
      | B_or -> for i=0 to words_a - 1 do r.[i] <- a.[i] ||| b.[i] done
      | B_xor -> for i=0 to words_a - 1 do r.[i] <- a.[i] ^^^ b.[i] done
      (* relational *)
      | B_eq -> 
        let result = ref true in
        for i=0 to words_a - 1 do
          result := !result && (a.[i] = b.[i]);
        done;
        if !result then r.[0] <- 1ul
        else r.[0] <- 0ul;
      | B_lt -> 
        let rec comp i = 
          if i < 0 then false
          else if a.[i] < b.[i] then true
          else if a.[i] > b.[i] then false
          else comp (i-1) in
        if comp (words_a-1) then r.[0] <- 1ul
        else r.[0] <- 0ul
      (* concatenation *)
      | B_cat -> 
        if (wb % 32) = 0 then (
          for i=0 to words_b - 1 do r.[i] <- b.[i] done;
          for i=0 to words_a - 1 do r.[i+words_b] <- a.[i] done
        ) else (
          let words_b = (wb + 31) / 32 in
          let shift_left = wb % 32 in
          let shift_right = 32-shift_left in
          let xi = ref (words_b-1) in
          let ai = ref 0 in
          let bits_a = ref wa in
          (* Copy b (at least 1 word will be copied) *)
          for bi=0 to words_b-1 do r.[bi] <- b.[bi] done;
          while (!bits_a > 0) do
            (* can fit the remaining bits into the last word *)
            if (!bits_a <= shift_right) then (
                r.[!xi] <- r.[!xi] ||| (a.[!ai] <<< shift_left);
                bits_a := 0
            (* at least one complete word to copy, or remaining bits dont fit into just the current word *)
            ) else (
                r.[!xi] <- r.[!xi] ||| (a.[!ai] <<< shift_left);
                r.[!xi+1] <- a.[!ai] >>> shift_right;
                bits_a := !bits_a - 32;
                xi := !xi + 1;
                ai := !ai + 1
            )
          done
        )
    );
    sel = (fun r a msb lsb -> 
      let lsw = lsb / 32 in
      let shift_right = lsb % 32 in
      if (msb / 32) = (lsb / 32) then (   (* msb and lsb in same word *)
        r.[0] <- (a.[lsw] >>> shift_right)
      ) else if shift_right = 0 then (
        let words = (msb - lsb + 32) / 32 in
        for i = 0 to words - 1 do
          r.[i] <- a.[lsw + i]
        done;
      ) else (
        let bits = msb - lsb + 1 in
        let msw = msb / 32 in
        let words = (bits + 31) / 32 in  
        let shift_left = 32 - shift_right in
        for i=0 to words-2 do  
          r.[i] <- (a.[lsw + i] >>> shift_right) ||| (a.[lsw + i + 1] <<< shift_left)
        done;
        if (lsw + words) >= (Array.length a) 
        then r.[words-1] <-  (a.[lsw + words - 1] >>> shift_right)
        else r.[words-1] <- ((a.[lsw + words - 1] >>> shift_right) ||| (a.[lsw + words] <<< shift_left));
      )
    );
    eq = (fun a b w -> 
      let words = (w + 31) / 32 in
      let result = ref true in
      for i=0 to words - 1 do
        result := !result && (a.[i] = b.[i]);
      done;
      !result 
    );
    to_hex_str = (fun d bits -> 
        let b4 = (bits + 3) / 4 in
        let rec build b =
          if b4 = b then ""
          else (build (b+1)) ^ (hex_of_int (UInt32.to_int ((d.[b/8] >>> (4*(b%8))) &&& 15ul))) in
        build 0
      );
    of_hex_str = (fun d str w -> 
        let len = String.length str in
        let idx i = len - i - 1 in
        let width = (w + 3) / 4 in
        let words = (w + 31) / 32 in
        for i=0 to words-1 do
          let nibs = min 8 (width - (i*8)) in
          d.[i] <- 0ul;
          for j=0 to nibs-1 do
            d.[i] <- d.[i] ||| (UInt32.of_int ((int_of_hex str.[idx ((i*8)+j)]) <<< (4*j)));
          done;
        done
      );
    to_bin_str = (fun d bits ->
        let rec build b =
          if b = bits then ""
          else 
            let word = b / 32 in
            let bit = b % 32 in
            (build (b+1)) ^ (if ((d.[word] >>> bit) &&& 1ul) = 1ul then "1" else "0")
        in 
        build 0
      );
  }

(****************************************************************)
(****************************************************************)

(** Create a simulator from the given circuit.  Performs the scheduling and "compiles" a task list. *)
let create (d : 'a sim_data_t) (circuit : Circuit) = 

  let os = output_string stdout in
  
  let inputs = circuit.Inputs in
  let outputs = circuit.Outputs in
  let wires = circuit.Wires in
  let regs = circuit.Regs in
  let mems = circuit.Memories in
  let logic = circuit.Logic @ circuit.Constants in

  let dependants (signal : Signal) =
    match signal.signal with
    | Signal_reg _ -> []
    | Signal_mem(_,_,_,_,_,w,we,data,r) -> [w;we;data;r] (* XXX the read address cannot be dependant on the output.  The write port must surely also be scheduled??? 
                                                            I think what we need to do is schedule the read portion, then update the write sycnronously along with
                                                            registers at the end of the cycle XXXX  Should work out a testcase for this. 
                                                            Update: the bug turned up in some code.  Adding all signals to the schedule makes it work.  But, this
                                                            will fail if theres a path from q to any of w, we or data, which is incorrect.  What needs to happen
                                                            is we schedule the read and execute it as usual.  Then at the end (when everything else is updated
                                                            (except registers) do the write.  *)
    | Signal_wire(_,_,_,d) -> if !d = empty then [] else [!d] 
    | _ -> signal.dependants in

  let schedule = scheduler dependants (logic @ wires @ mems) (inputs @ regs) in

  (* Check that clocks and resets are simple wire (or chains thereof) or empty.  If not write warning that the circuit will not simulate as expected *)
  let rec check_clk_rst (s : Signal) = 
    if s.IsEmpty then true
    else if s.IsWire then
      check_clk_rst (wire_connection s)
    else false
  in
  let check_clk s = 
    if not (check_clk_rst s) then
      os ("*** WARNING: The clock signal " ^ s.name ^ " is driven by logic.\nThis is not supported by the simulator so simulation may not be correct.\n")
  in
  let check_rst s = 
    if not (check_clk_rst s) then
      os ("*** WARNING: The reset signal " ^ s.name ^ " is driven by logic.\nThis is not supported by the simulator so simulation may not be correct.\n")
  in
    
  let make_data_map map (signal : Signal) = 
    match signal.signal with
    | Signal_empty    -> 
      map
    | Signal_const    (a,w,c) -> 
      let data = d.create w in 
      d.set_const data c;
      Map.add signal.uid data map
    | Signal_binop    (a,w,op,s0,s1) -> 
      let data = d.create w in
      Map.add signal.uid data map
    | Signal_unop     (a,w,op,s) -> 
      let data = d.create w in
      Map.add signal.uid data map
    | Signal_wire     (a,w,n,data) -> 
      let data = d.create w in
      Map.add signal.uid data map
    | Signal_mux      (a,w,sel,data) -> 
      let data = d.create w in
      Map.add signal.uid data map
    | Signal_select   (a,hi,lo,s) -> 
      let data = d.create (hi-lo+1) in
      Map.add signal.uid data map
    | Signal_reg      (a,w,clk,rst,rstval,ena,data) -> 
      let data = d.create w in
      check_clk clk;
      check_rst rst;
      Map.add signal.uid data map
    | Signal_mem      (a,dw,aw,size,clk,w,we,data,r) -> 
      let data = d.create dw in
      check_clk clk;
      Map.add signal.uid data map
    | Signal_behave   (a,w,b,dl) -> 
      let data = d.create w in
      Map.add signal.uid data map
    | Signal_inst     _ ->
      failwith "Instantiation not supported in simulation"
    | Signal_tri     _ ->
      failwith "Tristates not supported in simulation"
  in

  let data_map = List.fold_left make_data_map Map.empty (inputs @ outputs @ wires @ regs @ mems @ logic) in
  let seq_data_map = List.fold_left make_data_map Map.empty regs in
  
  let find (signal : Signal) = Map.find signal.uid data_map in
  let find_seq (signal : Signal) = Map.find signal.uid seq_data_map in

  (* default update task *)
  let def = fun() -> () in
  (* default reset task *)
  let def_zero data w = (fun() -> d.set_zero data w) in

  (* create functions which map behavioural code to assigns from if's and switches.
     The functions when executed return an indication of whether the code was executed *)
  let rec behave_tasks tgt_data code = 
    let code = List.map (behave_task tgt_data) code in
    (fun () -> List.fold_left (fun b x -> x() || b) false code) 
  and behave_task tgt_data code = 
    match code with
    | B_if(cond, on_true, on_false) -> (
      let cond = find cond in
      let eval_true = behave_tasks tgt_data on_true in
      let eval_false = behave_tasks tgt_data on_false in
      (fun () -> 
        if d.to_int cond <> 0 
        then eval_true()
        else eval_false()
      ))
    | B_switch(cond, cases) -> (
      let width_of_cond = width cond in
      let data_of_cond = find cond in
      (* map of case index constants to the tasks to execute. *)
      let case_tasks_map = List.fold_left (fun map (cond_match, exprs) -> 
        let data_of_cond_match = d.create (width cond_match) in
        d.set_const data_of_cond_match (string_of_const cond_match);
        let code_of_case = behave_tasks tgt_data exprs in
        Map.add data_of_cond_match code_of_case map
      ) Map.empty cases in
      (fun () -> 
        try 
          let t = Map.find data_of_cond case_tasks_map in
          t()
        with _ -> false 
      ))
    | B_assign(B_assign_tgt(_,_,_,_,_), expr) -> (
      let wid = width expr in
      let expr = find expr in
      (fun () -> d.copy tgt_data expr wid; true))
  in

  let compile (signal : Signal) = 
    match signal.signal with
    | Signal_empty 
    | Signal_const    (_) -> 
      def
      
    | Signal_binop    (a,w,op,s0,s1) -> 
      let data = find signal in
      let d0 = find s0 in
      let d1 = find s1 in
      let w0 = width s0 in
      let w1 = width s1 in
      (fun () -> d.binop op data d0 d1 w0 w1; d.mask data w)
      
    | Signal_unop     (a,w,op,s) -> 
      let data = find signal in
      let a = find s in
      (fun () -> d.unop op data a w; d.mask data w)
      
    | Signal_wire     (a,w,n,data) -> 
      let q = find signal in
      if !data = empty then 
        (fun () -> d.mask q w)  (* mask wires which are (or might be) inputs so the user cant overflow numbers *)
      else
        let data = find !data in
        (fun () -> d.copy q data w)
        
    | Signal_mux      (a,w,sel,dlist) -> 
      let len = List.length dlist in
      let data = find signal in
      let sel = find sel in
      let opts = List.mapi (fun i x -> i, find x) dlist in
      let opts_map = List.fold_left (fun map (i,x) -> Map.add i x map) Map.empty opts in
      (fun () -> 
        let idx = (min (d.to_int sel) (len-1)) in
        let dmux = (Map.find idx opts_map) in
        d.copy data dmux w)
        
    | Signal_select   (a,hi,lo,s) -> 
      let data = find signal in
      let a = find s in
      (fun () -> d.sel data a hi lo; d.mask data (hi-lo+1))
      
    (* Registers must update in two parts.  First we copy the data to the seq_data_map.  
       Later we'll copy to the actual value (which is read by other tasks).
       This is because registers which read other registers are not scheduled.  
       This isnt so with memories as they are read asycnhronously (and are therefore sheduled) *)
      
    | Signal_reg      (a,w,clk,rst,rstval,ena,data) -> 
      let q = find_seq signal in
      let data = find data in
      let wid = width signal in
      if ena = empty then 
        (fun () -> d.copy q data wid; d.mask q wid)
      else
        let ena = find ena in 
        (fun () -> if d.to_int ena <> 0 then (d.copy q data wid; d.mask q wid))
        
    | Signal_mem      (a,dw,aw,size,clk,w,we,data,r) -> 
      (* memories are implemented with a dynamic map *)
      let q = find signal in
      let w = find w in
      let we = find we in
      let data = find data in
      let r = find r in
      let map = ref Map.empty in
      let none = d.create dw in
      (fun () -> 
        (* read process *)
        let r = d.to_int r in
        let el = 
          try Map.find r !map 
          with _ -> none in
        d.copy q el dw;
        (* write process *)
        if (d.to_int we) = 1 then (
          let w = d.to_int w in
          let el = 
            try Map.find w !map
            with _ -> 
              let el = d.create dw in
              map := Map.add w el !map;
              el
            in
          d.copy el data dw
        );
      )
      
    | Signal_behave   (a,w,b,dl) -> 
      let tgt_data = find signal in
      let task = behave_tasks tgt_data b in
      (fun () -> 
        if not (task()) 
        then failwith ("Error executing behavioural code - no assignment to " ^ signal.name ^ " was detected")
      )
      
    | Signal_inst     (a,n,m,g,io,i,o) ->
      failwith "Instantiation nodes not supported in simulation"
      
    | Signal_tri     _ ->
      failwith "Tristate nodes not supported in simulation"
      
  in
  
  let compile_seq (signal : Signal) = 
    match signal.signal with
    | Signal_reg      (_) 
    | Signal_mem      (_) -> 
      let q0 = find_seq signal in
      let q1 = find signal in
      let wid = width signal in
      (fun () -> d.copy q1 q0 wid)
    | _ -> failwith "Expecting register or memory"
  in

  let rec compile_seq_reset (signal : Signal) = 
    match signal.signal with
    | Signal_reg(a,w,clk,rst,rstval,ena,data) -> 
      let wid = width signal in
      let q = find signal in
      let q_s = find_seq signal in
      let rstval_data = if rstval = empty then d.create wid else find rstval in
      if rst = empty then def (* do nothing *)
      else if rstval = empty then 
        (fun () -> d.set_zero q wid; d.set_zero q_s wid) (* reset to zero *)
      else 
        (fun () -> d.copy q rstval_data wid; d.copy q_s rstval_data wid) (* reset with given value *)
    | _ -> failwith "Expecting a register"
  in

  let scheduled_tasks = List.map compile schedule in
  let input_tasks = List.map compile inputs in
  let output_tasks = List.map compile outputs in
  let seq_tasks = List.map compile regs in
  let seq_finalize_tasks = List.map compile_seq regs in
  let seq_resets = List.map compile_seq_reset regs in

  Simulator (
    (fun () -> List.iter (fun x -> x()) seq_resets),
    (fun () -> List.iter (fun x -> x()) (input_tasks @ scheduled_tasks @ output_tasks @ seq_tasks @ seq_finalize_tasks)),
    List.map (fun (s : Signal) -> s.name, s.width, find s) inputs, 
    List.map (fun (s : Signal) -> s.name, s.width, find s) 
      (List.filter (fun s -> (wire_connection s <> empty) && (wire_name s <> "")) wires),
    List.map (fun (s : Signal) -> s.name, s.width, find s) outputs
  )

(** Creates an integer based simulator *)
let create_int sim = create simDataInt sim 
(** Creates an unsigned integer based simulator *)
let create_uint32 sim = create simDataUInt32 sim 
(** Creates a 64 bit integer based simulator *)
let create_uint64 sim = create simDataUInt64 sim 
(** Creates an array based simulator *)
let create_array sim = create simDataArray sim 

(****************************************************************)
(****************************************************************)

(** Resets the simulator *)
let sim_reset = function Simulator(reset,_,_,_,_) -> reset()
(** Cycles the simulator *)
let sim_cycle = function Simulator(_,cycle,_,_,_) -> cycle()
(** Simulator input ports *)
let sim_inputs = function Simulator(_,_,inputs,_,_) -> inputs
(** Simulator named wires ports *)
let sim_named_wires = function Simulator(_,_,_,wires,_) -> wires
(** Simulator output ports *)
let sim_outputs = function Simulator(_,_,_,_,outputs) -> outputs

(** Finds a simulator port given a list of ports and a string *)
let find_port ports str = 
  try 
    List.find (fun (n,w,d) -> n = str) ports
  with
  | _ -> failwith ("Couldn't find port " ^ str)

(** Name of a simulator port *)
let port_name (p,_,_) = p
(** Width of a simulator port *)
let port_width (_,w,_) = w
(** Data for a simulator port *)
let port_data (_,_,d) = d

(** Finds a simulator input port given a string *)
let find_input sim str = find_port (sim_inputs sim) str
(** Finds a simulator output port given a string *)
let find_output sim str = find_port (sim_outputs sim) str
(** Finds a simulator named wire given a string *)
let find_named_wire sim str = find_port (sim_named_wires sim) str

(** Finds the data of a simulator input port given a string *)
let find_input_data sim str = port_data (find_input sim str)
(** Finds the data of a simulator output port given a string *)
let find_output_data sim str = port_data (find_output sim str)
(** Finds the data of a simulator named wire given a string *)
let find_named_wire_data sim str = port_data (find_named_wire sim str)

(** Finds the width of a simulator input port given a string *)
let find_input_width sim str = port_width (find_input sim str)
(** Finds the width of a simulator output port given a string *)
let find_output_width sim str = port_width (find_output sim str)
(** Finds the width of a simulator named wire given a string *)
let find_named_wire_width sim str = port_width (find_named_wire sim str)

(****************************************************************************)
(****************************************************************************)

(** Takes a simulation datatype and number of cycles and generates the arrays required to draw a waveform. *)
let generator dcfg sim cycles = 
  
  let inputs_d = List.map (fun (n,w,d) -> (n,w,d), dcfg.create_array w cycles) (sim_inputs sim) in
  let named_wires_d = List.map (fun (n,w,d) -> (n,w,d), dcfg.create_array w cycles) (sim_named_wires sim) in
  let outputs_d = List.map (fun (n,w,d) -> (n,w,d), dcfg.create_array w cycles) (sim_outputs sim) in

  let clock_cycle = ref 0 in

  Simulator (
    (fun () -> 
      clock_cycle := 0;
      sim_reset sim;
      List.iter (fun ((n,w,d),x) -> for i=0 to cycles-1 do dcfg.set_zero x.[i] w done) inputs_d;
      List.iter (fun ((n,w,d),x) -> for i=0 to cycles-1 do dcfg.set_zero x.[i] w done) named_wires_d;
      List.iter (fun ((n,w,d),x) -> for i=0 to cycles-1 do dcfg.set_zero x.[i] w done) outputs_d;
    ),
    (fun () -> 
      if !clock_cycle >= cycles then failwith "Exceeded maximum simulation length";
      sim_cycle sim;
      (* record inputs after the cycle as they may have been (properly) truncated *)
      List.iter (fun ((n,w,d),a) -> dcfg.copy a.[!clock_cycle] d w) inputs_d;
      List.iter (fun ((n,w,d),a) -> dcfg.copy a.[!clock_cycle] d w) named_wires_d;
      List.iter (fun ((n,w,d),a) -> dcfg.copy a.[!clock_cycle] d w) outputs_d;
      clock_cycle := !clock_cycle + 1; 
    ),
    sim_inputs sim, sim_named_wires sim, sim_outputs sim
  ),
  SimDataGen (
    (List.map (fun ((n,w,_),a) -> (n,w,a)) inputs_d),
    (List.map (fun ((n,w,_),a) -> (n,w,a)) named_wires_d), 
    (List.map (fun ((n,w,_),a) -> (n,w,a)) outputs_d) 
  )

(** generator for int simulations *)
let generator_int = generator simDataInt
(** generator for uint32 simulations *)
let generator_uint32 = generator simDataUInt32
(** generator for uint64 simulations *)
let generator_uint64 = generator simDataUInt64
(** generator for array simulations *)
let generator_array = generator simDataArray

/// Takes two simulator objects and returns a single simulator object.  
/// The inputs from sim0 and copied to the inputs of sim1, and the wire and
/// output values of the simulators are compared.  Differences are recorded
/// by incrementing the reference variable num_errors
let create_dual_sim (dcfg : 'a sim_data_t) sim0 sim1 num_errors = 

  let (Simulator (reset_0, cycle_0, in_0, wire_0, out_0)) = sim0 in
  let (Simulator (reset_1, cycle_1, in_1, wire_1, out_1)) = sim1 in

  let task_fn fn l x = 
    try
      let name0, bits0, data0 = x in
      let name1, bits1, data1 = List.find (fun (name1,_,_) -> name0 = name1) l in
      (* return a function which copies x to y *)
      Some (fun () -> fn data1 data0 bits0)
    with _ ->
      None
  in
  
  (* look up x in l and copy *)
  let copy = task_fn dcfg.copy in
  (* look up x in l and check they are the same *)
  let check = task_fn (fun d0 d1 b -> if not (dcfg.eq d0 d1 b) then num_errors := !num_errors + 1) in

  (* create tasks which copy inputs and check outputs *)
  let make_tasks fn list0 list1 = 
    let tasks = 
      List.map (function Some x -> x | _ -> failwith "Unexpected")
        (List.filter ((<>) None)
          (List.map (fn list1) list0)) in
    (fun () -> List.iter (fun x -> x()) tasks)
  in
  
  let copy_tasks = make_tasks copy (sim_inputs sim0) (sim_inputs sim1) in
  let check_tasks = 
    let t0 = make_tasks check (sim_named_wires sim0) (sim_named_wires sim1) in
    let t1 = make_tasks check (sim_outputs sim0) (sim_outputs sim1) in
    (fun () ->
      t0();
      t1()
    ) 
  in
  let compare_name (n0,_,_) (n1,_,_) = compare n0 n1 in
  
  Simulator (
    (* reset *)
    (fun () -> reset_0(); reset_1()),
    (* cycle *)
    (fun () -> 
      (* copy sim0 inputs to sim1 inputs *)
      copy_tasks();
      (* sim cycle *)
      cycle_0();
      cycle_1();
      (* check the sim outputs and wires *)
      check_tasks()
    ),
    (* ports *)
    in_0,
    List.sort compare_name (wire_0 @ wire_1),
    List.sort compare_name (out_0 @ out_1)
  )

(** creates a dual sim object for int simulations *)
let create_dual_sim_int = create_dual_sim simDataInt
(** creates a dual sim object for uint32 simulations *)
let create_dual_sim_uint32 = create_dual_sim simDataUInt32
(** creates a dual sim object for uint64 simulations *)
let create_dual_sim_uint64 = create_dual_sim simDataUInt64
(** creates a dual sim object for array simulations *)
let create_dual_sim_array = create_dual_sim simDataArray

(** Takes two simulators and creates a dual sim object and a data generator for waveforming *)
let dual_sim_generator (dcfg : 'a sim_data_t) sim0 sim1 cycles num_errors = 
  let sim = create_dual_sim dcfg sim0 sim1 num_errors in
  generator dcfg sim cycles 

(** dual sim generator for int simulations *)
let dual_sim_generator_int = dual_sim_generator simDataInt
(** dual sim generator for uint32 simulations *)
let dual_sim_generator_uint32 = dual_sim_generator simDataUInt32
(** dual sim generator for uint64 simulations *)
let dual_sim_generator_uint64 = dual_sim_generator simDataUInt64
(** dual sim generator for array simulations *)
let dual_sim_generator_array = dual_sim_generator simDataArray

(* **************************************************** *)
(* running external tools *)
(* this is used by the cosim and csim modules *)
(* **************************************************** *)

(** run an external (shell) command and wait for it to complete.  Optionally display its stdout. *)
let execute_command' show_output exe_name args =
  let p = new Process() in
  p.StartInfo.UseShellExecute <- false;
  p.StartInfo.RedirectStandardOutput <- true;
  p.StartInfo.FileName <- exe_name;
  p.StartInfo.Arguments <- args;
  let _ = p.Start() in
  let output = p.StandardOutput.ReadToEnd() in
  p.WaitForExit(); 
  if show_output then output_string stdout output
  else ()

(** run an external (shell) and wait for it to complete.  Dont display its stdout. *)
let execute_command = execute_command' false

(** run an external (shell) command and return the process handle without waiting *)
let run_command exe_name args = 
  let p = new Process() in
  p.StartInfo.UseShellExecute <- false;
  //p.StartInfo.RedirectStandardOutput <- true;
  p.StartInfo.FileName <- exe_name;
  p.StartInfo.Arguments <- args;
  let _ = p.Start() in
  p
