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

namespace DigitalLogic.Numeric

open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.LanguagePrimitives
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Collections
open System.Collections.Generic
open Microsoft.FSharp.Math

module Ops = begin

  (** Signed or unsigned designation. *)
  type SignDesignator = Signed | Unsigned      

  (* This is here simply because it is a common module to open *)
  let rand = 
    let rand = new System.Random(1) in
    (fun range_lo range_hi -> int (rand.Next(int32 range_lo, int32 (range_hi+1))))

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  let inline ( *+ ) (x: ^a) (y: ^b) : ^c = (^a: (static member ( *+ ) : ^a * ^b -> ^c) (x,y))
  let inline (>>+) (x: ^a) (y: ^b) : ^c = (^a: (static member (>>+) : ^a * ^b -> ^c) (x,y))
  let inline (>>~) (x: ^a) (y: ^b) : ^c = (^a: (static member (>>~) : ^a * ^b -> ^c) (x,y))
  let inline (<<~) (x: ^a) (y: ^b) : ^c = (^a: (static member (<<~) : ^a * ^b -> ^c) (x,y))
  let inline (&~) (x: ^a) (y: ^b) : ^c = (^a: (static member (&~) : ^a * ^b -> ^c) (x,y))
  let inline (|~) (x: ^a) (y: ^b) : ^c = (^a: (static member (|~) : ^a * ^b -> ^c) (x,y))
  let inline (^~) (x: ^a) (y: ^b) : ^c = (^a: (static member (^~) : ^a * ^b -> ^c) (x,y))
  let inline (++) (x: ^a) (y: ^b) : ^c = (^a: (static member (++) : ^a * ^b -> ^c) (x,y))

  let inline (==~) (x: ^a) (y: ^b) : ^c = (^a: (static member (==~) : ^a * ^b -> ^c) (x,y))
  let inline (/=~) (x: ^a) (y: ^b) : ^c = (^a: (static member (/=~) : ^a * ^b -> ^c) (x,y))
  let inline (<~) (x: ^a) (y: ^b) : ^c = (^a: (static member (<~) : ^a * ^b -> ^c) (x,y))
  let inline (>~) (x: ^a) (y: ^b) : ^c = (^a: (static member (>~) : ^a * ^b -> ^c) (x,y))
  let inline (<=~) (x: ^a) (y: ^b) : ^c = (^a: (static member (<=~) : ^a * ^b -> ^c) (x,y))
  let inline (>=~) (x: ^a) (y: ^b) : ^c = (^a: (static member (>=~) : ^a * ^b -> ^c) (x,y))
  let inline (<+) (x: ^a) (y: ^b) : ^c = (^a: (static member (<+) : ^a * ^b -> ^c) (x,y))
  let inline (>+) (x: ^a) (y: ^b) : ^c = (^a: (static member (>+) : ^a * ^b -> ^c) (x,y))
  let inline (<=+) (x: ^a) (y: ^b) : ^c = (^a: (static member (<=+) : ^a * ^b -> ^c) (x,y))
  let inline (>=+) (x: ^a) (y: ^b) : ^c = (^a: (static member (>=+) : ^a * ^b -> ^c) (x,y))

  let inline ($==) (x: ^a) (y: ^b) : ^c = (^a: (static member ($==) : ^a * ^b -> ^c) (x,y))
  let inline (<==) (x: ^a) (y: ^b) : ^c = (^a: (static member (<==) : ^a * ^b -> ^c) (x,y))

  let inline (--) (x: ^a) (y: ^b) : ^c = (^a: (static member (--) : ^a * ^b -> ^c) (x,y))

end

open Ops

/// Functions for converting between numbers in various formats (ints, big ints, strings etc).
module Conversions = begin

  (** Integer to hex character (0..15) *)
  let int_of_hex_char c = 
      match c with
      | '0' -> 0
      | '1' -> 1
      | '2' -> 2
      | '3' -> 3
      | '4' -> 4
      | '5' -> 5
      | '6' -> 6
      | '7' -> 7
      | '8' -> 8
      | '9' -> 9
      | 'a' | 'A' -> 10
      | 'b' | 'B' -> 11
      | 'c' | 'C' -> 12
      | 'd' | 'D' -> 13
      | 'e' | 'E' -> 14
      | 'f' | 'F' -> 15
      | _ -> failwith "Invalid hex char"

  (** 0 or 1 -> '0' or '1' *)
  let int_of_bin_char = function
    | '0' -> 0
    | '1' -> 1
    | _ -> raise (failwith ("int_of_bin_char: Invalid binary character encountered"))

  (** Convert an integer to a binary string *)
  let bin_str_of_int l v = 
    let rec f bit vl = 
      if bit = l then ""
      else 
       (f (bit+1) (vl >>> 1) ^ (if (vl &&& 1) = 1 then "1" else "0")) in
    f 0 v 

  (** Convert a hexidecimal string to an integer *)
  let int_of_hex_str (s:string) = 
    let len = s.Length in
    let v = ref 0 in
    for i = 0 to (len-1) do
      v := (!v <<< 4) ||| (int_of_hex_char s.[i])
    done;
    !v
    
  (** Convert a binary string to an integer *)
  let int_of_bin_str (s:string) = 
    let len = s.Length in
    let v = ref 0 in
    for i = 0 to (len-1) do
      v := (!v <<< 1) ||| (int_of_bin_char s.[i])
    done;
    !v


  (** Convert a string in hexadecimal notation to a binary string. *)
  (** If the hex string is shorter than the required width, and the value *)
  (** is signed, the result is sign extended. *)
  let bin_str_of_hex_str sign width (hex:string) =
    let len = hex.Length in
    let len4 = len * 4 in
    let rec make_string i =
      if i = 0 then ""
      else (make_string (i-1)) ^ (bin_str_of_int 4 (int_of_hex_char hex.[i-1])) in
    let result = make_string len in 
    if width < len4 then
      (* String.sub result (len4-width) (width) *)
      result.[len4-width .. len4-1]
    else
      (* (String.make (width - len4) (if sign = Signed then result.[0] else '0')) ^ result *)
      System.String((if sign = Signed then result.[0] else '0'), width-len4) ^ result

  let rec hex_str_of_bin_str sign (s:string) = 
    let hex_of_bin s = 
      match s with
      | "0000" -> "0"
      | "0001" -> "1"
      | "0010" -> "2"
      | "0011" -> "3"
      | "0100" -> "4"
      | "0101" -> "5"
      | "0110" -> "6"
      | "0111" -> "7"
      | "1000" -> "8"
      | "1001" -> "9"
      | "1010" -> "a"
      | "1011" -> "b"
      | "1100" -> "c"
      | "1101" -> "d"
      | "1110" -> "e"
      | "1111" -> "f"
      | _ -> failwith "Invalid string"
    in
    let len = String.length s in
    match len with
    | 0 -> failwith "Invalid string"
    | 1 | 2 | 3 -> hex_of_bin ((if sign = Signed then String.make (4-len) s.[0] else String.make (4-len) '0') ^ s)
    | 4 -> hex_of_bin s
    | _ -> hex_str_of_bin_str sign (String.sub s 0 (len-4)) ^ hex_of_bin (String.sub s (len-4) 4)

  (** Converts a big int to a binary string *)
  let bin_str_of_big_int width b = 

    let b = ref b in
    let one = BigInt.One in
    let two = BigInt(2) in

    let str = System.String('0', width) in
    let len = str.Length in

    let rec make_string i = 
      if i = 0 then ""
      else (
        let q,m = BigInt.DivMod(!b, two) in
        b := q;
        (make_string (i-1)) ^ (if m = one then "1" else "0")
      ) in
    make_string len 

  (** Converts a decimal number, encoded in a string, to a binary string *)
  let bin_str_of_dec_str width d =
      bin_str_of_big_int width (BigInt.Parse d)

  (** Converts an array of integers to a binary string *)
  let bin_str_of_int_array l (a:int[]) = 
    let rec make_string i = 
      if i = l then ""
      else
        (make_string (i+1)) ^ (if (a.[i / 32] &&& (1 <<< (i % 32))) = 0 then "0" else "1") in
    make_string 0

  let bin_str_of_uint32_array l (a:uint32[]) = 
    let rec make_string i = 
      if i = l then ""
      else
        (make_string (i+1)) ^ (if (a.[i / 32] &&& (1ul <<< (i % 32))) = 0ul then "0" else "1") in
    make_string 0

  (* *********************************************************************************** *)
  (* Conversion to/from arrays of integers.  The
     arrays are passed as arguments.  The intent is that they preserve the rule
     that unused bits are kept at zero.  *)
  (* *********************************************************************************** *)

  (** Internal *)
  let mask32 l =
    let md = l % 32 in
    let md = if md = 0 then 32 else md in
    int (0xFFFFFFFFul >>> (32-md)) 

  (** ocaml int to array *)
  let array_of_int a l v = 
    let words = (l+31) / 32 in
    let mask = mask32 l in
    let l = ref l in
    (* clear array to start *)
    for i = 1 to (Array.length a) - 1 do
      a.[i] <- 0;
    done;
    (* copy data *)
    a.[0] <- v;
    for i = 1 to words-1 do
      a.[i] <- if v < 0 then -1 else 0;
    done;
    a.[words-1] <- a.[words-1] &&& mask

  (** Convert big_int to array *)
  let array_of_big_int a l v =
    let words = (l + 31) / 32 in
    let b = ref v in
    let mask = mask32 l in
    let pow16 = BigInt(65536) in
    (* clear array to start *)
    for i = 1 to (Array.length a) - 1 do
      a.[i] <- 0ul;
    done;
    (* get 32 bits from big_int *)
    let get_word b = 
      let q,m = BigInt.DivMod(!b, pow16) in
      let lo = uint32 (int m) in
      let q,m = BigInt.DivMod(q, pow16) in
      let hi = uint32 (int m) in
      b := q;
      lo ||| (hi <<< 16) in
    (* copy data *)
    for i = 0 to words-2 do
      a.[i] <- get_word b;
    done;
    a.[words-1] <- (get_word b) &&& (uint32 mask)

  (** Convert decimal string to array *)
  let array_of_dec_str a l d = 
    array_of_big_int a l (BigInt.Parse d)

  (** Convert binary string to array *)
  let uint32_array_of_bin_str a l (str:string) = 
    let words = (l + 31) / 32 in
    for i = 0 to (Array.length a) - 1 do
      a.[i] <- 0ul;
    done;
    for i = 0 to l-1 do
      if str.[l-i-1] = '1' then 
        a.[i / 32] <- a.[i / 32] ||| (1ul <<< (i % 32));
    done

  (** Convert hex string to array *)
  let array_of_hex_str sign a l str = 
    uint32_array_of_bin_str a l (bin_str_of_hex_str sign l str)

  (** Convert binary string to array *)
  let int_array_of_bin_str (a:int array) l (str:string) = 
    let words = (l + 31) / 32 in
    for i = 0 to (Array.length a) - 1 do
      a.[i] <- 0;
    done;
    for i = 0 to l-1 do
      if str.[l-i-1] = '1' then 
        a.[i / 32] <- a.[i / 32] ||| (1 <<< (i % 32));
    done

  (** Convert array to big_int *)
  let big_int_of_array sign (a:int[]) l = 
    let topbit = a.[(l-1) / 32] &&& (1 <<< ((l-1) % 32)) in
    let sign = (sign = Signed) && (topbit <> 0) in
    let b = ref BigInt.Zero in
    let p32 = BigInt.Pow(BigInt 2, BigInt 32) in
    let p16 = BigInt.Pow(BigInt 2, BigInt 16) in
    let p = ref BigInt.One in
    let words = (l + 31) / 32 in
    (* convert int to (positive) big_int *)
    let lsr' a s = int ((uint32 a) >>> s) in
    let get_word i = 
      if i > 0 then
        BigInt i
      else
        (BigInt (i &&& 65535)) + ((BigInt (lsr' i 16)) * p16) in
    (* create the unsigned value *)
    for i = 0 to words-1 do
      let v = a.[i] in
      let v = if i = (words-1) then v &&& (mask32 l) else v in
      let v = get_word a.[i] in
      b := !b + (v * !p);
      p := !p * p32;
    done;
    (* make negative if required *)
    if sign then
      !b - BigInt.Pow(BigInt 2, BigInt l)
    else
      !b
end

(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)

open Conversions

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IntBits = begin

  type IntBits = { bi_bits : int list }
    with 
    
      (* These are private functions. *)
      member x.ibits = x.bi_bits
      member x.ibits_msb = x.bi_bits
      member x.ibits_lsb = List.rev x.bi_bits

      (* These are public functions. *)
      member x.bits = List.map (fun x -> { bi_bits = [x] } ) x.bi_bits
      member x.bits_msb = x.bits
      member x.bits_lsb = List.rev x.bits

      member x.width = List.length x.ibits
      
    end

  let of_int_bits bits = { bi_bits = bits }

  let width (s:IntBits) = s.width

  let empty = of_int_bits []
  let gnd = of_int_bits [ 0 ]
  let vdd = of_int_bits [ 1 ]
  let constb b = of_int_bits b
  
  let select (s:IntBits) hi lo = 
    if lo > hi then failwith ("Invalid select range: " ^ (string hi) ^ " " ^ (string lo));
    if lo < 0 then failwith "Select lo is negative";
    if hi >= s.width then failwith "Select hi greater than width of target";
    if hi = s.width - 1 && lo = 0 then s
    else
      let rec sel i s = 
        match s with
        | [] -> []
        | hd::tl ->
          if i > hi then []
          else if i >= lo then hd :: sel (i+1) tl
          else sel (i+1) tl
      in
      of_int_bits (List.rev (sel 0 s.ibits_lsb))

  let bit s n = select s n n 

  let cat (a:IntBits) (b : IntBits) = of_int_bits (a.ibits @ b.ibits)
  let rec concat s = of_int_bits (List.concat (List.map (fun (b:IntBits) -> b.ibits) s))

  let concat_msb s = concat s
  let concat_lsb s = concat (List.rev s)

  let split s = 
    let w = width s in
    if w = 1 then s, empty
    else select s (w-1) (w/2), select s ((w/2)-1) 0

  let rec repeat s n = 
    if n = 0 then empty 
    else if n = 1 then s
    else if (n%2) = 1 then cat s (repeat (cat s s) (n/2))
    else repeat (cat s s) (n/2)
    
  let zero n = repeat gnd n
  let ones n = repeat vdd n
  let one n = 
    match n with
    | 1 -> vdd
    | _ -> cat (zero (n-1)) vdd

  let msb s = bit s (s.width-1)
  let msbs s = select s (s.width-1) 1
  let lsb s = bit s 0
  let lsbs s = select s (s.width-2) 0
  let se s = concat [ msb s; s ]
  let ue s = concat [ gnd; s ]

  let reduce op s = 
    match List.length s with
    | 0 | 1 -> failwith ("Cant reduce 0 or 1 bits")
    | _ -> List.fold_left (fun acc x -> op acc x) (List.hd s) (List.tl s)

  let reduce_1 op s = 
    match List.length s with
    | 1 -> List.hd s 
    | _ -> reduce op s

  let sll (a:IntBits) shift = 
    if shift < 0 then failwith ("Expecting positive shift amount, got " ^ string shift ^ ".");
    if shift = 0 then a
    else if shift >= a.width then zero a.width
    else cat (select a (a.width - 1 - shift) 0) (zero shift)
      
  let srl (a:IntBits) shift = 
    if shift < 0 then failwith ("Expecting positive shift amount, got " ^ string shift ^ ".");
    if shift = 0 then a
    else if shift >= a.width then zero a.width
    else cat (zero shift) (select a (a.width - 1) shift)

  let sra (a:IntBits) shift = 
    if shift < 0 then failwith ("Expecting positive shift amount, got " ^ string shift ^ ".");
    if shift = 0 then a
    else if shift >= a.width then repeat (msb a) a.width
    else cat (repeat (msb a) shift) (select a (a.width - 1) shift)

  let add (a : IntBits) (b : IntBits) = 
    let fa x y z = (x &&& y) ||| (x &&& z) ||| (y &&& z), x ^^^ y ^^^ z in
    of_int_bits (fst (List.fold_left2 (fun (res,carry) a b -> let carry, sum = fa a b carry in sum::res, carry) ([],0) a.ibits_lsb b.ibits_lsb))

  let sub (a : IntBits) (b : IntBits) = 
    let fs x y z = ((~~~ x) &&& (y ||| z)) ||| (x &&& y &&& z), (x ^^^ y) ^^^ z in
    of_int_bits (fst (List.fold_left2 (fun (res,carry) a b -> let carry, sum = fs a b carry in sum::res, carry) ([],0) a.ibits_lsb b.ibits_lsb))

  let negate (a : IntBits) = sub (zero (width a)) a

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  let band (a : IntBits) (b : IntBits) =
    of_int_bits (List.map2 (&&&) a.ibits b.ibits)

  let bor (a : IntBits) (b : IntBits) =
    of_int_bits (List.map2 (|||) a.ibits b.ibits)

  let bxor (a : IntBits) (b : IntBits) =
    of_int_bits (List.map2 (^^^) a.ibits b.ibits)

  let bnot (a : IntBits) =
    of_int_bits (List.map (fun a -> (~~~ a) &&& 1) a.ibits)

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  let mulu (a : IntBits) (b : IntBits) = 
    let _,r = List.fold_left (fun (i,acc) b -> 
      let acc = cat gnd acc in
      let a = concat [ gnd ; a ; repeat gnd i ] in
      i+1, add acc (band a (repeat b (width a)))
    ) (0,(zero (width a))) b.bits_lsb in
    r
    
  let muls (a : IntBits) (b : IntBits) = 
    let last = (width b) - 1 in
    let _,r = List.fold_left (fun (i,acc) b -> 
      let acc = cat (msb acc) acc in
      let a = concat [ msb a; a ; repeat gnd i ] in
      i+1, (if i = last then sub else add) acc (band a (repeat b (width a)))
    ) (0,(zero (width a))) b.bits_lsb in
    r
    
  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  let eq (a : IntBits) (b : IntBits) =
    bnot (reduce_1 bor (bxor a b).bits)

  let neq (a : IntBits) (b : IntBits) = bnot (eq a b)

  let lsu (a : IntBits) (b : IntBits) =
    let r = List.fold_left2 (fun less a b -> 
      match less with 
      | 0 -> if a < b then 1 else if a > b then -1 else 0
      | _ -> less
    ) 0 a.ibits b.ibits in
    of_int_bits [ (if r = 1 then 1 else 0) ]

  let gtu (a : IntBits) (b : IntBits) = lsu b a
  
  let lsequ (a : IntBits) (b : IntBits) = bnot (gtu a b)
  
  let gtequ (a : IntBits) (b : IntBits) = bnot (lsu a b)

  let lss (a : IntBits) (b : IntBits) =
    if a.width = 1 && b.width = 1 
    then (band a (bnot b)) (* 00=0, 01=0, 10=1, 11=0 *) 
    else lsu (cat (bnot (msb a)) (lsbs a)) (cat (bnot (msb b)) (lsbs b))
    
  let gts (a : IntBits) (b : IntBits) = 
    if a.width = 1 && b.width = 1 
    then (band b (bnot a)) (* 00=0, 01=1, 10=0, 11=0 *) 
    else gtu (cat (bnot (msb a)) (lsbs a)) (cat (bnot (msb b)) (lsbs b))      
    
  let lseqs (a : IntBits) (b : IntBits) = 
    if a.width = 1 && b.width = 1 
    then (bnot (gts a b)) (* 00=1, 01=0, 10=1, 11=1 *) 
    else lsequ (cat (bnot (msb a)) (lsbs a)) (cat (bnot (msb b)) (lsbs b))      

  let gteqs (a : IntBits) (b : IntBits) = 
    if a.width = 1 && b.width = 1 
    then (bnot (lss a b)) (* 00=1, 01=1, 10=0, 11=1 *) 
    else gtequ (cat (bnot (msb a)) (lsbs a)) (cat (bnot (msb b)) (lsbs b))

  let rec of_int w b = 
    if w = 1 then of_int_bits [ if b = 0 then 0 else 1 ]
    else cat (of_int (w-1) (b>>>1)) (of_int_bits [ b &&& 1 ])
    
  let rec of_string (s:string) =
    let w = s.Length in
    if w = 0 then empty
    else 
      cat 
        (of_int_bits [ if s.[0] = '1' then 1 else if s.[0] = '0' then 0 else failwith "Invalid binary string" ]) 
        (of_string (String.sub s 1 (w-1))) 
        
  type IntBits 
    with

      //member s.Item with get((hi : int),(lo : int)) = select s hi lo

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


      static member (++) ((a:IntBits), (b:IntBits)) = cat a b

      static member (+) ((a:IntBits), (b:IntBits)) = add a b
      static member (-) ((a:IntBits), (b:IntBits)) = sub a b
      static member ( * ) ((a:IntBits), (b:IntBits)) = mulu a b
      static member ( *+ ) ((a:IntBits), (b:IntBits)) = muls a b
      
      static member (&&&) ((a:IntBits), (b:IntBits)) = band a b
      static member (|||) ((a:IntBits), (b:IntBits)) = bor a b
      static member (^^^) ((a:IntBits), (b:IntBits)) = bxor a b
      static member (~~~) (a:IntBits) = bnot a
      
      static member (<<<) ((a:IntBits), (b:int)) = sll a b
      static member (>>>) ((a:IntBits), (b:int)) = srl a b
      static member (>>+) ((a:IntBits), (b:int)) = sra a b

      static member (==~) ((a:IntBits), (b:IntBits)) = eq a b
      static member (/=~) ((a:IntBits), (b:IntBits)) = neq a b
      
      static member (<~) ((a:IntBits), (b:IntBits)) = lsu a b
      static member (<=~) ((a:IntBits), (b:IntBits)) = lsequ a b
      static member (>~) ((a:IntBits), (b:IntBits)) = gtu a b
      static member (>=~) ((a:IntBits), (b:IntBits)) = gtequ a b
      
      static member (<+) ((a:IntBits), (b:IntBits)) = lss a b
      static member (<=+) ((a:IntBits), (b:IntBits)) = lseqs  a b
      static member (>+) ((a:IntBits), (b:IntBits)) = gts a b
      static member (>=+) ((a:IntBits), (b:IntBits)) = gteqs a b

      member x.to_int = 
        let rec ti acc l = 
          match l with
          | [] -> acc
          | hd::tl -> ti ((acc <<< 1) + hd) tl
        in
        ti 0 x.ibits

      member x.to_string = 
        List.fold_left 
          (fun acc x -> acc ^ (if x = 1 then "1" else if x = 0 then "0" else failwith "Invalid binary value")) 
          "" x.ibits

      member x.to_int_bits = x.ibits

    end
    
end

open IntBits

(* This is a wrapper around IntBits implement *)
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StringBits = begin

  type StringBits = { bs_bits : string }
    with

      member x.width = x.bs_bits.Length
      member x.str = x.bs_bits
      
    end

  let of_string bits = { bs_bits = bits }

  let width (s:StringBits) = s.width

  let empty = of_string ""
  let gnd = of_string "0"
  let vdd = of_string "1"
  let constb b = of_string b

  let binop1 (op:IntBits->IntBits->IntBits) (a:StringBits) (b:StringBits) = of_string ((op (IntBits.of_string a.str) (IntBits.of_string b.str)).to_string)
  let binop2 (op:IntBits->int->IntBits) (a:StringBits) (b:int) = of_string (op (IntBits.of_string a.str) b).to_string
  let unop (op:IntBits->IntBits) (a:StringBits) = of_string (op (IntBits.of_string a.str)).to_string
  
  let select (s:StringBits) hi lo = of_string ((select (IntBits.of_string s.str) hi lo).to_string)
  let bit s n = select s n n 
  
  let cat = binop1 (++)
  let add = binop1 (+)
  let sub = binop1 (-)
  let negate = unop negate
  let mulu = binop1 ( * )
  let muls = binop1 ( *+ )
  let band = binop1 (&&&)
  let bor = binop1 (|||)
  let bxor = binop1 (^^^)
  let bnot = unop (~~~)
  let sll = binop2 (<<<)
  let srl = binop2 (>>>)
  let sra = binop2 (>>+)
  let eq = binop1 (==~)
  let neq = binop1 (/=~)
  let lsu = binop1 (<~)
  let lsequ = binop1 (<=~)
  let gtu = binop1 (>~)
  let gtequ = binop1 (>=~)
  let lss = binop1 (<+)
  let lseqs = binop1 (<=+)
  let gts = binop1 (>+)
  let gteqs = binop1 (>=+)

  let concat s = of_string (concat (List.map (fun (b:StringBits) -> IntBits.of_string b.str) s)).to_string
  let concat_msb s = concat s
  let concat_lsb s = concat (List.rev s)

  let split (s:StringBits) = let a,b = split (IntBits.of_string s.str) in IntBits.of_string a.to_string, b.to_string
  let repeat = binop2 repeat

  let zero n = repeat gnd n
  let ones n = repeat vdd n
  let one n = 
    match n with
    | 1 -> vdd
    | _ -> cat (zero (n-1)) vdd

  let msb s = bit s (s.width-1)
  let msbs s = select s (s.width-1) 1
  let lsb s = bit s 0
  let lsbs s = select s (s.width-2) 0
  let se s = concat [ msb s; s ]
  let ue s = concat [ gnd; s ]

  let reduce op s = of_string (reduce op (List.map (fun (s:StringBits) -> IntBits.of_string s.str) s)).to_string
  let reduce_1 op s = of_string (reduce_1 op (List.map (fun (s:StringBits) -> IntBits.of_string s.str) s)).to_string

  let of_int w i = of_string ((of_int w i).to_string)
  let of_int_bits i = of_string ((IntBits.of_int_bits i).to_string)

  type StringBits
    with
    
      //member s.Item with get((hi : int),(lo : int)) = select s hi lo

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


      static member (++) ((a:StringBits), (b:StringBits)) = cat a b

      static member (+) ((a:StringBits), (b:StringBits)) = add a b
      static member (-) ((a:StringBits), (b:StringBits)) = sub a b
      static member ( * ) ((a:StringBits), (b:StringBits)) = mulu a b
      static member ( *+ ) ((a:StringBits), (b:StringBits)) = muls a b
      
      static member (&&&) ((a:StringBits), (b:StringBits)) = band a b
      static member (|||) ((a:StringBits), (b:StringBits)) = bor a b
      static member (^^^) ((a:StringBits), (b:StringBits)) = bxor a b
      static member (~~~) (a:StringBits) = bnot a
      
      static member (<<<) ((a:StringBits), (b:int)) = sll a b
      static member (>>>) ((a:StringBits), (b:int)) = srl a b
      static member (>>+) ((a:StringBits), (b:int)) = sra a b

      static member (==~) ((a:StringBits), (b:StringBits)) = eq a b
      static member (/=~) ((a:StringBits), (b:StringBits)) = neq a b
      
      static member (<~) ((a:StringBits), (b:StringBits)) = lsu a b
      static member (<=~) ((a:StringBits), (b:StringBits)) = lsequ a b
      static member (>~) ((a:StringBits), (b:StringBits)) = gtu a b
      static member (>=~) ((a:StringBits), (b:StringBits)) = gtequ a b
      
      static member (<+) ((a:StringBits), (b:StringBits)) = lss a b
      static member (<=+) ((a:StringBits), (b:StringBits)) = lseqs  a b
      static member (>+) ((a:StringBits), (b:StringBits)) = gts a b
      static member (>=+) ((a:StringBits), (b:StringBits)) = gteqs a b

      member x.bits = List.map (fun (x:IntBits) -> of_string x.to_string ) (IntBits.of_string x.str).bits
      member x.bits_msb = x.bits
      member x.bits_lsb = List.rev x.bits

      member x.to_int = (IntBits.of_string x.str).to_int
      member x.to_string = x.str
      member x.to_int_bits = (IntBits.of_string x.str).to_int_bits
   
    end
    
end
  
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ArrayBits = begin

  type ArrayBits = 
    { 
      ab_data : uint32 array; 
      ab_width : int 
    }
    with
      member x.data = x.ab_data
      member x.width = x.ab_width
      member x.words = (x.width + 31) / 32
    end

  let mask bits = 
    if bits % 32 = 0 then 0xFFFFFFFFul
    else (~~~ (0xFFFFFFFFul <<< (bits % 32)))

  (** Create bit vector with w bits *)
  let make w = 
    {
      ab_data = Array.create ((w+31)/32) 0ul; 
      ab_width = w;
    }

  (* functions which return functions performing the desired operation on the arguments *)

  let f_copy (dest:ArrayBits) (src:ArrayBits) = 
    assert (dest.width = src.width);
    let words = src.words in
    let src = src.data in
    let dest = dest.data in
    if words = 1 then
      (fun () -> dest.[0] <- src.[0])
    else
      (fun () -> 
        for i=0 to words-1 do
          dest.[i] <- src.[i]
        done
      )

  let f_copy_ena (dest:ArrayBits) (ena:ArrayBits) (src:ArrayBits) = 
    assert (dest.width = src.width);
    assert (ena.width = 1); (* in actual fact could be up to 32 *)
    let words = src.words in
    let src = src.data in
    let dest = dest.data in
    let ena = ena.data in
    if words = 1 then
      (fun () -> if ena.[0] <> 0ul then dest.[0] <- src.[0])
    else
      (fun () -> 
        if ena.[0] <> 0ul then 
          for i=0 to words-1 do
            dest.[i] <- src.[i]
          done
      )

  let f_copy_mask (dest:ArrayBits) (src:ArrayBits) = 
    assert (dest.width = src.width);
    let words = src.words in
    let src = src.data in
    let mask = mask dest.width in
    let dest = dest.data in
    if words = 1 then
      (fun () -> dest.[0] <- src.[0] &&& mask)
    else
      (fun () -> 
        for i=0 to words-2 do
          dest.[i] <- src.[i]
        done;
        dest.[words-1] <- src.[words-1] &&& mask
      )

  let f_zero (a:ArrayBits) = 
    let words = a.words in
    let data = a.data in
    (fun () ->
      for i=0 to words-1 do
        data.[i] <- 0ul
      done
    )

  let f_mask (a:ArrayBits) = (* masks the top word *)
    let words = a.words in
    let bits = a.width % 32 in
    (fun () -> 
      if bits <> 0 then 
        a.data.[words-1] <- a.data.[words-1] &&& (~~~ (0xFFFFFFFFul <<< bits))
    )

  let f_select (tgt_data:ArrayBits) (src_data:ArrayBits) hi lo = 
    assert (tgt_data.width = (hi-lo+1));
    assert (src_data.width >= (hi-lo+1));
    let tgt_data = tgt_data.data in
    let src_data = src_data.data in
    let lo_word = lo / 32 in
    let hi_word = hi / 32 in
    let lo_bit = lo % 32 in
    let hi_bit = hi % 32 in
    let tgt_bits = hi - lo + 1 in
    let mask = mask tgt_bits in
    (* single bit select *)
    if tgt_bits = 1 then
      (fun () -> tgt_data.[0] <- (src_data.[lo_word] >>> lo_bit) &&& 1ul)
    (* select is completely within a single word *)
    else if hi_word = lo_word then
    begin
      if lo_bit = 0 then
        (* just need to mask *)
        (fun () -> tgt_data.[0] <- src_data.[lo_word] &&& mask)
      else
        (* shift and mask *)
        (fun () -> tgt_data.[0] <- (src_data.[lo_word] >>> lo_bit) &&& mask)
    end
    (* multiple words, but no need to shift *)
    else if lo_bit = 0 then 
      let words = (hi - lo + 32) / 32 in
      (fun () -> 
        for i = 0 to words - 1 do
          tgt_data.[i] <- src_data.[lo_word + i]
        done;
        tgt_data.[words - 1] <- tgt_data.[words - 1] &&& mask
      )
    (* some other more complex case *)
    else 
      let bits = hi - lo + 1 in
      let msw = hi / 32 in
      let words = (bits + 31) / 32 in  
      let shift_left = 32 - lo_bit in
      (fun () ->
        (* merge and copy *)
        for i=0 to words-2 do  
          tgt_data.[i] <- (src_data.[lo_word + i] >>> lo_bit) ||| (src_data.[lo_word + i + 1] <<< shift_left)
        done;
        (* merge last word *)
        if (lo_word + words) >= (Array.length src_data) 
        then tgt_data.[words - 1] <-  (src_data.[lo_word + words - 1] >>> lo_bit)
        else tgt_data.[words - 1] <- ((src_data.[lo_word + words - 1] >>> lo_bit) ||| (src_data.[lo_word + words] <<< shift_left));
        (* mask data *)
        tgt_data.[words - 1] <- tgt_data.[words - 1] &&& mask
      )
  
  let f_cat (r:ArrayBits) (a:ArrayBits) (b:ArrayBits) = 
    assert (r.width = (a.width + b.width));
    let words_a = a.words in
    let words_b = b.words in
    let wa = a.width in
    let wb = b.width in
    let r = r.data in
    let a = a.data in
    let b = b.data in
    let tgt_mask = mask (wa+wb) in
    if wa = 0 then
      (fun () -> for i=0 to words_b - 1 do r.[i] <- b.[i] done)
    else if wb = 0 then
      (fun () -> for i=0 to words_a - 1 do r.[i] <- a.[i] done)
    else if wa + wb <= 32 then 
      (fun () -> r.[0] <- a.[0] <<< wb ||| b.[0])
    else if (wb % 32) = 0 then 
      (fun () ->
        for i=0 to words_b - 1 do r.[i] <- b.[i] done;
        for i=0 to words_a - 1 do r.[i+words_b] <- a.[i] done;
      ) 
    else (
      (* Do we need to mask the result? *)
      let words_b = (wb + 31) / 32 in
      let shift_left = wb % 32 in
      let shift_right = 32-shift_left in
      (fun () -> 
        let mutable xi = (words_b-1) in
        let mutable ai = 0 in
        let mutable bits_a = wa in
        (* Copy b (at least 1 word will be copied) *)
        for bi=0 to words_b-1 do r.[bi] <- b.[bi] done;
        while (bits_a > 0) do
          (* can fit the remaining bits into the last word *)
          if (bits_a <= shift_right) then (
              r.[xi] <- r.[xi] ||| (a.[ai] <<< shift_left);
              bits_a <- 0
          (* at least one complete word to copy, or remaining bits dont fit into just the current word *)
          ) else (
              r.[xi] <- r.[xi] ||| (a.[ai] <<< shift_left);
              r.[xi+1] <- a.[ai] >>> shift_right;
              bits_a <- bits_a - 32;
              xi <- xi + 1;
              ai <- ai + 1
          )
        done
      )
    )  
  
  let f_add (r:ArrayBits) (a:ArrayBits) (b:ArrayBits) = 
    assert (a.width = b.width);
    assert (r.width = b.width);
    let words = a.words in
    let w = a.width in
    let r = r.data in
    let a = a.data in
    let b = b.data in
    let tgt_mask = mask w in
    if w <= 32 then
      (fun () -> r.[0] <- (a.[0] + b.[0]) &&& tgt_mask)
    else
      (fun () ->
        let mutable tmp = 0UL in
        for i=0 to words - 1 do
          tmp <- (uint64 a.[i]) + (uint64 b.[i]) + tmp;
          r.[i] <- uint32 (tmp &&& 0xFFFFFFFFUL); (* is the mask necessary? *)
          tmp <- (tmp >>> 32) &&& 1UL;
        done;
        r.[words-1] <- r.[words - 1] &&& tgt_mask
      )  

  let f_sub (r:ArrayBits) (a:ArrayBits) (b:ArrayBits) = 
    assert (a.width = b.width);
    assert (r.width = b.width);
    let words = a.words in
    let w = a.width in
    let r = r.data in
    let a = a.data in
    let b = b.data in
    let tgt_mask = mask w in
    if w <= 32 then
      (fun () -> r.[0] <- (a.[0] - b.[0]) &&& tgt_mask)
    else
      (fun () ->
        let mutable tmp = 0UL in
        for i=0 to words - 1 do
          tmp <- (uint64 a.[i]) - (uint64 b.[i]) - tmp;
          r.[i] <- uint32 (tmp &&& 0xFFFFFFFFUL); (* is the mask necessary? *)
          tmp <- (tmp >>> 32) &&& 1UL;
        done;
        r.[words-1] <- r.[words - 1] &&& tgt_mask
      )  

  let f_band (r:ArrayBits) (a:ArrayBits) (b:ArrayBits) = 
    assert (a.width = b.width);
    assert (r.width = b.width);
    let words = a.words in
    let w = a.width in
    let r = r.data in
    let a = a.data in
    let b = b.data in
    if w <= 32 then (fun () -> r.[0] <- a.[0] &&& b.[0])
    else (fun () -> for i=0 to words - 1 do r.[i] <- a.[i] &&& b.[i] done)
    
  let f_bor (r:ArrayBits) (a:ArrayBits) (b:ArrayBits) = 
    assert (a.width = b.width);
    assert (r.width = b.width);
    let words = a.words in
    let w = a.width in
    let r = r.data in
    let a = a.data in
    let b = b.data in
    if w <= 32 then (fun () -> r.[0] <- a.[0] ||| b.[0])
    else (fun () -> for i=0 to words - 1 do r.[i] <- a.[i] ||| b.[i] done)
    
  let f_bxor (r:ArrayBits) (a:ArrayBits) (b:ArrayBits) = 
    assert (a.width = b.width);
    assert (r.width = b.width);
    let words = a.words in
    let w = a.width in
    let r = r.data in
    let a = a.data in
    let b = b.data in
    if w <= 32 then (fun () -> r.[0] <- a.[0] ^^^ b.[0])
    else (fun () -> for i=0 to words - 1 do r.[i] <- a.[i] ^^^ b.[i] done)
  
  let f_bnot (r:ArrayBits) (a:ArrayBits) = 
    assert (r.width = a.width);
    let words = a.words in
    let w = a.width in
    let r = r.data in
    let a = a.data in
    let tgt_mask = mask w in
    if w = 32 then
      (fun () -> r.[0] <- ~~~ a.[0])
    else if w < 32 then
      (fun () -> r.[0] <- (~~~ a.[0]) &&& tgt_mask)
    else
      (fun () -> 
        for i = 0 to words-1 do 
          r.[i] <- ~~~ a.[i] 
        done;
        r.[words-1] <- r.[words-1] &&& tgt_mask
      )
        
  let f_eq (r:ArrayBits) (a:ArrayBits) (b:ArrayBits) = 
    assert (a.width = b.width);
    assert (r.width = 1);
    let words = a.words in
    let w = a.width in
    let r = r.data in
    let a = a.data in
    let b = b.data in
    let rec eq i = 
      if i = words then 1ul
      else if a.[i] <> b.[i] then 0ul
      else eq (i+1)
    in
    if w <= 32 then (fun () -> r.[0] <- if a.[0] = b.[0] then 1ul else 0ul)
    else (fun () -> r.[0] <- eq 0)
        
  let f_lsu (r:ArrayBits) (a:ArrayBits) (b:ArrayBits) = 
    assert (a.width = b.width);
    assert (r.width = 1);
    let words = a.words in
    let w = a.width in
    let r = r.data in
    let a = a.data in
    let b = b.data in
    let rec comp i = 
      if i < 0 then false
      else if a.[i] < b.[i] then true
      else if a.[i] > b.[i] then false
      else comp (i-1) 
    in
    if w <= 32 then (fun () -> r.[0] <- if a.[0] < b.[0] then 1ul else 0ul)
    else (fun () -> r.[0] <- if comp (words-1) then 1ul else 0ul)  

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  let sign_bit (a:ArrayBits) = (* gets the sign bit *)
    let w = a.width - 1 in
    let word = w / 32 in
    let bit = w % 32 in
    if 0ul <> (a.data.[word] &&& (1ul <<< bit)) then true else false 

  let sign_extend (a:ArrayBits) = (* sign extends top word, if required (not 32 bits, is signed) *)
    let sb = sign_bit a in
    let w = a.width in
    if w % 32 <> 0 then (
      let word = w / 32 in
      let bit = w % 32 in
      let sign_extend a w = a ||| (0xFFFFFFFFul <<< w) in
      if sb then a.data.[word] <- sign_extend a.data.[word] bit
    );
    sb
  
  let mask_data (a:ArrayBits) = (* masks the top word *)
    let words = a.words in
    let bits = a.width % 32 in
    if bits <> 0 then
      a.data.[words-1] <- a.data.[words-1] &&& (~~~ (0xFFFFFFFFul <<< bits)) 
  
  let mul' words_r sign_a sign_b words_a words_b (r:uint32[]) (a:uint32[]) (b:uint32[]) = 
    let access (a:uint32[]) words sign i =
      if i >= words then (if sign then 0xFFFFFFFFul else 0ul)
      else a.[i] in
    let access_a = access a words_a sign_a in
    let access_b = access b words_b sign_b in
    let mask = 0xFFFFFFFFUL in
    for i = 0 to words_r - 1 do r.[i] <- 0ul done;
    for i = 0 to words_r - 1 do 
      let ib = ref 0 in
      for ia = i downto 0 do 
        let tmp = ref ((uint64 (access_a ia)) * (uint64 (access_b !ib))) in
        for ic = i to words_r - 1 do
          tmp := !tmp + (uint64 r.[ic]);
          r.[ic] <- uint32 (!tmp &&& mask);
          tmp := !tmp >>> 32;
        done;
        ib := !ib + 1;
      done;
    done

  let f_mulu (r:ArrayBits) (a:ArrayBits) (b:ArrayBits) = 
    assert (r.width = a.width + b.width);
    if r.width <= 32 then 
      (fun () -> r.data.[0] <- a.data.[0] * b.data.[0])
    else
      (fun () -> mul' r.words false false a.words b.words r.data a.data b.data)
      
  let f_muls (r:ArrayBits) (a:ArrayBits) (b:ArrayBits) = 
    assert (r.width = a.width + b.width);
    if r.width <= 32 then 
      let mask = mask r.width in
      let se w x = if x &&& (1ul<<<(w-1)) <> 0ul then x ||| (0xFFFFFFFFul <<< w) else x in
      (fun () -> r.data.[0] <- (se a.width a.data.[0] * se b.width b.data.[0]) &&& mask)
    else
      (fun () ->
        let sign_a = sign_extend a in (* add sign *)
        let sign_b = sign_extend b in
        mul' r.words sign_a sign_b a.words b.words r.data a.data b.data;
        if sign_a then mask_data a; (* restore *)
        if sign_b then mask_data b;
        mask_data r;
      )
  
  (* create the memory using an appropriate strategy based on size, and return a function for looking up data 
     If the memory size is less than array_max_size then the memory will be built using an array 
     Otherwise the memory is built with a map using sub-arrays of 1<<<log_block_array_size.  Lookups will dynamically generate the map.
   *)
  let f_mem log_array_max_size log_block_array_size d_bits a_bits = 
    assert (log_array_max_size < 32);
    assert (log_array_max_size >= log_block_array_size);
    let array_max_size = 1 <<< log_array_max_size in
    let block_array_size = 1 <<< log_block_array_size in
    if a_bits <= log_array_max_size then
      (* Build mem as an array *)
      let n_elem = 1 <<< a_bits in
      let a = Array.create n_elem (make d_bits) in
      for i=1 to n_elem-1 do
        a.[i] <- make d_bits
      done;
      let lookup (addr:ArrayBits) = a.[int addr.data.[0]] in 
      lookup
    else
      (* Build a map containing small array's of data.  Thus we look up the map and then access the array. *)
      (* XXXX - this strategy means it will be possible to access memory > n_elem (it will allow any address reachable with a_bits).  
         How can we (efficiently) perform a check to ensure this does not occur? *)
      let mask_block = (1ul <<< log_block_array_size) - 1ul in
      let map = ref Map.empty in
      let rec lookup (addr:ArrayBits) =
        let lo_bits = addr.data.[0] &&& mask_block in (* store low bits of address *)
        addr.data.[0] <- addr.data.[0] &&& (~~~mask_block); (* mask out low bits for look up *)
        match Map.tryfind addr !map with
        | None -> 
          (* Construct block *)
          let a = Array.create block_array_size (make d_bits) in
          for i = 0 to block_array_size - 1 do    
            a.[i] <- make d_bits
          done;
          (* Construct block address to use as key in the map *)
          let new_addr = make a_bits in
          (f_copy new_addr addr)();
          (* Add block to map indexed by the address *)
          map := Map.add new_addr a !map;
          (* restore low bits in address *)
          addr.data.[0] <- addr.data.[0] ||| lo_bits;
          (* call recursively to return actual data *)
          lookup addr
        | Some(x) ->
          (* restore low bits in address *)
          addr.data.[0] <- addr.data.[0] ||| lo_bits;
          let x = x.[int lo_bits] in
          (* return the data *)
          x
      in
      lookup

  let f_mux max_addr_bits a_bits (dlist:ArrayBits list) = 
    let num_elem = List.length dlist in
    let first_element = List.hd dlist in
    let d_bits = first_element.width in
    let last_element = (List.hd (List.rev dlist)) in
    assert (max_addr_bits < 32);
    if a_bits <= max_addr_bits then
      (* build an array *)
      let a = Array.create num_elem first_element in
      let rec build i (l:ArrayBits list) = 
        match l with
        | [] -> ()
        | hd :: tl -> 
          a.[i] <- hd;
          build (i+1) tl
      in
      build 0 dlist;
      let lookup (addr:ArrayBits) = 
        let idx = int addr.data.[0] in
        if idx < num_elem then a.[idx] 
        else last_element
      in
      lookup
    else
      (* build a map *)
      let zero = make a_bits in
      let one = make a_bits in
      one.data.[0] <- 1ul;
      let map,_ = List.fold_left (fun (map,cur_addr) el -> 
          let next_addr = make a_bits in
          (f_add next_addr cur_addr one)();
          Map.add cur_addr el map, next_addr
        ) (Map.empty, zero) dlist 
      in
      let lookup (addr:ArrayBits) = 
        match Map.tryfind addr map with
        | None -> last_element
        | Some(x) -> x
      in
      lookup

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)
  
  let of_string (bits:string) = 
    let len = bits.Length in
    let a = make len in
    uint32_array_of_bin_str a.data len bits;
    a
    
  let of_int_bits v = of_string (StringBits.of_int_bits v).to_string

  let of_int w v = of_string (StringBits.of_int w v).to_string

  let width (s:ArrayBits) = s.width

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  let empty = of_string ""          (* XXXX ???? *)
  let gnd = of_string "0"
  let vdd = of_string "1"
  let constb b = of_string b

  (* same size args *)
  let binop1 (op:ArrayBits->ArrayBits->ArrayBits->(unit->unit)) (a:ArrayBits) (b:ArrayBits) = 
    let r = make a.width in
    (op r a b)();
    r

  (* sum of arg widths *)
  let binop2 (op:ArrayBits->ArrayBits->ArrayBits->(unit->unit)) (a:ArrayBits) (b:ArrayBits) = 
    let r = make (a.width + b.width) in
    (op r a b)();
    r

  (* result width 1 *)
  let binop3 (op:ArrayBits->ArrayBits->ArrayBits->(unit->unit)) (a:ArrayBits) (b:ArrayBits) = 
    let r = make 1 in
    (op r a b)();
    r

  let unop (op:ArrayBits->ArrayBits->(unit->unit)) (a:ArrayBits) = 
    let r = make a.width in
    (op r a)();
    r
  
  let select (s:ArrayBits) hi lo = 
    let r = make (hi-lo+1) in
    (f_select r s hi lo) ();
    r

  let bnot = unop f_bnot

  let add = binop1 f_add
  let sub = binop1 f_sub
  let band = binop1 f_band
  let bor = binop1 f_bor
  let bxor = binop1 f_bxor
  
  let cat = binop2 f_cat
  let mulu = binop2 f_mulu
  let muls = binop2 f_muls
 
  let eq = binop3 f_eq
  let lsu = binop3 f_lsu

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  let rec concat s = List.fold_left (fun acc x -> cat x acc) empty s
  let concat_msb s = concat s
  let concat_lsb s = concat (List.rev s)

  let bit s n = select s n n 
  let msb s = bit s (s.width-1)
  let msbs s = select s (s.width-1) 1
  let lsb s = bit s 0
  let lsbs s = select s (s.width-2) 0
  let se s = concat [ msb s; s ]
  let ue s = concat [ gnd; s ]

  let rec repeat s n = 
    if n = 0 then empty 
    else if n = 1 then s
    else if (n%2) = 1 then cat s (repeat (cat s s) (n/2))
    else repeat (cat s s) (n/2)

  let zero n = make n
  let one n = let a = make n in a.data.[0] <- 1ul; a
  let ones n = 
    let a = make n in
    for i in 0 .. a.words - 1 do
      a.data.[i] <- 0xFFFFFFFFul
    done;
    a

  let negate (a : ArrayBits) = sub (zero (width a)) a

  let split s = 
    let w = width s in
    if w = 1 then s, empty
    else select s (w-1) (w/2), select s ((w/2)-1) 0

  let sll (a:ArrayBits) shift = 
    if shift < 0 then failwith ("Expecting positive shift amount, got " ^ string shift ^ ".");
    if shift = 0 then a
    else if shift >= a.width then zero a.width
    else cat (select a (a.width - 1 - shift) 0) (zero shift)
      
  let srl (a:ArrayBits) shift = 
    if shift < 0 then failwith ("Expecting positive shift amount, got " ^ string shift ^ ".");
    if shift = 0 then a
    else if shift >= a.width then zero a.width
    else cat (zero shift) (select a (a.width - 1) shift)

  let sra (a:ArrayBits) shift = 
    if shift < 0 then failwith ("Expecting positive shift amount, got " ^ string shift ^ ".");
    if shift = 0 then a
    else if shift >= a.width then repeat (msb a) a.width
    else cat (repeat (msb a) shift) (select a (a.width - 1) shift)

  let reduce op s = 
    match List.length s with
    | 0 | 1 -> failwith ("Cant reduce 0 or 1 bits")
    | _ -> List.fold_left (fun acc x -> op acc x) (List.hd s) (List.tl s)

  let reduce_1 op s = 
    match List.length s with
    | 1 -> List.hd s 
    | _ -> reduce op s

  let neq (a : ArrayBits) (b : ArrayBits) = bnot (eq a b)

  let gtu (a : ArrayBits) (b : ArrayBits) = lsu b a
  
  let lsequ (a : ArrayBits) (b : ArrayBits) = bnot (gtu a b)
  
  let gtequ (a : ArrayBits) (b : ArrayBits) = bnot (lsu a b)

  let lss (a : ArrayBits) (b : ArrayBits) =
    if a.width = 1 && b.width = 1 
    then (band a (bnot b)) (* 00=0, 01=0, 10=1, 11=0 *) 
    else lsu (cat (bnot (msb a)) (lsbs a)) (cat (bnot (msb b)) (lsbs b))
    
  let gts (a : ArrayBits) (b : ArrayBits) = 
    if a.width = 1 && b.width = 1 
    then (band b (bnot a)) (* 00=0, 01=1, 10=0, 11=0 *) 
    else gtu (cat (bnot (msb a)) (lsbs a)) (cat (bnot (msb b)) (lsbs b))      
    
  let lseqs (a : ArrayBits) (b : ArrayBits) = 
    if a.width = 1 && b.width = 1 
    then (bnot (gts a b)) (* 00=1, 01=0, 10=1, 11=1 *) 
    else lsequ (cat (bnot (msb a)) (lsbs a)) (cat (bnot (msb b)) (lsbs b))      

  let gteqs (a : ArrayBits) (b : ArrayBits) = 
    if a.width = 1 && b.width = 1 
    then (bnot (lss a b)) (* 00=1, 01=1, 10=0, 11=1 *) 
    else gtequ (cat (bnot (msb a)) (lsbs a)) (cat (bnot (msb b)) (lsbs b))

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  let copy (d:ArrayBits) = 
    let t = make d.width in
    (f_copy t d)();
    t

  let copy_ena (ena:ArrayBits) (d:ArrayBits) = 
    let t = make d.width in
    (f_copy_ena t ena d)();
    t

  let copy_mask (d:ArrayBits) = 
    let t = make d.width in
    (f_copy_mask t d)();
    t

  type ArrayBits 
    with

      //member s.Item with get((hi : int),(lo : int)) = select s hi lo

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


      static member (++) ((a:ArrayBits), (b:ArrayBits)) = cat a b

      static member (+) ((a:ArrayBits), (b:ArrayBits)) = add a b
      static member (-) ((a:ArrayBits), (b:ArrayBits)) = sub a b
      static member ( * ) ((a:ArrayBits), (b:ArrayBits)) = mulu a b
      static member ( *+ ) ((a:ArrayBits), (b:ArrayBits)) = muls a b
      
      static member (&&&) ((a:ArrayBits), (b:ArrayBits)) = band a b
      static member (|||) ((a:ArrayBits), (b:ArrayBits)) = bor a b
      static member (^^^) ((a:ArrayBits), (b:ArrayBits)) = bxor a b
      static member (~~~) (a:ArrayBits) = bnot a
      
      static member (<<<) ((a:ArrayBits), (b:int)) = sll a b
      static member (>>>) ((a:ArrayBits), (b:int)) = srl a b
      static member (>>+) ((a:ArrayBits), (b:int)) = sra a b

      static member (==~) ((a:ArrayBits), (b:ArrayBits)) = eq a b
      static member (/=~) ((a:ArrayBits), (b:ArrayBits)) = neq a b
      
      static member (<~) ((a:ArrayBits), (b:ArrayBits)) = lsu a b
      static member (<=~) ((a:ArrayBits), (b:ArrayBits)) = lsequ a b
      static member (>~) ((a:ArrayBits), (b:ArrayBits)) = gtu a b
      static member (>=~) ((a:ArrayBits), (b:ArrayBits)) = gtequ a b
      
      static member (<+) ((a:ArrayBits), (b:ArrayBits)) = lss a b
      static member (<=+) ((a:ArrayBits), (b:ArrayBits)) = lseqs  a b
      static member (>+) ((a:ArrayBits), (b:ArrayBits)) = gts a b
      static member (>=+) ((a:ArrayBits), (b:ArrayBits)) = gteqs a b

      member x.bits_lsb : ArrayBits list = [ for i in 0 .. x.width - 1 -> bit x i ]
      member x.bits_msb = List.rev x.bits
      member x.bits : ArrayBits list = x.bits_msb

      member x.to_uint = x.data.[0]
      member x.to_uint64 = (uint64 x.data.[0]) ||| ((uint64 x.data.[1]) <<< 32)
      member x.to_int = int x.data.[0]
      member x.to_string = bin_str_of_uint32_array x.width x.data
      member x.to_int_bits = (IntBits.of_string x.to_string).to_int_bits

      (* Properties to read/write the data arrays native .NET types and some special string based types.
      
        i   int     : sign extends on get and set
        u   int     : sign extends on set only
        y   SByte   : sign extends on get and set
        uy  Byte    : zero extends on get and set
        s   Int16   : sign extends on get and set
        us  UInt16  : zero extends on get and set
        l   Int32   : sign extends on get and set
        ul  UInt32  : zero extends on get and set
        L   Int64   : sign extends on get and set
        UL  UInt64  : zero extends on get and set
        
        i and u are special properies for working with f# ints:
        i : accesses the value as if it were signed.  Sign extension occurs where appropriate on get and set
        u : reads the value as if it were unsigned and writes it as if it were signed
        
        hs/hu : hex based strings
        a     : treats data a ArrayBits
      *)

      (* Clear the port *)
      member x.clear = 
        for i=0 to x.words - 1 do
          x.data.[i] <- 0ul
        done

      member x.sign_extend width = 
        assert (width > 0);
        let lo_word = (width-1) / 32 in
        let signed = (x.data.[0] &&& (1ul <<< (width - 1))) <> 0ul in
        let mask = if width = 32 then 0x0ul else 0xFFFFFFFFul <<< width in
        (* signed extend bottom word *)
        if signed then 
          x.data.[lo_word] <- x.data.[lo_word] ||| mask;        
          for i=lo_word+1 to x.words - 1 do
            x.data.[i] <- 0xFFFFFFFFul
          done
        
      /// int access property with signed get
      member x.i
        with get() = 
          let se v = if (x.width < 32) && ((v &&& (1 <<< (x.width-1)) <> 0)) then v ||| (0xFFFFFFFF <<< x.width) else v in
          se (int x.data.[0])
        and set(v : int) = 
          x.data.[0] <- uint32 v;
          x.sign_extend 32

      /// int access property with unsigned get (32 bit values will turn out signed though)
      member x.u
        with get() = 
          int x.data.[0]
        and set(v : int) = 
          x.data.[0] <- uint32 v;
          x.sign_extend 32

      /// sbyte access property      
      member x.y
        with get() = 
          sbyte (int x.data.[0])
        and set(v : sbyte) = 
          x.data.[0] <- uint32 v;
          x.sign_extend 32
        
      /// byte access property       
      member x.uy
        with get() = 
          let se v = if (x.width < 8) && ((v &&& (1uy <<< (x.width-1)) <> 0uy)) then v ||| (0xFFuy <<< x.width) else v in
          byte x.data.[0]
        and set(v : byte) = 
          x.clear;
          x.data.[0] <- uint32 v
        
      /// int16 access property      
      member x.s
        with get() = 
          let se v = if (x.width < 16) && ((v &&& (1s <<< (x.width-1)) <> 0s)) then v ||| (0xFFFFs <<< x.width) else v in
          se (int16 x.data.[0])
        and set(v : int16) = 
          x.data.[0] <- uint32 v;
          x.sign_extend 32
        
      /// uint16 access property     
      member x.us
        with get() = 
          uint16 x.data.[0]
        and set(v : uint16) = 
          x.clear;
          x.data.[0] <- uint32 v
        
      /// int32 access property      
      member x.l
        with get() = 
          let se v = if (x.width < 32) && ((v &&& (1l <<< (x.width-1)) <> 0l)) then v ||| (0xFFFFFFFFl <<< x.width) else v in
          se (int32 x.data.[0])
        and set(v : int32) = 
	  x.data.[0] <- uint32 v;
          x.sign_extend 32
        
      /// uint32 access property     
      member x.ul 
        with get() = 
          x.data.[0]
        and set(v : uint32) = 
          x.clear;
          x.data.[0] <- v

      /// uint64 access property     
      member x.UL
        with get() = 
          let w = x.width in
          let lo = uint64 x.data.[0] in
          let hi = if w > 32 then uint64 x.data.[1] else 0UL in 
          lo ||| (hi <<< 32)
        and set(v : uint64) = 
          let w = x.width in
          x.clear;
          x.data.[0] <- uint32 (v &&& 0xFFFFFFFFUL);
          if w > 32 then
            x.data.[1] <- uint32 (v >>> 32)

      /// int64 access property      
      member x.L
        with get() = 
          let se v = if (x.width < 64) && ((v &&& (1L <<< (x.width-1)) <> 0L)) then v ||| (0xFFFFFFFFFFFFFFFFL <<< x.width) else v in
          se (int64 x.UL)
        and set(v : int64) = 
          x.UL <- uint64 v;
          x.sign_extend 64

      // TODO: BigInt's (need to convert numeric based stuff to F# BigInt rather than Big_int so we can use the nice operator based API)

      // Using binary strings for access
      member x.b
        with get() = bin_str_of_uint32_array x.width x.data
        and  set(v : string) = Conversions.uint32_array_of_bin_str x.data x.width v

      // Using hex strings for access
      member x.hs
        with get() = Conversions.hex_str_of_bin_str Ops.Signed (bin_str_of_uint32_array x.width x.data)
        and  set(v : string) = Conversions.array_of_hex_str Ops.Signed x.data x.width v

      member x.hu
        with get() = Conversions.hex_str_of_bin_str Ops.Unsigned (bin_str_of_uint32_array x.width x.data)
        and  set(v : string) = Conversions.array_of_hex_str Ops.Unsigned x.data x.width v

      // Access as array bits
      member x.a
        with get() = x
        and set(v) = (f_copy x v)()

    end

end

(** Array which automatically resizes itself when an out of bounds index is written (what I thought the .net ResizeArray would do) *)
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ResizingArray = 
begin

  type 'a ResizingArray = 
    {
      mutable ra_data : 'a array;
      mutable ra_total : int;
      mutable ra_length : int;
      ra_nil : 'a;
    }
    with
      member x.length = x.ra_length
      member x.total = x.ra_total
      member x.nil = x.ra_nil
      member x.data = x.ra_data
    end

  let create n init = 
    {
      ra_data = Array.create n init;
      ra_total = n;
      ra_length = 0;
      ra_nil = init;
    }

  (* Resizing strategy - 
      * when an out of bounds index is read, the nil value is return.  Nil is the init value specified on construtction
      * when an out of bounds index is written, the array is resized to twice the size of the index.  Nill is written into all unused locations
   *)
  type 'a ResizingArray 
    with
      member x.Item 
        with get(idx:int) =
          if idx >= x.total then x.nil
          else x.ra_data.[idx]
        and set (idx:int) (v:'a) = 
          if idx >= x.total then (
            (* double array length *)
            x.ra_total <- idx * 2;
            let ra_data = Array.create x.ra_total x.nil in
            (* copy old data *)
            for i=0 to x.length - 1 do
              ra_data.[i] <- x.ra_data.[i]
            done;
            x.ra_data <- ra_data;
            (* set new data *)
            x.ra_data.[idx] <- v;
            x.ra_length <- max x.length (idx+1)
          ) else
            (* set new data *)
            x.ra_data.[idx] <- v;
            x.ra_length <- max x.length (idx+1)
    end
    
end


