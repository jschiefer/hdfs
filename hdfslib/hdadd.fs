#light "off"
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

/// <P>Basic addition algorithms</P>
/// 
/// <P><A HREF="http://www.aoki.ecei.tohoku.ac.jp/arith/mg/algorithm.html">http://www.aoki.ecei.tohoku.ac.jp/arith/mg/algorithm.html</A></P>
/// 
/// <P><A HREF="http://tima-cmp.imag.fr/~guyot/Cours/Oparithm/english/Additi.htm">http://tima-cmp.imag.fr/~guyot/Cours/Oparithm/english/Additi.htm</A></P>

module DigitalLogic.Circuits.Add

open DigitalLogic.Numeric.Ops
open DigitalLogic.Signal
open DigitalLogic.Util

exception Adder_error of string

(*******************************************************************)
(*******************************************************************)

let check_same x y = DigitalLogic.Signal.check_same [x;y]

let rec map3 fn x y z = 
    match x with
    | [] -> []
    | _ -> (fn (List.head x) (List.head y) (List.head z)) :: map3 fn (List.tail x) (List.tail y) (List.tail z)
    
(*******************************************************************)
(*******************************************************************)

(** full adder (3 to 2 compressor) *)
let fa x y z = 
  let s = (x ^^^ y) ^^^ z in
  let c = (x &&& y) ||| (x &&& z) ||| (y &&& z) in
  c, s
  
(** half adder *)
let ha (x : Signal) y = 
  let s = x ^^^ y in
  let c = x &&& y in
  c, s

(** 1 bit subtractor *)
let fs (x : Signal) y z = 
  let s = (x ^^^ y) ^^^ z in
  let b = ( (~~~ x) &&& (y ||| z) ) ||| (x &&& y &&& z) in
  b, s

(** partial full adder *)
let pfa x y c = 
  let p = x ^^^ y in
  let g = x &&& y in
  let s = p ^^^ c in
  p, g, s

(** carry ripple adder *)
let carry_ripple_adder cin x y =
  check_same x y;
  let x = List.rev x.bits in
  let y = List.rev y.bits in
  let rec build x y sum carry = 
    if (List.length x) = 0 then carry :: sum
    else
      let c, s = fa (List.head x) (List.head y) carry in
      build (List.tail x) (List.tail y) (s::sum) c in
  let sum = build x y [] cin in
  concat sum

(*
let carry_lookahead_adder_block cin x y = 
    check_same x y;
    let len = width x in
    let x = rev (bits x) in
    let y = rev (bits y) in
    let c = map (fun i -> signal ("c" ^ (string_of_int i)) 1) (range len) in
    let pfa = map3 (fun x y c -> pfa x y c) x y c in
    
    let p = map (fun (p,_,_) -> p) pfa in
    let g = map (fun (_,g,_) -> g) pfa in
    let sum = map (fun (_,_,s) -> s) pfa in
    
    let rec cla c pfa =
        match pfa with
        | [] -> []
        | (p,g,s)::tl ->
            let c = g |: (p &: c) in
            c :: cla c tl in

    let cla = cla cin pfa in
    iter2 (fun c_wire carry -> c_wire <== carry) c (lselect (cin :: cla) 0 (len-1));
    
    let pg = reduce_1 (&:) p in
    let gg n = 
        let g = nth g n in
        let p = lselect p (n+1) (len-1) in
        reduce_1 (&:) (g :: p) in
    let gg = reduce_1 (|:) (map (fun n -> gg n) (range len)) in
    
    let carry_out = nth cla (len-1) in
    carry_out, rev sum, pg, gg

(* builds an n bit carry look ahead adder out of various carry look ahead sub blocks *)
let carry_lookahead_adder_sub sub_block_size x y = 
    check_same x y;
    let len = width x in
    let num_sub = len / sub_block_size in
    if (num_sub*sub_block_size) <> len then
        failwith "width of operands must be a multiple of the sub block size";
    let li = range len in
    let c = map (fun i -> signal ("cb" ^ (string_of_int i)) 1) li in
    let sub_sums = map (fun i -> 
        let sx = select x ((sub_block_size*(i+1))-1) (sub_block_size*i) in
        let sy = select y ((sub_block_size*(i+1))-1) (sub_block_size*i) in
        carry_lookahead_adder_block gnd sx sy) li in
    let rec cla c pfa =
        match pfa with
        | [] -> []
        | (co,sum,p,g)::tl ->
            let c = g |: (p &: c) in
            c :: cla c tl in
    let cla = cla gnd sub_sums in
    iter2 (fun c_wire carry -> c_wire <== carry) c (lselect (gnd :: cla) 0 (len-1));
    
    reduce_1 (++) (rev (map (fun (_,sum,_,_) -> Design.concat sum) sub_sums))
*)

(** Carry look ahead adder *)
let carry_lookahead_adder cin x y = 
    check_same x y;
    let len = x.width in
    let x = List.rev x.bits in
    let y = List.rev y.bits in
    let c = [ for _ in { 0 .. len - 1} -> wire 1 ] in
    let pfa = map3 (fun x y c -> pfa x y c) x y c in
    let rec cla c pfa =
        match pfa with
        | [] -> []
        | (p,g,s)::tl ->
            let c = g |~ (p &~ c) in
            c :: cla c tl in
    let cla = cla cin pfa in
    List.iter2 (fun c_wire carry -> c_wire <== carry) c (lselect (cin :: cla) 0 (len-1));
    let sum = List.map (fun (p,g,s) -> s) pfa in
    let carry_out = List.nth cla (len-1) in
    concat (carry_out :: (List.rev sum))

(* TODO: carry_save_adder, ... *)


