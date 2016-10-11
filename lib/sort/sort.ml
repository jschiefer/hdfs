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

/// <P>Sorting networks.</P>
/// <ul>
/// <li>(>:) unsigned sort up</li>
/// <li>(<:) unsigned sort down</li>
/// <li>(>+) signed sort up</li>
/// <li>(<+) signed sort down</li>
/// </ul>
/// <P><A HREF="http://www.inf.fh-flensburg.de/lang/algorithmen/sortieren/">http://www.inf.fh-flensburg.de/lang/algorithmen/sortieren/</A></P>
module DigitalLogic.Circuits.Sort

open DigitalLogic.Numeric.Ops
open DigitalLogic.Signal
open DigitalLogic.Util

/// Bitonic sort
let bitonic (sort_fn : Signal->Signal->Signal) pipeline inputs vld =

    let asc = vdd in
    let dsc = gnd in

    let halve_list l = 
      let len = List.length l in
      let l0 = lselect l 0 ((len/2)-1) in
      let l1 = lselect l (len/2) (len-1) in
      l0,l1 in

    let compare a b dir = 
      let c = dir ==~ (sort_fn a b) in
      pipeline (mux2 c b a), pipeline (mux2 c a b) in

    let rec merge l dir vld = 
      if (List.length l) > 1 then
        let l0,l1 = halve_list l in
        let l0,l1 = List.unzip (List.map2 (fun a b -> compare a b dir) l0 l1) in
        let l0,v0 = merge l0 dir (pipeline vld) in
        let l1,_  = merge l1 dir (constb "0") in
        (l0@l1), v0
      else l, vld in

    let rec sort l dir vld = 
      if ((List.length l) > 1) then 
        let l0,l1 = halve_list l in
        let s0,v0 = (sort l0 asc vld) in
        let s1,_  = (sort l1 dsc vld) in
        merge (s0@s1) dir v0
      else l, vld in

    sort inputs asc vld

(** Odd-even transposition sort *)
let odd_even_tranposition sort_fn pipeline inputs vld =
  let len = List.length inputs in

  let rec sort l = 
    match l with
    | [] -> []
    | [a] -> [pipeline a]
    | a :: b :: tl ->
      let c = sort_fn a b in
      (pipeline (mux2 c a b)) :: (pipeline (mux2 c b a)) :: sort tl in
  
  let rec sort_stages n l vld = 
    if n = (len/2) then l, vld
    else 
      (* even *)
      let even = sort l in
      let vld = pipeline vld in
      let odd = sort (List.tl even) in
      let vld = pipeline vld in
      sort_stages (n+1) ((pipeline (List.hd even)) :: odd) vld in
  
  sort_stages 0 inputs vld


/// <P>Odd even merge sort (written in an imperative style).<P>
/// <P><I>Note: Pipelining doesnt work (results come a different times) and valid not well thought out. As a combinatorial network is the best of the bunch though.</I><P>
let odd_even_merge sort_fn pipeline inputs vld =
  let a = Array.of_list inputs in
  let compare_exchange i j =
    let ai = a.[i] in
    let aj = a.[j] in
    let c = sort_fn ai aj in
    a.[i] <- pipeline (mux2 c ai aj);
    a.[j] <- pipeline (mux2 c aj ai) 
  in
    
  let rec merge lo n r vld = 
    let m = r * 2 in
    if m < n then (
      let i = ref (lo + r) in
      let _ = merge lo n m gnd in
      let vld = merge (lo + r) n m vld in
      while (!i + r) < (lo + n) do
        compare_exchange !i (!i + r);
        i := !i + m
      done;
      pipeline vld
    ) else (
      compare_exchange lo (lo+r);
      pipeline vld
    )
  in
  
  let rec merge_sort lo n vld = 
    if (n>1) then (
      let m = n / 2 in
      let _ = merge_sort lo m vld in
      let vld = merge_sort (lo+m) m vld in
      merge lo n 1 vld
    ) else 
      vld
  in
  
  let vld = merge_sort 0 (Array.length a) vld in
  Array.to_list a, vld
  
let test_sort() = 
  let inputs = List.mapi (fun y x -> constb x ++ consti 3 y) [ "0001"; "0011"; "0101"; "0010"; "1111"; "1011"; "0000"; "1010" ] in
  let outputs, vld = odd_even_merge (fun a b -> (select a 6 3) >~ (select b 6 3)) (fun x -> x) inputs (constb "0") in
  let outputs = List.mapi (fun i o -> output ("o" ^ (string i)) o) outputs in
  let circuit = DigitalLogic.Circuit.create outputs in
  //Circuit.write_file Verilog.write "output/" "odd_even_merge" ".v" circuit
  DigitalLogic.Resources.report stdout circuit

(* Sequential sorting network *)

(** Sequential sorter, somewhat like insertion sort.  1 input (or output)/cycle *)
let seq_sort sort_fn clock reset cnt clr d (we:Signal) a = 
  (* debug *)
  let (--) a b = a in
  
  (* enable, load/shift *)
  let ctrl = [ for i in { 0 .. cnt-1 } -> wire 1 -- "en", wire 1 -- "ld"] in
  
  (* registers storing partially/fully sorted values *)
  let _,_,sregs = 
    List.fold_left (fun (v_prev,prev,regs) (ena,ld) ->
      let v = reg clock reset empty (ena |~ clr) (mux2 clr gnd (mux2 ld vdd v_prev)) in
      let r = reg clock reset empty ena (mux2 ld d prev) in
      v, r, (v -- "V", r -- "Q") :: regs
    ) (vdd,d,[]) ctrl in
  let sregs = List.zip (List.rev sregs) ctrl in
  
  (* control logic *)
  List.fold_left (fun prev_less ((vld,cur),(ena,ld)) ->
    let less = mux2 vld (sort_fn d cur) vdd -- "less"in
    ena <== (less &~ we);
    ld <== (~~~ prev_less);
    less
  ) gnd sregs |> ignore;
  
  let vq,_ = List.unzip sregs in
  let v,q = List.unzip vq in
  
  (* reults *)
  mux a v, mux a q

let test_seq_sort() = 

  let v,q = seq_sort (<~) clock reset 10
              (input "clr" 1)
              (input "d" 8) 
              (input "we" 1)
              (input "a" 4) in
  let circuit = DigitalLogic.Circuit.create [ output "q" q; output "v" v; ] in
  
  DigitalLogic.Circuit.write_file DigitalLogic.Verilog.write "output/" "ssort" ".v" circuit;
  
  let sim = DigitalLogic.Simulator.create circuit in
  let we = sim.port "we" in
  let a = sim.port "a" in
  let d = sim.port "d" in
  let clr = sim.port "clr" in
  let q = sim.port "q" in
  let v = sim.port "v" in
  
  let tst = [| 12;39;155;23;177;200;76 |] in
  sim.reset;
  
  for i=0 to 6 do
    d.i <- tst.[i];
    we.i <- 1;
    sim.cycle
  done;
  we.i <- 0;
  for i=0 to 9 do
    a.i <- i;
    sim.cycle;
    if v.i <> 0 then printf "%i: %i\n" i q.i
  done;
    
  clr.i <- 1;
  sim.cycle;
  clr.i <- 0;
  
  let tst = [| 82;79;12;0;21;155;122 |] in
  for i=0 to 4 do
    d.i <- tst.[i];
    we.i <- 1;
    sim.cycle
  done;
  we.i <- 0;
  for i=0 to 9 do
    a.i <- i;
    sim.cycle;
    if v.i <> 0 then printf "%i: %i\n" i q.i
  done
  

