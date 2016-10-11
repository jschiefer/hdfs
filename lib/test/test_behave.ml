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

open DigitalLogic
open Numeric.Ops
open Signal
open List

(** Processes behavioral assignments into a format that can be put in the signal graph. *)
let behave code = 
  let os = output_string stdout in

  (* not used...
  (* return true if complete, false if incomplete. Fail if multiple cases found *)      
  let check_cases cond cases = 
    let idx_set cases = fold_left (fun set (idx, cond) -> 
        let idx_str = string_of_const idx in
        if Set.mem idx_str set then failwith "Duplicate case index found";
        Set.add (bin_str_of_const idx) set) 
      Set.empty cases in
    let num = length cases in
    let max_num = 1 <<< (width cond) in
    if max_num < num 
    then failwith ("There are more cases (" ^ string_of_int num ^ ") than are representable with " ^ string_of_int (width cond) ^ " bits")
    else 
      ignore (idx_set cases)
  in *)

  (* find all assigned wires *)
  let rec behave_targets targets nodes = fold_left behave_target targets nodes
  and behave_target targets node = 
    match node with
    | B_if(cond, on_true, on_false) -> behave_targets targets (on_true @ on_false)
    | B_switch(cond, cases) -> 
      iter (fun (x : Signal) -> 
        if not x.IsConst then 
          failwith "Case indexes must be constants (no expressions - even if logically constant - allowed)\n";
        if width x <> width cond then
          failwith "Width of case index must equal width of select condition\n") (map fst cases);
      behave_targets targets (List.concat (map snd cases))
    | B_assign(B_assign_tgt(qtarget,target,def,hi,lo), expr) ->
      match target.signal with
      | Signal_wire(_,_,_,d) when !d = empty -> B_assign_tgt(qtarget,target,def,hi,lo) :: targets
      | _ -> failwith "Targets in behavioral expressions must be unassigned wires"
  in
  
  //let targets = unique_list (behave_targets [] code) in     // .... unique, but doesnt account for the index range properly .... really not especially efficient either
  
  (* construct a map of <target> -> <(hi,lo) set> 
     the list of targets provides all we need to produce this map *)
  let rec build_target_range_map t =
    List.fold_left (fun m (B_assign_tgt(qtarget,target,def,hi,lo)) -> 
      let b = (B_assign_tgt(qtarget,target,def,width qtarget - 1,0)) in
      match  Map.tryfind b m with
      | Some(s) -> Map.add b (Set.add (hi,lo) s) m
      | None -> Map.add b (Set.singleton (hi,lo)) m
    ) Map.empty t
  in
  let targets = behave_targets [] code |> build_target_range_map in

  let dump_targets t =
    Map.iter (fun (u:BehaveAssignTarget) s ->
      let B_assign_tgt(qtarget,target,def,hi,lo) = u in
      printf "uid = %i/%i/%i [%i:%i]\n" qtarget.uid target.uid def.uid hi lo;
      Set.iter (fun (hi,lo) -> printf "(%i,%i) " hi lo) s;
      printf "\n"
    ) t
  in
  dump_targets targets;
  
  (* print the detected targets *)
  //List.iter (fun (B_assign_tgt(qtarget,target,def,hi,lo)) -> printf "TGT %i/%i: [%i:%i]\n" (qtarget.uid) (target.uid) hi lo) targets;

  (* Given a list of pairs (hi,lo) representing bit ranges in part selection of a behave assignment, 
     this function returns a list of ranges which represent the unique parts of the selections. 
     For example:
     76543210               76543210
     *****    is split into ***
        *****                  **
                                 ***
  *)
  let unique_target_ranges max l = 
    let rec edges hh ll = function [] -> (hh,ll) | (h,l) :: tl -> edges (h::(l-1)::hh) (l::(h+1)::ll) tl in
    (* in case its missing, add the total range to the input list, then work out upper and lower split points *)
    let hh, ll = ((max,0) :: l) |> edges [] [] in  
    (* sort lists of split points *)
    let hh, ll = sort compare hh, sort compare ll in 
    (* get rid of out of bounds splits (which come at max+1, -1) and non-unique splits *)
    let rec unique prev = function [] -> [] | hd :: tl -> if hd = prev || hd < 0 || hd > max then unique hd tl else hd :: unique hd tl in 
    (* combine high and low splits - dunno why but it always seems to come up with the right answer..! *)
    combine (hh |> unique (-1)) (ll |> unique (-1)) 
  in
  let kmap fn m = Map.fold (fun k x m -> Map.add k (fn k x) m) m Map.empty in
  let targets = kmap (fun (B_assign_tgt(_,_,_,h,_)) hl -> Set.of_list (unique_target_ranges h (Set.to_list hl))) targets in
  
  dump_targets targets;  
  
  (* Apply the default assignments *)
  //let default_code = map (fun t -> match t with B_assign_tgt(qtarget,target,def,hi,lo) -> B_assign(B_assign_tgt(qtarget,target,def,hi,lo), def)) targets in
  let default_code = Map.fold (fun (B_assign_tgt(qtarget,target,def,hi,lo)) _ l -> (B_assign(B_assign_tgt(qtarget,target,def,hi,lo), def)) :: l) targets [] in
  let code = default_code @ code in  
  //let targets = map (fun t -> match t with B_assign_tgt(qtarget,target,def,hi,lo) -> target) targets in
  Circuit.print_behave stdout "" code
  
(*
  
  (* split by targets *)
  let rec behave_split_list target nodes = 
    let match_assign_to_target target = function
    | B_assign(B_assign_tgt(_,x,_,_,_),_) when x = target -> true
    | B_assign(_) -> false
    | _ -> true in
    map (behave_split target) (filter (match_assign_to_target target) nodes)
  and behave_split target node = 
    match node with
    | B_if(cond, on_true, on_false) -> B_if(cond, behave_split_list target on_true, behave_split_list target on_false)
    | B_switch(cond, cases) -> B_switch(cond, map (fun (i, cases) -> (i, (behave_split_list target cases))) cases)
    | B_assign(target, expr) -> node
  in
  let codes = map (fun target -> behave_split_list target code) targets in

  (* return true if cases are complete, false if incomplete. *)      
  let case_complete cond cases = (1 <<< (width cond)) >= (length cases) in
  
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
    let nodes, opt = fold_left (fun (l,b) node -> if is_empty node then (l,true) else (node::l,b)) ([],opt) nodes in
    let nodes = rev nodes in
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
        if is_complete (hd tl) 
        then assigns (nl,true) tl 
        else assigns (a::nl,opt) tl 
    in
    let nodes, opt = assigns ([],opt) nodes in
    let nodes = rev nodes in
    let nodes, opts = List.split (map (optimise opt) nodes) in
    nodes, fold_left (||) opt opts
    
  and optimise opt node = 
    match node with
    | B_if(cond, on_true, on_false) -> 
      (* recursively optimise *)
      let on_true, opt = optimise_list opt on_true in
      let on_false, opt = optimise_list opt on_false in
      B_if(cond, on_true, on_false), opt
    | B_switch(cond, cases) -> 
      (* filter empty cases *)
      let cases, opt = fold_left (fun (cur,opt) case -> match case with _,[] -> (cur,true) | _ -> (case::cur,opt)) ([],opt) cases in
      let cases = rev cases in
      (* recursively optimise *)
      let cases, opts = List.split (map (fun (i,case) -> let n,o = optimise_list opt case in (i,n),o) cases) in 
      B_switch(cond, cases), fold_left (||) opt opts
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
  let rec dependants_list deps nodes = fold_left dependants deps nodes
  and dependants deps node = 
    match node with
    | B_if(cond, on_true, on_false) -> dependants_list (cond::deps) (on_true @ on_false)
    | B_switch(cond, cases) ->
      (*let deps = cond :: ((map fst cases) @ deps) in // this adds the constants to the dependants map which is not really what we want *)
      dependants_list (cond::deps) (List.concat (map snd cases)) 
    //| B_assign(target, expr) -> expr::deps
    (* note: if the default assignment is optimised then we lose the fact 
       that the default is also a dependant which breaks the f# generator. 
       On the other hand I'm not sure what effect this will have on the
       scheduler. *)
    | B_assign(B_assign_tgt(qtarget, target, def, high, low), expr) -> def::expr::deps 
  in

  (* now wire up the signals *)
  iter2 (fun code target ->
    let code, opt = run_optimiser code in 
    let dependants = unique_signal_list (dependants_list [] code) in
    let behave_signal = { su = Signal_behave(signal_incr_uid(), width target, code, dependants) } in
    assign target behave_signal;
  ) codes targets
*)

let test_behave() = 
  let c0 = input "c0" 2 in
  let c1 = input "c1" 1 in
  let c2 = input "c2" 1 in
  let c2 = constb "0" in

  let s0 = input "s0" 2 in
  let s1 = input "s1" 2 in
  let s2 = input "s2" 2 in
  let s3 = input "s3" 2 in
  let s = [s0;s1;s2;s3] in

  let w0 = (b_wire0 2) -- "wire0" in
  let w1 = (b_wire0 2) -- "wire1" in
  let w2 = (b_wire0 2) -- "wire2" in
  let w3 = (b_wire0 2) -- "wire3" in
  let w = [w0;w1;w2;w3] in

  (* Generate some behavioural circuits.  Note we reuse signals on the left of $==.  
     This isnt really allowed.  if converted the later $== assignments to these wires will fail *)

  let test0 = [
    b_if (c0) [ 
      w0 $== s0;
    ] []
  ] in
    
  let test1 = [
    w3 $== s0;
    b_switch (c0) [
      b_case (consti 2 (-2)) [ w1 $== s0; ];
      b_case (consti 2 (-1)) [ w1 $== s1; ];
      b_case (consti 2   0)  [ w1 $== s2; ];
      b_case (consti 2   1)  [ w1 $== s3; ];
    ];
    b_switch (c0) [
      b_case (consti 2 0) [ w2 $== s0; b_if c0 [ w3 $== s1; ] []; ];
      b_case (consti 2 1) [ w2 $== s1; ];
      b_case (consti 2 2) [ w2 $== s2; ];
      b_case (consti 2 3) [ w2 $== s3; ];
    ];
  ] in

  let test2 = [
    w0 $== s0;
    w1 $== s1;
    b_if (c0) [
      w0 $== s0;
      w1 $== s1; (* aggressive mode should remove this *)
      b_if (c1) [ 
        w0 $== s3; (* this is removed *)
        w1 $== s2;
      ] [
        w1 $== s3;
        b_switch (c0) 
          (map (fun i -> b_case (consti 2 i) [ (nth w i) $== (nth s i); ]) [0; 1; 2; 3])
      ];
    ] [];
  ] in

  let r0 = b_regc enable 4 -- "b_reg0" in
  let r1 = b_regc enable 4 -- "b_reg1"  in
  let test3 = [
    r1.[2,2] $== s1 ++ s1;
    //r1 $== s1 ++ s1;
    b_if (c0) [
      r0.[2,2] $== s1 ++ s1;
    ] [
      //r1.[3,0] $== s1 ++ s1;
      r1 $== s1 ++ s1;
    ]
  ] in

  (* compile *)
  behave test3;
  //let outputs = map (fun (n,s) -> output n s) [ ("w1",w1.q); ("w2",w2.q); ("w3",w3.q) ] in
  let outputs = map (fun (n,s) -> output n s) [ ("w1",r0.q); ("w2",r1.q) ] in
  //let outputs = map (fun (n,s) -> output n s) [ ("w2",r1.q) ] in
  let circuit = Circuit.create outputs in
  
  Resources.report stdout circuit;
  Circuit.write_file Vhdl.write "output/" "test_behave" ".vhd" circuit;
  Circuit.write_file Verilog.write "output/" "test_behave" ".v" circuit;
  Circuit.write_file Fsharp.write "output/" "test_behave" ".fs" circuit

//let _ = test_behave()
