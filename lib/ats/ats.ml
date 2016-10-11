(** Abstract transition system *)
(* http://csg.lcs.mit.edu/pubs/memos/Hoe/HoePhDthesis.pdf *)

(* 

  definition of ATS
  -----------------

ATS = [ S, S0, X ]
S = [ R1..Rnr, A1..Ana, F1..Fnf, 01..Ono, I1..Ini ]
S0 = [ vR1..vRnr, vA1..vAna, vF1..vFnf, v01..vOno, vI1..vIni ]
X = [ T1..Tm ]
T = [ Pi, u ]
L = exp
u = [ aR1..aRnr, aA1..aAna, aF1..aFnf, a01..aOno, aI1..aIni ]
aR = E | set(exp)
aA = E | a-set(expI, expD)
aF = E | enq(exp) | deq() | enq-deq(exp) | clear()
aO = E | set(exp)
aI = E
exp = Constant | PrimOp(exp1..expN) | R.get() | A.a-get(idx) | 
      F.first() | F.notfull() | F.notempty() | O.get() | I.get()

*)


#light
module DigitalLogic.Circuits.AbstractTransitionSystem
open DigitalLogic
open Circuit
open Signal
open List
open Simulator

(* debug stuff *)
let print_set = Set.iter (fun x -> printf "%i " x)

let print_dependants sets = 
  let states, deps = sets
  
  printf "States\n"
  print_set states
  printf "\n"
 
  iter (fun (read, write) ->  
    printf "Transition - \n"
    printf " - Read  "
    print_set read
    printf "\n"

    printf " - Write "
    print_set write
    printf "\n"
  ) deps

let print_pairs set = 
  Set.iter (fun (x,y) -> printf "(%i, %i) " x y) set
  printf "\n"


(*

  Roughly:
  
  * We have n state elements (fifos, arrays etc)
  * We have a rule for every state element and call this a transition
  * We have a set of transitions defining the algorithm
  * Using a round-robin scheduler we should be able to simulate this system
    * How best?  Translate the scheduler to HW, or embed simulations within
      some outer framework.  The form is tricky but doable, the later is perhaps
      a little better for debugging.
    * Hardware coversion:
      How best to described the state elements?  Alls fine until you consider that they have to
      read the state elements.  Perhaps we can use the behavioural construct for this (assignment, 
      which I guess means deferred assignment 
  
  * We then need to augment this description with constraints about CF + SC

      “Conflict Free” (CF), that is, they do not update the
      same state and neither updates the state read by the
      other rule. Arvind and Hoe further observed that two
      rules (R1 and R2) can execute simultaneously if one
      rule (R2) does not read any of the state that the other
      rule (R1) writes. In this case, simultaneous execution of
      R1 and R2 appears the same as sequential execution of
      R1 followed by R2. For this to hold, R2 writes must
      take precedence over writes to the same state by R1.
      Such rules are called “Sequentially Composable” (SC)

  * Then comes an opitimised scheduler which is going to be _the hard bit_.
  * I wonder also if we could embed a "conflict" map into the hardware so
    it could schedule itself?
*)

type AtomicStateTransitions = 
  Ats of
    bassign_tgt_t list *                (* N state elements *)
    (Signal * (Signal list)) list * (* T transitions: control + actions *)
    (int*int) list                      (* user conflict free annotations *)
  
(* for each transition, find the state elements it reads, and those it writes *)
let ats_dependants ats = 
  let Ats(states, actions, ucf) = ats
  let b_uid (B_assign_tgt(q,_,_)) = uid q
  (* create a set of state transitons *)
  let states_set = fold_left (fun set el -> Set.add (b_uid el) set) Set.empty states in
  
  let rec read_set_of_signal (visited, reads) signal = 
    if Set.mem (uid signal) visited then
      (* already visited this node *)
      visited,reads
    else
      (* add signal to visited set *)
      let visited = Set.add (uid signal) visited
      let deps = dependants signal
      let reads = 
        match signal with
        | Signal_reg(uid,_,_,_,_,_,_) when Set.mem uid states_set -> Set.add uid reads
        | _ -> reads
      fold_left read_set_of_signal (visited, reads) (dependants signal)
        
  let dependants_sets = 
    map (fun (ctrl,rules) -> 
      (* search the expressions for state elements they read *)
      let _, reads = fold_left read_set_of_signal (read_set_of_signal (Set.empty, Set.empty) ctrl) rules
      (* find the state elements that are written in each transition *)
      let writes = fold_left (fun set (state,rule) -> if rule = empty then set else Set.add (b_uid state) set) Set.empty (combine states rules)
      reads, writes
    ) actions      
      
  states_set, dependants_sets

(* test is two transitions are conflict free *)
let is_conflict_free (a_read, a_write) (b_read, b_write) = 
  Set.is_empty (Set.inter a_write b_write) &&
  Set.is_empty (Set.inter a_write b_read) &&
  Set.is_empty (Set.inter b_write a_read)

(* creates a set of conflict free edges between transitions.  
   The set contains pairs of ints - the transititions - (l,h) where l<h *)
let conflict_free l = 
  let rec conflict_free' set i0 l = 
    match l with
    | [] -> set
    | hd::tl ->
      let _,set = fold_left (fun (i1,set) el -> i1+1, if is_conflict_free hd el then Set.add (i0,i1) set else set) (i0+1,set) tl
      conflict_free' set (i0+1) tl 
  conflict_free' Set.empty 0 l

(* merge in user conflict free entries (ie for mutual exlusions).  
   Reorder so (l,h) l<h, remove l=h *)
let merge_conflict_free cf user_cf = 
  fold_left (fun set el -> 
    let l,h = el
    let el = if l<h then l,h else h,l
    if l=h then set
    else Set.add el set
  ) cf user_cf

(* This creates the scheduling groups.  Analysis might be easier with
   a graph data structure. *)
let create_scheduling_groups num_transitions conflicts = 
  (* creates [ set0, set1, ...setN ] where each set corresponds to the vertices vertex N is connected to *)
  let referenced_vertices =
    let add i (l,h) s =
      if i = l then Set.add h s else 
      if i = h then Set.add l s 
      else s
    (* this formats the edges in a different way.  can it be created more like this instead? *)
    map (fun i -> Set.fold (add i) conflicts Set.empty) [ 0 .. num_transitions-1 ]
    
  (* Given one of the sets, run through it and create the union of all dependants.
     If the union is empty, return it.  If not try again.  Not 100% sure about the way sets are cleared as they are found. *)
  let rva = Array.of_list referenced_vertices
  let merge set = Set.fold (fun e s -> let r = rva.(e) in rva.(e) <- Set.empty; Set.union r s) set Set.empty 
  let rec merge_all set =
    let deps = merge set
    if Set.is_empty (Set.diff deps set) then set
    else merge_all (Set.union set deps)

  for i=0 to num_transitions-1 do
    rva.(i) <- merge_all rva.(i)
    
  let rv = filter (fun x -> not (Set.is_empty x)) (Array.to_list rva)
  (* union all the sets, take difference to find unconnected vertices *)
  let urv = fold_left (fun set el -> Set.union set el) Set.empty rv 
  let diff = fold_left (fun set i -> Set.add i set) Set.empty [ 0 .. num_transitions-1 ]
  let drv = map (fun x -> Set.add x Set.empty) (Set.to_list (Set.diff diff urv))
  rv @ drv

(* within a scheduling group it is still possible to fire multiple transitions depending on which ones are ready *)
let create_enumerated_shedule num_transitions groups conflicts = 
  (* create an edge table *)
  let edges = Array2.create num_transitions num_transitions false
  Set.iter (fun (x,y) -> 
    edges.(x,y) <- true
    edges.(y,x) <- true
  ) conflicts

  (* create a 2^n x n bit lookup table 
     The current rules are:
      1 node - do nothing
      2 nodes - must be connected
      3 .. 8 nodes - create a schedule
      >8 - rom too big (can we split into smaller groups somehow?  How would be choose the partitioning?)
  *)
  let create_enumerated_shedule group = 
    let n = Set.size group
    if n <= 2 then
      None
    else if n > 8 then 
      None 
    else
      let n2 = 1 <<< n
      let rom = Array.create n2 0   (* rom values *)
      let nodes = Array.of_list (Set.to_list group)
      (* create masks *)
      let masks = Array.create n 0
      for i=0 to n-1 do
        let idx_1 = nodes.(i)
        for j=0 to n-1 do
          let idx_2 = nodes.(j)
          masks.(i) <- (masks.(i) <<< 1) ||| (if edges.(idx_1, idx_2) then 1 else 0)
          
      let rec count_bits v = 
        if v = 0 then 0
        else 1 + count_bits (v>>>1)
          
      let select_best i masks =
        let mutable best = i &&& (~~~ masks.(0))
        let mutable best_bits = count_bits best
        for mask in masks do
          let value = i &&& (~~~ mask)
          let bits = count_bits value
          if bits > best_bits then
            best_bits <- bits
            best <- value
        best
      
      for i=0 to n2-1 do
        rom.(i) <- select_best i masks
      Some rom
    
  List.map create_enumerated_shedule groups

let create ats = 
  (* first lets ensure the data structure is valid *)
  let Ats(states, actions, ucf) = ats
  let num_states = length states
  let num_actions = length actions
  let log_num_actions = Util.clog2 (num_actions - 1)
  let filter_none l = map (function Some x -> x | _ -> failwith "") (filter (function Some _ -> true | _ -> false) l)
  assert (num_states > 0)
  assert (num_actions > 0)
  assert (length actions = num_actions)
  iter (fun (_,state_assigns) -> assert (length state_assigns = num_states)) actions
  iter (fun (l,h) -> assert ((l < num_actions) && (h < num_actions))) ucf
  
  (* dependants analysis *)
  let states_set, dependants_sets = ats_dependants ats
  print_dependants (states_set, dependants_sets)
  
  (* conflict free analysis *)
  let conflict_free_set = conflict_free dependants_sets
  (* add in user constraints *)
  let conflict_free_set = merge_conflict_free conflict_free_set ucf
  printf "conflict free:\n"
  print_pairs conflict_free_set

  (* conflicting edges *)
  let full_edges = 
    [ for x in 0 .. num_actions-1
        for y in 0 .. num_actions-1
        when x < y
        -> (x, y) 
    ]
  let conflict_set = Set.diff (Set.of_list full_edges) conflict_free_set 
  printf "conflicting:\n"
  print_pairs conflict_set

  (* scheduler *)
  let scheduling_groups = create_scheduling_groups num_actions conflict_set
  iteri (fun i x ->
    printf "sched group %i: " i
    print_set x
    printf "\n"
  ) scheduling_groups

  (* enumerated scheduler *)
  let enum_schedules = create_enumerated_shedule num_actions scheduling_groups conflict_set

  let state = b_reg clock reset empty empty log_num_actions
  behave 
    [
      b_switch (q' state)
        (mapi (fun action_no (ctrl,assigns) -> 
          b_case (consti log_num_actions action_no) 
            [
              state $== (q' state) +: (one log_num_actions);
              b_if (ctrl) 
                (filter_none (map2 (fun state assign ->
                  if assign = empty then None
                  else Some (state $== assign)
                ) states assigns))
                []
            ]
        ) actions)
    ]
  map (fun (B_assign_tgt(q,_,_)) -> q) states, q' state, ~: (Util.reduce_1 (|:) (map fst actions))

let tst_gcd : AtomicStateTransitions = 
  (* states *)
  let a,b = b_reg clock reset (consti 8 15) empty 8, b_reg clock reset (consti 8 9) empty 8
  let states = [ a; b ]
  (* control *)
  let a,b = a.q, b.q
  let a_lt_b = a <: b
  let a_gte_b = ~: a_lt_b
  let actions = 
    [ 
      (* mod rule *)
      a_gte_b &&& (b /=: 0), [ a -: b; b ]; 
      (* flip rule *)
      a_lt_b, [ b; a ]; 
    ]
  Ats(states, actions, [  ])
  
(* mutual exclusion test.  This needs to be hand added to the table *)
let tst_me a_in b_in = 
  let reg c w = b_reg clock reset (consti w c) empty w
  let st, a, b = reg 0 1, reg 15 8, reg 9 8
  let states = [ st; a; b ]
  let st, a, b = q' st, q' a, q' b
  let actions = 
    [ 
      ~~~ st, [ vdd; a_in; b_in ]; 
      st, [ vdd; b; a ]; 
    ]
  Ats(states, actions, [])

let tst_cf =
  let reg c w = b_reg clock reset (consti w c) empty w
  let t1, t2, t3, t4, t5, t6 = 
    reg 0 8, reg 0 8, reg 0 8, 
    reg 0 8, reg 0 8, reg 0 8
  let states = [ t1; t2; t3; t4; t5; t6 ]
  let t1, t2, t3, t4, t5, t6 = q' t1, q' t2, q' t3, q' t4, q' t5, q' t6
  let conflicts = 2
  let actions = 
    if conflicts = 0 then
      [
        vdd, [ t1; t2; t3; t4; t5; t6 ];
        vdd, [ t1; t2; t3; t4; t5; t6 ];
        vdd, [ t1; t2; t3; t4; t5; t6 ];
        vdd, [ t1; t2; t3; t4; t5; t6 ];
        vdd, [ t1; t2; t3; t4; t5; t6 ];
        vdd, [ t1; t2; t3; t4; t5; t6 ];
      ]
    else if conflicts = 1 then
      [
        vdd, [ t1; empty; empty; empty; empty; empty ];
        vdd, [ empty; t2; empty; empty; empty; empty ];
        vdd, [ empty; empty; t3; empty; empty; empty ];
        vdd, [ empty; empty; empty; t4; empty; empty ];
        vdd, [ empty; empty; empty; empty; t5; empty ];
        vdd, [ empty; empty; empty; empty; empty; t6 ];
      ]
    else 
      (* conflict groups:
        t1, t4, t6,
        t2, t5,
        t3 *)  
      [
        vdd, [ t1; empty; empty; empty; empty; empty ];
        vdd, [ empty; t2; empty; empty; empty; empty ];
        vdd, [ empty; empty; t3; empty; empty; empty ];
        vdd, [ t1; empty; empty; t4; empty; empty];
        vdd, [ empty; t5; empty; empty; t5; empty ];
        vdd, [ empty; empty; empty; t4; empty; t6 ];
      ]    
  Ats(states, actions, [])
  
let test_ats() = 
  //let outputs, state, ready = create tst_gcd
  let outputs, state, ready = create tst_cf
  //let outputs, state, ready = create (tst_me (input "a_in" 8) (input "b_in" 8))
  let circuit = Circuit.create (map (fun x -> x -- "o") (state :: ready :: outputs))
  let sim, SimDataGen(i,w,o) = generator_int (create_int circuit) 20
  write_file Verilog.write "output/" "gcd" ".v" circuit
  
  let fi = find_input_data sim
  //let a_in = fi "a_in"
  //let b_in = fi "b_in"

  sim_reset sim
  //a_in := 1;
  //b_in := 2;
  for i=0 to 19 do
    sim_cycle sim

  Waveform.draw_int Waveform.wave_dark (i@w@o)

do test_ats()
