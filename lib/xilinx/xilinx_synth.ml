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

/// <p>Synthesis of HDFS circuits to Xilinx FPGA's.<p>
/// <p>Maps signals to Xilinx instantiations.  Once complete, and with the addition of an EDIF writer,
///    this can be put directly into the P&R tools bypassing a synthesis step.<P>
/// <ul>
///     <li><b>+, -</b>          - done</li>
///     <li><b>*, *+</b>         - done (needs to be improved)</li>
///     <li><b>&, |, ^, ~</b>    - done</li>
///     <li><b><, ==</b>         - done</li>
///     <li><b>registers</b>     - done (partial - more register types can be used, though I'm not sure if there's any point)</li>
///     <li><b>memories</b>      - TODO</li>
/// </ul>
/// <p><i>It goes without saying that the quality of results is nothing like a commercial synthesizer...</i><p>

#light
module DigitalLogic.Synthesis.Xilinx
open DigitalLogic.Numeric.Ops
open DigitalLogic.Circuit
open DigitalLogic.Signal
open DigitalLogic.Circuits

(* When set to true simulation models of the xilinx primitives are used.
   This allows "synthesized" circuits to be simulated.  
   It is not suitable for synthesis and are really for testing the synthesis routines.  *)
let xsyn_sim = ref false

let g_lutN g_str sel = 
  match g_str with
  | Some(G_Vec(str)) -> mux sel [ for i in { String.length str - 1 .. -1 .. 0 } -> constb (String.of_char str.[i]) ]
  | _ -> failwith "No init string specified"

let g_lut1 g_str a = if !xsyn_sim then g_lutN g_str a else Xilinx.g_lut1 g_str a
let g_lut2 g_str a b = if !xsyn_sim then g_lutN g_str (b++a) else Xilinx.g_lut2 g_str a b
let g_lut3 g_str a b c = if !xsyn_sim then g_lutN g_str (c++b++a) else Xilinx.g_lut3 g_str a b c
let g_lut4 g_str a b c d = if !xsyn_sim then g_lutN g_str (d++c++b++a) else Xilinx.g_lut4 g_str a b c d 
let g_lut5 g_str a b c d e = if !xsyn_sim then g_lutN g_str (e++d++c++b++a) else Xilinx.g_lut5 g_str a b c d e
let g_lut6 g_str a b c d e f = if !xsyn_sim then g_lutN g_str (f++e++d++c++b++a) else Xilinx.g_lut6 g_str a b c d e f

let muxcy ci di sel = if !xsyn_sim then mux2 sel ci di else Xilinx.muxcy ci di sel 
let inv a = if !xsyn_sim then ~~~ a else Xilinx.inv a
let xorcy ci li = if !xsyn_sim then ci ^^^ li else Xilinx.xorcy ci li 
let muxf5 f t s = if !xsyn_sim then mux2 s t f else Xilinx.muxf5 f t s 
let muxf6 f t s = if !xsyn_sim then mux2 s t f else Xilinx.muxf6 f t s 
let muxf7 f t s = if !xsyn_sim then mux2 s t f else Xilinx.muxf7 f t s  
let muxf8 f t s = if !xsyn_sim then mux2 s t f else Xilinx.muxf8 f t s 
let fdce clock enable reset (d:Signal) = if !xsyn_sim then d.reg(clock, reset, 0, enable) else Xilinx.fdce clock enable reset (d:Signal) 
let fdpe clock enable (d:Signal) reset = if !xsyn_sim then d.reg(clock, reset, 1, enable) else Xilinx.fdpe clock enable (d:Signal) reset 
let mult_and a b = if !xsyn_sim then a &&& b else Xilinx.mult_and a b

let ram16x1s a0 a1 a2 a3 d clk we =
  if !xsyn_sim then 
    let a = a3 ++ a2 ++ a1 ++ a0
    memory 16 clk a we d a
  else Xilinx.ram16x1s a0 a1 a2 a3 d clk we 

let ram32x1s a0 a1 a2 a3 a4 d clk we =
  if !xsyn_sim then 
    let a = a4 ++ a3 ++ a2 ++ a1 ++ a0
    memory 32 clk a we d a
  else Xilinx.ram32x1s a0 a1 a2 a3 a4 d clk we 

let ram64x1s a0 a1 a2 a3 a4 a5 d clk we =
  if !xsyn_sim then 
    let a = a5 ++ a4 ++ a3 ++ a2 ++ a1 ++ a0
    memory 64 clk a we d a
  else Xilinx.ram64x1s a0 a1 a2 a3 a4 a5 d clk we 

let ram128x1s a0 a1 a2 a3 a4 a5 a6 d clk we =
  if !xsyn_sim then 
    let a = a6 ++ a5 ++ a4 ++ a3 ++ a2 ++ a1 ++ a0
    memory 128 clk a we d a
  else Xilinx.ram128x1s a0 a1 a2 a3 a4 a5 a6 d clk we 

(* Basic (and pretty much untested) model of the virtex 4 DSP slice 
 *   A           I 18 The multiplier's A input. This signal can also be used as the adder's Most Significant Word (MSW) input.
 *   B           I 18 The multiplier's B input. This signal can also be used as the adder's Least Significant Word (LSW) input.
 *   C           I 48 The adder's C input.
 *   OPMODE      I 7  Controls the input to the X, Y, and Z multiplexers in the DSP48 slices (see OPMODE, Table 1-7).
 *   SUBTRACT    I 1  0 = add, 1 = subtract.
 *   CARRYIN     I 1  The carry input to the carry select logic.
 *   CARRYINSEL  I 2  Selects carry source (see CARRYINSEL, Table 1-8).
 *   CEA         I 1  Clock enable: 0 = hold, 1 = enable AREG.
 *   CEB         I 1  Clock enable: 0 = hold, 1 = enable BREG.
 *   CEC         I 1  Clock enable: 0 = hold, 1 = enable CREG.
 *   CEM         I 1  Clock enable: 0 = hold, 1 = enable MREG.
 *   CEP         I 1  Clock enable: 0 = hold, 1 = enable PREG.
 *   CECTRL      I 1  Clock enable: 0 = hold, 1 = enable OPMODEREG, CARRYINSELREG.
 *   CECINSUB    I 1  Clock enable: 0 = hold, 1 = enable SUBTRACTREG and general interconnect carry input.
 *   CECARRYIN   I 1  Clock enable: 0 = hold, 1 = enable (carry input from internal paths).
 *   RSTA        I 1  Reset: 0 = no reset, 1 = reset AREG.
 *   RSTB        I 1  Reset: 0 = no reset, 1 = reset BREG.
 *   RSTC        I 1  Reset: 0 = no reset, 1 = reset CREG.
 *   RSTM        I 1  Reset: 0 = no reset, 1 = reset MREG.
 *   RSTP        I 1  Reset: 0 = no reset, 1 = reset PREG.
 *   RSTCTRL     I 1  Reset: 0 = no reset, 1 = reset SUBTRACTREG, OPMODEREG, CARRYINSELREG.
 *   RSTCARRYIN  I 1  Reset: 0 = no reset, 1 = reset (carry input from general interconnect and internal paths).
 *   CLK         I 1  The DSP48 clock.
 *   BCIN        I 18 The multiplier's cascaded B input. This signal can also be used as the adder's LSW input.
 *   PCIN        I 48 Cascaded adder's Z input from the previous DSP slice.
 *   BCOUT       O 18 The B cascade output.
 *   PCOUT       O 48 The P cascade output.
 *   P           O 48 The product output.
 *)

(* opmode's *)

let xmux_0   = consti 2 0
let xmux_m   = consti 2 1
let xmux_p   = consti 2 2
let xmux_ab  = consti 2 3

let ymux_0   = consti 2 0
let ymux_m   = consti 2 1
let ymux_c   = consti 2 3

let zmux_0   = consti 3 0
let zmux_pc  = consti 3 1
let zmux_p   = consti 3 2
let zmux_c   = consti 3 3
let zmux_pcs = consti 3 5
let zmux_ps  = consti 3 6
 
let dsp48_slice_v4
  (* generics *)
  areg breg creg preg mreg
  carryinreg carryinselreg
  opmodereg subtractreg
  b_input
  (* signals *)
  a b         
  c         
  opmode    
  subtract carryin carryinsel
  cea ceb cec cem cep       
  cectrl cecinsub cecarryin 
  rsta rstb rstc rstm rstp      
  rstctrl rstcarryin
  clk 
  bcin pcin =
  //let (--) a b = a
  let (--) a b = a -- ("dsp48_" ^ b)

  List.iter 
    (fun (a,b) -> assert (width a = b))
    [
      a          , 18; b          , 18; c          , 48; opmode     , 7 ;
      subtract   , 1 ; carryin    , 1 ; carryinsel , 2 ; cea        , 1 ;
      ceb        , 1 ; cec        , 1 ; cem        , 1 ; cep        , 1 ;
      cectrl     , 1 ; cecinsub   , 1 ; cecarryin  , 1 ; rsta       , 1 ;
      rstb       , 1 ; rstc       , 1 ; rstm       , 1 ; rstp       , 1 ;
      rstctrl    , 1 ; rstcarryin , 1 ; clk        , 1 ; bcin       , 18;
      pcin       , 48;
    ]
    
  let sreg clk (rst:Signal) en d = reg clk rst empty en (rst.mux2(0, d))
  let rec oreg clk rst en d l = 
    if l = 0 then d
    else sreg clk rst en (oreg clk rst en d (l-1))

  (* control registers *)
  
  assert (carryinreg >= 0 && carryinreg <= 1)
  let carryin = oreg clk rstcarryin cecinsub carryin carryinreg
  assert (carryinselreg >= 0 && carryinselreg <= 1)
  let carryinsel = oreg clk rstctrl cectrl carryinsel carryinselreg
  assert (opmodereg >= 0 && opmodereg <= 1)
  let opmode = oreg clk rstctrl cectrl opmode opmodereg
  assert (subtractreg >= 0 && subtractreg <= 1)
  let subtract = oreg clk rstctrl cecinsub subtract subtractreg

  (* input registers *)
  
  assert (areg >= 0 && areg <= 2)
  let a = oreg clk rsta cea a areg

  let b =
    match b_input with
    | "DIRECT" -> b
    | "CASCADE" -> bcin
    | _ -> failwith "b_input mode must be DIRECT or CASCADE"
  assert (breg >= 0 && breg <= 2)
  let b = oreg clk rstb ceb b breg

  assert (creg >= 0 && creg <= 2) (* note: in reality this is a shared input across two tiles in a slice *)
  let c = oreg clk rstc cec c creg

  (* multiplier - what are the two partial products for? *)

  let m0 = (a *+ b.[8..0].ue) -- "m0"
  let m1 = (a *+ b.[17..9]) -- "m1"
  assert (mreg >= 0 && mreg <= 1)
  let m0 = (sresize (oreg clk rstm cem m0 mreg) 48) -- "m0r"
  let m1 = ((sresize (oreg clk rstm cem m1 mreg) 48) <<< 9) -- "m1r"
  
  (* carry in *)
  
  let p_fb = wire 48 (* add/sub output *)
  
  let p' = mux2 (opmode.[5..4] ==~ 2) (~~~ p_fb.[47..47]) (~~~ pcin.[47..47])  (* Z mux = 2(010) or 6(110) (p or shift p) *)
  let a' = mux2 (opmode.[1..0] ==~ 3) (~~~ a.[17..17]) (~~~ (a.[17..17] ^^^ b.[17..17])) (* X mux = A:B, otherwise X,Y mux = m *)
  let cin = zero 47 ++ mux carryinsel [ carryin; p'; a'; sreg clk rstcarryin cecarryin a' ]
  
  (* x, y, z mux's *)
  
  (* see 1.4,5,6&7 *)
  let x = mux opmode.[1..0] [ zero 48; m0; p_fb; sresize (a ++ b) 48; ] -- "x"
  let y = mux opmode.[3..2] [ zero 48; m1; zero 48; c ] -- "y"
  let z = mux opmode.[6..4] [ zero 48; pcin; p_fb; c; zero 48; pcin >>+ 17; p_fb >>+ 17; zero 48 ] -- "z"

  (* add/sub *)
  
  let p = mux2 subtract (z - (x + y + cin)) (z + x + y + cin)
  assert (preg >=0 && preg <= 1)
  let p = oreg clk rstp cep p preg
  p_fb <== 
    match preg with
    | 0 -> zero 48 (* note: this would otherwise cause a combinatorial feedback path, not sure if it affects actual functionality (can see how) *)
    | _ -> p

  b, (* BCOUT *)  
  p, (* PCOUT *)
  p  (* P *)

(* ********************************************************************* *)
(* Syntax for generating lut init value *)
(* ********************************************************************* *)

(** Type used for constructing LUT equations *)
type LutInitAst = 
  | LutGnd
  | LutVdd
  | LutInput of int
  | LutAnd of LutInitAst * LutInitAst 
  | LutOr of LutInitAst * LutInitAst 
  | LutXor of LutInitAst * LutInitAst 
  | LutNot of LutInitAst 
  with
    (** AND *)
    static member (&&&) (a, b) = LutAnd(a,b)
    (** OR *)
    static member (|||) (a, b) = LutOr(a,b)
    (** XOR *)
    static member (^^^) (a, b) = LutXor(a,b)
    (** NOT *)
    static member (~~~) a = LutNot(a)
    (** Inequality *)
    static member (/=~) (a, b) = a ^^^ b
    (** Equality *)
    static member (==~) (a, b) = ~~~ (a ^^^ b)
  end

(** Construct a LUT input node *)
let lut_input n = LutInput(n)
(** Lut input bit 0 *)
let i0 = LutInput(0)
(** Lut input bit 1 *)
let i1 = LutInput(1)
(** Lut input bit 2 *)
let i2 = LutInput(2)
(** Lut input bit 3 *)
let i3 = LutInput(3)
(** Lut input bit 4 *)
let i4 = LutInput(4)
(** Lut input bit 5 *)
let i5 = LutInput(5)
(** All 6 possible lut inputs *)
let iall = [ i0; i1; i2; i3; i4; i5 ]

(** Evaluate the LUT equation and return the init string *)
let eval lut_n (ops : LutInitAst) = 
  let rec eval_lut n = 
    let rec eval (s : LutInitAst) = 
      match s with
      | LutGnd -> 0
      | LutVdd -> 1
      | LutInput(a) -> 
        if a < lut_n then ((n >>> a) &&& 1)
        else failwith ("\n***Lut" ^ string lut_n ^ " is not big enough to process input i" ^ string a)
      | LutAnd(a,b) -> (eval a) &&& (eval b)
      | LutOr(a,b)  -> (eval a) ||| (eval b)
      | LutXor(a,b) -> (eval a) ^^^ (eval b)
      | LutNot(a)   -> (~~~ (eval a)) &&& 1
    if n = (1 <<< lut_n) then ""
    else eval_lut (n + 1) ^ (if eval ops = 1 then "1" else "0") 
  eval_lut 0
  
(** Evaluate the LUT equation and return the init string wrapped as a generic for the LUT instance *)
let eval_g n o = Some(g_vec (eval n o))

(* ********************************************************************* *)
(* ********************************************************************* *)
 
(* LUT generation - todo - match strings with special patterns (in particular all ones or zeros) and optimise to vdd or gnd *)

(** Generate a lut 1,2,3,4,5 or 6 from an equation. *)
let x_lut lut_fn i = 
  match i with
  | []              -> failwith "Cannot have a 0 input lut"
  | [a]             -> g_lut1 (eval_g 1 lut_fn) a
  | [a;b]           -> g_lut2 (eval_g 2 lut_fn) a b
  | [a;b;c]         -> g_lut3 (eval_g 3 lut_fn) a b c
  | [a;b;c;d]       -> g_lut4 (eval_g 4 lut_fn) a b c d 
  | [a;b;c;d;e]     -> g_lut5 (eval_g 5 lut_fn) a b c d e
  | [a;b;c;d;e;f]   -> g_lut6 (eval_g 6 lut_fn) a b c d e f
  | _               -> failwith "Cannot generate luts with more than 6 input bits"

(** Generate a lut directly from an init string *)
let x_lutc resize_fn str i = 
  let res size str = 
    let len = String.length str 
    if len = size then str
    else if len < size then resize_fn size str
    else failwith "Lut init string too long"
  match i with
  | []              -> failwith "Cannot have a 0 input lut"
  | [a]             -> g_lut1 (Some(g_vec (res 2 str))) a
  | [a;b]           -> g_lut2 (Some(g_vec (res 4 str))) a b
  | [a;b;c]         -> g_lut3 (Some(g_vec (res 8 str))) a b c
  | [a;b;c;d]       -> g_lut4 (Some(g_vec (res 16 str))) a b c d 
  | [a;b;c;d;e]     -> g_lut5 (Some(g_vec (res 32 str))) a b c d e
  | [a;b;c;d;e;f]   -> g_lut6 (Some(g_vec (res 64 str))) a b c d e f
  | _               -> failwith "Cannot generate luts with more than 6 input bits"
  
(** Generate a lut directly from an init string.  If the string is shorter than needed, pad top bits with 0. *)
let x_lutc0 str i = x_lutc (fun size str -> String.make (size-String.length str) '0' ^ str) str i
(** Generate a lut directly from an init string.  If the string is shorter than needed, pad top bits with 1. *)
let x_lutc1 str i = x_lutc (fun size str -> String.make (size-String.length str) '1' ^ str) str i
(** Generate a lut directly from an init string.  If the string is shorter than needed, pad top bits with MSB of string. *)
let x_lutcn str i = x_lutc (fun size str -> String.make (size-String.length str) str.[0] ^ str) str i

(** Feed each bit of the input list into a lut implementing the given equation.  len = List.length l, 0 < len <= 6 *)
let x_lut_map fn l = 
  check_same l
  let width = (List.hd l).width
  let rec mapper n = 
    if n = width then empty
    else mapper (n+1) ++ (x_lut fn (l |> List.map (fun x -> x.[n])))
  mapper 0

(** AND a and b (vectors) *)
let x_and a b = x_lut_map (i0 &&& i1) [a; b]
(** OR a and b (vectors) *)
let x_or  a b = x_lut_map (i0 ||| i1) [a; b]
(** XOR a and b (vectors) *)
let x_xor a b = x_lut_map (i0 ^^^ i1) [a; b]

(* NOT a (vector) *)
let x_not a = concat (List.map (fun a -> inv a) (a:Signal).bits)

///////////////////////

(** Generic reduction circuit using the carry chain.
   Args:
    max_lut         : maximum lut size to use (most likely 4 or 6)
    lut_inv, lut_op : the lut contents are set to
                        if lut_inv then ~~~ (i0 "lut_op" i1 "lut_op" ... iN)
                        else (i0 "lut_op" i1 "lut_op" ... iN)
    mux_din         : di input of muxcy
    carry_in        : initial carry chain input
*)
let x_reduce_carry max_lut lut_inv lut_op mux_din carry_in a =  
  let rec x_reduce carry_in (a:Signal) = 
    let max_w = min a.width max_lut
    let lut_args = DigitalLogic.Util.lselect iall 0 (max_w-1)
    let lut_fn = List.fold_left (fun acc a -> lut_op acc a) (List.hd lut_args) (List.tl lut_args)
    let lut_fn = if lut_inv then ~~~ lut_fn else lut_fn
    let sel = x_lut lut_fn (List.rev a.[max_w-1 .. 0].bits)
    let carry_out = muxcy carry_in mux_din sel 
    if max_w = a.width then carry_out
    else x_reduce carry_out a.[a.width-1 .. max_w]
  x_reduce carry_in a

let x_and_reduce max_lut a = x_reduce_carry max_lut false (&&&) gnd vdd a
let x_or_reduce max_lut a = x_reduce_carry max_lut true (|||) vdd gnd a

///////////////////////

(** Generic reduction circuit using a tree of luts *)
let rec x_reduce_tree max_lut lut_op (a:Signal) =  
  let rec x_reduce_level (a:Signal) =
    let max_w = min a.width max_lut
    let lut_args = DigitalLogic.Util.lselect iall 0 (max_w-1)
    let lut_fn = List.fold_left (fun acc a -> lut_op acc a) (List.hd lut_args) (List.tl lut_args)
    let lut = x_lut lut_fn (List.rev a.[max_w-1 .. 0].bits)
    if max_w = a.width then lut
    else (x_reduce_level a.[a.width-1 .. max_w]) ++ lut
  if a.width = 1 then a
  else x_reduce_tree max_lut lut_op (x_reduce_level a)

let x_xor_reduce max_lut a = x_reduce_tree max_lut (^^^) a

///////////////////////
(*
  How the adders work:
    sum = a ^^^ b ^^^ c_cin
    carry = (a &&& b) ||| (a &&& cin) ||| (b &&& cin)
    So calculate d = a ^^^ b in the lut
    Feed d, and the previous carry in, into xorcy to get the sum
    To calculate the carry, using muxcy, set d = b, ci to previous carry in and sel to lut out
    a b c | o | x | m |
    ------+---+---+---+
    0 0 0 | 0 | 0 | 0 |
    0 0 1 | 0 | 1 | 0 |
    0 1 0 | 1 | 1 | 0 |
    0 1 1 | 1 | 0 | 1 |
    1 0 0 | 1 | 1 | 0 |
    1 0 1 | 1 | 0 | 1 |
    1 1 0 | 0 | 0 | 1 |
    1 1 1 | 0 | 1 | 1 |
    ------+---+---+---+
    o = lut output
    x = sum (xorc output)
    m = carry (carry output)
*)

let x_add_carry lut_op cin a b = 
  check_same [a;b]
  let add_lut a b cin = 
    let o = x_lut lut_op [ a; b ]
    let c = muxcy cin b o
    let s = xorcy cin o
    c, s
  let res, carry_out = 
    List.fold_left2 (fun (res, cin) a b ->
      let c, s = add_lut a b cin
      s :: res, c
    ) ([],cin) (List.rev a.bits) (List.rev b.bits)
  carry_out, concat res

let x_add a b = snd (x_add_carry (i0 ^^^ i1) gnd a b)
let x_sub a b = snd (x_add_carry (~~~ (i0 ^^^ i1)) vdd b a)

(* op = 1: add. op = 0: sub *)
let x_add_sub op (a:Signal) (b:Signal) =
  let add_sub_lut a b cin =
    let o = x_lut ( (i2 &&& (i0 ^^^ i1)) ||| ((~~~ i2) &&& (~~~ (i0 ^^^ i1))) ) [ a; b; op ]
    let c = muxcy cin a o
    let s = xorcy cin o
    c, s
  let res, carry_out = 
    List.fold_left2 (fun (res, cin) a b ->
      let c, s = add_sub_lut a b cin
      s :: res, c
    ) ([],inv op) (List.rev a.bits) (List.rev b.bits)
  carry_out, concat res

///////////////////////

let x_eq max_lut a b = 
  let rec interleave a b =
    match a,b with
    | [], [] -> []
    | a::aa, b::bb -> a :: b :: interleave aa bb
    | _ -> failwith "lists are not the same length"
  let rec eq_pairs (a:LutInitAst list) =
    match a with
    | [] -> []
    | a::b::tl -> (a ==~ b) :: eq_pairs tl
    | _ -> failwith "list not even length"
  let rec eq carry (a:Signal) (b:Signal) =
    let max_w = (min a.width (max_lut / 2))                         (* number of bits to process *)
    let lut_args = DigitalLogic.Util.lselect iall 0 ((max_w*2)-1)   (* lut args (twice the number of bits to process) *)
    let lut_fn = List.fold_left (&&&) LutVdd (eq_pairs lut_args)    (* lut function *)
    let carry_out = muxcy carry gnd (x_lut lut_fn (interleave a.[max_w-1 .. 0].bits b.[max_w-1 .. 0].bits))   (* equality / and reduce within lut, put into carry chain *)
    if max_w = a.width then carry_out                               (* recurse to lower bits *)
    else eq carry_out a.[a.width-1 .. max_w] b.[b.width-1 .. max_w]
  eq vdd a b

let x_less a b = fst (x_add_carry (~~~ (i0 ^^^ i1)) vdd b a)

///////////////////////
(* Utilities for building roms and muxes *)

let rec list_select_n cnt max d ds = 
  if cnt = max then d, List.rev ds
  else
    match d with
    | [] -> failwith "Empty mux data."
    | [d] -> list_select_n (cnt+1) max [d] (d::ds)
    | d::tl -> list_select_n (cnt+1) max tl (d::ds)

(* Combine luts with FMUX's *)
let rec x_fmux_luts (sel:Signal) luts = 
  let rec combine_luts cnt luts = 
    match luts with
    | [] -> failwith "Empty lut list."
    | [a] -> a
    | _ ->
      (* combine pairs *)
      let mux = match cnt with 0 -> muxf5 | 1 -> muxf6 | 2 -> muxf7 | _ -> muxf8
      let rec combine_pairs (l:Signal list) = 
        match l with
        | [] -> []
        | a :: [] -> [a] // ???should we do "mux a a sel"???
        | a :: b :: tl -> (concat (List.map2 (fun a b -> mux a b sel.[cnt]) a.bits b.bits)) :: combine_pairs tl
      combine_luts (cnt+1) (combine_pairs luts)
  combine_luts 0 luts

///////////////////////

(* 2:1 mux (4:1 possible in v5?) *)
let x_mux_lut (s : Signal) (d:Signal list) = 
  assert (s.width = 1)
  match d with
  | [] -> failwith "No data to mux"
  | [a] -> a        // ???Is it ok to fall though .. what about fmux???
  | [d0;d1] ->
    assert (d0.width = d1.width)
    concat (List.map2 (fun d0 d1 -> x_lut ((i0 &&& i1) ||| ((~~~i0) &&& i2)) [ s; d1; d0 ]) d0.bits d1.bits)
  | _ -> failwith "Can only fit 2:1 mux in a lut"
  
let x_mux_wide max_mux (s : Signal) (d:Signal list) = 
  assert (s.width <= (max_mux+1))
  if List.length d = 1 then List.hd d
  else if s.width = 1 then x_mux_lut s d
  else
    let num_mux_luts = 1 <<< (s.width-1)
    let rec build_muxs cnt d = 
      if cnt = num_mux_luts then []
      else
        let d_next, d_cur = list_select_n 0 2 d []
        x_mux_lut s.[0] d_cur :: build_muxs (cnt+1) d_next
    let muxs = build_muxs 0 d
    x_fmux_luts s.[s.width-1 .. 1] muxs

let rec x_mux max_mux (s : Signal) (d : Signal list) =
  if List.length d = 1 then List.hd d
  else if s.width <= (max_mux+1) then x_mux_wide max_mux s d
  else
    let num_wide_muxs = 1 <<< (s.width - max_mux - 1)
    let rec build_muxs cnt d =
      if cnt = num_wide_muxs then []
      else
        let d_next, d_cur = list_select_n 0 (1 <<< (max_mux+1)) d []
        x_mux_wide max_mux s.[max_mux .. 0] d_cur :: build_muxs (cnt+1) d_next
    let muxs = build_muxs 0 d
    x_mux max_mux s.[s.width-1 .. max_mux+1] muxs

///////////////////////

/// Build a N-bit rom out of N lookup tables. The depth of the rom is limited to what fits in a single lut.  
/// The lut size is taken from the number of address bits. 
/// An error will occur if there are too many (>6) address bits, or too many data elements.  *)
let x_rom_lut (s:Signal) (d:Signal list) = 
  List.iter (fun (d:Signal) -> assert d.IsConst) d
  let dwidth = (List.hd d).width
  let sel_bits = List.rev s.bits
  let l i = x_lutcn (fold_strings "" (List.rev (List.map (fun (d:Signal) -> String.of_char (string_of_const d).[i]) d))) sel_bits
  concat (List.map (fun i -> l i) [ 0 .. dwidth-1 ])

(** Build rom from multiple luts tying together with muxfX's *)
let x_rom_wide max_lut max_mux (s:Signal) (d:Signal list) =
  if s.width > (max_lut + max_mux) then failwith "Wide rom function too large"
  if s.width <= max_lut then x_rom_lut s d
  else
    let num_luts = 1 <<< (s.width - max_lut)
    (* Build all the luts for the rom *)
    let rec build_luts cnt d = 
      (* Select data for next lut, repeating last element *)
      if cnt = num_luts then []
      else
        let d_next, d_cur = list_select_n 0 (1 <<< max_lut) d []
        x_rom_lut s.[max_lut-1 .. 0] d_cur :: build_luts (cnt+1) d_next
    let luts = build_luts 0 d
    (* Combine luts with FMUX's *)
    x_fmux_luts s.[s.width-1 .. max_lut] luts

(** Build large roms from multiple cascaded x_rom_wide functions *)
let rec x_rom 
  max_lut (* 1 .. 6, depending on family *)
  max_mux (* 0 .. 4, depending on family *)
  (s:Signal) (d:Signal list) = 
  if s.width <= (max_lut + max_mux) then x_rom_wide max_lut max_mux s d
  else
    let num_wide_roms = 1 <<< (s.width - max_lut - max_mux)
    let rec build_roms cnt d =
      if cnt = num_wide_roms then []
      else
        let d_next, d_cur = list_select_n 0 (1 <<< (max_lut + max_mux)) d []
        x_rom_wide max_lut max_mux s.[max_lut+max_mux-1 .. 0] d_cur :: build_roms (cnt+1) d_next
    let muxs = build_roms 0 d
    x_mux max_mux s.[s.width-1 .. max_lut+max_mux] muxs

///////////////////////

(** Register primitive *)
let x_reg clock (reset:Signal) (rst_val:Signal) (enable:Signal) (d:Signal) =
  (* fdce - reset to zero, fdpe - reset to one.  There are other less general register types, but I dont know if they buy us anything. *)
  let w = d.width
  let enable = if enable.IsEmpty then vdd else enable
  let rst_val = if rst_val.IsEmpty then consti w 0 else rst_val
  let reset = if reset.IsEmpty then gnd else reset
  concat 
    (List.map2 (fun (r:Signal) d -> 
      if string_of_const r = "0" 
      then fdce clock enable reset d 
      else fdpe clock enable d reset
    ) rst_val.bits d.bits)

///////////////////////

let x_dist_ram_sp_prim clk we (a:Signal) d = 
  match a.width with
  | 4 -> ram16x1s a.[0] a.[1] a.[2] a.[3] d clk we
  | 5 -> ram32x1s a.[0] a.[1] a.[2] a.[3] a.[4] d clk we
  | 6 -> ram64x1s a.[0] a.[1] a.[2] a.[3] a.[4] a.[5] d clk we
  | 7 -> ram128x1s a.[0] a.[1] a.[2] a.[3] a.[4] a.[5] a.[6] d clk we
  | _ -> failwith "Invalid dist_ram_sp address width"



///////////////////////

let rec x_mul sign (a:Signal) (b:Signal) = 
  let out_width = a.width + b.width
  let ex (a:Signal) = if sign = Signed then a.msb else gnd

  let x_mul_lut a0 a1 b0 b1 carry =
    let o = x_lut ( (i0 &&& i1) ^^^ (i2 &&& i3) ) [ a0; b1; a1; b0 ]
    let a = mult_and a0 b1 
    let c = muxcy carry a o 
    let s = xorcy carry o
    c, s

  let x_mul_2 (a:Signal) (b:Signal) = 
    let a1 = (((ex a) ++ (ex a) ++ a) -- "A1").bits_lsb
    let a0 = (((ex a) ++ a ++ gnd) -- "A0").bits_lsb
    let rec build_pp a0 a1 b0 b1 c = 
      match a0,a1 with
      | [], [] -> []
      | a0::[], a1::[] -> [ snd (x_mul_lut a0 a1 b0 b1 c) ]
      | a0::a0t, a1::a1t -> 
        let c, s = x_mul_lut a0 a1 b0 b1 c 
        s :: build_pp a0t a1t b0 b1 c
      | _ -> failwith "x_mul, adjusted A operands not same sizes"
    concat_lsb (build_pp a0 a1 b.[0] b.[1] gnd)

  let x_mul_1 (a:Signal) (b:Signal) = 
    x_and (ex a ++ ex a ++ a) (b.repeat (a.width+2))  (* XXX and then extend *)
    
  let rec build_products i (a:Signal) (b:Signal) = 
    match b.width with
    | 1 -> [ i, x_mul_1 a b ]
    | 2 -> [ i, x_mul_2 a b.[1..0] ]
    | _ -> (i, (x_mul_2 a b.[1..0])) :: build_products (i+2) a b.msbs.msbs

  let rec adder_tree pp = 
    let rec adder' level pp = 
      match pp with 
      | [] -> []
      | (i,(p:Signal)) :: [] -> 
        [ i, p ++ (gnd.repeat level) ]
      | (i0,p0) :: (i1,p1) :: tl -> 
        (i1, (x_add (((ex p0).repeat level) ++ p0) (p1 ++ (gnd.repeat level)))) :: adder' level tl
    match pp with
    | [] -> failwith "No partial products to add"
    | a :: [] -> a
    | (i0,p0) :: (i1,p1) :: tl -> 
      adder_tree (adder' (i1-i0) pp)

  (* This means the final adder is 1 bit bigger than it should be.  
     I guess this might waste some LUT's.  It occurs when b.width is odd. 
     I cant seem to get the final adjustment right to avoid this. It must
     be fixable though. *)
  (snd (adder_tree (build_products 0 a b))).[out_width-1 .. 0]

(** Unsigned muliplier *)
let x_mulu a b = x_mul Unsigned a b

(** Signed muliplier *)
let x_muls max_mux (a:Signal) (b:Signal) = 
  match b.width with
  | 0 -> failwith "Multiplier a operand has 0 width"
  | 1 -> x_mux max_mux b [ (zero (a.width + b.width)); (x_sub (zero (a.width + b.width)) (a.msb ++ a)) ]
  | n ->
    (* unsigned multiply, with sign extension of partial results *)
    let m = x_mul Signed a b.lsbs 
    (* subtract based on top bit *)
    x_sub (m.msb ++ m) (x_mux max_mux b.msb [ zero (a.width + b.width); a.msb ++ a ++ (zero (b.width-1)) ])  
    
///////////////////////

(** Synthesize a circuit to xilinx components. *)
let x_synthesize outputs = 
  let max_lut, max_mux = 4,4
  
  let fanout_map = connected_nodes_map outputs 
  let fanout (s:Signal) = 
    match Map.tryfind s.uid fanout_map with
    | Some(f,x) -> f
    | None -> 0 (* hmmm.  If a node is an output, the fanout wont be set. Presumably not a problem here, as outputs dont get "synthesized" *)

  (* We maintain a two auxilliary data structures:
     1) a set of signals we have visited updated as the function is entered (ie before each recursion)
     2) a map of node id's to rewritten node id's updated as we exit the function. *)
  let rec synth_node (signal:Signal) set map = 
    let is_visited (signal : Signal) set = Set.mem (signal.uid) set in
    if is_visited signal set then 
      Map.find signal.uid map, set, map
    else
      let set = Set.add (signal.uid) set in
      let r (new_signal : Signal) set map = new_signal, set, Map.add signal.uid new_signal map
      match signal.signal with
      | Signal_empty -> 
        r signal set map
      | Signal_const    (a,w,c)                     -> (* rewrite as vdd/gnd components? May be needed for netlisting. *)
        r signal set map
      | Signal_binop    (a,w,op,s0,s1)              -> 
        let s0, set, map = synth_node s0 set map in
        let s1, set, map = synth_node s1 set map in
        let signal = 
          match op with
          | B_add -> x_add s0 s1
          | B_sub -> x_sub s0 s1
          | B_mulu -> x_mulu s0 s1
          | B_muls -> x_muls max_mux s0 s1
          | B_and -> x_and s0 s1
          | B_or -> x_or s0 s1
          | B_xor -> x_xor s0 s1
          | B_eq -> x_eq max_lut s0 s1
          | B_lt -> x_less s0 s1
          | B_cat -> s0 ++ s1 (* nothing to do here *)
        in
        r signal set map
      | Signal_unop     (a,w,op,s)                  -> 
        let s, set, map = synth_node s set map in
        r (x_not s) set map
      | Signal_wire     (a,w,n,d)                   -> (* Note: we can either create a new wire and assign with <==, or build it directly. 
                                                          In the former case note that names will be mangled.  This should probably be made 
                                                          configurable. *)
        let d, set, map = synth_node !d set map in
        r { su = Signal_wire(signal_incr_uid(),w,ref !n,ref d) } set map
      | Signal_mux      (a,w,sel,d)                 -> 
        let sel, set, map = synth_node sel set map in
        let d, set, map = synth_nodes d set map in
        r (x_mux max_mux sel d) set map
      | Signal_select   (a,hi,lo,s)                 -> 
        let s, set, map = synth_node s set map in
        r s.[hi .. lo] set map
        
      (* Ram with registered output.  To convert to block ram, the fanout of the memory array must be 1 
         (that is nothing else reads it - the memory is only accessed though it's output register). *)
      | Signal_reg      (a,w,clk,rst,rstval,ena,d) when d.IsMem && (fanout d = 1) ->  
        failwith "BLOCK RAM INFERRED RBW.  Not yet implemented."
        
      (* Register *)
      | Signal_reg      (a,w,clk,rst,rstval,ena,d)  ->  (* clocks/resets? *)
        let ena, set, map = synth_node ena set map in
        let d, set, map = synth_node d set map in
        r (x_reg clk rst rstval ena d) set map
      
      (* Ram with registered read/write addresses.  To convert to block ram we will be sucking the read address register into the ram. 
         Thus they cannot be accessed by other logic.  On the other hand the register can be replicated.  This isnt so much of a problem - 
         the register is needed anyway and the address register in the block ram is free anyway. *)
      | Signal_mem      (a,dw,aw,size,clk,w,we,d,r) when r.IsReg -> 
        failwith "BLOCK RAM INFERRED WBR.  Not yet implemented."

      (* Distributed RAM *)
      | Signal_mem      (a,dw,aw,size,clk,w,we,d,r) -> 
        (*
        let w, set = synth_node set w in
        let we, set = synth_node set we in
        let d, set = synth_node set d in
        let r, set = synth_node set r in
        su (Signal_mem(a,dw,aw,size,clk,w,we,d,r)), set
        *)
        failwith "Memories not yet implemented"
      | Signal_behave   (a,w,b,d)                   -> 
        (*
        let d, set = synth_nodes set d in
        su (Signal_behave(a,w,b,d)), set
        *)
        failwith "Behavioual signals not yet implemented"
      | Signal_inst     (a,n,m,g,io,i,o)             ->       (* not sure about io here *)
        (* I think the inputs and inouts will be ok.  However, the output nodes are going to be altered as the wires are changed.  
           I think all outputs must be wires which helps.  So, we need to do some work on wires, if they point to an instantiation,
           and fix the output list.  To do this will require collecting instance information during parsing.  The second pass will look
           like:
           1) Go though all instantiaions.  Look up and remap wires in the output list.  Return (another) new instantiation
           2) Go through wires and find the ones which point to instantiations.  Point them to the correct instantiation node.  This only
              Changes the D reference, not the node itself so the rest of the graph is OK.
           *)
        printf "WARNING: Instatiation may not work properly\n"
        let ii, set, map = synth_nodes (List.map snd i) set map in
        let ioo, set, map = synth_nodes (List.map snd io) set map in
        r { su = Signal_inst(signal_incr_uid(), n, m, g, (List.map2 (fun i ii -> fst i, ii) io ioo), (List.map2 (fun i ii -> fst i, ii) i ii), o) } set map
      | Signal_tri      (a,w,d) -> 
        (*
        let oe = List.map fst d in
        let dd = List.map snd d in
        let oe, set = synth_nodes set oe in
        let dd, set = synth_nodes set dd in
        let d = List.map2 (fun x y -> (x,y)) oe dd in
        su (Signal_tri(a,w,d)), set 
        *)
        failwith "Tristates not yet implemented"

  and synth_nodes signals set map = 
    let signals, set, map = 
      List.fold_left (fun (signals,set,map) signal -> 
        let signal,set,map = synth_node signal set map in
        (signal::signals,set,map)
      ) ([], set, map) signals in
    List.rev signals, set, map
  in
  
  let signals,_,_ = synth_nodes outputs Set.empty Map.empty in
  signals

(* ------------------------------------------------------------------------------ 
                       TEST CODE
   ------------------------------------------------------------------------------ *)

let test() = 
#if LUT_SIM
  xsyn_sim := true
#endif

  let msign = Unsigned
  let bits_a = 6
  let bits_b = 6
  let bits_o = bits_a + bits_b
  let a = input "a" bits_a
  let b = input "b" bits_b
  
(*
  let m = memory 256 clock (input "we" 1) a b c;
  let logic = 
      [
        //a + b + c;
        //Ram.ram_rbw 256 clock (input "we" 1) a b (input "re" 1) c;
        reg clock empty empty (input "re" 1) m;
        m;
      ]
      
  let outputs = List.mapi (fun i x -> output ("o" ^ string i) x) logic

  let c_map = connected_nodes_map outputs
  Map.iter (fun k (f,l) ->
    printf "Node %i fanout %i ->\n  " k f
    List.iter (fun x -> printf "%i " x) l
    printf "\n"
  ) c_map
  printf "%s\n" (any_to_string logic)
  
  let outputs = x_synthesize outputs
*)

  let outputs = 
    [
      //output "o" (x_and a b);
      //output "o" (x_and a b);
      //output "o" (x_not a);
      //output "o" (x_and_reduce 6 a)
      //output "x" (a.reduce (^^^))
      //output "o" (x_sub a b)
      //output "o" (x_rom 4 3 a.[bits-1 .. 0] [ for i in { 0 .. (1<<<bits)-1 } -> consti bits i ])
      //output "o" (x_rom_lut a.[bits-1 .. 0] [ for i in { 0 .. (1<<<bits)-1 } -> consti bits i ])
      //output "o" (x_mux 4 a.[bits-1 .. 0] [ for i in { 0 .. (1<<<bits)-1 } -> consti bits i ])
      //output "o" (x_reg Clock Reset (consti(bits, 10)) Enable a)
      //output "o" (x_eq 4 a b);
      //output "o" (x_lut (i0 &&& i1) [ a; b ])
      //output "o1" (snd (x_add_sub (input "op" 1) a b))
      
      output "o1" ((if msign = Signed then x_muls 4 else x_mulu) a b)
    ]
    
  let circuit = create outputs
  write_file DigitalLogic.Verilog.write "output/" "lut" ".v" circuit
  write_file DigitalLogic.Vhdl.write "output/" "mlut2" ".vhd" circuit
  
#if LUT_SIM
  //let sim, Simulator.SimDataGen(i,w,o) = Simulator.generator_int (Simulator.create_int circuit) 100
  let sim = Simulator.create_int circuit
  
  let a = Simulator.find_input_data sim "a"
  let b = try Simulator.find_input_data sim "b" with _ -> ref 0
  let e = try Simulator.find_input_data sim "enable" with _ -> ref 0
  let o0 = try Simulator.find_output_data sim "o0" with _ -> ref 0
  let o1 = try Simulator.find_output_data sim "o1" with _ -> ref 0
  //let x = Simulator.find_output_data sim "x"


  let se w d = 
    if msign = Unsigned then !d
    else if !d &&& (1<<<(w-1)) <> 0 then !d ||| ((-1) <<< w) else !d
  
  Simulator.sim_reset sim
  e := 1;
  let lo_a, hi_a = 
    if msign = Signed then (-(1<<<(bits_a-1))), ((1<<<(bits_a-1))-1) 
    else 0, ((1<<<bits_a)-1)

  let lo_b, hi_b = 
    if msign = Signed then (-(1<<<(bits_b-1))), ((1<<<(bits_b-1))-1) 
    else 0, ((1<<<bits_b)-1)

  for i=lo_a to hi_a do
    for j=lo_b to hi_b do
      a := i
      b := j
      Simulator.sim_cycle sim
      if (se bits_o o1) <> (i * j) then
        printf "%i * %i = %i (%i)\n" i j (se bits_o o1) (i * j)
        failwith "Invalid multiplication"

#endif  

#if XSYN_TST
do test()
#endif

