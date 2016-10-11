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

(** Cordic circuit generator. *)
module DigitalLogic.Circuits.Cordic

open DigitalLogic.Numeric.Ops
open DigitalLogic.Signal
open DigitalLogic.Util

/// <P>Cordic calculation type</P>
/// <ul>
/// <li><b>Cordic0</b> simplified cordic calculation (x is not updated)</li>
/// <li><b>Cordic1</b> standard cordic equation</li>
/// <li><b>Cordic2</b> hyperbolic</li>
/// </ul>  
type cordic_type_t =  Cordic0 | Cordic1 | Cordic2    

/// <P>Type of cordic circuit to generate</P>
/// <ul>
/// <li><b>CordicComb</b> combinatorial</li>
/// <li><b>CordicSeq</b> Fully unrolled</li>
/// <li><b>CordicIter</b> Iterative</li>
/// </ul>  
type cordic_gen_t = 
      CordicComb of cordic_type_t   
    | CordicSeq of cordic_type_t    
    | CordicIter of cordic_type_t   

/// <P>Parameterized cordic circuit generation.<P>
/// <P>Inputs.<P>
/// <ul>
///   <LI><B>pre</B>       Pre-rotation mode.  -1 none, 0 or 1 different rotation modes</LI>
///   <LI><B>cdc_type</B>  Type of cordic to build</LI>
///   <LI><B>fix</B>       fix point precision of arithmetic</LI>
///   <LI><B>iters</B>     number of iterations</LI>
///   <LI><B>ena</B>       register enable</LI>
///   <LI><B>vld_in</B>    input valid signal</LI>
///   <LI><B>x</B>         input data</LI>
///   <LI><B>y</B>         input data</LI>
///   <LI><B>z</B>         input data</LI>
///   <LI><B>vecmode</B>   vectoring mode</LI>
/// </ul>
/// <P>outputs<P>
/// <ul>
///   <LI><B>x</B>         output data</LI>
///   <LI><B>y</B>         output data</LI>
///   <LI><B>z</B>         output data</LI>
///   <LI><B>vld_out</B>   output valid signal</LI>
/// </ul>
let cordic pre cdc_type fix iters clock reset ena vld_in x y z vecmode = 

  let pi = 3.1415926535897932384626433832795 in

  (* 2 to the power -i *)
  let atan i = atan (2.0 ** (float (-i)))  in

  (* (1/2) * log2 ((1+t) / (1-t)) *)
  let atanh i = 
    let t = (2.0 ** (float (-(i+1)))) in
    0.5 * (log ((1.0 + t) / (1.0 - t))) in

  let pipeline x = match cdc_type with CordicComb _ -> x | _ -> reg clock reset empty ena x in

  let bits = width x in
  let scale = 2.0 ** (float (bits-fix)) in
  let fconst f = consti bits (int (scale * f)) in

  let shift_mux hyper addr v = 
    let range = if hyper then List.tl (range (iters+1)) else (range iters) in
    mux addr (List.map (fun x -> v >>+ x) range) in

  (* iter -> index map for hyperbolic functions *)
  let index_map iters = 
    let rec imap i j =
      let iters_left = iters - i in
      if (i = iters) then []
      else if (j > 0) && ((j % 3) = 0) && (iters_left > 1) then
        [ j; j ] @ imap (i+2) (j+1)
      else 
        [ j ] @ imap (i+1) (j+1) in
    imap 0 0 in

  let prerotate0 x y z =  
    (* XXX check this function :
      let n = neg (mux2 d x y) this was included ... looks
      like it was rewritten and left behind *)
    let bits = width x in
    let d = y <+ (zero bits) in
    let neg x = (zero bits) - x in
    mux2 d (neg y) y, mux2 d x (neg x), z - (mux2 d (fconst (pi / 2.0)) (fconst (pi / -2.0))) in

  let prerotate1 x y z = 
    let bits = width x in
    let d = x <+ (zero bits) in
    let neg x = (zero bits) - x in
    mux2 d (neg x) x, mux2 d (neg y) y, mux2 d (z - (fconst pi)) z in

  (* pre-rotation mode *)
  let x,y,z = 
    match pre with
    | -1 -> x,y,z
    |  0 -> prerotate0 x y z
    |  1 -> prerotate1 x y z
    | _ -> failwith "Invalid pre-rotatation mode" in

  let cordic0 x y z vecmode xt yt t = 
    let c = ( ( (vecmode >=+ (zero bits)) &&& (y <+ (zero bits)) ) ||| ( (vecmode <+ (zero bits)) &&& (z >=+ (zero bits)) ) ) in
    x, (mux2 c (y + xt) (y - xt)), (mux2 c (z - t) (z + t)) in

  let cordic1 x y z vecmode xt yt t = 
    let c = ( ( (vecmode >=+ (zero bits)) &&& (y <+ vecmode) ) ||| ( (vecmode <+ (zero bits)) &&& (z >=+ (zero bits)) ) ) in
    (mux2 c (x - yt) (x + yt)), (mux2 c (y + xt) (y - xt)), (mux2 c (z - t) (z + t)) in

  let cordic2 x y z vecmode xt yt t = 
    let c = ( ( (vecmode >=+ (zero bits)) &&& (y <+ (zero bits)) ) ||| ( (vecmode <+ (zero bits)) &&& (z >=+ (zero bits)) ) ) in
    (mux2 c (x + yt) (x - yt)), (mux2 c (y + xt) (y - xt)), (mux2 c (z - t) (z + t)) in

  let cordic_full cordic x y z vecmode tab hyper =
    let imap = if hyper then (index_map iters) else (range iters) in
    let rec cf i vld x y z vecmode = 
      if i = iters then
        (x,y,z,vld)
      else
        let idx = List.nth imap i in
        let idx_sft = if hyper then idx + 1 else idx in
        let x,y,z = cordic x y z vecmode (x >>+ idx_sft) (y >>+ idx_sft) (List.nth tab idx) in
        let x,y,z,vld,vecmode = pipeline x, pipeline y, pipeline z, pipeline vld, pipeline vecmode in
        cf (i+1) vld x y z vecmode in
    cf 0 vld_in x y z vecmode in

  let cordic_iter cordic ix iy iz vecmode tab hyper =
    let bits = width ix in
    let iter_bits = clog2 (iters-1) in
    (* counter used to control the iteration and the rom address *)
    let count_next = wire iter_bits in
    let count = pipeline (mux2 vld_in (zero iter_bits) count_next) in
      count_next <== count + (one iter_bits);
    (* remap count for hyperbolic functions in needed *)
    let idx = if hyper then 
                mux count (List.map (fun x -> consti (width count) x) (index_map iters)) 
              else count in
    (* cordic circuit *)
    let sx,sy,sz = wire bits, wire bits, wire bits in
    let rx,ry,rz = pipeline (mux2 vld_in ix sx), pipeline (mux2 vld_in iy sy), pipeline (mux2 vld_in iz sz) in
    let nx,ny = shift_mux hyper idx rx, shift_mux hyper idx ry in 
    let tab = mux idx tab in 
    let cx,cy,cz = cordic rx ry rz vecmode nx ny tab in
      sx <== cx; sy <== cy; sz <== cz; 
    let vld_out = (count ==~ (iters-1)) in
    cx, cy, cz, vld_out in

  let tab0 = List.map (fun x -> fconst (2.0 ** (float (-x)))) (range iters) in
  let tab1 = List.map (fun x -> fconst (atan x)) (range iters) in
  let tab2 = List.map (fun x -> fconst (atanh x)) (range iters) in

  let fn, cdc, tab, hyper = 
    let sel_cdc fn typ = 
      match typ with 
      | Cordic0 -> fn, cordic0, tab0, false
      | Cordic1 -> fn, cordic1, tab1, false 
      | Cordic2 -> fn, cordic2, tab2, true in
    match cdc_type with
    | CordicComb cdc -> sel_cdc cordic_full cdc
    | CordicSeq  cdc -> sel_cdc cordic_full cdc
    | CordicIter cdc -> sel_cdc cordic_iter cdc in

  fn cdc x y z vecmode tab hyper

