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

(** Fully elaborated fft algorithm. *)
module DigitalLogic.Circuits.Fft

open DigitalLogic.Numeric.Ops
open DigitalLogic.Signal
open DigitalLogic.Util
open List
open Microsoft.FSharp.Math

let pi = 3.1415926535897932384626433832795 

let wn n = 
  let f = 2.0 * pi / n in
  Complex.Create(cos f, - (sin f))

exception FFT_error of string

let even_odd l = 
  let rec even_odd l = 
    match l with 
    | [] -> []
    | a::b::tl -> (a,b)::(even_odd tl)
    | _ -> raise (FFT_error ("Invalid length in even_odd - should be even")) in
  List.unzip (even_odd l) 

(* Warning: Temporarily hacked out of the ocaml library which is probably not a good idea *)
let norm (x : Complex) =
  (* Watch out for overflow in computing re^2 + im^2 *)
  let r = Microsoft.FSharp.Core.Operators.abs (x.RealPart) in
  let i = Microsoft.FSharp.Core.Operators.abs (x.ImaginaryPart) in
  if r = 0.0 then i
  else if i = 0.0 then r
  else if r >= i then
    let q = i / r in r * sqrt(1.0 + q * q)
  else
    let q = r / i in i * sqrt(1.0 + q * q)
let exp (x : Complex) =
  let e = exp x.RealPart in 
  Complex.Create(e * cos x.ImaginaryPart, e * sin x.ImaginaryPart )
let log (x : Complex) = 
  Complex.Create(log (norm x), atan2 x.ImaginaryPart x.RealPart)
let pow x y = exp (y * (log x))

(* This is complex ^ real_part - can it be done differently? yes, r is 0..1..2..3 etc so a multiply will do *)
let c_pow c r = pow c (Complex.Create(r, 0.0))

(** Simple reference implementation of a FFT *)
let rec fft_ref inputs w = 

  let n = length inputs in
    
  if n > 1 then
  begin
    let even,odd = even_odd inputs in
    let q = fft_ref even (c_pow w 2.0) in
    let t = fft_ref odd (c_pow w 2.0) in
    let rec sum k q t = 
      if k = n then []
      else
      begin
        let qk = nth q (k % (n/2)) in
        let tk = nth t (k % (n/2)) in
        let kf = float k in
        let wf = c_pow w kf in
        let rs = (qk + (wf * tk)) in
        (* Printf.printf "q[%.1f %.1f] + w[%.1f %.1f] * t[%.1f %.1f] = [%.1f %.1f]\n" qk.re qk.im wf.re wf.im tk.re tk.im rs.re rs.im; *)
        (rs :: (sum (k+1) q t))
      end in
    sum 0 q t
  end
  else
    inputs

(** Hardware fft implementation *)
let rec fft inputs w wbits = 
  let n = length inputs in

  let re = fst in
  let im = snd in

  let cmulw w b = 
    let l = width (re b) in
    
    let rr = (select ((re w) *+ (re b)) (l+wbits-3) (wbits - 2)) in
    let ii = (select ((im w) *+ (im b)) (l+wbits-3) (wbits - 2)) in
    let ri = (select ((re w) *+ (im b)) (l+wbits-3) (wbits - 2)) in
    let ir = (select ((im w) *+ (re b)) (l+wbits-3) (wbits - 2)) in

    let r = rr - ii in
    let i = ri + ir in
    r, i in

  let cadd a b = 
    ( (re a) + (re b), (im a) + (im b) ) in

  let cw (w : Complex) wbits =
    let scalei = (pow2 (wbits-2)) in
    let scalef = float scalei in
    let r = int (scalef * w.RealPart) in
    let i = int (scalef * w.ImaginaryPart) in
    consti wbits r, consti wbits i in
  
  if n > 1 then
  begin
    let even,odd = even_odd inputs in
    let q = fft even (c_pow w 2.0) wbits in
    let t = fft odd (c_pow w 2.0) wbits in
    let rec sum k q t = 
      if k = n then []
      else
      begin
        let qk = nth q (k % (n/2)) in
        let tk = nth t (k % (n/2)) in
        let kf = float k in
        let wf = c_pow w kf in
        let rr,ir = (cadd qk (cmulw (cw wf wbits) tk)) in
        let rr,ir = regc vdd rr, regc vdd ir in
        ((rr,ir) :: (sum (k+1) q t))
      end in
    sum 0 q t
  end
  else
    inputs


