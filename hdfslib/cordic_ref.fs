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

let pi = 3.1415926535897932384626433832795 

(****************************************************************************************************)
(****************************************************************************************************)

let rec icordic0 i iters x y z vecmode = 
  if i = iters then (x,y,z)
  else
    let t = (2.0 ** (float (-i))) in
    if ((vecmode >= 0.0 && y < 0.0) || (vecmode < 0.0  && z >= 0.0)) then
      icordic0 (i+1) iters x (y + (x * t)) (z - t) vecmode
    else 
      icordic0 (i+1) iters x (y - (x * t)) (z + t) vecmode 

let rec icordic1 i iters x y z vecmode = 
    (* 2 to the power -i (not complex) *)
    let atan i = atan (2.0 ** (float (-i))) in
    if i = iters then (x,y,z)
    else
        let t = (2.0 ** (float (-i))) in
        if ((vecmode >= 0.0 && y < vecmode) || (vecmode < 0.0  && z >= 0.0)) then
            icordic1 (i+1) iters (x - (y * t)) (y + (x * t)) (z - (atan i)) vecmode
        else 
            icordic1 (i+1) iters (x + (y * t)) (y - (x * t)) (z + (atan i)) vecmode 

let rec icordic2 i iters x y z vecmode = 
(* (1/2) * log2 ((1+t) / (1-t)) *)
  let atanh i = 
    let t = (2.0 ** (float (-(i+1)))) in
    0.5 * (log ((1.0 + t) / (1.0 - t))) in
  let cdc i x y z vecmode = 
    let t = (2.0 ** (float (-(i+1)))) in
    if ((vecmode >= 0.0 && y < 0.0) || (vecmode < 0.0 && z >= 0.0)) then
      x + (y * t), y + (x * t), z - (atanh i)
    else 
      x - (y * t), y - (x * t), z + (atanh i) in
  if i = iters then (x,y,z)
  else
    let x,y,z = 
      if (i > 0) && ((i % 3) = 0) then 
        let x, y, z = cdc i x y z vecmode in
        cdc i x y z vecmode
      else
        cdc i x y z vecmode in
    icordic2 (i+1) iters x y z vecmode 

let cordic0 = icordic0 0 
let cordic1 = icordic1 0 
let cordic2 = icordic2 0 

let x c = let x,y,z = c in x 
let y c = let x,y,z = c in y 
let z c = let x,y,z = c in z 

let invGain1 iters = 1.0 / (x (cordic1 iters 1.0 0.0 0.0 (-1.0))) 
let invGain2 iters = 1.0 / (x (cordic2 iters 1.0 0.0 0.0 (-1.0))) 

(****************************************************************************************************)
(****************************************************************************************************)

let prerotate0 x y z = 
    let d = if y < 0.0 then 1.0 else -1.0 in
    - (d * y), (d * x), (z - (d * pi / 2.0))

let prerotate1 x y z = 
    let d = if x < 0.0 then -1.0 else 1.0 in
    (d * x), (d * y), if x < 0.0 then z - pi else z 

//let prerotate = prerotate0
let prerotate x y z = x,y,z

let mul iters a b = y (cordic0 iters a 0.0 b (-1.0))

let div iters a b = y (cordic0 iters b a 0.0 0.0)

let atan iters a = z (cordic1 iters 1.0 a 0.0 0.0)

let sincos iters a = 
    let xn,yn,zn = prerotate (invGain1 iters) 0.0 a in
    let x,y,z = cordic1 iters xn yn zn (-1.0) in
    y, x 

let tan iters a = 
    let sin,cos = sincos iters a in
    sin / cos 

let asin iters a = 
    let x,y,z = cordic1 iters (invGain1 iters) 0.0 0.0 (if a < 0.0 then (- a) else a) in
    if a < 0.0 then (- z) else z 

let magphase iters x y = 
    let xn,yn,zn = prerotate x y 0.0 in
    let mag,_,phase = cordic1 iters xn yn zn 0.0 in
    mag, phase 

let polar_to_rect iters m p = 
    let x,y,z = cordic1 iters m 0.0 p (-1.0) in
    y, x 

let sinhcosh iters a = 
    let x,y,z = cordic2 iters (invGain2 iters) 0.0 a (-1.0) in
    y, x 

let tanh iters a = 
    let sinh,cosh = sinhcosh iters a in
    sinh / cosh 

let atanh iters a = z (cordic2 iters 1.0 a 0.0 0.0) 

let log iters a = 2.0 * (z (cordic2 iters (a + 1.0) (a - 1.0) 0.0 0.0)) 

let sqrt iters a = (invGain2 iters) * (x (cordic2 iters (a + 0.25) (a - 0.25) 0.0 0.0)) 

let exp iters a = 
    let sinh,cosh = sinhcosh iters a in
    sinh + cosh 
