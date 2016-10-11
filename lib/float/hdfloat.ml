(** Semi working floating point add and multiply routines.
    There's another weeks work to get these routines sound.
    A few weeks of concerted testing (or lots of feedback)
    to make them robust.
    
    Going forward - get it working roughly with denormals,
    look at rounding/clipping, make denormal's configurable,
    set up regression tests. *)

(*
  Floating point arithmetic.

  description from http://www.psc.edu/general/software/packages/ieee/ieee.html
  - If E=255 and F is nonzero, then V=NaN ("Not a number") (msb = 1 in snan, msb = 0 in qnan)
  - If E=255 and F is zero and S is 1, then V=-Infinity
  - If E=255 and F is zero and S is 0, then V=Infinity
  - If 0<E<255 then V=(-1)**S * 2 ** (E-127) * (1.F) where "1.F" is intended to represent the 
    binary number created by prefixing F with an implicit leading 1 and a binary point.
  - If E=0 and F is nonzero, then V=(-1)**S * 2 ** (-126) * (0.F) These are "de-normal" values.
  - If E=0 and F is zero and S is 1, then V=-0
  - If E=0 and F is zero and S is 0, then V=0 

  http://tima-cmp.imag.fr/~guyot/Cours/Oparithm/english/Flottan.htm
  some of the logic derived from this excellent page

  E = 11111111 = 255 => E = 255 - 127 =  128 = 0 10000000  *** inf or NaN
  E = 11111110 = 254 => E = 254 - 127 =  127 = 0 01111111
  E = 11111101 = 253 => E = 253 - 127 =  126 = 0 01111110
                          ...
  E = 10000001 = 129 => E = 129 - 127 =    2 = 0 00000010
  E = 10000000 = 128 => E = 128 - 127 =    1 = 0 00000001
  E = 01111111 = 127 => E = 127 - 127 =    0 = 0 00000000
  E = 01111110 = 126 => E = 126 - 127 =   -1 = 1 11111111
  E = 01111101 = 125 => E = 125 - 127 =   -2 = 1 11111110
                          ...
  E = 00000010 =   2 => E =   2 - 127 = -125 = 1 10000010
  E = 00000001 =   1 => E =   1 - 127 = -126 = 1 10000001
  E = 00000000 =   0 => E =   0 - 127 = -127 = 1 10000000  *** denormal, 0 or -0

  so (if we ignore denormal)...
  E - BIAS is
  let E = (E + 1)
  let E = (~: (msb E)) ++ (lsbs E)
*)

open Hdcaml
open Hdutil
open Hdshift
open Design
open Hdfixed
open List
open Printf

(****************************************************************************************************************)
(****************************************************************************************************************)

module type FloatSpec =
sig
  val exponent : int
  val mantissa : int
  val denormals : bool
end

(****************************************************************************************************************)
(****************************************************************************************************************)

module Float =
functor (S : FloatSpec) ->
struct

  exception Float_error of string

  (* set up basic definitions *)
  let exp = S.exponent 
  let man = S.mantissa
  let num_bits = exp + man + 1
  let denormals = S.denormals

  (* constants *)

  (* +ve limits *)
  let max_positive = 
    let e = String.make exp '1' in e.[exp-1] <- '0';
    let m = String.make man '0' in
    constf ("0" ^ e) m 

  let min_positive = 
    let e = String.make exp '0' in e.[exp-1] <- '1';
    let m = String.make man '0' in
    constf ("0" ^ e) m 

  (* -ve limits *)
  let max_negative = 
    let e = String.make exp '1' in e.[exp-1] <- '0';
    let m = String.make man '0' in
    constf ("1" ^ e) m 

  let min_negative = 
    let e = String.make exp '0' in e.[0] <- '1';
    let m = String.make man '0' in
    constf ("1" ^ e) m 

  (* zero *)
  let zero_positive = 
    let e = String.make exp '0' in e.[0] <- '1';
    let m = String.make man '0' in
    constf ("0" ^ e) m 

  let zero_negative = 
    let e = String.make exp '0' in e.[0] <- '1';
    let m = String.make man '0' in
    constf ("1" ^ e) m 

  (* infinity (are these all the possible cases? do the mantissa bits matter?) *)
  let inf_positive =
    let e = String.make exp '1' in 
    let m = String.make man '0' in
    constf ("0" ^ e) m 

  let inf_negative =
    let e = String.make exp '1' in 
    let m = String.make man '0' in
    constf ("1" ^ e) m 

  (* not a number *)
  let snan = 
    let e = String.make exp '1' in 
    let m = String.make man '0' in m.[0] <- '1';  (* msb = 1 *)
    constf ("0" ^ e) m 

  let qnan = 
    let e = String.make exp '1' in 
    let m = String.make man '0' in m.[1] <- '1';  (* mbs = 0 *)
    constf ("0" ^ e) m 

  let exp_bias =
    let e = String.make exp '1' in e.[0] <- '0';
    const e 
        
  let exp_one = 
    let e = String.make exp '0' in e.[exp-1] <- '1';
    const e

  (* Selecting parts of the number *)
  let get_sign a = bitf a exp
  let get_exp a = selectf a (exp-1) 0
  let get_man a = selectf a (-1) (-man)

  (* inf or nan *)

  let is_inf a = 
    let exp = (get_exp a) ==: (ones exp) in
    let man = (get_man a) /=: (one man) in
    exp &: man

  let is_nan a = 
    let exp = (get_exp a) ==: (ones exp) in
    let man = (get_man a) /=: (zero man) in
    exp &: man

  let nan_is_snan a = (msb (get_man a)) ==: gnd
  let nan_is_qnan a = (msb (get_man a)) ==: vdd

  let is_snan a = (is_nan a) &: (nan_is_snan a)
  let is_qnan a = (is_nan a) &: (nan_is_qnan a)

  let is_zero a = 
    let exp = (get_exp a) ==: (zero exp) in
    let man = (get_man a) ==: (zero man) in
    exp &: man

  (* counts the number of leading zeros in the input vector.  No idea how it really works *)
  let count_leading_zeros vector = 

    let llen = clog2 ((width vector)-1) in
    if (pow2 llen) != (width vector) then 
      raise (Float_error ("count_leading_zeros: input vector must be a power of two in length"));

    (* the magic cells... *)
    let cell2 a b = [ (a &: (~: b)); (a &: b) ] in
    let celln ca a b = [ a |: (ca &: b) ] in
    let rec build_cell msb al bl = (* al, bl same length, start from lsb *)
      match length al with
      | 1 -> cell2 (hd al) (hd bl)
      | _ -> 
        let a,b = hd al, hd bl in
        (celln msb a b) @ (build_cell msb (tl al) (tl bl)) in
    let build_cell al bl = rev (build_cell (hd al) (rev al) (rev bl)) in

    (* [a;b;c;d;...] -> [(a,b);(c;d);...] *)
    let rec pair_list l = 
      match l with
      | [] -> []
      | [a] -> raise (Float_error ("pair_of_lists: input list must be of even length"))
      | a :: b :: tl -> (a,b) :: (pair_list tl) in

    (* pair the list, apply the cells which do their magic, which gives a list which is then paired and the process repeated.
       Basically a tree is contructed in which pairs are taken from the list which start off as one bit each.  This yields a 
       list of 2 bit results.  At the next level two bit pairs yield 3 bit results, and so on until there is one result. *)
    let rec build_stage bits = 
      match bits with
      | [(al,bl)] -> Design.concat (build_cell al bl)
      | _ -> build_stage (pair_list (map (fun (al,bl) -> build_cell al bl) bits)) in

    build_stage 
      (pair_list (map (fun x -> [x]) (bits (~: vector))))

  let add a b = 

    (* get the clipped difference between exponents and bits indicating which is larger and equality *)
    let decode_exp a_exp b_exp = 
      let diff = (ue a_exp) -: (ue b_exp) in
      let a_lt_b = msb diff in
      let a_ne_b = reduce (|:) (bits diff) in (* might be a little faster to compute: reduce (|:) (a_exp xor b_exp), or simply (a_exp /=: b_exp) *)
      let absdiff = abs diff in
      (* clip absdiff to max shift width, which is based on size of mantissa (plus guard bits) *)
      let len_ad = width absdiff in
      let lman = clog2 (man+3) in (* +3 for 1 bit to left, 2 bits to right (guard and round).  Doesnt include sticky (I think) *)
      let absdiff = 
        if lman = len_ad then absdiff
        else if lman > len_ad then (zero (len_ad - lman)) ++ absdiff
        else 
          let top_bits = select absdiff (len_ad-1) lman in
          mux2 (top_bits ==: (zero (width top_bits))) (select absdiff (lman-1) 0) (ones lman) in
      absdiff, a_lt_b, a_ne_b in

    (* builds a right shift circuit and or's together all bits that are discarded *)
    let mantissa_shift_right mantissa shift = 

      let build_shift_stage ctrl bits shift_by sticky_in = 
        let num_bits = length bits in
        let bits_shifted_out = lselect bits (num_bits-shift_by) (num_bits-1) in
        let sticky_bit = ((reduce_1 (|:) bits_shifted_out) &: ctrl) |: sticky_in in
        let bits_shifted = map 
          (fun x -> mux2 ctrl (if (x-shift_by) < 0 then gnd else nth bits (x-shift_by)) 
          (nth bits x)) (range num_bits) in
        bits_shifted, sticky_bit in

      let rec build_all_stages shift_bits (data,sticky) shift_by = 
        if shift_bits = [] then data,sticky
        else
          build_all_stages (tl shift_bits) (build_shift_stage (hd shift_bits) data shift_by sticky) (shift_by * 2) in

      let shifted,sticky = build_all_stages (rev (bits shift)) (bits mantissa, gnd) 1 in
      Design.concat shifted, sticky in

    let mantissa_swap a_lt_b a_ne_b a_man b_man = 
      let m_lt = a_man <: b_man in
      let a_lt_b = mux2 a_ne_b a_lt_b m_lt in (* if exp's equal then check mantissas. otherwise exp says which one *)
      let smaller = mux2 a_lt_b a_man b_man in
      let larger = mux2 a_lt_b b_man a_man in
      smaller, larger, a_lt_b in

    let mantissa_shift_normalise mantissa = 
      let shifted_right1 = lsbs (mantissa >>: 1) in  (* drop top bit *)
      let leading_zeros = count_leading_zeros (pad_right_pow2 (lsbs mantissa) vdd) in (* XXX how far should we actually count ??? *)
      let shifted_left = shift_left (lsbs mantissa) leading_zeros in
      let msb = msb mantissa in   (* shift right *)
      mux2 msb shifted_right1 shifted_left, msb, leading_zeros in

    let mantissa_result_normalise_and_round mantissa = 
        let normalised, right, leading_zeros = mantissa_shift_normalise mantissa in
        let normalised = lsbs normalised in (* drop implicit 1 bit *)
        let rounded = resize overflow_saturate round_ieee754 Unsigned {f_vec=normalised; f_fix=3} man 0 in (* XXX does this perform clipping correctly? *)
        rounded.f_vec, mux2 right (ones ((width leading_zeros)+1)) (gnd ++ leading_zeros) in

    let exponent_result exp_a_lt_b ea eb leading_zeros =
      let elen = width ea in
      let zlen = width leading_zeros in
      let leading_zeros = 
        if elen = zlen then leading_zeros
        else if elen > zlen then (repeat (msb leading_zeros) (elen-zlen)) ++ leading_zeros
        else select leading_zeros (elen-1) 0 in
      (mux2 exp_a_lt_b eb ea) -: leading_zeros in

    let sa, sb = get_sign a, get_sign b in
    let ea, eb = get_exp a, get_exp b in
    let ma, mb = get_man a, get_man b in

    let exp_absdiff, exp_a_lt_b, exp_a_ne_b = decode_exp ea eb in
    let mantissa_smaller, mantissa_larger, a_lt_b = mantissa_swap exp_a_lt_b exp_a_ne_b (vdd ++ ma) (vdd ++ mb) in        
    let mantissa_smaller_shifted, sticky = mantissa_shift_right (mantissa_smaller ++ (const "00")) exp_absdiff in
                
    let mantissa_smaller_shifted = gnd ++ mantissa_smaller_shifted ++ sticky in
    let mantissa_smaller_shifted = mux2 (sa ^: sb) (~: mantissa_smaller_shifted) mantissa_smaller_shifted in

    let mantissa_larger = gnd ++ mantissa_larger ++ const "000" in
    let added = mantissa_smaller_shifted +: mantissa_larger in

    let result_sign = mux2 a_lt_b sb sa in
    let mantissa, leading_zeros = mantissa_result_normalise_and_round added in
    let exponent = exponent_result exp_a_lt_b ea eb leading_zeros in
    { f_vec = result_sign ++ exponent ++ mantissa; f_fix = exp }
        
  let sub a b = 
      let b = ( (~: (get_sign b)) ++ (get_exp b) ++ (get_man b) ) in
      add a {f_vec=b; f_fix=man}
    
  (* About denormals.  Usually the mantissa is 1.xxxx * 1.xxxx which leads to 01.xxx or 10.xxxx or 11.xxx.
    In the later two cases we shift the mantissa one place to the right and add one to the exponent.
    In the case of denormal numbers we can end up with another case - 00.xxxx.  Now we need to shift the 
    mantissa to the right as many places as needed until we have renormalized the number.  This will depends
    on the size of the (denormal) input arguments.
    
    For input nans, it not quite clear what we have to do.  The pentium seems to propogate them by copying 
    one of the nans through and silently converting them to a signalling nan. *) 
  let mul a b =
    
    (* Remove bias from exponents, sum and add in offset (controlled by shift right of matissa).
       Check for overflow/underflow, and clip if required. *)
    let exponent_mul ea eb eoff ea_denormal eb_denormal = 
        
      (* subtract the bias from the exponent to get a signed representation of it. If denomal just flip top bit. *)
      let subtract_exp_bias e denormal = 
        let e = e +: (mux2 denormal (zero exp) (one exp)) in
        (~: (msb e)) ++ (lsbs e) in

      (* add the bias to get the exponent *)
      let add_exp_bias e = 
        let e = (~: (msb e)) ++ (lsbs e) in
        e -: (one exp) in
        
      (* subtract bias from exponents *)
      let ea, eb = subtract_exp_bias ea ea_denormal, subtract_exp_bias eb eb_denormal in
      let ea, eb = ea -- "ea", eb -- "eb" in
      (* signed sum of exponents with offset *)
      let e = (se ea) +: (se eb) +: (mux2 eoff (one (exp+1)) (zero (exp+1))) in
      let e = e -- "exponent" in
      let e_top = select e ((width e)-1) ((width e)-2) in
      let overflow  = ( ~: (msb e_top)) &: (lsb e_top) in (* 01 => > 128 *)
      let underflow = ( ~: (lsb e_top)) &: (msb e_top) in (* 10 => < -127, can still be -127 which is denormal 
                                                             and probably requires work on the mantissa *)
      (* drop top bit *)
      let e = lsbs e in
      (* add bias back into exponent and deal with overflow *)
      let e_bias = mux2 overflow (ones exp) (mux2 underflow (zero exp) (add_exp_bias e)) in
      overflow, underflow, e_bias in
      
    let handle_nans ea_nan ea_inf eb_nan eb_inf sign exponent mantissa = 
      mux2 (ea_nan |: eb_nan) snan.f_vec (sign ++ exponent ++ mantissa) in
            
    let sa, sb = get_sign a, get_sign b in
    let ea, eb = get_exp a, get_exp b in
    let ma, mb = get_man a, get_man b in
    let sign = (sa ^: sb) in

    (* get class of exponent *)
    let ea_denormal = if denormals then ea ==: (zero (width ea)) else gnd in
    let eb_denormal = if denormals then eb ==: (zero (width eb)) else gnd in
    let ma_zero = ma ==: zero man in
    let mb_zero = mb ==: zero man in
    let ea_ones = ea ==: ones exp in
    let eb_ones = eb ==: ones exp in
    let ea_nan  = ea_ones &: (~: ma_zero) in
    let eb_nan  = eb_ones &: (~: mb_zero) in
    let ea_inf  = ea_ones &: ma_zero in
    let eb_inf  = eb_ones &: mb_zero in
      
    let ea_denormal, eb_denormal = ea_denormal -- "ea_denormal", eb_denormal -- "eb_denormal" in 

    (* multiply mantissa's *)
    let mantissa = ((~: ea_denormal) ++ ma) *: ((~: eb_denormal) ++ mb) in
    let mantissa = mantissa -- "mantissa" in
    let right_shift = msb mantissa in
    let mantissa = mux2 (right_shift -- "right_shift") (mantissa >>: 1) mantissa in
        
    (* Renormalisation for denormals (when result isnt denormal itself).  
       Dont need to shift the entire multiplication result, 
       but that should drop off after synthesis anyway *)
    let mantissa = 
      if denormals then
        let leading_zeros = count_leading_zeros (pad_right_pow2 (lsbs mantissa) vdd) in 
        let shifted_left = shift_left (lsbs mantissa) (leading_zeros -- "leading_zeros") in 
        lsbs (shifted_left)
      else
        lsbs (lsbs mantissa) in
        
    let mantissa = mantissa  -- "mantissa_shifted"in
    let overflow, underflow, exponent = exponent_mul ea eb right_shift ea_denormal eb_denormal in
    let overflow, underflow = overflow -- "overflow", underflow  -- "underflow"in

    (* select mantissa + guard bits, get sticky bit *)
    let mantissa = select mantissa ((width mantissa)-1) (man-2) in
    let sticky = reduce (|:) (bits (select mantissa (man-3) 0)) in        
    let mantissa = mantissa ++ sticky in
    let rounded = resize overflow_saturate round_ieee754 Unsigned {f_vec=mantissa; f_fix=3} man 0 in 
    let rounded = mux2 (overflow |: underflow) (zero man) rounded.f_vec in
    let result = handle_nans ea_nan ea_inf eb_nan eb_inf sign exponent rounded in
    { f_vec = result; f_fix = exp }
 
  let string_of_const s = 
    let s = s.f_vec in
    match s with
    | Circuit.Signal_const(_,s) -> (String.sub s 0 1) ^ " " ^ (String.sub s 1 exp) ^ " " ^ (String.sub s (exp+1) man) 
    | _ -> raise (Float_error ("signal not constant in string_of_const"))


end

(****************************************************************************************************************)
(* standard floating point types *)
(****************************************************************************************************************)

module FloatSpec32 =
struct 
  let exponent = 8
  let mantissa = 23
  let denormals = true
end

module FloatSpec64 =
struct 
  let exponent = 11
  let mantissa = 52
  let denormals = true
end

module FloatSpec128 = 
struct 
  let exponent = 15
  let mantissa = 112
  let denormals = true
end

(****************************************************************************************************************)
(****************************************************************************************************************)
