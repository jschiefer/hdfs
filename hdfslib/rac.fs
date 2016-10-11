#light "off"
(** Rom-accumulator.  Implements the equation A0.x0 + A1.x1. + ... + AN.xN using shifts and adds. *)
(** Update: 6/2/2007 - the signed designator now refers only to the data (and thus the requirement for *)
(** an adder and a subtractor) and not the rom coeffs which can be signed even when the data is not.  *)
(** This may alter the number of accumulator bits required for truly unsigned racs *)
module DigitalLogic.Circuits.Rac

open DigitalLogic.Numeric.Ops
open DigitalLogic.Signal
open DigitalLogic.Util

(** The number of bits required to represent all numbers from a .. b (or b .. a).  Tries to account for sign correctly.  *)
let num_bits_range a b = 
    let mn = min a b in
    let mx = max a b in
    let mns = mn < 0 in
    let mxs = mx < 0 in
    let mnb = num_bits mn in
    let mxb = num_bits mx in
    match mns, mxs with
    | false, false 
    | true, true -> max mnb mxb
    | true, false -> max mnb (mxb+1)
    | _ -> failwith "Logic error determining bit range"

(** Rom builder.  Limited to 32 bit precision at the moment *)
let build_rom coeffs = 
    let rec rom_value idx addr = 
        if addr > 0ul then
            (if addr &&& 1ul = 1ul then (List.nth coeffs idx) else 0) + (rom_value (idx + 1) (addr >>> 1))
        else 0 in
    List.map (rom_value 0) [ 0ul .. (1ul <<< List.length coeffs) - 1ul ]

exception Rac_error of string

(** Builds a signed or unsigned RAC *)
let rac signed clock reset ena load inputs coeffs coeff_bits = 

    let input_bits  = width (List.hd inputs) in
    let output_bits = input_bits + coeff_bits in
    let num_inputs  = List.length inputs in
    let num_coeffs  = List.length coeffs in
    if num_inputs <> num_coeffs then raise (Rac_error ("Number of inputs must match number of coefficients"));

    let rom = build_rom coeffs in

    (* piso registers *)
    let piso_reg idx = 
        let input     = List.nth inputs idx in
        let piso_wire = wire input_bits in
        let piso_reg  = reg clock reset empty ena (mux2 load input piso_wire) in
        piso_wire <== (piso_reg >>~ 1);
        piso_reg in
    let piso_regs = List.map piso_reg (range num_inputs) in

    (* control signals *)
    let ctrl_wire = wire input_bits in
    let ctrl = reg clock reset empty ena (mux2 load (consti input_bits 1) ctrl_wire) in
    ctrl_wire <== (ctrl <<~ 1);
    let first = select ctrl 0 0 in
    let last  = select ctrl (input_bits-1) (input_bits-1) in
    let rdy   = reg clock reset empty ena last in

    (* rom address *)
    let addr_bits = List.map (fun x -> lsb x) piso_regs in
    let addr_wire = wire (num_inputs) in
    let addr      = List.fold_left (fun addr bit -> bit ++ addr) (List.hd addr_bits) (List.tl addr_bits) in
        addr_wire <== addr;

    (* rom *)
    let rom = mux addr (List.map (fun x -> consti coeff_bits x) rom) in

    (* accumulator *)
    //let rom_value = (if signed then (msb rom) else (consti 1 0)) ++ rom ++ (consti input_bits 0) in
    let rom_value = (msb rom) ++ rom ++ (consti input_bits 0) in // 6/2/2007
    let acc_wire  = wire (output_bits + 1) in 
    let addsub    = 
        if signed then 
            mux2 last (acc_wire - rom_value) (acc_wire + rom_value) 
        else 
            (acc_wire + rom_value) in 
    let acc = reg clock reset empty ena (mux2 first rom_value addsub) in
    //acc_wire <== (if signed then (>>+) else (>>:)) acc 1;
    acc_wire <== (acc >>+ 1); // 6/2/2007
    (msbs acc), rdy

