(** Division *)
module DigitalLogic.Circuits.Divide
open DigitalLogic.Numeric.Ops
open DigitalLogic.Signal

(** Unsigned divide by constant *)
let divide_const
  dividend      (* unsigned vector *)
  max_dividend  (* positive integer *)
  divisor       (* positive integer *)
  =
  assert (divisor <> 0);
  let max_quotient = max_dividend / divisor in
  let quotient_bits = DigitalLogic.Util.clog2 max_quotient in
  let quot_bits, remainder = 
    List.fold_left (fun (bits, cur) i ->
      let c = consti (width dividend) (divisor <<< i) in
      let gteq = cur >=~ c in
      gteq :: bits, mux2 gteq (cur - c) cur
    ) ([],dividend) [ quotient_bits - 1 .. -1 .. 0 ] in
  concat_lsb quot_bits, remainder

(** Unsigned divide *)
let divide dividend divisor = 
  let quot_bits, remainder = 
    List.fold_left (fun (bits, cur) (i:int) ->
      let c = ((zero (width dividend)) ++ divisor) <<~ i in
      let gteq = cur >=~ c in
      gteq :: bits, mux2 gteq (cur - c) cur
    ) ([], zero divisor.width ++ dividend) [ (width dividend) - 1 .. -1 .. 0 ] in
  concat_lsb quot_bits, remainder
