open DigitalLogic
open DigitalLogic.Design

let complex_mul (ar,ai) (br,bi) = 
  let r = (ar *+ br) -: (ai *+ bi) in
  let i = (ar *: bi) +: (ai *: br) in
  r, i
  
let build_circuit() = 
  let bits = 10 in
  let r, i = complex_mul
    ((input "ar" bits), (input "ai" bits))
    ((input "br" bits), (input "bi" bits)) in
  let circuit = Circuit.create [
    (output "r" r); 
    (output "i" i)
  ] in
  ...
