open DigitalLogic
open Design

(*********************************************************)

let counter_structural load init = 
  let w = width init in
  let d = wire w in
  let q = regc enable (mux2 load init (d +: (one w))) in
  d <== q;
  q
  
do write_file 
  Verilog.write 
    "output/" "counter_structural" ".v"
    (Circuit.create [ output "q" (counter_structural (input "load" 1) (input "init" 16)) ])
