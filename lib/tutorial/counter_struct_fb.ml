open DigitalLogic
open Design

(*********************************************************)

let counter_structural_fb load init = 
  let w = width init in
  regc_fb enable (fun d -> mux2 load init (d +: (one w))) w

do write_file 
  Verilog.write 
    "output/" "counter_structural_fb" ".v"
    (Circuit.create [ output "q" (counter_structural_fb (input "load" 1) (input "init" 16)) ])
