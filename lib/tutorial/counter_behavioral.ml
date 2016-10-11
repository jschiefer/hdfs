open DigitalLogic
open Design

let counter_behavioral load init = 
  let q = b_regc enable (width init) in
  behave [
    b_if (load) [
      q $== init;
    ] [
      q $== (q' q) +: (one (width init));
    ]
  ];
  q' q

do write_file 
  Verilog.write 
    "output/" "counter_behavioural" ".v"
    (Circuit.create [ output "q" (counter_behavioural (input "load" 1) (input "init" 16)) ])
