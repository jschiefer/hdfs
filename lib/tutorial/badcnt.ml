let counter clock reset enable load init = 
  let q = wire in
  let d = regc enable (if load then init else q) in
  q <== d;
  d
