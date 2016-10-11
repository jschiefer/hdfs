open DigitalLogic
open DigitalLogic.Design

let statemachine s_start = 
  let s_done  = wire 1 in
  let s_state = wire 2 in
  let s_zero  = constb "00" in
  let s_one   = constb "01" in
  let s_two   = constb "10" in
  let s_three = constb "11" in
  behave [
    b_switch (s_state) [
      b_case s_zero [
        b_if (s_start) [
          s_state $== s_one;
          s_done $== gnd;
        ] [
          s_done $== vdd;
        ]
      ];
      b_case (s_one) [
        s_state $== s_two;
      ];
      b_case (s_two) [
        s_state $== s_three;
      ];
      b_case (s_three) [
        s_state $== s_one;
      ];
    ]
  ];
  let s_state = regc enable s_state in
  let s_done = regc enable s_done in
  s_done

