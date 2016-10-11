(* UART *)

open DigitalLogic
open Design

type reset_type_t = AsyncReset | SyncReset

let uart 
  reset_type  (* sync or async *)
  reset       (* 1 bit *)
  address     (* 1 bit *)
  writeData   (* 8 bits *)
  write       (* 1 bit *)
  read        (* 1 bit *)
  serialIn    (* 1 bit *)
  = 

  (* select reset type for the circuit by overriding the state elements *)
  let b_reg, reg, make_states = 
    match reset_type with
    | AsyncReset -> 
      (fun rv d -> b_reg clock reset (consti d rv) empty d), 
      (fun rv d -> reg clock reset (consti (width d) rv) empty d), 
      Util.make_states clock reset empty
    | SyncReset -> 
      (fun rv d -> b_reg clock empty (consti d rv) empty d), 
      (fun rv d -> reg clock empty (consti (width d) rv) empty d), 
      Util.make_states clock empty empty in
      
  (* State machine loop counting construct *)
  let rec b_loop l = 
    match l with
    | [] -> []
    | (reg, limit, code0, code1) :: tl ->
      let w = width (q' reg) in
        (reg $== q' reg +: one w) ::
        code0 @ 
        [
          b_if ((q' reg) ==: (consti w (limit-1))) (
            (reg $== zero w) ::
            code1 @
            (b_loop tl)
          ) []
        ]
  in
  (* clear a list of behavioral variables *)
  let b_clear = List.map (fun x -> x $== (zero (width (q' x)))) in

  (* state signals *)
  let txState, txs' = make_states [ "txIDLE"; "txSTART"; "txSEND"; "txSTOP" ] in
  let rxState, rxs' = make_states [ "rxIDLE"; "rxRECEIVE"; "rxSTOP"; "rxFULL"; "rxERROR" ] in

  (* register map *)
  let c_TX = gnd in
  let c_RX = gnd in
  let c_STATUS = vdd in
  let status rx_ready rx_error tx_ready = (zero 5) ++ tx_ready ++ rx_error ++ rx_ready in

  let tx = b_reg 0 8 in
  let rx = b_reg 0 8 in
  let readData = b_reg 0 8 in
  let serialOut = b_reg 1 1 in
  
  let rxSampleCount = b_reg 0 3 in
  let rxBitCount = b_reg 0 3 in
  let txSampleCount = b_reg 0 3 in
  let txBitCount = b_reg 0 3 in
  
  (* Clock domain crossing.  Avoid metastability issues. *)
  let serialIn = reg 1 (reg 1 serialIn) in

  let state_machine = [

    (* transmit statemachine *)
    b_switch (q' txState) [

      b_case (txs' "txSTART") 
        ((txState $== txs' "txSEND") :: 
         (b_clear [ serialOut; txSampleCount; txBitCount ]));
      
      b_case (txs' "txSEND") 
        (b_loop [ 
          (txSampleCount, 8, [], [ serialOut $== bit (q' tx) 0; tx $== ((q' tx) >>: 1); ]); 
          (txBitCount, 8, [], [ txState $== txs' "txSTOP" ]);
        ]);
      
      b_case (txs' "txSTOP") 
        (b_loop [ (txSampleCount, 8, [], [ serialOut $== vdd; txState $== txs' "txIDLE"] ) ])
        
    ];

    (* receive statemachine *)
    b_switch (q' rxState) [
    
      b_case (rxs' "rxIDLE") [
        b_if (serialIn ==: gnd) (
          (rxState $== rxs' "rxRECEIVE") ::
          (b_clear [ rxSampleCount; rxBitCount ])
        ) []
      ];
      
      b_case (rxs' "rxRECEIVE") (
        b_loop [ 
          (rxSampleCount, 8, [], [ rx $== serialIn ++ (select (q' rx) 7 1) ]); 
          (rxBitCount, 8, [], [ rxState $== rxs' "rxSTOP" ]);
        ]
      );
      
      b_case (rxs' "rxSTOP") (
        b_loop [ (rxSampleCount, 8, [], [ rxState $== mux2 serialIn (rxs' "rxFULL") (rxs' "rxERROR") ]) ]
      );
      
    ];
    
    (* write registers *)
    b_if (write &: (address ==: c_TX)) [
      tx $== writeData;
      txState $== txs' "txSTART";
    ] [];
    
    (* read registers *)
    readData $== (zero 8);
    b_if (read) [
    
      b_switch (address) [
      
        b_case (c_RX) [
          readData $== q' rx;
          rxState $== rxs' "rxIDLE"
        ];
        
        b_case (c_STATUS) [
          readData $== status 
            ((q' rxState) ==: (rxs' "rxFULL"))
            ((q' rxState) ==: (rxs' "rxERROR"))
            ((q' txState) ==: (txs' "txIDLE"));
        ];
        
      ];
    ] [];    
    
  ] in
  
  (* if we're using a synchronous reset, wrap the statemachine *)
  let state_machine = 
    if reset_type = SyncReset then [
      b_if (reset) ([ serialOut $== vdd; txState $== txs' "txIDLE"; rxState $== rxs' "rxIDLE"; ] @ 
      (b_clear [tx; rx; readData; rxSampleCount; rxBitCount; txSampleCount; txBitCount;])) state_machine 
    ]
    else state_machine in

  behave state_machine;
  
  (* outputs *)
  q' readData, (* 8 bits *)
  q' serialOut (* 1 bit *)

let gen_uart() = 
  let readData, serialOut = uart AsyncReset reset
    (input "address" 1)
    (input "writeData" 8)
    (input "write" 1)
    (input "read" 1)
    (input "serialIn" 1)
  in
  let outputs = [
    output "readData" readData;
    output "serialOut" serialOut;
  ] in
  let circuit = Circuit.create outputs in
  write_file Verilog.write "output/" "hdfs_uart" ".v" circuit;
  write_file Vhdl.write "output/" "hdfs_uart" ".vhd" circuit
  
do gen_uart()
