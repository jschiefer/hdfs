module hdfs_test();

    reg clock;

    // Inputs
    reg [3:0] _d0, d0;
    reg [13:0] _d1, d1;

    // Outputs
    wire [13:0] q0;
    wire [3:0] q1;
    wire [3:0] q2;
    
    initial $hdfs_drive_signals(2, clock, _d0, _d1);            // number of data signals, clock, inputs
    initial $hdfs_monitor_signals(3, clock, q0, q1, q2);        // number of data signals, clock, outputs

    // Drive the clock within the simulator.
    always begin
        clock <= 0; #5;
        clock <= 1; #5;
    end
    
    // This bit of magic gets the input signals to work correctly with submodule.
    always @(posedge clock)
    begin
        d0 <= _d0;
        d1 <= _d1;
    end
    
    hdfs_mod the_hdfs_11234 (.clock(clock), .d0(d0), .d1(d1), .q0(q0), .q1(q1), .q2(q2));

endmodule
