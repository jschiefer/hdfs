module hdfs_mod(clock, d0, d1, q0, q1, q2);
    input clock;
    input [3:0] d0;
    input [13:0] d1;
    output reg [13:0] q0;
    output reg [3:0] q1;
    output [3:0] q2;

    always @(posedge clock)
        q1 <= d0;

    always @(posedge clock)
        q0 <= d1;

    assign q2 = d0 + d1[3:0];
        
endmodule
