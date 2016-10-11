///////////////////////////////////////////////////////////

module counter_behavioural #(
    parameter bits = 16
) (
    input clock, reset, enable, load,
    input [bits-1:0] init,
    output reg [bits-1:0] q
);
    always @(posedge clock, posedge reset)
        if (reset) 
            q <= 0;
        else if (enable) 
            if (load) q <= init;
            else q <= q + 1;
endmodule

///////////////////////////////////////////////////////////

module register #(
    parameter bits = 16
) (
    input clock, reset, enable, 
    input [bits-1:0] d,
    output reg [bits-1:0] q
);
    always @(posedge clock, posedge reset)
        if (reset) 
            q <= 0;
        else if (enable) 
            q <= d;
endmodule

module mux2 #(
    parameter bits = 16
) (
    input sel,
    input [bits-1:0] d0, d1,
    output [bits-1:0] q
);
    assign q = sel ? d1 : d0;
endmodule

module add #(
    parameter bits = 16
) (
    input [bits-1:0] a, b,
    output [bits-1:0] q
);
    assign q = a + b;
endmodule


module counter_structural #(
    parameter bits = 16
) (
    input clock, reset, enable, load,
    input [bits-1:0] init,
    output [bits-1:0] q
);
    wire [bits-1:0] qp, qf;
    add #(bits) theadd (q, 1, qp);
    mux2 #(bits) themux (load, init, qp, qf);
    register #(bits) thereg (clock, reset, enable, qf, q);
endmodule

