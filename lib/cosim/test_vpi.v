module test_vpi;

    reg clk;
    integer count=0;
    reg [31:0] a0;
    wire [15:0] a1=a0[15:0] * 2;
    reg [17:0] a2;

    always begin
        clk <= 1; #5;
        clk <= 0; #5;
    end
   
    always @(posedge clk) 
    begin
        a2 <= a0 * 3;
        count <= count + 1;
        if (count == 10) $stop;      
    end

    always @(posedge clk) $hdfs_drive(a0, a2);
    always @(negedge clk) $hdfs_monitor(a0, a1, a2);

endmodule // test_vpi

