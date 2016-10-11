 assign hdfs_138 = 2'b01;
 always @(posedge clock, posedge reset)
  if (reset) hdfs_140 <= hdfs_138;
  else if (enable) hdfs_140 <= hdfs_139;


 always @(posedge clock)
   hdfs_103 <= hdfs_102;
