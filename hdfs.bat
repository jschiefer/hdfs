mkdir bin
mkdir output
fsc.exe -a -o bin/hdfs.dll src/numeric.ml src/signal.ml src/circuit.ml src/elaborate.ml src/resources.ml src/simulator.ml src/c.ml src/verilog.ml src/vhdl.ml src/edif.ml src/fsharp.ml src/simulatorV01.ml 
fsc.exe -a -o bin/hdfslib.dll -r bin/hdfs.dll lib/util/hdutil.ml lib/util/hdadd.ml lib/util/hdmul.ml lib/util/hddiv.ml lib/util/hdram.ml lib/fixed/fixed.ml lib/sort/sort.ml lib/rac/rac.ml lib/cordic/cordic.ml lib/fft/fft.ml lib/hdcaml/hdcaml.ml lib/forms/waveform.ml lib/forms/hierform.ml 
