MTI_INC = C:\\dev\\tools\\modelsim61f\\include
MTI_LIB = C:\\dev\\tools\\modelsim61f\\win32pe

all: cosim.dll cosim_mti.dll NamedPipeNative.dll cosim.exe

test: 
	vlib work
	vlog -quiet test_mod.v
	vlog -quiet test.v
	vsim -quiet -c -do run.do -pli cosim_mti.dll hdfs_test

# modelsim vpi module.
cosim_mti.dll: cosim_mti.c
	cl /nologo -c -I $(MTI_INC) cosim_mti.c
	link -nologo -dll -export:init_usertfs cosim_mti.obj $(MTI_LIB)\\mtipli.lib

# Access win32 named pipes from .net
NamedPipeNative.dll: NamedPipeNative.cs
	csc /nologo /target:library NamedPipeNative.cs

# hdfs cosimulation pipes and control functions
cosim.dll: NamedPipeNative.dll cosim.ml
	fsc -a -r NamedPipeNative.dll cosim.ml

# Testing
cosim.exe: NamedPipeNative.dll cosim.ml
	fsc -r NamedPipeNative.dll cosim.ml

ivl:
	iverilog-vpi cosim_vpi.c
	iverilog test_vpi.v
	vvp -M . -m cosim_vpi a.out

clean:
	-rm *.obj *.exp *.lib *.exe *.dll
	-rm -fr work
