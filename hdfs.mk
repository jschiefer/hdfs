include cfg.mk

BUILD_ALL = $(DLL_PATH)/hdfs.dll $(DLL_PATH)/hdfslib.dll
ifeq ($(xilinx),true)
	BUILD_ALL += $(DLL_PATH)/xilinx.dll
endif

all: $(BUILD_ALL)

######################################################
# compile the main hdfs dll's 
######################################################

HDFS_SRC = \
	src/numeric.ml \
	src/signal.ml \
	src/circuit.ml \
	src/elaborate.ml \
	src/resources.ml \
	src/simulator.ml \
	src/c.ml \
	src/verilog.ml \
	src/vhdl.ml \
	src/edif.ml \
	src/fsharp.ml \
	src/simulatorV01.ml \
	#src/eventsim.ml \

HDFS_LIB_SRC = \
	lib/util/hdutil.ml \
	lib/util/hdadd.ml \
	lib/util/hdmul.ml \
	lib/util/hddiv.ml \
	lib/util/hdram.ml \
	lib/fixed/fixed.ml \
	lib/sort/sort.ml \
	lib/rac/rac.ml \
	lib/cordic/cordic.ml \
	lib/fft/fft.ml \
    lib/hdcaml/hdcaml.ml

#ifneq ($(mono),true)

HDFS_LIB_SRC += \
	lib/forms/waveform.ml \
	lib/forms/hierform.ml 
    
ifeq ($(glee),true)
HDFS_LIB_SRC += lib/forms/graphview.ml
endif

#endif

ifneq ($(cosim),)
	HDFS_LIB_SRC += lib/cosim/cosim.ml
#	HDFS_LIB_SRC += lib/cosim/cosimV01.ml lib/cosim/cosim.ml
endif

HDFS_LIB_LINK_DLLS = -r $(DLL_PATH)/hdfs.dll
HDFS_LIB_LINK_DEPS = $(HDFS_LIB_SRC)
ifeq ($(cosim),mti)
	HDFS_LIB_LINK_DEPS += $(DLL_PATH)/cosim_mti.dll
endif
ifeq ($(cosim),ivl)
	HDFS_LIB_LINK_DEPS += $(DLL_PATH)/cosim_vpi.vpi
endif

$(DLL_PATH)/hdfs.dll: $(HDFS_SRC)
	- mkdir bin
	- mkdir output
	$(FSC_DLL) $@ -r FSharp.PowerPack.dll $(HDFS_SRC) 

$(DLL_PATH)/hdfslib.dll: $(DLL_PATH)/hdfs.dll $(HDFS_LIB_LINK_DEPS) bin output
	$(FSC_DLL) $@ $(HDFS_LIB_LINK_DLLS) -r FSharp.PowerPack.dll $(HDFS_LIB_SRC)

buildbat:
	echo "mkdir bin" > hdfs.bat
	echo "mkdir output" >> hdfs.bat
	echo "fsc.exe -a -o $(DLL_PATH)/hdfs.dll $(HDFS_SRC)" >> hdfs.bat
	echo "fsc.exe -a -o $(DLL_PATH)/hdfslib.dll $(HDFS_LIB_LINK_DLLS) $(HDFS_LIB_SRC)" >> hdfs.bat

# Compile everything into 1 dll.
static: $(HDFS_LIB_LINK_DEPS) 
	$(FSC_DLL) $(DLL_PATH)/hdfs_s.dll --standalone $(HDFS_SRC) $(HDFS_LIB_LINK_DLLS) $(HDFS_LIB_SRC) output/xilinx.ml lib/xilinx/xilinx_synth.ml

######################################################
# Generate documentation
######################################################

gendocs:
	- mkdir docs/hdfs_api
	$(FSC_DLL) bin/hdfs_api.dll --generate-html --html-output-directory docs $(HDFS_LIB_LINK_DLLS) $(HDFS_SRC) $(HDFS_LIB_SRC) 

######################################################
# verilog cosimulation
# note: cosim_pipe used to be required.  This has now been
# removed as the required functions are bound to directly in
# F#.  Saves the hassle of the C# compiler, but we still need
# the C++ compilier for the modelsim DLL (that cannot be 
# managed code).
######################################################

$(DLL_PATH)/cosim_mti.dll: lib/cosim/cosim_mti.c
	$(CL) /nologo -c -I $(MTI_INC) lib/cosim/cosim_mti.c
	$(LINK) -nologo -dll -export:init_usertfs cosim_mti.obj $(MTI_LIB)/mtipli.lib
	rm cosim_mti.lib cosim_mti.exp cosim_mti.obj
	mv cosim_mti.dll bin

$(DLL_PATH)/cosim_vpi.vpi: lib/cosim/cosim_vpi.c
	iverilog-vpi -mingw=c:\cygwin lib/cosim/cosim_vpi.c
	mv lib/cosim/cosim_vpi.vpi bin/cosim_vpi.vpi

######################################################
# Xilinx library
######################################################

XIL_LIB_LINK_DLLS = -r $(DLL_PATH)/hdfs.dll -r $(DLL_PATH)/hdfslib.dll
XIL_LIB_LINK_DEPS = $(DLL_PATH)/hdfs.dll $(DLL_PATH)/hdfslib.dll

$(EXE_PATH)/xilinx.exe: lib/xilinx/xilinx_parser.mly lib/xilinx/xilinx_lexer.mll lib/xilinx/xilinx_types.ml lib/xilinx/xilinx_gen.ml 
	$(FSC_YACC) -o output/xilinx_parser.fs lib/xilinx/xilinx_parser.mly
	$(FSC_LEX) -o output/xilinx_lexer.fs lib/xilinx/xilinx_lexer.mll
	$(FSC_EXE) $@ lib/xilinx/xilinx_types.ml -r FSharp.PowerPack.dll output/xilinx_parser.fs output/xilinx_lexer.fs lib/xilinx/xilinx_gen.ml

$(DLL_PATH)/xilinx.dll: $(EXE_PATH)/xilinx.exe lib/xilinx/xilinx_synth.ml
	$(EXEC) $(EXE_PATH)/xilinx.exe $(XILINX_UNISIM) output/xilinx.ml
	$(FSC_DLL) $@ $(XIL_LIB_LINK_DLLS) --times -O- -r FSharp.PowerPack.dll -r $(DLL_PATH)/hdfslib.dll output/xilinx.ml lib/xilinx/xilinx_synth.ml

######################################################
# Tests
######################################################

ALLTESTS = \
	$(EXE_PATH)/test_general.exe \
	$(EXE_PATH)/test_ops.exe \
	$(EXE_PATH)/test_csim.exe \
	$(EXE_PATH)/test_behave.exe \
	$(EXE_PATH)/test_inst.exe \
	$(EXE_PATH)/test_clks.exe \
	$(EXE_PATH)/test_io.exe \
	$(EXE_PATH)/test_fixed.exe 

ifneq ($(mono),true)
	EXEC = 
	ALLTESTS += \
		$(EXE_PATH)/test_sim.exe \
		$(EXE_PATH)/test_sft.exe \
		$(EXE_PATH)/test_mem.exe 
else
	EXEC = mono
endif

ifeq ($(cosim),mti)
	ALLTESTS += \
		$(EXE_PATH)/test_asim.exe \
		$(EXE_PATH)/test_cosim.exe 
endif

ifeq ($(xilinx),true)
	ALLTESTS += \
		$(EXE_PATH)/test_edif.exe \
		$(EXE_PATH)/test_xilinx.exe 
endif

alltests: $(ALLTESTS)

LINK_DLLS = $(DLL_PATH)/hdfs.dll $(DLL_PATH)/hdfslib.dll
ifeq ($(xilinx),true)
	LINK_DLLS += -r $(DLL_PATH)/xilinx.dll 
endif

$(EXE_PATH)/test_general.exe: $(LINK_DLLS) lib/test/test_general.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/test/test_general.ml 
	$(EXEC) $(EXE_PATH)/test_general.exe
	- $(VCOM) output/test_general.vhd
	- $(VLOG) output/test_general.v

$(EXE_PATH)/test_ops.exe: $(LINK_DLLS) lib/test/test_ops.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/test/test_ops.ml 
	$(EXEC) $(EXE_PATH)/test_ops.exe
	- $(VCOM) output/test_ops.vhd
	- $(VLOG) output/test_ops.v

$(EXE_PATH)/test_csim.exe: $(LINK_DLLS) lib/test/test_csim.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/test/test_csim.ml 
	$(EXEC) $(EXE_PATH)/test_csim.exe
	- cl /LD -Isrc output/test_csim.cpp
	- (cd output; csc /debug /t:library test_csim.cs)
	- gcc -o bin/test_csim_c.exe -Isrc output/test_csim.c lib/test/test_csim.c
	- g++ -o bin/test_csim_cpp.exe -Isrc lib/test/test_csim.cpp

$(EXE_PATH)/test_behave.exe: $(LINK_DLLS) lib/test/test_behave.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/test/test_behave.ml 
	$(EXEC) $(EXE_PATH)/test_behave.exe
	- $(VCOM) output/test_behave.vhd
	- $(VLOG) output/test_behave.v

$(EXE_PATH)/test_sim.exe: $(LINK_DLLS) lib/test/test_sim.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/test/test_sim.ml 
	$(EXEC) $(EXE_PATH)/test_sim.exe

$(EXE_PATH)/test_asim.exe: $(LINK_DLLS) lib/test/test_asim.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/test/test_asim.ml 
	$(EXEC) $(EXE_PATH)/test_asim.exe

$(EXE_PATH)/test_mem.exe: $(LINK_DLLS) lib/test/test_mem.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/test/test_mem.ml
	$(EXEC) $(EXE_PATH)/test_mem.exe
	- $(VLOG) output/test_mem.v
	- $(VCOM) output/test_mem.vhd

$(EXE_PATH)/test_sft.exe: $(LINK_DLLS) lib/test/test_sft.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/test/test_sft.ml
	$(EXEC) $(EXE_PATH)/test_sft.exe

$(EXE_PATH)/test_inst.exe: $(LINK_DLLS) lib/test/test_inst.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/test/test_inst.ml
	$(EXEC) $(EXE_PATH)/test_inst.exe
	- $(VLOG) output/test_inst.v
	- $(VCOM) output/test_inst.vhd

$(EXE_PATH)/test_clks.exe: $(LINK_DLLS) lib/test/test_clks.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/test/test_clks.ml
	$(EXEC) $(EXE_PATH)/test_clks.exe
	- $(VLOG) output/test_clks.v
	- $(VCOM) output/test_clks.vhd

$(EXE_PATH)/test_cosim.exe: $(LINK_DLLS) lib/test/test_cosim.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/test/test_cosim.ml
	$(EXEC) $(EXE_PATH)/test_cosim.exe

$(EXE_PATH)/test_simbug.exe: $(LINK_DLLS) lib/test/test_simbug.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/test/test_simbug.ml
	$(EXEC) $(EXE_PATH)/test_simbug.exe

$(EXE_PATH)/test_io.exe: $(LINK_DLLS) lib/test/test_io.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/test/test_io.ml
	$(EXEC) $(EXE_PATH)/test_io.exe
	- $(VLOG) output/test_io.v
	- $(VCOM) output/test_io.vhd

$(EXE_PATH)/test_xilinx.exe: $(LINK_DLLS) lib/test/test_xilinx.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/test/test_xilinx.ml
	$(EXEC) $(EXE_PATH)/test_xilinx.exe
	- $(VLOG) output/test_xilinx.v
	- $(VCOM) output/test_xilinx.vhd

$(EXE_PATH)/test_edif.exe: $(LINK_DLLS) lib/test/test_edif.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/test/test_edif.ml
	$(EXEC) $(EXE_PATH)/test_edif.exe
	- $(VLOG) output/test_edif.v
	- $(VCOM) output/test_edif.vhd

$(EXE_PATH)/test_fixed.exe: $(LINK_DLLS) lib/test/test_fixed.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/test/test_fixed.ml
	$(EXEC) $(EXE_PATH)/test_fixed.exe 3 1 4 2

$(EXE_PATH)/regress.exe: $(LINK_DLLS) lib/test/regress_sim.ml lib/test/regress_int_bits.ml lib/test/regress_array_bits.ml
	$(FSC_EXE) $@ $(LINK_DLLS) -r "$(NUNIT)/nunit.framework.dll" lib/test/regress_sim.ml lib/test/regress_int_bits.ml lib/test/regress_array_bits.ml

$(EXE_PATH)/vlogp.exe: $(LINK_DLLS) lib/parsers/verilog.mll lib/parsers/verilog.mly lib/parsers/test.ml lib/parsers/test.v
	$(FSC_YACC) -o output/verilog_parser.fs lib/parsers/verilog.mly
	$(FSC_LEX) -o output/verilog_lexer.fs lib/parsers/verilog.mll
	$(FSC_EXE) $@ output/verilog_parser.fs output/verilog_lexer.fs lib/parsers/test.ml
	$(EXE_PATH)/vlogp.exe lib/parsers/test.v

######################################################
# Example applications
######################################################

allapps: $(EXE_PATH)/cordic_gen.exe $(EXE_PATH)/rac_gen.exe $(EXE_PATH)/sort_gen.exe

$(EXE_PATH)/cordic_gen.exe: bin/hdfs.dll bin/hdfslib.dll lib/cordic/cordic_ref.ml lib/cordic/cordic_gen.ml
	$(FSC_EXE) $@ -r bin/hdfs.dll -r bin/hdfslib.dll lib/cordic/cordic_ref.ml lib/cordic/cordic_gen.ml
	$(EXEC) $(EXE_PATH)/cordic_gen.exe -n cordic_test -vhdl -vlog -fs -rp -w "output/"
	- $(VLOG) output/cordic_test.v
	- $(VCOM) output/cordic_test.vhd

$(EXE_PATH)/rac_gen.exe: bin/hdfs.dll bin/hdfslib.dll lib/rac/rac_gen.ml 
	$(FSC_EXE) $@ -r bin/hdfs.dll -r bin/hdfslib.dll lib/rac/rac_gen.ml 
	$(EXEC) $(EXE_PATH)/rac_gen.exe -n rac_test -w "output/" -vhdl -vlog -fs -rp -- 1 2 3
	- $(VLOG) output/rac_test.v
	- $(VCOM) output/rac_test.vhd

$(EXE_PATH)/sort_gen.exe: bin/hdfs.dll bin/hdfslib.dll lib/sort/sort_gen.ml
	$(FSC_EXE) $@ -r bin/hdfs.dll -r bin/hdfslib.dll lib/sort/sort_gen.ml
	$(EXEC) $(EXE_PATH)/sort_gen.exe -t oem -p -n sort_test -w "output/" -vhdl -vlog -fs -rp -i 8
	- $(VLOG) output/sort_test.v
	- $(VCOM) output/sort_test.vhd

$(EXE_PATH)/cordic_win.exe: bin/hdfs.dll bin/hdfslib.dll lib/cordic/cordic_ref.ml lib/cordic/cordic_win.ml
	$(FSC_EXE) $@ -r bin/hdfs.dll -r bin/hdfslib.dll lib/cordic/cordic_ref.ml lib/cordic/cordic_win.ml

######################################################
# Tutorial 
######################################################
$(EXE_PATH)/tut_hello.exe: $(LINK_DLLS) lib/tutorial/hello.ml
	$(FSC_EXE) $@ $(LINK_DLLS) lib/tutorial/hello.ml

######################################################
######################################################

clean:
	- rm $(DLL_PATH)/*.dll $(EXE_PATH)/*.exe $(EXE_PATH)/*.pdb
	- rm output/*
