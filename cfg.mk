###########################################################################
# General settings
###########################################################################

# executables 
EXE_PATH = bin

# This is where dlls are written to and referenced from.
DLL_PATH = bin

# Build type: nothing, release or debug
build = 

# Cosimulation: nothing or mti (modelsim)
cosim = 

# Build xilinx lib (takes an age): nothing or true
xilinx = 

# Path to unisim source file
XILINX_UNISIM = /opt/Xilinx/10.1/ISE/vhdl/src/unisims/unisim_VCOMP.vhd

# Set to true if building under mono
mono = true

# If the GLEE library is installed (see microsoft research), this allows the graphviewer to be built
glee = 

# NUnit test framwork
NUNIT = C:\dev\tools\NUnit-Net-2.0 2.2.9\bin

# This is required to get signed dll's/exe's which can be put in the GAC.  
# It works but isnt properly integrated yet.
KEYFILE = hdfskey.snk

###########################################################################
# f# compiler
###########################################################################

FSC_PATH = mono /home/andyman/dev/tools/FSharp-1.9.6.2/bin

FSC_EXE = $(FSC_PATH)/fsc.exe -o 
#FSC_DLL = $(FSC_PATH)/fsc --keyfile $(KEYFILE) -a -o 
FSC_DLL = $(FSC_PATH)/fsc.exe -a -o 

ifeq ($(build),debug)
	FSC_EXE = $(FSC_PATH)/fsc.exe -g -o 
	FSC_DLL = $(FSC_PATH)/fsc.exe -g -a -o 
endif

ifeq ($(build),release)
	FSC_EXE = $(FSC_PATH)/fsc.exe -O3 -o 
	FSC_DLL = $(FSC_PATH)/fsc.exe -O3 -a -o 
endif

FSC_YACC = $(FSC_PATH)/fsyacc.exe 
FSC_LEX = $(FSC_PATH)/fslex.exe 

###########################################################################
# .net tools
###########################################################################

# You need to set up vavars32.bat for these to work (I copied it into cygwin.bat).
# You'll also need to install the "platform sdk for windows server 2003", 
# copy the include and lib directorys into "ms visual studio 8/VC/PlatformSDK"
# and ensure the paths are correct.  Crazy.  
# Note also, if using cygwin link.exe may be the GNU on rather than the MS one.
CL = cl
#CSC = csc /keyfile:$(KEYFILE) 
CSC = csc 
LINK = "c:/Program Files/Microsoft Visual Studio 9.0/VC/bin/link.exe"

# dunno how to work with these tools...
GACUTIL = 
NGEN = 

###########################################################################
# hardware compilers
###########################################################################

#MTI = C:/dev/tools/modeltech_6.2f

VLOG = $(MTI)\win32pe\vlog.exe -lint
VCOM = $(MTI)\win32pe\vcom.exe -lint
VLIB = $(MTI)\win32pe\vlib.exe

MTI_LIB = $(MTI)/win32pe
MTI_INC = $(MTI)/include

XST = C:\Xilinx\bin\nt\xst.exe
