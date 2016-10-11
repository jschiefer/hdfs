Compilation of HDFS using Visual Studio
---------------------------------------

Five projects are provided which together build the HDFS dll's.

hdfs.fsharpp		: Main hdfs dll
hdfslib.fsharpp		: hdfs utility library dll
hdfsxilinx.fsharpp	: compilation of Xilinx binding dll
hdfsxilinx.vcproj	: Generation of Xilinx binding
hdfscosim.vcproj	: Modelsim cosimulation VPI dll

Requried Tools
--------------

A full build requires the following tools:

* F# compiler
* Visual Studio Shell (integrated) 2008
* Modelsim
* Xilinx ISE (webpack or full)
* Visual C++ 2005 Express Edition

Note that this corresponds to the set up on my machine.  Other (later)
versions of Visual Studio (in particular Visual C++ 2008 Express) should
also work fine.

Compilation process
-------------------

HDFS should be compiled as follows.

1) Compile hdfs.dll
2) Compile hdfslib.dll
3) Compile the modelsim VPI dll
4) Compile the Visual C++ Xilinx project.
   - This step is actually just a wrapper around the fslex and fsyacc 
     tools.  It would be better if this could be done in Visual
     Stdio Shell, but I cant figure out how to make a custom project
     to do this.
5) Compile the Visual Studio Shell Xilinx project.

Only steps 1 + 2 are really required.  3, 4 + 5 are optional.

The modelsim VPI dll requires an installation of Modelsim.  The xilinx
library requires an installation of ISE, or a copy of the file
unisim_VCOMP.vhd.

A few paths need to be set up within the projects to match the installation
paths of the tools on a specific machine.  In particular;

* hdfscosim.vcproj - the include and library paths in the project options
  must match you installation of Modelsim.
* hdfsxilinx.vcproj - In order to find the fslex and fxyacc tools, add your
  $(FSHARP)/bin directory to:
  tools->options->VC++ Directories->Executable files

  Further, the file xilinx_gen.ml is actually built using a custom build
  rule which executes fsi with the location of unisim_VCOMP.vhd as a 
  command line parameter.  This should be altered according to your
  setup.




