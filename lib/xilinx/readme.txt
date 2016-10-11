General
-------

Parse the xilinx unisim_VCOMP.vhd file into a library of instantiation components.

(Tested with unisim_VCOMP.vhd from ISE 8.1 and 8.2.3)

A file of xilinx instantiations is then created.  3 functions are given for each module.  The first 
takes all generics and inputs as arguments and is prefixed by "g_" ie

let g_something = generic_0 generic_1 input_0 input_1

The 2nd takes an association list of generics mapping name to data and is prefixed by "l_" ie

let l_something = generics_list input_0 input_1

The last has no prefix and doesnt take any generics at all

let something = input_0 input_1

For automatic "generators" versions of the functions which take and return association lists of
inputs and outputs might yet make sense (and is farily easy to do).

(72K lines and counting....)

Problems
--------

* std_logic_Vector(0 downto 0) ports get rewritten as std_logic ports.

* for some models generics are used for simulation and attributes for synthesis.  v2 "INIT" on 
  flops & latches seem to work this way.
