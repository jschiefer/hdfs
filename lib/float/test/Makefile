float_gen.exe: ../hdfloat.ml float_gen.ml
	ocamlc -g -o float_gen.exe graphics.cma fnflib.cma nums.cma hdcaml.cma hdlib.cma float_gen.ml

float_circuit.c: float_gen.exe
	ocamlrun -b float_gen.exe -e 8 -m 23 -d -mul -c 	# ieee 32 bit floating point with denormal

.PHONY: test
test: sim_float.c float_circuit.c
	gcc -o sim_float.exe float_circuit.c sim_float.c
	./sim_float.exe 100

clean:
	- rm *.exe *.cmi *.cmo
	- rm float_circuit*

