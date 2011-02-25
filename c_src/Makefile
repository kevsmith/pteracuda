all: *.cu
	nvcc -O2 -shared -c -Xcompiler -fPIC $?

clean: *.cu
	nvcc -c -clean $?