all: *.cu
	nvcc -O2 -c -Xcompiler -shared -Xcompiler -fPIC $?

clean: *.cu
	nvcc -c -clean $?