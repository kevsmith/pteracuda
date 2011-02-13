all: *.cu
	nvcc -shared -c -Xcompiler -fPIC $?

clean: *.cu
	nvcc -c -clean $?