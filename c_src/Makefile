all: *.cu
	nvcc -DTHRUST_DEBUG -c -Xcompiler -shared -Xcompiler -fPIC $?

clean: *.cu
	nvcc -c -clean $?