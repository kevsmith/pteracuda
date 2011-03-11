all: compile

compile: pcuda_ops.o

pcuda_ops.o:
	nvcc -O2 -c -Xcompiler -shared -Xcompiler -fPIC pcuda_ops.cu

clean:
	@nvcc -c -clean *.cu
