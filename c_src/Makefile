all: compile

compile: pcuda_ops.o

pcuda_ops.o:
	nvcc -g -c -Xcompiler -shared -Xcompiler -fPIC pcuda_ops.cu

clean:
	@nvcc -c -clean *.cu
