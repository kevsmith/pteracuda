all: compile

compile: pcuda_ops.o

pcuda_ops.o:
	nvcc $(NVCCFLAGS) pcuda_ops.cu

clean:
	@nvcc -c -clean *.cu
