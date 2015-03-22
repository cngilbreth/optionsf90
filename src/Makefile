FC = gfortran -Wall -pedantic -fcheck=all -g

all: test example1 example2

test: options.o test.o
	$(FC) $+ -o $@

example1: options.o example1.o
	$(FC) $+ -o $@

example2: options.o example2.o
	$(FC) $+ -o $@

clean:
	rm -f test *.o *.mod

%.o: %.f90
	$(FC) -c $< -o $@
