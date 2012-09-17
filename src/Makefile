FC = gfortran -Wall -pedantic -fcheck=all -g

test: options.o test.o
	$(FC) $+ -o $@

example: options.o example.o
	$(FC) $+ -o $@

clean:
	rm -f test *.o *.mod

%.o: %.f90
	$(FC) -c $< -o $@
