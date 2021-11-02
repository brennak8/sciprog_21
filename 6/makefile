FC = gfortran
OBJ = functions.o mat_mult.o

all: $(OBJ)
	$(FC) $(OBJ)
functions.o: functions.f90
	$(FC) -c functions.f90
mat_mult.o: mat_mult.f90
	$(FC) -c mat_mult.f90
clean:
	rm -f *.o
