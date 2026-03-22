Currently only works with gfortran compilation

Unit tests set up and run with cmake.
test100.f90 is designed to pass
test200.f90 is designed to fail

To create the Makefile and executables run

cmake ./

To compile the testing code run

make

TO run the tests 

make test

-------------------------------------------------
cmake requires the CMakeLists.txt configuration file.
Must compile with gfortran.

To add a new test, create the test and then add to 
the CMakeLists.txt file.
-------------------------------------------------
