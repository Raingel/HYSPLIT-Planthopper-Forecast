#
# Makefile to build unit testse.
#
# Last Revised:
#  7 JUL 2021 (SYZ) - Initial.
# 20 SEP 2021 (SYZ) - Fix link error after changes to libhysplit.a
#

# Resolve system dependencies.
include ../../Makefile.inc

# Add locations of c/c++ header files.
CXXFLAGS+= -I$(GTEST_INC) -I../../library/hysplit

# Define libraries to link with.
LIBS= $(GTEST_LIB) -L../../library -lhysplit -lpthread -lstdc++

# Rule to build an executable from a c++ file.
.cpp:
	$(CXX) $(CXXFLAGS) -c $<
	$(FC) $(FFLAGS) -o $@ $@.o $(LIBS)

# List of c++ files for unit tests.
SRCS= genctr_test.cpp \
      cutils_test.cpp

build: $(SRCS:_test.cpp=_test)

clean:
	rm -f *.o *_test
