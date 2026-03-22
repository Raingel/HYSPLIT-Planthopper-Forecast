7 JUL 2021

Compilation of unit tests in this directory requires the Google Test library.
At the time of writing, the latest version of Google Test is 1.11.0. Compiling
1.11.0 needs gcc compiler 5 or later.

For Linux with gcc 4.8, for example, Red Hat Enterprise Linux 7, an earlier
version of Google Test may be used. The unit tests are written and tested with
Google Test 1.8.x.

To install Google Test in your home directory, run the following commands:

    cd ~
    git clone https://github.com/google/googletest.git
    cd googletest
    git checkout v1.8.x

Then use cmake version 3 to build Google Test:

    mkdir build
    cd build
    cmake3 ..
    make

Include files are located in the ${HOME}/googletest/googletest/include. The
following library files are found in ${HOME}/googletest/build/googlemock/gtest:

    libgtest.a
    libgtest_main.a

Your Makefile.inc in the HYSPLIT top-level directory should have the following
lines for the Makefile in this directory to work:

    GTEST_INC= ${HOME}/googletest/googletest/include
    GTEST_LIB= -L${HOME}/googletest/build/googlemock/gtest -lgtest -lgtest_main 
