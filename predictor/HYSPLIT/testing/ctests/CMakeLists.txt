# This is the main CMake file for HYSPLIT testing.
#
# 2023 May 22 (amc) added testprfmod and testesat

cmake_minimum_required(VERSION 3.17)

file(STRINGS "VERSION" pVersion)

project(
  hysplit
  VERSION ${pVersion}
  LANGUAGES C Fortran)

#include(GNUInstallDirs)

#if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
#  message(STATUS "Setting build type to 'Release' as none was specified.")
#  set(CMAKE_BUILD_TYPE
#    "Release"
#    CACHE STRING "Choose the type of build." FORCE)
#  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
#    "MinSizeRel" "RelWithDebInfo")
#endif()

#find_package(Jasper REQUIRED)
#find_package(PNG REQUIRED)

find_library(HLIB
             NAMES libhysplit.a
             HINTS "../../library")

find_library(FCSUBS
             NAMES libgfortranfcsubs.a
             HINTS "../../library")

find_library(FGSB
             NAMES liblbfgsb.a
             HINTS "../../library")

#add_subdirectory(src)

include(CTest)
#if(BUILD_TESTING)
#  add_subdirectory(tests)
#  # Copy the VERSION file, it's needed in a test.
#  FILE(COPY ${CMAKE_CURRENT_SOURCE_DIR}/VERSION DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/tests)
#endif()

# Determine whether or not to generate documentation.
#option(ENABLE_DOCS "Enable generation of doxygen-based documentation." OFF)
#if(ENABLE_DOCS)
#  find_package(Doxygen REQUIRED)
#endif()

get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

set (CMAKE_Fortran_FORMAT FREE)
#set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
#set (CMAKE_Fortran_FLAGS_RELEASE "-O0 -g -cpp")
set (CMAKE_Fortran_FLAGS "-O0 -g -cpp")

add_executable(test100 test100.f90)
# test200 is designed to fail.
#add_executable(test200 test200.f90)



add_executable(testmetdiv testmetdiv.f90  testmet.f90 utests.f90)
target_include_directories(testmetdiv PUBLIC "../../library/")
target_link_libraries(testmetdiv ${HLIB} ${FCSUBS} ${FGSB})
target_include_directories(testmetdiv PRIVATE "../../library/hysplit/")
target_compile_features(testmetdiv PRIVATE)


add_executable(testesat testesat.f90  utests.f90)
target_include_directories(testesat PUBLIC "../../library/")
target_link_libraries(testesat ${HLIB} ${FCSUBS} ${FGSB})
target_include_directories(testesat PRIVATE "../../library/hysplit/")
target_compile_features(testesat PRIVATE)


add_executable(testprfmod testprfmod.f90  utests.f90)
target_include_directories(testprfmod PUBLIC "../../library/")
target_link_libraries(testprfmod ${HLIB} ${FCSUBS} ${FGSB})
target_include_directories(testprfmod PRIVATE "../../library/hysplit/")
target_compile_features(testprfmod PRIVATE)

add_executable(testzindex testzindex.f90 utests.f90)
target_include_directories(testzindex PUBLIC "../../library/")
target_link_libraries(testzindex ${HLIB} ${FCSUBS} ${FGSB})
target_include_directories(testzindex PRIVATE "../../library/hysplit/")
target_compile_features(testzindex PRIVATE)


add_executable(testadviec testadviec.f90 testmet.f90 utests.f90)
target_include_directories(testadviec PUBLIC "../../library/")
target_link_libraries(testadviec ${HLIB} ${FCSUBS} ${FGSB})
target_include_directories(testadviec PRIVATE "../../library/hysplit/")
target_compile_features(testadviec PRIVATE)

add_executable(testcnllxy testcnllxy.f90 utests.f90)
target_include_directories(testcnllxy PUBLIC "../../library/")
target_include_directories(testcnllxy PRIVATE "../../library/hysplit/" ".")
target_compile_features(testcnllxy PRIVATE)
target_link_libraries(testcnllxy ${HLIB} ${FCSUBS} ${FGSB})

add_executable(testlonavg testlonavg.f90 utests.f90)
target_include_directories(testlonavg PUBLIC "../../library/")
target_include_directories(testlonavg PRIVATE "../../library/hysplit/" ".")
target_compile_features(testlonavg PRIVATE)
target_link_libraries(testlonavg ${HLIB} ${FCSUBS} ${FGSB})

add_executable(testmapbox testmapbox.f90 utests.f90)
target_include_directories(testmapbox PUBLIC "../../library/")
target_include_directories(testmapbox PRIVATE "../../library/hysplit/" ".")
target_compile_features(testmapbox PRIVATE)
target_link_libraries(testmapbox ${HLIB} ${FCSUBS} ${FGSB})

add_executable(teststrutl teststrutl.f90 utests.f90)
target_include_directories(teststrutl PUBLIC "../../library/")
target_include_directories(teststrutl PRIVATE "../../library/hysplit/" ".")
target_compile_features(teststrutl PRIVATE)
target_link_libraries(teststrutl ${HLIB} ${FCSUBS} ${FGSB})

add_executable(testchmgea testchmgea.f90 utests.f90)
target_include_directories(testchmgea PRIVATE "../../library/hysplit/" ".")
target_compile_features(testchmgea PRIVATE)
target_link_libraries(testchmgea ${HLIB} ${FCSUBS} ${FGSB})

add_executable(testregion testregion.f90 utests.f90)
target_include_directories(testregion PRIVATE "../../library/hysplit/" ".")
target_compile_features(testregion PRIVATE)
target_link_libraries(testregion ${HLIB} ${FCSUBS} ${FGSB})

#add_executable(testsort testsort.f90 utests.f90)
#target_include_directories(testsort PRIVATE "../../library/hysplit/" ".")
#target_compile_features(testsort PRIVATE)
#target_link_libraries(testsort ${HLIB} ${FCSUBS} ${FGSB})

add_executable(testrandom testrandom.f90 utests.f90)
target_include_directories(testrandom PRIVATE "../../library/hysplit/" ".")
target_compile_features(testrandom PRIVATE)
target_link_libraries(testrandom ${HLIB} ${FCSUBS} ${FGSB})

add_executable(testparvr3 testparvr3.f90 utests.f90)
target_include_directories(testparvr3 PRIVATE "../../library/hysplit/" ".")
target_compile_features(testparvr3 PRIVATE)
target_link_libraries(testparvr3 ${HLIB} ${FCSUBS} ${FGSB})

add_executable(testrdizsg testrdizsg.f90 utests.f90)
target_include_directories(testrdizsg PRIVATE "../../library/hysplit/" ".")
target_compile_features(testrdizsg PRIVATE)
target_link_libraries(testrdizsg ${HLIB} ${FCSUBS} ${FGSB})

add_executable(testrdwhyb testrdwhyb.f90 utests.f90)
target_include_directories(testrdwhyb PRIVATE "../../library/hysplit/" ".")
target_compile_features(testrdwhyb PRIVATE)
target_link_libraries(testrdwhyb ${HLIB} ${FCSUBS} ${FGSB})


add_test(testadviec testadviec)
add_test(testmetdiv testmetdiv)
add_test(testprfmod testprfmod)
add_test(test100 test100)
#add_test(test200 test200)
add_test(testcnllxy testcnllxy)
add_test(testlonavg testlonavg)
add_test(testmapbox testmapbox)
add_test(teststrutl teststrutl)
add_test(testchmgea testchmgea)
add_test(testregion testregion)
add_test(testsort testsort)
add_test(testrandom testrandom)
add_test(testparvr3 testparvr3)
add_test(testrdizsg testrdizsg)
add_test(testrdwhyb testrdwhyb)

