#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.61])
AC_INIT([arlshapes], [1.0], [albion.taylor@noaa.gov])
AC_CONFIG_SRCDIR([shapeHysplitOut.c])
AC_CONFIG_HEADERS([config.h:config.hin arcview2/config.h arcview1/config.h contour/config.h:contour/config.hin])

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB

# Checks for libraries.
SAVE_LIBS=${LIBS}
# FIXME: Replace `main' with a function in `-lc':
AC_CHECK_LIB([c], [printf])
LIBS=${SAVE_LIBS}
# FIXME: Replace `main' with a function in `-lm':
AC_CHECK_LIB([m], [sqrt])

AC_ARG_WITH([debug],AS_HELP_STRING([--with-debug],[compile in debug mode default=no ]),[],[with_debug=no])
AS_IF([test "x$with_debug" != "xno" ],AC_DEFINE([DEBUGGER],[1]) )

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([float.h limits.h stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_STRUCT_TM
AC_C_BIGENDIAN

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_MALLOC
AC_FUNC_MKTIME
AC_FUNC_REALLOC
AC_FUNC_STRFTIME
AC_CHECK_FUNCS([floor memmove memset pow sqrt strcasecmp strchr strrchr strstr strncasecmp strspn ])

AC_SUBST([ARFLAGS])
AC_SUBST([DEBUG])

AC_CONFIG_FILES([Makefile arcview1/Makefile arcview2/Makefile contour/Makefile shapeutil/Makefile])

AC_OUTPUT
