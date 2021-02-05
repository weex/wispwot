AC_INIT([wispwot], [0.1], [Draketo])
AC_CONFIG_SRCDIR([run-wispwot.w])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall foreign])

GUILE_PKG([3.0 2.2 2.0 2.9])
GUILE_PROGS
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile installation.])
fi
GUILE_SITE_DIR

dnl set installation prefix for language file to Guile location
AC_PREFIX_PROGRAM([guile])

# Run automake
# Use GNU style.
AM_MAINTAINER_MODE([enable])
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([scripts/wispwot], [chmod +x scripts/wispwot])
AC_CONFIG_FILES([run-wispwot.w:run-wispwot.w], [chmod +x run-wispwot.w])

# add all files which must be available in the build folder
# during make distcheck (there the source folder is read only).
AC_CONFIG_LINKS([AUTHORS.in:AUTHORS.in])
AC_OUTPUT