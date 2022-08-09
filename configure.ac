#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT([guile-wlroots], [0.0.1], [])
AC_CONFIG_SRCDIR([wlroots])
AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.14 gnu tar-ustar silent-rules subdir-objects \
 color-tests parallel-tests -Woverride -Wno-portability])

AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile wlroots/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_USE_SYSTEM_EXTENSIONS
# Checks for programs.
AC_PROG_CC

# Checks for libraries.
m4_pattern_forbid([PKG_CHECK_MODULES])

GUILE_PKG([3.0])
GUILE_PROGS
GUILE_SITE_DIR

dnl PKG_CHECK_MODULES([WAYLAND_SERVER], [wayland-server])
PKG_CHECK_MODULES([WLROOTS], [wlroots])
PKG_CHECK_VAR([WLROOTS_LIBDIR],[wlroots],[libdir])
dnl PKG_CHECK_VAR([WAYLAND_LIBDIR],[wayland-server],[libdir])
dnl PKG_CHECK_VAR([WAYLAND_SCANNER],[wayland-scanner],[wayland_scanner])
dnl PKG_CHECK_VAR([WAYLAND_PROTOCOLS],[wayland-protocols],[pkgdatadir])
AC_SUBST([WLROOTS_LIBDIR])
AC_SUBST([WAYLAND_LIBDIR])
dnl AC_SUBST([XCB_LIBDIR])

#AC_CHECK_LIB()

# Checks for header files.
AC_CHECK_HEADERS([wayland-server.h])
AC_CHECK_HEADERS([libguile.h])
# Checks for typedefs, structures, and compiler characteristics.
LT_INIT([dlopen])
GUILE_MODULE_REQUIRED([bytestructures guile])
GUILE_MODULE_REQUIRED([wayland])
dnl LT_CONFIG_LTDL_DIR([libltdl])
dnl LTDL_INIT
# Checks for library functions.

#AC_CHECK_LIB([guile],[scm_to_pointer])

AC_OUTPUT
