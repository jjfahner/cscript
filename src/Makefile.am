SUBDIRS = native
AM_CPPFLAGS = -I. -I$(top_srcdir)/include -Iinclude -I$(top_srcdir)/src
noinst_LIBRARIES = libcs.a
libcs_a_SOURCES = args.cpp convert.cpp error.cpp eval.cpp srcfile.cpp function.cpp lexer.cpp native.cpp object.cpp opcodes.cpp report.cpp timer.cpp value.cpp
libcs_a_LIBADD = native/libcsnative.a
bin_PROGRAMS = cscript
cscript_SOURCES = cmdargs.cpp main.cpp
cscript_LDADD = libcs.a native/libcsnative.a
