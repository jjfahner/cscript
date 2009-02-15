AM_CPPFLAGS = -I. -I$(top_srcdir)/include -Iinclude -I$(top_srcdir)/src
noinst_LIBRARIES = libcs.a
libcs_a_SOURCES = args.cpp consio.cpp convert.cpp diskio.cpp error.cpp eval.cpp file.cpp function.cpp io.cpp lexer.cpp native.cpp object.cpp regex.cpp report.cpp socket.cpp typeinfo.cpp value.cpp
bin_PROGRAMS = cscript
cscript_SOURCES = cmdargs.cpp main.cpp
cscript_LDADD = libcs.a
