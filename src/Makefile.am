SUBDIRS = native

AM_CPPFLAGS = -I. -I$(top_srcdir)/include -Iinclude -I$(top_srcdir)/src

noinst_LIBRARIES = libcs.a

BUILT_SOURCES = parser.c parser.h lexer.c

libcs_a_SOURCES = eval.cpp args.cpp convert.cpp error.cpp gc.cpp function.cpp lexer.cpp native.cpp object.cpp opcodes.cpp report.cpp srcfile.cpp timer.cpp value.cpp

libcs_a_LIBADD = native/libcsnative.a

bin_PROGRAMS = cscript
cscript_SOURCES = cmdargs.cpp main.cpp
cscript_LDADD = libcs.a native/libcsnative.a

LEMDIR = ${srcdir}/../tools/posix
parser.c parser.h: parser.in
	cp ${LEMDIR}/lempar.c ${srcdir}
	${LEMDIR}/lemon ${srcdir}/parser.in

RE2CDIR = ${srcdir}/../tools/posix
lexer.c: lexer.in
	${RE2CDIR}/re2c -iswb --no-generation-date -o ${srcdir}/lexer.c ${srcdir}/lexer.in
