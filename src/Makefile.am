SUBDIRS = native

AM_CPPFLAGS = -I. -I$(top_srcdir)/include -Iinclude -I$(top_srcdir)/src

noinst_LIBRARIES = libcs.a

BUILT_SOURCES = parser.c parser.h re2c lexer.c

libcs_a_SOURCES = eval.cpp args.cpp convert.cpp error.cpp gc.cpp function.cpp lexer.cpp native.cpp object.cpp opcodes.cpp report.cpp srcfile.cpp timer.cpp value.cpp

libcs_a_LIBADD = native/libcsnative.a

bin_PROGRAMS = cscript
cscript_SOURCES = cmdargs.cpp main.cpp
cscript_LDADD = libcs.a native/libcsnative.a

LEMDIR = ${srcdir}/../tools/lemon

parser.c parser.h: parser.in ${LEMDIR}/lemon ${LEMDIR}/lempar.c
	cp ${LEMDIR}/lempar.c ${srcdir}
	${LEMDIR}/lemon ${srcdir}/parser.in


RE2CDIR = ${srcdir}/../tools/re2c
re2c: ${RE2CDIR}/Makefile
	cd ${RE2CDIR} && ./configure && make && cd ${srcdir}

lexer.c: re2c lexer.in
	${RE2CDIR}/re2c -iswb --no-generation-date -o ${srcdir}/lexer.c ${srcdir}/lexer.in
