AM_CPPFLAGS = -I. -I$(top_srcdir)/include -Iinclude -I$(top_srcdir)/src

noinst_LIBRARIES = libcsregex.a

BUILT_SOURCES = parser.gen.c

libcsregex_a_SOURCES = compiler.cpp regex.cpp

LEMDIR = ${srcdir}/../../tools/posix
parser.gen.c: parser.gen.in
	cp ${LEMDIR}/lempar.c ${srcdir}
	${LEMDIR}/lemon ${srcdir}/parser.gen.in
