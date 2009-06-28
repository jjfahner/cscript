SUBDIRS = native

AM_CPPFLAGS = -I. -I$(top_srcdir)/include -Iinclude -I$(top_srcdir)/src

noinst_LIBRARIES = libcs.a

BUILT_SOURCES = csparser.gen.c cslexer.gen.c xmlparser.gen.c xmllexer.gen.c native.gen.cpp

libcs_a_SOURCES = args.cpp \
		  ast.cpp \
                  context.cpp \
                  convert.cpp \
		  cslexer.cpp \
		  csparser.cpp \
		  datatype.cpp \
		  eval.cpp \
		  gc.cpp \
		  function.cpp \
		  lexstream.cpp \
		  list.cpp \
		  native.cpp \
		  object.cpp \
		  opcodes.cpp \
		  report.cpp \
		  scriptobj.cpp \
		  thread.cpp \
		  timer.cpp \
		  value.cpp \
		  xmllexer.cpp \
		  xmlparser.cpp

libcs_a_LIBADD = native/libcsnative.a

bin_PROGRAMS = cscript
cscript_SOURCES = cmdargs.cpp main.cpp
cscript_LDADD = libcs.a native/libcsnative.a

LEMDIR = ${srcdir}/../tools/posix
csparser.gen.c: csparser.gen.in
	cp ${LEMDIR}/lempar.c ${srcdir}
	${LEMDIR}/lemon ${srcdir}/csparser.gen.in
xmlparser.gen.c: xmlparser.gen.in
	${LEMDIR}/lemon ${srcdir}/xmlparser.gen.in

RE2CDIR = ${srcdir}/../tools/posix
cslexer.gen.c: cslexer.gen.in csparser.gen.c
	${RE2CDIR}/re2c -iswb --no-generation-date -o ${srcdir}/cslexer.gen.c ${srcdir}/cslexer.gen.in
xmllexer.gen.c: xmllexer.gen.in xmlparser.gen.c
	${RE2CDIR}/re2c -iswb --no-generation-date -o ${srcdir}/xmllexer.gen.c ${srcdir}/xmllexer.gen.in

native.gen.cpp: native.awk
	cd ${srcdir} && awk -f native.awk ../include/*.h        >  native.gen.cpp
	cd ${srcdir} && awk -f native.awk ../include/native/*.h >> native.gen.cpp

