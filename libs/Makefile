# Generated automatically from Makefile.in by configure.
#
SHELL=/bin/sh



top_srcdir=..
srcdir=.
prefix=/usr
exec_prefix=${prefix}
lib_dir=${prefix}/lib
x_includes=-I/usr/X11R6/include
x_libraries=-L/usr/X11R6/lib
scwm_icons=/usr/X11R6/include/X11/bitmaps:/usr/X11R6/include/X11/pixmaps:/usr/icons
scwm_rc=.scwmrc
scwm_name=scwm

EXTRA_DEFINES=-DXPM -DSHAPE -DACTIVEDOWN_BTNS -DINACTIVE_BTNS -DMINI_ICONS -DVECTOR_BUTTONS -DPIXMAP_BUTTONS -DGRADIENT_BUTTONS -DMULTISTYLE -DEXTENDED_TITLESTYLE -DBORDERSTYLE -DUSEDECOR -DWINDOWSHADE


CC=gcc
INCLUDES=-I${top_srcdir}/include ${x_includes}
CFLAGS=${INCLUDES} -O ${EXTRA_DEFINES}
MAKEDEPEND=makedepend
INSTALL=/usr/bin/install -c
INSTALL_DATA=${INSTALL} -m 644
RM=rm -f
AR=ar cr
RANLIB=ranlib

STATIC_LIB=lib${scwm_name}.a

SRCS=\
	ClientMsg.c \
	GetFont.c \
	Grab.c \
	Module.c \
	Parse.c \
	Picture.c \
	Strings.c \
	System.c \
	envvar.c \
	safemalloc.c \
	sleep.c \
	wild.c


OBJS=\
	ClientMsg.o \
	GetFont.o \
	Grab.o \
	Module.o \
	Parse.o \
	Picture.o \
	Strings.o \
	System.o \
	envvar.o \
	safemalloc.o \
	sleep.o \
	wild.o


all: ${STATIC_LIB}

depend:
	${MAKEDEPEND} ${CFLAGS} ${SRCS}

install: all
	${INSTALL_DATA} ${STATIC_LIB} ${lib_dir}

clean:
	${RM} ${OBJS} ${STATIC_LIB} *~ core *.bak

distclean: clean

${STATIC_LIB}: ${OBJS}
	${AR} ${STATIC_LIB} ${OBJS}
	${RANLIB} ${STATIC_LIB}

#



