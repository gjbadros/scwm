/* 
   scwmsend.c (modified from gwmsend.c by Greg J. Badros)
   Author: Anders Holst (aho@nada.kth.se)
   Copyright (C) 1994  Anders Holst 
     This file is copyrighted under the same terms as the rest of SCWM
     (see the X Inc license for details). There is no warranty that it
     works. 
   Compiles with:
     gcc -o scwmsend scwmsend.c -L/usr/lib/X11R6 -lX11
   Usage:
     scwmsend 'scheme-expression'
 */

/* Colas Nahaboo <colas@sa.inria.fr> 22 July 1995:
 * XChangeProperty now in PropModeAppend to avoid race conditions
 */

#include <stdio.h>
#include <stdlib.h>
#include <X11/Xatom.h>
#include <X11/X.h>
#include <X11/Xlib.h>

Display* display;
char* dispName;
Atom scwmprop;
char buf[512];

void die(char* str) 
{ 
  fprintf(stderr, str);
  exit(1);
}

int initDisplay()
{
  if (!(dispName = getenv("DISPLAY")))
    return 0;
  if (!(display = XOpenDisplay(dispName)))
    return 0;
  scwmprop = XInternAtom(display, "SCWM_EXECUTE", 1); 
  if (scwmprop == None)
    return 0;
  return 1;
}

void sendScwm(const char *str)
{
  int len = strlen(str);
  XChangeProperty(display, DefaultRootWindow(display),
                  scwmprop, XA_STRING, 8 /* bits in a byte */,
		  PropModeReplace, str, len);
  /* Was PropModeAppend, above */
  XFlush(display); 
}

void main(int argc, char** argv)
{
  char szInput[65536];
  if (argc != 2)
    die("Usage: scwmsend 'expression'\n");
  if (!initDisplay())
    die("Could not connect to server. Check your DISPLAY environment variable.\n");
  if (strcmp("-i",argv[1]) == 0 ) {
     fread(szInput,sizeof(char),65536,stdin);
     sendScwm(szInput);
  } else {
    sendScwm(argv[1]);
  }
}
