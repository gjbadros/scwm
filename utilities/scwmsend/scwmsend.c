/* 
 * scwmsend.c 
 * (modified from gwmsend.c by Greg J. Badros <gjb@cs.washington.edu>)
 * Original Author: Anders Holst (aho@nada.kth.se)
 * Copyright (C) 1994  Anders Holst 
 * This file is copyrighted under the same terms as the rest of 
 * (see the X Inc license for details). There is no warranty that it
 * works. 
 * Compiles with:
 * gcc -o scwmsend scwmsend.c -L/usr/lib/X11R6 -lX11
 * Usage:
 * scwmsend 'scheme-expression'
 */

/*
 * FIXGJB: This should probably be completely rewritten
 */

/* Colas Nahaboo <colas@sa.inria.fr> 22 July 1995:
 * XChangeProperty now in PropModeAppend to avoid race conditions
 * Changed back by gjb, but will look into using PropModeAppend
 */

#include <stdio.h>
#include <stdlib.h>
#include <X11/Xatom.h>
#include <X11/X.h>
#include <X11/Xlib.h>

Display *display;
char *dispName;
Atom XA_SCWM_EXECUTE;
Atom XA_SCWM_RESULT;

void 
die(char *str)
{
  fprintf(stderr, str);
  exit(1);
}

Bool
initDisplay()
{
  if (!(dispName = getenv("DISPLAY")))
    return False;
  if (!(display = XOpenDisplay(dispName)))
    return False;
  XA_SCWM_EXECUTE = XInternAtom(display, "SCWM_EXECUTE", True);
  XA_SCWM_RESULT  = XInternAtom(display, "SCWM_RESULT", True);
  if (XA_SCWM_EXECUTE == None || 
      XA_SCWM_RESULT == None) {
    fprintf(stderr,"Could not intern atoms!\n");
    return False;
  }
  return True;
}

void 
sendScwm(const char *str)
{
  int len = strlen(str);

  XChangeProperty(display, DefaultRootWindow(display),
		  XA_SCWM_EXECUTE, XA_STRING, 8 /* bits in a byte */ ,
		  PropModeReplace, str, len);
  /* Was PropModeAppend, above */
  XFlush(display);
}

char *
SzGetResultProperty()
{
  Atom xproptype;
  int xpropformat = 0;
  unsigned long citems = 0;
  unsigned long bytes_after = 0;
  unsigned char *pchReturn = 0;
  int retval = XGetWindowProperty(display, DefaultRootWindow(display),
				  XA_SCWM_RESULT, 0,
				  1000 /* max length */,
				  False /* no delete */,
				  AnyPropertyType,
				  &xproptype, &xpropformat, &citems,
				  &bytes_after, &pchReturn);

  if (retval != Success || xproptype != XA_STRING) {
    fprintf(stderr,"Did not get result string property correctly!\n");
    return NULL;
  }
  return (char *) pchReturn;
}


void 
main(int argc, char **argv)
{
  char szInput[65536];
  XEvent xev;

  if (argc != 2)
    die("Usage: scwmsend 'expression'\n");
  if (!initDisplay())
    die("Could not connect to server. Check your DISPLAY environment variable.\n");

  XSelectInput(display,DefaultRootWindow(display), PropertyNotify);
  if (strcmp("-i", argv[1]) == 0) {
    fread(szInput, sizeof(char), 65536, stdin);

    sendScwm(szInput);
  } else {
    sendScwm(argv[1]);
  }

  exit(0);
  /* FIXGJB: can't get the property from the root window --
     I get a bad access error; maybe this should just
     wait until I know more about libice ..... --11/13/97 gjb */

  do {
    XNextEvent(display,&xev);
  /*
    while (!XCheckWindowEvent(display,DefaultRootWindow(display),
			      PropertyNotify, &xev));
  */
    fprintf(stderr,"got event\n");
    if (xev.xproperty.atom == XA_SCWM_RESULT) {
    /*      char *szResult = SzGetResultProperty(); */
      char *szResult = "none";
      printf("Result: %s\n",szResult);
      break;
    }
  }
  while (True);
}
