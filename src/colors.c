/* $Id$
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
 *
 * This module is derived from code written by Rob Nation 
 * Copyright 1993 Robert Nation. No restrictions are placed on this code,
 * as long as the copyright notice is preserved
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <pwd.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>

#include "colors.h"

#include "scwm.h"
#include "screen.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

/* Defined in error.c */
extern SCM sym_scwm_error;

static void 
nocolor(char *note, char *name)
{
  char err[512];
  scwm_msg(ERR, "nocolor", "cannot %s color %s", note, name);
  /* Do it by hand until we come up w/ a better error API. */

  sprintf(err, "cannot %s color %s", note, name);
  scm_error(sym_scwm_error, "[color allocation]", "%s",
	    gh_list(gh_str02scm(err), SCM_UNDEFINED),
	    gh_list(gh_str02scm(err), SCM_UNDEFINED));
}

/*
 * Allocates a linear color gradient (veliaa@rpi.edu)
 * GJB:FIXME:: this should use color objects, not strings
 */
static Pixel *
AllocLinearGradient(char *s_from, char *s_to, int npixels)
{
  Pixel *pixels;
  XColor from, to, c;
  int r, dr, g, dg, b, db;
  int i = 0, got_all = 1;

  if (npixels < 1)
    return NULL;
  if (!s_from || !XParseColor(dpy, Scr.ScwmRoot.attr.colormap, s_from, &from)) {
    nocolor("parse", s_from);
    return NULL;
  }
  if (!s_to || !XParseColor(dpy, Scr.ScwmRoot.attr.colormap, s_to, &to)) {
    nocolor("parse", s_to);
    return NULL;
  }
  c = from;
  r = from.red;
  dr = (to.red - from.red) / npixels;
  g = from.green;
  dg = (to.green - from.green) / npixels;
  b = from.blue;
  db = (to.blue - from.blue) / npixels;
  pixels = NEWC(npixels,Pixel);
  c.flags = DoRed | DoGreen | DoBlue;
  for (; i < npixels; ++i) {
    if (!XAllocColor(dpy, Scr.ScwmRoot.attr.colormap, &c))
      got_all = 0;
    pixels[i] = c.pixel;
    c.red = (unsigned short) (r += dr);
    c.green = (unsigned short) (g += dg);
    c.blue = (unsigned short) (b += db);
  }
  if (!got_all) {
    char s[256];

    sprintf(s, "color gradient %s to %s", s_from, s_to);
    nocolor("alloc", s);
  }
  return pixels;
}


/*
 * Allocates a nonlinear color gradient (veliaa@rpi.edu)
 */
Pixel *
AllocNonlinearGradient(char *s_colors[], int clen[],
		       int nsegs, int npixels)
{
  Pixel *pixels = NEWC(npixels,Pixel);
  int i = 0, curpixel = 0, perc = 0;

  if (nsegs < 1) {
    scwm_msg(ERR, "AllocNonlinearGradient",
	     "must specify at least one segment");
    FREE(pixels);
    return NULL;
  }
  for (; i < npixels; i++)
    pixels[i] = 0;

  for (i = 0; (i < nsegs) && (curpixel < npixels) && (perc <= 100); ++i) {
    Pixel *p;
    int j = 0, n = clen[i] * npixels / 100;

    p = AllocLinearGradient(s_colors[i], s_colors[i + 1], n);
    if (!p) {
      scwm_msg(ERR, "AllocNonlinearGradient",
	       "could not allocate gradient");
      FREEC(pixels);
      return NULL;
    }
    for (; j < n; ++j)
      pixels[curpixel + j] = p[j];
    perc += clen[i];
    curpixel += n;
    FREEC(p);
  }
  for (i = curpixel; i < npixels; ++i)
    pixels[i] = pixels[i - 1];
  return pixels;
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

