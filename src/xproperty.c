/* $Id$
 * xproperty.c
 *
 * This module is all original code 
 * by Robert Bihlmeyer (additions/modifications by Greg J. Badros)
 * It may be used or distributed under either the FVWM license 
 * (see COPYING.fvwm) or the GNU General Public License (see COPYING.GPL and
 * the description below)
 *
 * Copyright (C) 1998, Robert Bihlmeyer and Greg J. Badros
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.GPL.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, this file may alternatively be distributed under 
 * the fvwm license (see COPYING.FVWM).
 *
 */

#define XPROPERTY_IMPLEMENTATION

#include <config.h>
#include <guile/gh.h>
#include <libguile.h>
#include <X11/X.h>
#include "scwm.h"
#include "window.h"
#include "xproperty.h"
#include "xmisc.h"

SCM
mark_xproperty(SCM obj)
{
  if (!SCM_GC8MARKP(obj)) {
    SCM_SETGC8MARK(obj);
    scm_gc_mark(XPROPERTYTYPE(obj));
  }

  return SCM_BOOL_F;
}

size_t
free_xproperty(SCM obj)
{
  free(XPROPERTYDATA(obj));
  free(XPROPERTY(obj));

  return 0;
}

int
print_xproperty(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<xproperty ", port);
  scm_write(XPROPERTYTYPE(obj), port);
  { /* scope */
    char *szPropertyType = gh_scm2newstr(XPROPERTYTYPE(obj), NULL);
    if (STREQ(szPropertyType,"STRING")) {
      scm_puts(", ", port);
      scm_write(gh_str2scm(XPROPERTYDATA(obj),XPROPERTYLEN(obj)), port);
    }
    free(szPropertyType);
  }
  scm_putc('>', port);
  return 1;
}

SCWM_PROC (xproperty_p, "xproperty?", 1, 0, 0,
           (SCM obj))
{
  return SCM_BOOL_FromBool(XPROPERTY_P(obj));
}

/* Note: data passes into ownership of the xproperty and must not be free'd */
static SCM
make_xproperty (char *type, unsigned len, void *data)
{
  SCM answer;
  scwm_xproperty *xprop;

  xprop = (scwm_xproperty *) safemalloc(sizeof(scwm_xproperty));
  xprop->type = gh_str02scm(type);
  xprop->len = len;
  xprop->data = data;

  SCM_DEFER_INTS;
  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, (SCM) scm_tc16_scwm_xproperty);
  SCM_SETCDR(answer, (SCM) xprop);
  SCM_ALLOW_INTS;

  return answer;
}

SCWM_PROC (set_window_text_property, "set-window-text-property", 3, 0, 0,
           (SCM win, SCM propname, SCM value))
{
  Status status;
  if (!WINDOWP(win)) {
    scm_wrong_type_arg(s_set_window_text_property, 1, win);
  }
  if (!gh_string_p(propname)) {
    scm_wrong_type_arg(s_set_window_text_property, 2, propname);
  }
  if (!gh_string_p(value)) {
    scm_wrong_type_arg(s_set_window_text_property, 3, value);
  }
  { /* scope */
    char *szName = gh_scm2newstr(propname, NULL);
    Atom propname = XInternAtom(dpy, szName, False);
    char *szValue = gh_scm2newstr(value, NULL);
    XTextProperty *ptextprop = PNewXTextPropertyFromSz(szValue);
    XSetTextProperty(dpy, SCWMWINDOW(win)->w, ptextprop, propname);
    free(szName); szName = NULL;
    free(szValue); szValue = NULL;
    free(ptextprop); ptextprop = NULL;
  }

  return SCM_UNDEFINED;
}


SCWM_PROC (window_xproperty, "window-xproperty", 2, 1, 0,
           (SCM win, SCM name, SCM consume))
{
  SCM answer;
  Atom type, prop;
  char *str = NULL;
  unsigned int format;
  unsigned long len, nitems, bytes_left;
  unsigned char *data = NULL;
  Boolean del;
  
  if (!WINDOWP(win)) {
    scm_wrong_type_arg(s_window_xproperty, 1, win);
  }
  if (!gh_string_p(name)) {
    scm_wrong_type_arg(s_window_xproperty, 2, name);
  }
  if (consume == SCM_UNDEFINED)
    del = False;
  else {
    del = gh_scm2bool(consume);
  }
  
  str=gh_scm2newstr(name, NULL);
  prop=XInternAtom(dpy, str, False);
  free(str);
  
  len = 32;			/* try a small size first */
  while (True) {
    XGetWindowProperty(dpy, SCWMWINDOW(win)->w, prop, 0, len,
		       del, AnyPropertyType, &type, &format, &nitems,
		       &bytes_left, &data);
    if (bytes_left)
      len += bytes_left>>2+1;	/* adjust size and try again */
    else
      break;
  }
  
  if (type==None)
    return SCM_BOOL_F;
  str=XGetAtomName(dpy, type);
  answer=make_xproperty(str, nitems*format/8, data);
  XFree(str);
  
  return answer;
}

SCWM_PROC (xproperty_to_string, "xproperty->string", 1, 0, 0,
           (SCM prop))
{
  if (!XPROPERTY_P(prop)) {
    scm_wrong_type_arg(s_xproperty_to_string, 1, prop);
  }

  return gh_str2scm(XPROPERTYDATA(prop),XPROPERTYLEN(prop));
}

SCWM_PROC (string_to_xproperty, "string->xproperty", 1, 0, 0,
           (SCM str))
{
  char *string;
  int len;
  
  if (!gh_string_p(str)) {
    scm_wrong_type_arg(s_string_to_xproperty, 1, str);
  }

  string=gh_scm2newstr(str,&len);
  return make_xproperty("STRING",len,string);
}

static scm_smobfuns xproperty_smobfuns =
{
  &mark_xproperty,
  &free_xproperty,
  &print_xproperty,
  0
};

void
init_xproperty()
{
  scm_tc16_scwm_xproperty = scm_newsmob(&xproperty_smobfuns);

#ifndef SCM_MAGIC_SNARFER
#include "xproperty.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
