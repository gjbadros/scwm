/* $Id$
 * xproperty.c
 *
 * This module is all original code 
 * by Robert Bihlmeyer (additions/modifications by Greg J. Badros)
 *
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>
#include <libguile.h>
#include <X11/X.h>
#include "scwm.h"
#include "window.h"
#include "xproperty.h"
#include "xmisc.h"

/**CONCEPT: X Properties
   X windows has a notion of window properties, which live in the X
server. X window properties are often used to implement protocols
between applications and the window manager, which can have several
levels of standardization, from official X standard, to standardized
by some other organization, to made up informally by groups of
programmers, to specific to one window manager, to specific to an
individual, or installation.

  Scwm already internally implements many of these protocols,
including all X standard protocols, as well as the Motif and Open Look
protocols. However, the user should be able to implement any of these
he likes, including making up his own for personal use.

  This is possible through the low-level xproperty interface. Scwm has
(for now) distinguished Scheme objects representing X properties, and
several procedures to get, set and manipulate them.
 */

SCM
mark_xproperty(SCM obj)
{
  SCM_SETGC8MARK(obj);
  GC_MARK_SCM_IF_SET(XPROPERTYTYPE(obj));

  return SCM_BOOL_F;
}

size_t
free_xproperty(SCM obj)
{
  FREE(XPROPERTYDATA(obj));
  FREE(XPROPERTY(obj));

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
      scm_write(gh_str2scm((char *) XPROPERTYDATA(obj),XPROPERTYLEN(obj)), port);
    }
    FREE(szPropertyType);
  }
  scm_putc('>', port);
  return 1;
}

SCWM_PROC(xproperty_p, "xproperty?", 1, 0, 0,
           (SCM obj))
     /** Return #t if OBJ is an xproperty object, otherwise #f. */
#define FUNC_NAME s_xproperty_p
{
  return SCM_BOOL_FromBool(XPROPERTY_P(obj));
}
#undef FUNC_NAME

/* Note: data passes into ownership of the xproperty and must not be free'd */
static SCM
make_xproperty (char *type, unsigned len, void *data)
{
  SCM answer;
  scwm_xproperty *xprop;

  xprop = NEW(scwm_xproperty);
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

SCWM_PROC(set_window_text_property, "set-window-text-property", 3, 0, 0,
           (SCM win, SCM propname, SCM value))
     /** Set a text property named PROPNAME on WIN.
Uses format 8 (byte) and type "XA_STRING", and VALUE as the data. */
#define FUNC_NAME s_set_window_text_property
{
  if (!WINDOWP(win)) {
    scm_wrong_type_arg(FUNC_NAME, 1, win);
  }
  if (!gh_string_p(propname)) {
    scm_wrong_type_arg(FUNC_NAME, 2, propname);
  }
  if (!gh_string_p(value)) {
    scm_wrong_type_arg(FUNC_NAME, 3, value);
  }
  { /* scope */
    char *szName = gh_scm2newstr(propname, NULL);
    Atom propname = XInternAtom(dpy, szName, False);
    char *szValue = gh_scm2newstr(value, NULL);
    XTextProperty *ptextprop = PNewXTextPropertyFromSz(szValue);
    XSetTextProperty(dpy, PSWFROMSCMWIN(win)->w, ptextprop, propname);
    FREE(szName); szName = NULL;
    FREE(szValue); szValue = NULL;
    FREE(ptextprop); ptextprop = NULL;
  }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(window_xproperty, "window-xproperty", 2, 1, 0,
           (SCM win, SCM name, SCM consume))
     /** Get and maybe delete the property called NAME from WIN.
The property will be deleted upon getting (in an atomic operation)
if the boolean value CONSUME is #t */
#define FUNC_NAME s_window_xproperty
{
  SCM answer;
  Atom type, prop;
  char *str = NULL;
  int format;
  unsigned long len, nitems, bytes_left;
  unsigned char *data = NULL;
  Boolean del;
  
  if (!WINDOWP(win)) {
    scm_wrong_type_arg(FUNC_NAME, 1, win);
  }
  if (!gh_string_p(name)) {
    scm_wrong_type_arg(FUNC_NAME, 2, name);
  }
  if (consume == SCM_UNDEFINED)
    del = False;
  else {
    del = gh_scm2bool(consume);
  }
  
  str=gh_scm2newstr(name, NULL);
  prop=XInternAtom(dpy, str, False);
  FREE(str);
  
  len = 32;			/* try a small size first */
  while (True) {
    XGetWindowProperty(dpy, PSWFROMSCMWIN(win)->w, prop, 0, len,
		       del, AnyPropertyType, &type, &format, &nitems,
		       &bytes_left, &data);
    if (bytes_left)
      len += (bytes_left>>2)+1;	/* adjust size and try again */
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
#undef FUNC_NAME

SCWM_PROC(xproperty_to_string, "xproperty->string", 1, 0, 0,
           (SCM prop))
     /** Convert that data portion of xproperty object PROP to a string. */
#define FUNC_NAME s_xproperty_to_string
{
  if (!XPROPERTY_P(prop)) {
    scm_wrong_type_arg(FUNC_NAME, 1, prop);
  }

  return gh_str2scm((char *)XPROPERTYDATA(prop),XPROPERTYLEN(prop));
}
#undef FUNC_NAME

SCWM_PROC(string_to_xproperty, "string->xproperty", 1, 0, 0,
           (SCM str))
/** Create an xproperty object from STR. */
#define FUNC_NAME s_string_to_xproperty
{
  char *string;
  int len;
  
  if (!gh_string_p(str)) {
    scm_wrong_type_arg(FUNC_NAME, 1, str);
  }

  string=gh_scm2newstr(str,&len);
  return make_xproperty("STRING",len,string);
}
#undef FUNC_NAME

MAKE_SMOBFUNS(xproperty);

void
init_xproperty()
{
  REGISTER_SCWMSMOBFUNS(xproperty);

#ifndef SCM_MAGIC_SNARFER
#include "xproperty.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
