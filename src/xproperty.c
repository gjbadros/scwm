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
#include <assert.h>
#include "scwm.h"
#include "errors.h"
#include "screen.h"
#include "guile-compat.h"
#include "window.h"
#include "xproperty.h"
#include "xmisc.h"

/* also used by window.c */
SCWM_GLOBAL_SYMBOL(sym_root_window,"root-window");

SCWM_SYMBOL(sym_replace,"replace");
SCWM_SYMBOL(sym_append,"append");
SCWM_SYMBOL(sym_prepend,"prepend");

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

  This is possible through the low-level X property interface. Scwm has
one procedure to get, and set them, respectively.
 */

static int Set8Value(void **dest, long el)
{
  if (el < -0x80 || el > 0x7f)
    return 0;
  **(INT8 **)dest=el;
  (*(INT8 **)dest)++;
  return 1;
}

static int Set16Value(void **dest, long el)
{
  if (el < -0x8000 || el > 0x7fff)
    return 0;
  **(INT16 **)dest=el;
  (*(INT16 **)dest)++;
  return 1;
}

static int Set32Value(void **dest, long el)
{
  **(INT32 **)dest=el;
  (*(INT32 **)dest)++;
  return 1;
}

SCWM_PROC(X_property_set_x, "X-property-set!", 3, 3, 0,
	  (SCM win, SCM name, SCM value, SCM type, SCM format, SCM action))
     /** Set X property NAME on window WIN to VALUE.
WIN is the window to set the X property on, or 'root-window.
NAME and TYPE are strings. TYPE defaults to "STRING".
FORMAT may be one of the integers 8, 16, and 32, defining the element size
of the VALUE. It is 8 by default.
VALUE may be a string, if FORMAT is 8, and may always be a vector
of FORMAT-bit integers.
ACTION - one of the symbols 'replace, 'prepend, or 'append signifying
how the new VALUE should be merged (if at all) with the existing
value. */
#define FUNC_NAME s_X_property_set_x
{
  int fmt, len, mode;
  void *val;
  char *str;
  Atom aprop, atype;
  Window w;

  if (win == sym_root_window) {
    w = Scr.Root;
  } else if (WINDOWP(win)) {
    w = PSWFROMSCMWIN(win)->w;
  } else {
    scm_wrong_type_arg(FUNC_NAME, 1, win);
  }
  if (!gh_string_p(name)) {
    scm_wrong_type_arg(FUNC_NAME, 2, name);
  }
  if (format == SCM_UNDEFINED) {
    fmt=8;
  } else if (gh_number_p(format)) {
    fmt=gh_scm2int(format);
    if (fmt != 8 && fmt != 16 && fmt != 32) {
      scwm_error(FUNC_NAME, "FORMAT must be 8, 16, or 32.");
      return SCM_UNSPECIFIED;
    }
  } else {
    scm_wrong_type_arg(FUNC_NAME, 5, format);
  }
  if (fmt == 8 && gh_string_p(value)) {
    val=gh_scm2newstr(value, &len);
  } else if (gh_vector_p(value)) {
    int i;
    int (*setter)(void **, long);
    void *v;
    switch (fmt) {
    case 8:
      setter=&Set8Value;
      break;
    case 16:
      setter=&Set16Value;
      break;
    case 32:
      setter=&Set32Value;
      break;
    }
    len=gh_vector_length(value);
    v=val=safemalloc(len*fmt/8);
    for (i=0; i<len; i++) {
      SCM el=gh_vector_ref(value, gh_int2scm(i));
      if (!gh_number_p(el) || !setter(&v, gh_scm2long(el))) {
	scm_wrong_type_arg(FUNC_NAME, 3, value);
      }
    }
  } else {
    scm_wrong_type_arg(FUNC_NAME, 3, value);
  }
  if (type == SCM_UNDEFINED) {
    atype=XA_STRING;
  } else if (gh_string_p(type)) {
    str=gh_scm2newstr(type, NULL);
    atype=XInternAtom(dpy, str, False);
    FREE(str);
  } else {
    FREE(val);
    scm_wrong_type_arg(FUNC_NAME, 4, value);
  }
  if (action == SCM_UNDEFINED || action == sym_replace) {
    mode=PropModeReplace;
  } else if (action == sym_prepend) {
    mode=PropModePrepend;
  } else if (action == sym_append) {
    mode=PropModeAppend;
  } else {
    FREE(val);
    scwm_error(FUNC_NAME, "ACTION must be one of 'replace, 'prepend, or "
	       "'append");
  }
  str=gh_scm2newstr(name, NULL);
  aprop=XInternAtom(dpy, str, False);
  FREE(str);

  /* Should this check return code? My man page is silent about possible
     return values. */
  XChangeProperty(dpy, w, aprop, atype, fmt, mode, val, len);
  FREE(val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(X_property_get, "X-property-get", 2, 1, 0,
	  (SCM win, SCM name, SCM consume))
     /** Get X property NAME of window WIN.
WIN is the window to check, or 'root-window.
NAME is a string.
If CONSUME is #t, the X property is deleted after getting it. Default is
not to delete.
If the X property could not be found, #f is returned.
If the X property could be found, a list "(value type format)" is returned.
"type" is a string.
"format" is either 8, 16, or 32, giving the size of the elements of "value".
"value" is a string, if "format" is 8, or a vector of integers otherwise. */
#define FUNC_NAME s_X_property_get
{
  Boolean del;
  char *str;
  unsigned char *val;
  INT16 *v16;
  INT32 *v32;
  int i, dwords, fmt;
  long len, bytes_left;
  Atom aprop, atype;
  SCM value, type;
  Window w;

  if (win == sym_root_window) {
    w = Scr.Root;
  } else if (WINDOWP(win)) {
    w = PSWFROMSCMWIN(win)->w;
  } else {
    scm_wrong_type_arg(FUNC_NAME, 1, win);
  }
  if (!gh_string_p(name)) {
    scm_wrong_type_arg(FUNC_NAME, 2, name);
  }
  if (consume == SCM_UNDEFINED) {
    del=False;
  } else if (gh_boolean_p(consume)) {
    del=gh_scm2bool(consume);
  } else {
    scm_wrong_type_arg(FUNC_NAME, 3, consume);
  }

  str=gh_scm2newstr(name, NULL);
  aprop=XInternAtom(dpy, str, False);
  FREE(str);

  dwords=32;			/* try a small size first */
  while (True) {
    XGetWindowProperty(dpy, w, aprop, 0, dwords, del, AnyPropertyType,
		       &atype, &fmt, &len, &bytes_left, &val);
    if (bytes_left)
      dwords += (bytes_left>>2)+1;	/* adjust size and try again */
    else
      break;
  }

  if (atype==None)
    return SCM_BOOL_F;

  switch (fmt) {
  case 8:
    value=gh_str2scm(val, len);
    break;
  case 16:
    v16=(INT16 *)val;
    value=gh_make_vector(gh_int2scm(len), SCM_UNSPECIFIED);
    for (i=0; i<len; i++) {
      gh_vector_set_x(value, gh_int2scm(i), gh_long2scm(*v16++));
    }
    break;
  case 32:
    v32=(INT32 *)val;
    value=gh_make_vector(gh_int2scm(len), SCM_UNSPECIFIED);
    for (i=0; i<len; i++) {
      gh_vector_set_x(value, gh_int2scm(i), gh_long2scm(*v32++));
    }
    break;
  }
  XFree(val);
  str=XGetAtomName(dpy, atype);
  type=gh_str02scm(str);
  XFree(str);
  return gh_list(value,type,gh_int2scm(fmt),SCM_UNDEFINED);
}
#undef FUNC_NAME

/**CONCEPT: X atoms
   X windows allows certain entities (for example, X properties [FIXME: XREF
to X properties]) to have arbitrary names. To avoid exchanging strings ever so
often, these names are in fact X atoms.

New X atoms can be created, or old ones retrieved simply by specifying
the string the atom stands for. An X atom can also be converted back to a
string. Scwm provides primitives for these actions. */

SCWM_PROC(string_to_X_atom, "string->X-atom", 1, 0, 0,
	  (SCM string))
     /** Returns an X atom representing STRING.
If STRING contains NULL-characters, the behaviour is undefined. */
#define FUNC_NAME s_string_to_X_atom
{
  char *str;
  Atom a;

  if (!gh_string_p(string)) {
    scm_wrong_type_arg(FUNC_NAME, 1, string);
  }
  str=gh_scm2newstr(string, NULL);
  a=XInternAtom(dpy, str, False);
  FREE(str);
  assert(sizeof(Atom) == sizeof(unsigned long));
  return gh_ulong2scm((unsigned long)a);
}
#undef FUNC_NAME

SCWM_PROC(X_atom_to_string, "X-atom->string", 1, 0, 0,
	  (SCM atom))
     /** Returns the string represented by ATOM.
Returns #f, if the X atom was not known. */
#define FUNC_NAME s_X_atom_to_string
{
  char *str;
  SCM ret;

  if (!gh_number_p(atom)) {
    scm_wrong_type_arg(FUNC_NAME, 1, atom);
  }
  assert(sizeof(Atom) == sizeof(unsigned long));
  str=XGetAtomName(dpy, gh_scm2ulong(atom));
  if (str == NULL) {
    return SCM_BOOL_F;
  }
  ret=gh_str02scm(str);
  XFree(str);
  return ret;
}
#undef FUNC_NAME

void
init_xproperty()
{
#ifndef SCM_MAGIC_SNARFER
#include "xproperty.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* c-hanging-braces-alist: ((brace-list-open) (brace-list-close) (substatement-open after) (block-close . c-snug-do-while) (extern-lang-open before after)) */
/* c-cleanup-list: (brace-else-brace brace-elseif-brace scope-operator list-close-comma defun-close-semi) */
/* End: */
