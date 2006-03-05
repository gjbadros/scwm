/* $Id$
 * xproperty.c
 *
 * Copyright (C) 1998-1999, Robert Bihlmeyer and Greg J. Badros
 *
 * This module is all original code 
 * by Robert Bihlmeyer and Greg J. Badros
 *
 * It may be used or distributed under either the FVWM license 
 * (see COPYING.fvwm) or the GNU General Public License (see COPYING.GPL and
 * the description below)
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

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <assert.h>
#include <X11/X.h>

#include <libguile.h>

#define XPROPERTY_IMPLEMENTATION
#include "xproperty.h"

#include "scwm.h"
#include "errors.h"
#include "screen.h"
#include "guile-compat.h"
#include "window.h"
#include "xmisc.h"

/* also used by window.c */
SCWM_GLOBAL_SYMBOL(sym_root_window,"root-window");

SCWM_SYMBOL(sym_replace,"replace");
SCWM_SYMBOL(sym_append,"append");
SCWM_SYMBOL(sym_prepend,"prepend");

/**CONCEPT: X Properties
   X has a notion of window properties, which live in the X
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

Atom InternAtomFromScm(Display *dpy, SCM s, Bool f)
{
  char *sz = scm_to_locale_string(s);
  Atom answer = XInternAtom(dpy, sz, f);
  free(sz);
  return answer;
}


SCM_DEFINE(X_property_set_x, "X-property-set!", 3, 3, 0,
	  (SCM win, SCM name, SCM value, SCM type, SCM format, SCM action),
"Set X property NAME on window WIN to VALUE.\n\
WIN is the window to set the X property on, an X window id, or 'root-window.\n\
NAME and TYPE are strings or X/11 atoms (longs). TYPE defaults to \"STRING\".\n\
FORMAT may be one of the integers 8, 16, and 32, defining the element size\n\
of the VALUE. It is 8 by default.\n\
If FORMAT is 8, VALUE may be a string or a list of null-terminated strings.\n\
Otherwise it will be a vector of FORMAT-bit integers.\n\
ACTION may be one of the symbols 'replace, 'prepend, or 'append signifying\n\
how the new VALUE should be merged (if at all) with the existing\n\
value.")
#define FUNC_NAME s_X_property_set_x
{
  int fmt, len, mode;
  void *val;
  Atom aprop, atype;
  Window w;
  Bool fGotString = False;

  VALIDATE_ARG_WIN_ROOTSYM_OR_NUM_COPY(1,win,w);
  VALIDATE_ARG_ATOM_OR_STRING_COPY(2,name,aprop);

  if (format == SCM_UNDEFINED) {
    fmt=8;
  } else if (scm_is_number(format)) {
    fmt=scm_to_int(format);
    if (fmt != 8 && fmt != 16 && fmt != 32) {
      scwm_error(FUNC_NAME, "FORMAT must be 8, 16, or 32.");
      return SCM_UNSPECIFIED;
    }
  } else {
    SCWM_WRONG_TYPE_ARG(5, format);
  }
  if (fmt == 8 && scm_is_string(value)) {
    fGotString = True;
    val = scm_to_locale_stringn(value, &len);
  } else if (scm_is_true(scm_vector_p(value))) {
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
    default:
      assert(0);		/* we checked this above */
    }
    len = scm_c_vector_length(value);
    v = val = (len == 0 ? NULL : safemalloc(len*fmt/8));
    for (i=0; i<len; i++) {
      SCM el=scm_vector_ref(value, scm_from_int(i));
      if (!scm_is_number(el) || !setter(&v, scm_to_long(el))) {
	SCWM_WRONG_TYPE_ARG(3, value);
      }
    }
  } else {
    SCWM_WRONG_TYPE_ARG(3, value);
  }
  if (type == SCM_UNDEFINED) {
    atype=XA_STRING;
  } else if (scm_is_string(type)) {
    atype = InternAtomFromScm(dpy, type, False);
  } else if (scm_is_number(type)) {
    atype = scm_to_long(type);
  } else {
    /* overly cautious below; both gh_free and FREE just do free() now --01/22/00 gjb */
    if (fGotString) free(val);
    else FREE(val);
    SCWM_WRONG_TYPE_ARG(4, type);
  }
  if (action == SCM_UNDEFINED || action == sym_replace) {
    mode=PropModeReplace;
  } else if (action == sym_prepend) {
    mode=PropModePrepend;
  } else if (action == sym_append) {
    mode=PropModeAppend;
  } else {
  /* overly cautious below; both gh_free and FREE just do free() now --01/22/00 gjb */
    if (fGotString) free(val);
    else FREE(val);
    scwm_error(FUNC_NAME, "ACTION must be one of 'replace, 'prepend, or "
	       "'append");
    /* NOTREACHED */
    return SCM_UNSPECIFIED;
  }

  /* Should this check return code? My man page is silent about possible
     return values. */
  XChangeProperty(dpy, w, aprop, atype, fmt, mode, val, len);
  
  /* overly cautious below; both gh_free and FREE just do free() now --01/22/00 gjb */
  if (fGotString) free(val);
  else FREE(val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* GetXProperty - retrieves an X property PROP from window WIN.
 * The property is deleted afterwards if DEL is True.
 * Returns a pointer to the property value, which must be freed with XFree().
 * TYPE, FMT, NITEMS are set according to the property's properties.
 */
unsigned char *GetXProperty(Window win, Atom prop, Bool del,
			    Atom *type, int *fmt, unsigned long *nitems)
{
  int dwords=32;		/* try a small size first */
  unsigned long bytes_left;
  unsigned char *val = NULL;

  while (True) {
    if (XGetWindowProperty(dpy, win, prop, 0, dwords, del, AnyPropertyType,
			   type, fmt, nitems, &bytes_left, &val)!=Success) {
      break;
    }
    if (!bytes_left)
      break;
    dwords += (bytes_left>>2)+1; /* adjust size and try again */
  }
  return val;
}

/* Return a list of the possibly-multiple
   null-terminated strings in the buffer of
   length cch pointed to by pch */
static
SCM
ScmListOfStringsFromStringLen( char *pch, int cch)
{
  if (strlen(pch) == cch)
    return scm_from_locale_stringn(pch,cch);
  else {
    SCM items = SCM_EOL;
    char *pchNull = pch + cch - 1;
    while (pchNull >= pch) {
      if ('\0' == *pchNull) {
        items = scm_cons(scm_from_locale_string(pchNull+1),items);
      }
      --pchNull;
    }
    items = scm_cons(scm_from_locale_string(pch),items);
    return items;
  }
}

SCM_DEFINE(X_property_get, "X-property-get", 2, 1, 0,
	  (SCM win, SCM name, SCM consume_p),
"Get X property NAME of window WIN.\n\
WIN is the window to check, an X window id, or 'root-window.\n\
NAME is a string or an X/11 atom (long).\n\
If CONSUME? is #t, the X property is deleted after getting it. Default is\n\
not to delete.\n\
If the X property could not be found, #f is returned.\n\
If the X property could be found, a list '(value type format) is returned.\n\
\"type\" is a string.\n\
\"format\" is either 8, 16, or 32, giving the size of the elements of \"value\".\n\
\"value\" is a string, if \"format\" is 8, or a vector of integers otherwise.")
#define FUNC_NAME s_X_property_get
{
  Bool del;
  char *str;
  unsigned char *val;
  INT16 *v16;
  INT32 *v32;
  int i, fmt;
  long len;
  Atom aprop, atype;
  SCM value, type;  Window w;

  VALIDATE_ARG_WIN_ROOTSYM_OR_NUM_COPY(1,win,w);
  VALIDATE_ARG_ATOM_OR_STRING_COPY(2,name,aprop);
  VALIDATE_ARG_BOOL_COPY_USE_F(3,consume_p,del);

  /* GetXProperty will return NULL if the XGetWindowProperty
     fails (e.g., if the window disappears very quickly;
     gimp pops up windows and takes them down quickly enough
     to cause this problems). */
  val = GetXProperty(w, aprop, del, &atype, &fmt, &len);

  if (val == NULL || atype==None)
    return SCM_BOOL_F;

  switch (fmt) {
  case 8:
    value = ScmListOfStringsFromStringLen(val,len);
    break;
  case 16:
    v16=(INT16 *)val;
    value=scm_make_vector(scm_from_int(len), SCM_BOOL_F);
    for (i=0; i<len; i++) {
      scm_vector_set_x(value, scm_from_int(i), scm_from_long(*v16++));
    }
    break;
  case 32:
    v32=(INT32 *)val;
    value=scm_make_vector(scm_from_int(len), SCM_BOOL_F);
    for (i=0; i<len; i++) {
      scm_vector_set_x(value, scm_from_int(i), scm_from_long(*v32++));
    }
    break;
  default:
    scwm_error(FUNC_NAME, "XGetWindowProperty returned format != 8, 16, 32");
    value = SCM_BOOL_F;
  }
  XFree(val);
  str=XGetAtomName(dpy, atype);
  type=scm_from_locale_string(str);
  XFree(str);
  return scm_list_n(value,type,scm_from_int(fmt),SCM_UNDEFINED);
}
#undef FUNC_NAME

SCM_DEFINE(X_property_delete_x, "X-property-delete!", 2, 0, 0,
	  (SCM win, SCM name),
"Delete X property NAME of window WIN.\n\
WIN is the window to check, an X window id, or 'root-window.\n\
NAME is a string. The return value is unspecified.")
#define FUNC_NAME s_X_property_delete_x
{
  Atom aprop;
  Window w;

  VALIDATE_ARG_WIN_ROOTSYM_OR_NUM_COPY(1,win,w);
  VALIDATE_ARG_ATOM_OR_STRING_COPY(2,name,aprop);

  XDeleteProperty(dpy, w, aprop);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(X_properties, "X-properties", 1, 0, 0,
	  (SCM win),
"Returns a list of WIN's X property names.\n\
WIN is the window to query, an X window id, or 'root-window.")
#define FUNC_NAME s_X_properties
{
  Atom *props;
  Window w;
  int n, i;
  char *name;
  SCM properties = SCM_EOL;

  VALIDATE_ARG_WIN_ROOTSYM_OR_NUM_COPY(1,win,w);

  props = XListProperties(dpy, w, &n);
  if (props) {
    for (i=n; i--; ) {
      name = XGetAtomName(dpy, props[i]);
      properties = scm_cons(scm_from_locale_string(name), properties);
      XFree(name);
    }
    XFree(props);
  }
  return properties;
}
#undef FUNC_NAME

/**CONCEPT: X atoms
   X windows allows certain entities (for example, X properties [FIXME: XREF
to X properties]) to have arbitrary names. To avoid exchanging strings ever so
often, these names are in fact X atoms.

New X atoms can be created, or old ones retrieved simply by specifying
the string the atom stands for. An X atom can also be converted back to a
string. Scwm provides primitives for these actions. */

SCM_DEFINE(string_to_X_atom, "string->X-atom", 1, 0, 0,
	  (SCM string),
"Returns an X atom representing STRING.\n\
If STRING contains NULL-characters, the behaviour is undefined.")
#define FUNC_NAME s_string_to_X_atom
{
  char *sz;
  Atom a;

  VALIDATE_ARG_STR_NEWCOPY(1,string,sz);
  a = InternAtomFromScm(dpy,string,False);
  assert(sizeof(Atom) == sizeof(unsigned long));
  return scm_from_ulong((unsigned long)a);
}
#undef FUNC_NAME

SCM_DEFINE(X_atom_to_string, "X-atom->string", 1, 0, 0,
	  (SCM atom),
"Returns the string represented by ATOM.\n\
Returns #f, if the X atom was not known.")
#define FUNC_NAME s_X_atom_to_string
{
  char *sz;
  SCM answer;
  Atom at;

  VALIDATE_ARG_ATOM_COPY(1,atom,at);
  sz = XGetAtomName(dpy, at);
  if (!sz) return SCM_BOOL_F;
  answer = scm_from_locale_string(sz);
  XFree(sz);
  return answer;
}
#undef FUNC_NAME


SCM_DEFINE(X_get_selection_owner,"X-get-selection-owner", 1, 0, 0,
          (SCM atom),
"Return the window that owns the selection denoted by ATOM.\n\
ATOM is likely one of the atoms: \"PRIMARY\" or \"SECONDARY\".\n\
See also `string->X-atom'.  Return value is either a window\n\
object, 'root-window, or an integer window Id.")
#define FUNC_NAME s_X_get_selection_owner
{
  Atom aSelection;
  Window w;
  VALIDATE_ARG_ATOM_OR_STRING_COPY(1,atom,aSelection);
  w = XGetSelectionOwner(dpy,aSelection);
  if (w == Scr.Root) {
    return sym_root_window;
  } else {
    ScwmWindow *psw = PswFromWindow(dpy, w);
    if (psw)
      return SCM_FROM_PSW(psw);
    else
      return scm_from_long(w);
  }
}
#undef FUNC_NAME

SCM_DEFINE(X_convert_selection,"X-convert-selection", 4, 0, 0,
          (SCM selection, SCM target, SCM property, SCM requestor_window),
"Ask the owner of selection SELECTION to provide its value.\n\
The owner should convert the selection to type TARGET and put set\n\
the X property PROPERTY on REQUESTOR-WINDOW when it is transferred.\n\
SELECTION, TARGET, and PROPERTY are each atoms.  REQUESTOR-WINDOW\n\
is a window object or 'root-window.")
#define FUNC_NAME s_X_convert_selection
{
  Atom aSelection, aTarget, aProperty;
  Window reqwin;
  VALIDATE_ARG_ATOM_OR_STRING_COPY(1,selection,aSelection);
  VALIDATE_ARG_ATOM_OR_STRING_COPY(2,target,aTarget);
  VALIDATE_ARG_ATOM_OR_STRING_COPY(3,property,aProperty);
  VALIDATE_ARG_WIN_ROOTSYM_OR_NUM_COPY(4,requestor_window,reqwin);
  XConvertSelection(dpy, aSelection, aTarget, aProperty, reqwin, CurrentTime);
  return SCM_UNDEFINED;
}
#undef FUNC_NAME

void
init_xproperty()
{
#include "xproperty.x"
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* c-hanging-braces-alist: ((brace-list-open) (brace-list-close) (substatement-open after) (block-close . c-snug-do-while) (extern-lang-open before after)) */
/* c-cleanup-list: (brace-else-brace brace-elseif-brace scope-operator list-close-comma defun-close-semi) */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

