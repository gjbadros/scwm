/* $Id$
 * xproperty.h
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

#ifndef XPROPERTY_H__
#define XPROPERTY_H__

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <X11/Xlib.h>
#include <assert.h>

Atom InternAtomFromScm(Display *dpy, SCM s, Bool f);

unsigned char *GetXProperty(Window, Atom, Bool, Atom *,
			    int *, unsigned long *);

#define VALIDATE_ARG_ATOM_COPY(pos,scm,cvar) \
  do { \
  assert(sizeof(Atom) == sizeof(unsigned long)); \
  if (gh_number_p(scm)) cvar = (Atom) gh_scm2ulong(scm); \
  else SCWM_WRONG_TYPE_ARG(pos,scm); \
  } while (0)

#define VALIDATE_ARG_ATOM_OR_STRING_COPY(pos,scm,cvar) \
  do { \
  assert(sizeof(Atom) == sizeof(unsigned long)); \
  if (gh_number_p(scm)) cvar = (Atom) gh_scm2ulong(scm); \
  else if (gh_string_p(scm)) { \
    cvar = InternAtomFromScm(dpy,scm,False); \
  } else SCWM_WRONG_TYPE_ARG(pos,scm); \
  } while (0)


#endif /* XPROPERTY_H__ */


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

