/* $Id$
 * scwm-constraints.h
 *
 * (C) 1998 Greg J. Badros
 */

#include <config.h>

#ifdef USE_CASSOWARY

#include <assert.h>
#include "scwm-constraints.h"
#include "window.h"
#include "xmisc.h"
#include <strstream.h>

void
ScwmClVariable::change_value(Number n) 
{ 
  super::change_value(n);
  strstream ss;
  ss << *this << ends;
  fprintf(stderr,"change_value %s to %g\n",ss.str(),n);
  if (_psw && FXWindowAccessible(dpy,_psw->w)) {
    MovePswToCurrentPosition(_psw);
  }
}

#endif
