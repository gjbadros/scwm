/* $Id$
 * Copyright (C) 1998, 1999 Maciej Stachowiak and Greg J. Badros
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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>
#include "scwm.h"
#include "window.h"
#include "callbacks.h"

SCM window_property_change_hook;


void
signal_window_property_change(SCM win, SCM prop, SCM new_val, SCM old_val)
{
  apply_hooks(window_property_change_hook, 
              gh_list (win, prop, new_val, old_val, SCM_UNDEFINED));
}


SCWM_PROC(set_window_property_x, "set-window-property!", 3, 0, 0,
          (SCM win, SCM prop, SCM val))
     /** Set window property PROP of WIN to VAL.
PROP should be a symbol. VAL may be any Scheme object. This name/value
pair will be associated with the window, and may be retrieved with
`window-property'. Passing #f as the value will delete the property
instead. Soon, some properties will have magical meanings, altering
particular fields in the window structure. Also, a
window-property-change-hook mechanism will soon be implemented for
notification of all window property changes. This is not yet done. The
window property primitives should be considered in flux. */
#define FUNC_NAME s_set_window_property_x
{
  SCM old_val;
  ScwmWindow *psw = NULL;
  if (!WINDOWP(win) || !VALIDWINP(win)) {
    scm_wrong_type_arg (FUNC_NAME, 1, win);
  }

  if (!gh_symbol_p(prop)) {
    scm_wrong_type_arg (FUNC_NAME, 2, prop);
  }

  psw = PSWFROMSCMWIN(win);
 
  old_val = scm_hashq_ref(psw->other_properties, prop, SCM_BOOL_F);

  if (val==SCM_BOOL_F) {
    scm_hashq_remove_x(psw->other_properties, prop);
  } else {
    scm_hashq_set_x(psw->other_properties, prop, val);
  }

  signal_window_property_change(win, prop, val, old_val);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCWM_PROC(window_property, "window-property", 2, 0, 0,
          (SCM win, SCM prop))
     /** Retrieve window property PROP of WIN.
PROP should be a symbol. #f will be returned if the property does not
exist (whether set by `set-window-property!' or otherwise). Soon, some
properties will have magical meanings, accessing particular fields in
the window structure. Also, a window-property-change-hook mechanism
will soon be implemented for notification of all window property
changes. This is not yet done. The window property primitives should
be considered in flux. */
#define FUNC_NAME s_window_property
{
  if (!WINDOWP(win) || !VALIDWINP(win)) {
    scm_wrong_type_arg (FUNC_NAME, 1, win);
  }

  if (!gh_symbol_p(prop)) {
    scm_wrong_type_arg (FUNC_NAME, 2, prop);
  }

  return scm_hashq_ref(PSWFROMSCMWIN(win)->other_properties, prop, SCM_BOOL_F);
}
#undef FUNC_NAME




void 
init_winprop()
{
  SCWM_HOOK(window_property_change_hook,"window-property-change-hook",4);
  /** This hook is invoked whenever a window property changes.
The hook procedures are invoked with four arguments, the window whose
property changed, the name of the changed property, the new value and
the old value. */

#ifndef SCM_MAGIC_SNARFER
#include "winprop.x"
#endif
}
