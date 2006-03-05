/* $Id$
 * Copyright (C) 1998, 1999, 2000  Greg J. Badros and Maciej Stachowiak 
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
#include "scwmconfig.h"
#endif

#define WINPROP_IMPLEMENTATION

#include "winprop.h"

#include "scwm.h"
#include "window.h"
#include "callbacks.h"

#define HANDLER_TABLE_SIZE 7

SCWM_HOOK(window_property_change_hook,"window-property-change-hook",4,
"This hook is invoked whenever a window property changes.\n\n"
"The hook procedures are invoked with four arguments, the window whose\n"
"property changed, the name of the changed property, the new value and\n"
"the old value.");

SCM property_handler_hash_table;


SCM
mark_property_handler(SCM ARG_IGNORE(obj))
{
  return SCM_BOOL_F;
}

size_t 
free_property_handler(SCM ARG_IGNORE(obj))
{
  return 0;
}

/* GJB:FIXME:MS: Maybe we should register a descriptive string name with the
   property handler and print that here? */
int 
print_property_handler(SCM ARG_UNUSED(obj), SCM port, scm_print_state *ARG_IGNORE(pstate))
{
  scm_puts("#<property-handler>", port);
  return 1;
}


void set_property_handler(SCM prop, scwm_property_handler *handler)
{
  SCM new_obj;
  SCWM_NEWCELL_SMOB(new_obj, scm_tc16_scwm_property_handler, handler);
  scm_hashq_set_x (property_handler_hash_table, prop, new_obj);
}


void
signal_window_property_change(SCM win, SCM prop, SCM new_val, SCM old_val)
{
  scwm_run_hook(window_property_change_hook, 
                scm_list_n(win, prop, new_val, old_val, SCM_UNDEFINED));
}


SCM_DEFINE(set_window_property_x, "set-window-property!", 3, 0, 0,
          (SCM win, SCM prop, SCM val),
"Set window property PROP of WIN to VAL.\n\
PROP should be a symbol. VAL may be any Scheme object. This name/value\n\
pair will be associated with the window, and may be retrieved with\n\
`window-property'. Passing #f as the value will delete the property\n\
instead. Soon, some properties will have magical meanings, altering\n\
particular fields in the window structure. Also, a\n\
window-property-change-hook mechanism will soon be implemented for\n\
notification of all window property changes. This is not yet done. The\n\
window property primitives should be considered in flux.")
#define FUNC_NAME s_set_window_property_x
{
  SCM old_val;
  SCM handler;
  ScwmWindow *psw;

  VALIDATE_ARG_WIN_COPY(1,win,psw);
  VALIDATE_ARG_SYM(2,prop);

  psw = PSWFROMSCMWIN(win);
 
  handler = scm_hashq_ref(property_handler_hash_table, prop, SCM_BOOL_F);

  if (scm_is_true(handler)) {
    CALL_PH_SETTER(handler, win, val);
  } else {
    old_val = scm_hashq_ref(psw->other_properties, prop, SCM_BOOL_F);
    
    if (scm_is_false(val)) {
      scm_hashq_remove_x(psw->other_properties, prop);
    } else {
      scm_hashq_set_x(psw->other_properties, prop, val);
    }
    
    signal_window_property_change(win, prop, val, old_val);
  }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE(window_property, "window-property", 2, 0, 0,
          (SCM win, SCM prop),
"Retrieve window property PROP of WIN.\n\
PROP should be a symbol. #f will be returned if the property does not\n\
exist (whether set by `set-window-property!' or otherwise). Soon, some\n\
properties will have magical meanings, accessing particular fields in\n\
the window structure. Also, a window-property-change-hook mechanism\n\
will soon be implemented for notification of all window property\n\
changes. This is not yet done. The window property primitives should\n\
be considered in flux.")
#define FUNC_NAME s_window_property
{
  SCM handler;
  ScwmWindow *psw;

  VALIDATE_ARG_WIN_COPY(1,win,psw);
  VALIDATE_ARG_SYM(2,prop);

  handler = scm_hashq_ref(property_handler_hash_table, prop, SCM_BOOL_F);

  if (scm_is_true(handler)) {
    return CALL_PH_GETTER(handler, win);
  } else {
    return scm_hashq_ref(PSWFROMSCMWIN(win)->other_properties, 
			 prop, SCM_BOOL_F);
  }
}
#undef FUNC_NAME


void 
init_winprop()
{
  REGISTER_SCWMSMOBFUNS(property_handler);

  property_handler_hash_table = 
    scm_make_vector (scm_from_int(HANDLER_TABLE_SIZE), SCM_EOL);

  scm_permanent_object(property_handler_hash_table);

#include "winprop.x"
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */
