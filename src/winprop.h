/* $Id$ */
/*
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

#ifndef WINPROP_H
#define WINPROP_H

#include <guile/gh.h>

#undef EXTERN
#undef EXTERN_SET
#ifdef WINPROP_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

EXTERN long scm_tc16_scwm_property_handler;


typedef void (*scwm_prop_setter)(SCM win, SCM new_val);
typedef SCM (*scwm_prop_getter)(SCM win);

typedef struct {
  scwm_prop_setter setter;
  scwm_prop_getter getter;
} scwm_property_handler;


#define PROPERTY_HANDLER_P(X) (SCM_NIMP((X)) && SCM_CAR((X)) == (SCM)scm_tc16_scwm_property_handler)
#define PROPERTY_HANDLER(X)  ((scwm_property_handler *)SCM_CDR((X)))
#define CALL_PH_SETTER(X, win, val) \
    ((*(PROPERTY_HANDLER((X))->setter))(win,val))
#define CALL_PH_GETTER(X, win) ((*(PROPERTY_HANDLER((X))->getter))(win))

void signal_window_property_change(SCM win, SCM prop, SCM new_val, SCM old_val);

void set_property_handler(SCM prop, scwm_property_handler *handler);


#endif /* WINPROP_H */
