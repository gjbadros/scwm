/* $Id$
 * xproperty.h
 *
 * This module is all original code 
 * by Robert Bihlmeyer, modified by Greg J. Badros
 *
 * It may be used or distributed under either the FVWM license 
 * (see COPYING.fvwm) or the GNU General Public License (see COPYING.GPL and
 * the description below)
 *
 *      Copyright (C) 1998, Robert Bihlmeyer
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
#include <config.h>
#endif

#include <guile/gh.h>

#undef EXTERN
#undef EXTERN_SET
#ifdef XPROPERTY_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

EXTERN long scm_tc16_scwm_xproperty;

typedef struct {
  SCM type;			/* property type as scheme string */
  unsigned len;			/* length of data */
  void *data;			/* points to the property's value */
} scwm_xproperty;

#define XPROPERTY_P(X) (SCM_NIMP((X)) && \
			SCM_CAR((X)) == (SCM)scm_tc16_scwm_xproperty)
#define XPROPERTY(X)  ((scwm_xproperty *)SCM_CDR((X)))
#define XPROPERTYTYPE(X) (((scwm_xproperty *)SCM_CDR(X))->type)
#define XPROPERTYLEN(X) (((scwm_xproperty *)SCM_CDR(X))->len)
#define XPROPERTYDATA(X) (((scwm_xproperty *)SCM_CDR(X))->data)

SCM mark_xproperty(SCM obj);
size_t free_xproperty(SCM obj);
int print_xproperty(SCM obj, SCM port, scm_print_state * pstate);

SCM xproperty_p (SCM obj);
SCM window_xproperty (SCM win, SCM name, SCM consume);
SCM xproperty_to_string(SCM prop);
SCM string_to_xproperty(SCM str);

void init_xproperty(void);

#endif /* XPROPERTY_H__ */


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
