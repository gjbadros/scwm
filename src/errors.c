/* $Id$ */
/*
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
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

#include "scwm.h"

#include <guile/gh.h>

#include "errors.h"

SCWM_GLOBAL_SYMBOL(sym_scwm_error, "scwm-error");

void 
scwm_error(const char *subr, const char *szErr)
{
  scm_error(sym_scwm_error, subr, "%s",
	    gh_list(gh_str02scm((char *)szErr), SCM_UNDEFINED),
            SCM_UNDEFINED);
}

void
scwm_error_message (SCM message, SCM args)
{
  scm_display_error_message(message, args, scm_current_error_port());
}

void
init_errors()
{
#ifndef SCM_MAGIC_SNARFER
#include "errors.x"
#endif
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

