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

#ifndef GUILE_COMPAT_H
#define GUILE_COMPAT_H

#include <libguile.h>

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Check if the scm variable is undefined or #f -- these cases
   correspond to places where we want to use a default value
   either because the args were omitted, or #f was used to skip
   the argument to get to an argument that the client wanted to 
   specify.
   Intentionally not named SCM_UNSET, since that would imply
   it's part of guile */
#ifndef UNSET_SCM
#define UNSET_SCM(x) (scm_is_eq(x, SCM_UNDEFINED) || scm_is_false(x))
#endif

#define GC_MARK_SCM_IF_SET(scm) do { if (scm && !UNSET_SCM(scm)) \
     { scm_gc_mark(scm); } } while (0)



#ifndef SCM_EOF_OBJECT_P
#define SCM_EOF_OBJECT_P(x) ((x) == SCM_EOF_VAL)
#endif


#if 1
SCM 
scm_internal_cwdr_no_unwind (scm_t_catch_body body, void *body_data,
			     scm_t_catch_handler handler, void *handler_data,
			     SCM_STACKITEM *stack_start);
#endif

#if 1
#ifndef HAVE_SCM_INTERNAL_STACK_CATCH
extern SCM scm_internal_stack_catch (SCM tag,
				     scm_t_catch_body body,
				     void *body_data,
				     scm_t_catch_handler handler,
				     void *handler_data);
#endif
#endif

typedef void (*main_prog_t) (int argc, char **argv);

#endif /* GUILE_COMPAT_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

