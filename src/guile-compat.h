/* $Id$ */
/*
 *      Copyright (C) 1997, Maciej Stachowiak and Greg J. Badros
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

#include <config.h>

#ifndef HAVE_SCM_PUTS
#define scm_putc(x,y) scm_gen_putc(x,y)
#define scm_puts(x,y) scm_gen_puts(scm_regular_port,x,y)
#endif

#ifndef HAVE_GH_LENGTH
#define gh_length gh_list_length
#endif /* HAVE_GH_LENGTH */

#ifndef HAVE_SCM_INTERNAL_SELECT
#define scm_internal_select select
#endif

SCM scm_parse_path (char *path, SCM tail);

#ifndef HAVE_GH_VECTOR_SET_X
#define gh_vector_set_x gh_vset
#endif

#ifndef HAVE_GH_VECTOR_REF
#define gh_vector_ref gh_vref
#endif

#ifdef HAVE_SCM_THE_LAST_STACK_FLUID
#define DEREF_LAST_STACK scm_fluid_ref(SCM_CDR(scm_the_last_stack_fluid))
#define SET_LAST_STACK(X) scm_fluid_set_x (SCM_CDR (scm_the_last_stack_fluid), (X))

#else
#define DEREF_LAST_STACK SCM_CDR(scm_the_last_stack_var)
#define SET_LAST_STACK(X) SCM_SETCDR(scm_the_last_stack_var, (X))
#endif

#ifndef SCM_EOF_OBJECT_P
#define SCM_EOF_OBJECT_P(x) ((x) == SCM_EOF_VAL)
#endif

#ifndef HAVE_SCM_INTERNAL_CWDR

/* Simulate cwdr with catch. */
#define scm_internal_cwdr(body, body_data, handler, handler_data, \
			  stack_start) \
scm_internal_catch (SCM_BOOL_T, body, body_data, handler, handler_data)
#endif /* SCM_INTERNAL_CWDR */

#ifndef HAVE_SCM_INTERNAL_STACK_CATCH
extern SCM scm_internal_stack_catch (SCM tag,
				     scm_catch_body_t body,
				     void *body_data,
				     scm_catch_handler_t handler,
				     void *handler_data);
#endif

#ifndef HAVE_SCM_INTERNAL_PARSE_PATH
#define scm_internal_parse_path scm_parse_path
#endif

#endif GUILE_COMPAT_H





