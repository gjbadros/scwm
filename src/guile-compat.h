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

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include <guile/gh.h>

/* pair gh_free with gh_scm2newstr, etc. */
#ifndef gh_free
#define gh_free(x) free(x)
#endif

/* Check if the scm variable is undefined or #f -- these cases
   correspond to places where we want to use a default value
   either because the args were omitted, or #f was used to skip
   the argument to get to an argument that the client wanted to 
   specify.
   Intentionally not named SCM_UNSET, since that would imply
   it's part of guile */
#define UNSET_SCM(x) (((x) == SCM_UNDEFINED) || ((x) == SCM_BOOL_F))

#define GC_MARK_SCM_IF_SET(scm) do { if (scm && !UNSET_SCM((scm))) \
     { scm_gc_mark((scm)); } } while (0)

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

#ifndef HAVE_SCM_INTERNAL_PARSE_PATH
SCM scm_parse_path (char *path, SCM tail);
#endif

#ifndef HAVE_GH_VECTOR_SET_X
#define gh_vector_set_x gh_vset
#endif

#ifndef HAVE_GH_VECTOR_REF
#define gh_vector_ref gh_vref
#endif

#ifdef HAVE_SCM_THE_LAST_STACK_FLUID
  /* from libguile/fluids.h --07/01/98 gjb */
SCM scm_fluid_ref SCM_P ((SCM fluid));
SCM scm_fluid_set_x SCM_P ((SCM fluid, SCM value));
#define DEREF_LAST_STACK scm_fluid_ref(gh_cdr(scm_the_last_stack_fluid))
#define SET_LAST_STACK(X) scm_fluid_set_x (gh_cdr (scm_the_last_stack_fluid), (X))

#else
#define DEREF_LAST_STACK gh_cdr(scm_the_last_stack_var)
#define SET_LAST_STACK(X) gh_set_cdr_x(scm_the_last_stack_var, (X))
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

SCM 
scm_internal_cwdr_no_unwind (scm_catch_body_t body, void *body_data,
			     scm_catch_handler_t handler, void *handler_data,
			     SCM_STACKITEM *stack_start);



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

#ifndef gh_memq
#define gh_memq scm_memq
#endif

typedef void (*main_prog_t) (int argc, char **argv);

#ifdef HAVE_SCM_LOAD_STARTUP_FILES
#define scwm_gh_enter gh_enter
#else /* !HAVE_SCM_LOAD_STARTUP_FILES */
void scwm_gh_enter (int argc, char *argv[], main_prog_t c_main_prog);
#endif /* !HAVE_SCM_LOAD_STARTUP_FILES */

#ifdef __cplusplus
}
#endif


SCM make_output_strport(char *fname);
#ifndef HAVE_SCM_STRPORT_TO_STRING
SCM scm_strport_to_string(SCM port);
#endif

#endif GUILE_COMPAT_H

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

