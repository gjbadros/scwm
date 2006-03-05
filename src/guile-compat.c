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

#include "guile-compat.h"
#include "scwm-guile.h"

#ifdef __cplusplus
extern "C" {
#endif

#define DBG -1

void  scwm_msg(int , const char *id, const char *msg,...);

#undef USE_STACKJMPBUF

struct cwdr_no_unwind_handler_data {
  int run_handler;
  SCM tag, args;
};

static SCM
cwdr_no_unwind_handler (void *data, SCM tag, SCM args)
{
  struct cwdr_no_unwind_handler_data *c = 
    (struct cwdr_no_unwind_handler_data *) data;

  c->run_handler = 1;
  c->tag = tag;
  c->args = args;
  return SCM_UNSPECIFIED;
}


SCM 
scm_internal_cwdr_no_unwind (scm_t_catch_body body, void *body_data,
			     scm_t_catch_handler handler, void *handler_data,
			     SCM_STACKITEM *stack_start)
{
  return scm_internal_cwdr (body, body_data, handler, handler_data, stack_start);
#if 0
#ifdef USE_STACKJMPBUF
  scm_contregs static_jmpbuf;
#endif
  int old_ints_disabled = scm_ints_disabled;
  SCM old_rootcont;
  struct cwdr_no_unwind_handler_data my_handler_data;
  SCM answer;
  void *pRootContinuation = NULL;

  /* Create a fresh root continuation.  */
  { /* scope */
    SCM new_rootcont;
    SCM_NEWCELL (new_rootcont);
    scwm_defer_ints();
#ifdef USE_STACKJMPBUF
    SCM_SETJMPBUF (new_rootcont, &static_jmpbuf);
#else
    /* GJB:FIXME:MS:: this was leaking, but now I explicitly
       deallocate it, below.  Not sure what fix you were looking
       for so it should probably still be revisited. */
    pRootContinuation =
      scm_must_malloc ((long) sizeof (scm_contregs),
                       "inferior root continuation");
#ifdef SCWM_DEBUG_SCM_INTERNAL_CWDR_NO_UNWIND
    scwm_msg(DBG,"scm_internal_cwdr_no_unwind","+");
#endif
    SCM_SETJMPBUF (new_rootcont,pRootContinuation);
#endif
    SCM_SETCAR (new_rootcont, scm_tc7_contin);
    SCM_DYNENV (new_rootcont) = SCM_EOL;
    SCM_BASE (new_rootcont) = stack_start;
    SCM_SEQ (new_rootcont) = -1;
#ifdef DEBUG_EXTENSIONS
    SCM_DFRAME (new_rootcont) = 0;
#endif
    old_rootcont = scm_rootcont;
    scm_rootcont = new_rootcont;
    scwm_allow_ints();
  }

#ifdef DEBUG_EXTENSIONS
  SCM_DFRAME (old_rootcont) = scm_last_debug_frame;
  scm_last_debug_frame = 0;
#endif

  /* now invoke the function */
  my_handler_data.run_handler = 0;
  answer = scm_internal_catch (SCM_BOOL_T,
                               body, body_data,
                               cwdr_no_unwind_handler, &my_handler_data);

  scwm_defer_ints();
#ifdef SCWM_DEBUG_SCM_INTERNAL_CWDR_NO_UNWIND
  scwm_msg(DBG,"scm_internal_cwdr_no_unwind","-");
#endif
  scm_must_free(pRootContinuation);
  SCM_SETJMPBUF (scm_rootcont, NULL);
#ifdef DEBUG_EXTENSIONS
  scm_last_debug_frame = SCM_DFRAME (old_rootcont);
#endif
  scm_rootcont = old_rootcont;
  scwm_allow_ints();
  scm_ints_disabled = old_ints_disabled;

  /* Now run the real handler iff the body did a throw. */
  if (my_handler_data.run_handler)
    return handler (handler_data, my_handler_data.tag, my_handler_data.args);
  else
    return answer;
#endif /* 0 */
}


#define HAVE_SCM_INTERNAL_STACK_CATCH 1
#ifndef HAVE_SCM_INTERNAL_STACK_CATCH
/* scm_internal_stack_catch
   Use this one if you want debugging information to be stored in
   scm_the_last_stack_{fluid,var} on error. */

static SCM
ss_handler (void *data, SCM tag, SCM throw_args)
{
#if 0
  /* Save the stack */
  SET_LAST_STACK(scm_make_stack (scm_cons(SCM_BOOL_T, SCM_EOL)));
  /* Throw the error */
  return scm_throw (tag, throw_args);
#else
  scm_puts("ss_handler: ", scm_current_error_port());
  scm_print1(tag, scm_current_error_port(), 0);
  scm_puts(": ", scm_current_error_port());
  scm_print1(throw_args, scm_current_error_port(), 0);
  scm_putc('\n', scm_current_error_port());

  scm_handle_by_message_noexit(data, tag, throw_args);
  scm_force_output(scm_current_error_port());
  scm_ithrow(tag, throw_args, 1);
  return SCM_UNSPECIFIED; /* never returns */
#endif } struct cwss_data { SCM tag; scm_t_catch_body body; void *data; }; static SCM
cwss_body (void *data)
{
  struct cwss_data *d = data;
  return scm_internal_lazy_catch (d->tag, d->body, d->data, ss_handler, NULL);
}

SCM
scm_internal_stack_catch (SCM tag,
			  scm_t_catch_body body, void *body_data,
			  scm_t_catch_handler handler, void *handler_data)
{
  struct cwss_data d;
  d.tag = tag;
  d.body = body;
  d.data = body_data;
  return scm_internal_catch (tag, cwss_body, &d, handler, handler_data);
}

#endif /* HAVE_SCM_INTERNAL_STACK_CATCH */


#ifdef __cplusplus
}
#endif


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

