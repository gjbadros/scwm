/* $Id$ */
/*
 *      Copyright (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
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

#include <sys/time.h>
#include <unistd.h>
#include <values.h>
#include <guile/gh.h>
#include <config.h>
#include <libguile.h>
#include "scwm.h"
#include "callbacks.h"
#include "guile-compat.h"

SCM timer_hooks;
SCM error_hook;

struct scwm_body_apply_data {
  SCM proc;
  SCM args;
};

SCM 
scwm_handle_error (void *data, SCM tag, SCM throw_args)
{
  SCM port = scm_def_errp;

  if (scm_ilength (throw_args) >= 3)
    {
      SCM stack = DEREF_LAST_STACK;
      SCM subr = SCM_CAR (throw_args);
      SCM message = SCM_CADR (throw_args);
      SCM args = SCM_CADDR (throw_args);

      scm_newline (port);
      scm_display_backtrace (stack, port, SCM_UNDEFINED, SCM_UNDEFINED);
      scm_newline (port);
      scm_display_error (stack, port, subr, message, args, SCM_EOL);
      return SCM_BOOL_F;
    }
  else
    {
      scm_puts ("uncaught throw to ", port);
      scm_prin1 (tag, port, 0);
      scm_puts (": ", port);
      scm_prin1 (throw_args, port, 1);
      scm_putc ('\n', port);
      exit (2);
    }
  return apply_hooks_message_only(error_hook, gh_cons(tag, throw_args));
}

static SCM
scwm_body_apply (void *body_data)
{
  struct scwm_body_apply_data *ad = (struct scwm_body_apply_data *) body_data;
  return scm_apply (ad->proc, ad->args, SCM_EOL);
}


/* Use scm_internal_cwdr to establish a new dynamic root - this causes
   all throws to be caught and prevents continuations from exiting the
   dynamic scope of the callback. This is needed to prevent callbacks
   from disrupting scwm's flow control, which would likely cause a
   crash. */

static SCM
ssdr_handler (void *data, SCM tag, SCM throw_args)
{
  /* Save the stack */
  SET_LAST_STACK(scm_make_stack (scm_cons (SCM_BOOL_T, SCM_EOL)));
  /* Throw the error */
  return scm_throw (tag, throw_args);
}

struct cwssdr_data
{
  SCM tag;
  scm_catch_body_t body;
  void *data;
};

static SCM
cwssdr_body (void *data)
{
  struct cwssdr_data *d = (struct cwssdr_data *) data;
  return scm_internal_lazy_catch (d->tag, d->body, d->data, ssdr_handler, 
				  NULL);
}

SCM
scm_internal_stack_cwdr (scm_catch_body_t body,
			 void *body_data,
			 scm_catch_handler_t handler,
			 void *handler_data,
			 SCM_STACKITEM *stack_item)
{
  struct cwssdr_data d;
  d.tag = SCM_BOOL_T;
  d.body = body;
  d.data = body_data;
  return scm_internal_cwdr (cwssdr_body, &d, handler, handler_data, 
			    stack_item);
}



SCM
scwm_safe_apply (SCM proc, SCM args)
{
  SCM_STACKITEM stack_item;
  struct scwm_body_apply_data apply_data;

  apply_data.proc = proc;
  apply_data.args = args;

#if 0
  return scm_internal_stack_catch(SCM_BOOL_T,scwm_body_apply, &apply_data,
				  scwm_handle_error, "scwm");
#endif
  return scm_internal_stack_cwdr(scwm_body_apply, &apply_data,
				 scwm_handle_error, "scwm",
				 &stack_item);
}


SCM
scwm_safe_apply_message_only (SCM proc, SCM args)
{
  SCM_STACKITEM stack_item;
  struct scwm_body_apply_data apply_data;

  apply_data.proc = proc;
  apply_data.args = args;

  return scm_internal_cwdr(scwm_body_apply, &apply_data,
			   scm_handle_by_message_noexit, "scwm",
			   &stack_item);
}

SCM
scwm_safe_call0 (SCM thunk)
{
  return scwm_safe_apply (thunk, SCM_EOL);
}


SCM
scwm_safe_call1 (SCM proc, SCM arg)
{
  /* This means w must cons (albeit only once) on each callback of
     size one - seems lame. */
  return scwm_safe_apply (proc, scm_listify(arg, SCM_UNDEFINED));
}

/* Slightly tricky - we want to catch errors per expression, but only
   establish a new dynamic root per load operation, as it's perfectly
   OK for a file to invoke a continuation created by a different
   expression in the file as far as scwm is concerned. So we set a
   dynamic root for the whole load operation, but also catch on each
   eval. */


static SCM
scwm_body_eval_x (void *body_data)
{
  SCM expr = *(SCM *) body_data;
  return scm_eval_x (expr);
}


inline static SCM 
scwm_catching_eval_x (SCM expr) {
  return scm_internal_stack_catch (SCM_BOOL_T, scwm_body_eval_x, &expr,
			  scwm_handle_error, "scwm");
}

inline static SCM 
scwm_catching_load_from_port (SCM port)
{
  SCM expr;
  SCM answer = SCM_UNSPECIFIED;

  while (!SCM_EOF_OBJECT_P(expr = scm_read (port))) {  
    answer = scwm_catching_eval_x (expr);
  }
  scm_close_port (port);

  return answer;
}

static SCM
scwm_body_load (void *body_data)
{
  SCM filename = *(SCM *) body_data;
  SCM port = scm_open_file (filename, gh_str02scm("r"));
  return scwm_catching_load_from_port (port);
}

static SCM
scwm_body_eval_str (void *body_data)
{
  char *string = (char *) body_data;
  SCM port = scm_mkstrport (SCM_MAKINUM (0), gh_str02scm(string), 
			    SCM_OPN | SCM_RDNG, "scwm_safe_eval_str");
  return scwm_catching_load_from_port (port);
}



SCM_PROC (s_safe_load, "safe-load", 1, 0, 0, safe_load);

SCM safe_load (SCM fname)
{
  SCM_STACKITEM stack_item;
  if (!gh_string_p(fname)) {
    scm_wrong_type_arg(s_safe_load, 1, fname);
  }

  return scm_internal_cwdr(scwm_body_load, &fname,
			   scm_handle_by_message_noexit, "scwm", 
			   &stack_item);
}

SCM scwm_safe_load (char *filename)
{
  return safe_load(gh_str02scm(filename));
}

SCM scwm_safe_eval_str (char *string)
{
  SCM_STACKITEM stack_item;
  return scm_internal_cwdr(scwm_body_eval_str, string,
			   scm_handle_by_message_noexit, "scwm", 
			   &stack_item);
}

/* Hooks. */

SCM call0_hooks (SCM hook)
{
  SCM p;
  SCM hook_name;
  SCM hook_list;
  /* Ensure hook list is a list. */

  hook_name = SCM_CAR(hook);
  hook_list = SCM_CDR(hook);

  if (!gh_list_p(hook_list)) {
    /* Warn that hook list is not a list. */
    SCM_SETCAR(hook, SCM_EOL);
    hook_list=SCM_EOL;
  }

  for (p = hook_list; p != SCM_EOL; p = SCM_CDR(p)) {
    scwm_safe_call0 (SCM_CAR(p));
  }
  
  return SCM_UNSPECIFIED;
}

SCM call1_hooks (SCM hook, SCM arg)
{
  SCM p;
  SCM hook_name;
  SCM hook_list;
  /* Ensure hook list is a list. */

  hook_name = SCM_CAR(hook);
  hook_list = SCM_CDR(hook);

  if (!gh_list_p(hook_list)) {
    /* Warn that hook list is not a list. */
    SCM_SETCAR(hook, SCM_EOL);
    hook_list=SCM_EOL;
  }

  for (p = hook_list; p != SCM_EOL; p = SCM_CDR(p)) {
    scwm_safe_call1 (SCM_CAR(p), arg);
  }
  
  return SCM_UNSPECIFIED;
}

SCM apply_hooks (SCM hook, SCM args)
{
  SCM p;
  SCM hook_name;
  SCM hook_list;

  hook_name = SCM_CAR(hook);
  hook_list = SCM_CDR(hook);

  /* Ensure hook list is a list. */
  if (!gh_list_p(hook_list)) {
    /* Warn that hook list is not a list. */
    SCM_SETCAR(hook, SCM_EOL);
    hook_list=SCM_EOL;
  }

  for (p = hook_list; p != SCM_EOL; p = SCM_CDR(p)) {
    scwm_safe_apply (SCM_CAR(p), args);
  }
  
  return SCM_UNSPECIFIED;
}

/* This is needed for running error hooks - if an error hook throws an
   error, we really don't want to invoke the standard handler (which
   would invoke the error hooks again), we should just fall through
   and assume the caller is catching errors and doing something
   appropriate. */

SCM apply_hooks_message_only (SCM hook, SCM args)
{
  SCM p;
  SCM hook_name;
  SCM hook_list;

  hook_name = SCM_CAR(hook);
  hook_list = SCM_CDR(hook);

  /* Ensure hook list is a list. */
  if (!gh_list_p(hook_list)) {
    /* Warn that hook list is not a list. */
    SCM_SETCDR(hook, SCM_EOL);
    hook_list=SCM_EOL;
  }

  for (p = hook_list; p != SCM_EOL; p = SCM_CDR(p)) {
    scwm_safe_apply_message_only (SCM_CAR(p), args);
  }
    
  return SCM_UNSPECIFIED;
}

/* Timer hooks. */

SCM_PROC(s_add_timer_hook_x, "add-timer-hook!", 2, 0, 0, add_timer_hook_x);

SCM add_timer_hook_x(SCM usec, SCM proc)
{
  SCM newcell;
  SCM p, last;
  SCM th_list;


  if (!gh_number_p(usec) || 
      (scm_num2long(usec, (char *) SCM_ARG1, s_add_timer_hook_x) < 0)) {
    scm_wrong_type_arg(s_add_timer_hook_x, 1, usec);
  }

  if (!gh_procedure_p(proc)) {
    scm_wrong_type_arg(s_add_timer_hook_x, 2, proc);
  }

  th_list=SCM_CDR(timer_hooks);
  
  newcell=gh_cons(usec, proc);

  update_timer_hooks ();

  for (p = th_list, last = timer_hooks; p != SCM_EOL; 
       last = p, p = SCM_CDR(p)) {
    SCM cur = SCM_CAR(p);
    if (SCM_FALSEP(scm_gr_p(usec, SCM_CAR(cur)))) {
      break;
    }
  }

  SCM_SETCDR(last, gh_cons(newcell, p));

  return newcell;
}

SCM_PROC(s_remove_timer_hook_x, "remove-timer-hook!", 1, 0, 0, remove_timer_hook_x);

SCM remove_timer_hook_x(SCM handle)
{
  SCM_SETCDR(timer_hooks,scm_delq_x (handle, SCM_CDR(timer_hooks)));

  return SCM_UNSPECIFIED;
}


long shortest_timer_timeout()
{
  if (SCM_CDR(timer_hooks)==SCM_EOL) {
    return -1;
  } else {
    return (gh_scm2long(SCM_CAADR(timer_hooks)));
  }
}

static struct timeval last_timeval;

void update_timer_hooks()
{
  struct timeval tmp;
  long sdelta;
  long usdelta;
  SCM p;

  gettimeofday(&tmp, NULL);

  sdelta = tmp.tv_sec - last_timeval.tv_sec;
  usdelta = tmp.tv_usec - last_timeval.tv_usec;

  /* Be careful to avoid overflow. */
  if (sdelta > MAXLONG / 1000000) {
    sdelta = MAXLONG;
  } else {
    sdelta = sdelta * 1000000;
  }

  if (MAXLONG - sdelta < usdelta) {
    usdelta = MAXLONG;
  } else {
    usdelta = usdelta + sdelta;
  }

  for (p = SCM_CDR(timer_hooks); p != SCM_EOL; p = SCM_CDR(p)) {
    SCM cur = SCM_CAR(p);
    long val;

    val = gh_scm2long(SCM_CAR(cur));
    val = max (0, val - usdelta);
    SCM_SETCAR(cur, gh_long2scm(val));
  }

  last_timeval = tmp;
}

void run_timed_out_timers()
{
  SCM p = SCM_CDR(timer_hooks);

  while (p != SCM_EOL) {
    SCM cur = SCM_CAR(p);
    if (gh_scm2long(SCM_CAR(cur)) == 0) {
      p = SCM_CDR(p);
      SCM_SETCDR(timer_hooks, p);
      scwm_safe_call0(SCM_CDR(cur));
    } else {
      break;
    }
  }
}

/* Input hooks. */

static SCM input_hooks;
static SCM new_input_hooks;

SCM_PROC(s_add_input_hook_x, "add-input-hook!", 2, 0, 0, add_input_hook_x);

SCM 
add_input_hook_x (SCM port, SCM proc)
{
  SCM newcell;
  SCM p, last;

  if (!SCM_OPINFPORTP(port)) {
    scm_wrong_type_arg(s_add_input_hook_x, 1, port);
  }

  if (!gh_procedure_p(proc)) {
    scm_wrong_type_arg(s_add_input_hook_x, 2, proc);
  }

  newcell=gh_cons(port, proc);

  SCM_SETCDR(input_hooks, gh_cons(newcell, SCM_CDR(input_hooks)));
  SCM_SETCDR(new_input_hooks, gh_cons(newcell, SCM_CDR(new_input_hooks)));

  return newcell;
}

SCM_PROC(s_remove_input_hook_x, "remove-input-hook!", 1, 0, 0, remove_input_hook_x);

SCM 
remove_input_hook_x(SCM handle)
{
  SCM_SETCDR(input_hooks,scm_delq_x (handle, SCM_CDR(input_hooks)));

  return SCM_UNSPECIFIED;
}


void 
add_hook_fds_to_set(fd_set *in_fdset, int *fd_width)
{
  SCM cur;
  
  for (cur = SCM_CDR(input_hooks); cur != SCM_EOL; cur= SCM_CDR(cur)) {
    if (SCM_OPINFPORTP(SCM_CAAR(cur))) {
      int fd = gh_scm2int(scm_fileno(SCM_CAAR(cur)));
      
      FD_SET (fd, in_fdset);
      
      if (fd > *fd_width)
	*fd_width = fd;
    }
  }
}

void
force_new_input_hooks()
{
  SCM cur;

  for (cur = SCM_CDR(new_input_hooks);
       cur != SCM_EOL;
       cur = SCM_CDR(cur)) {
    SCM item = SCM_CAR(cur);
    SCM port = SCM_CAR(item);
    SCM proc = SCM_CDR(item);
    while (SCM_BOOL_F!=gh_memq(item, input_hooks) && 
	   SCM_OPINFPORTP(port) &&
	   SCM_BOOL_T==scm_char_ready_p(port)) {
      scwm_safe_call0(proc);
    }
  }
  SCM_SETCDR(new_input_hooks, SCM_EOL);
}

void 
run_input_hooks(fd_set *in_fdset)
{
  SCM prev, cur;

  for (prev=input_hooks, cur=SCM_CDR(prev);
       cur != SCM_EOL;
       prev=cur, cur=SCM_CDR(cur)) {
    SCM item = SCM_CAR(cur);
    SCM port = SCM_CAR(item);
    SCM proc = SCM_CDR(item);

    if (FD_ISSET(gh_scm2int(scm_fileno(SCM_CAR(item))), in_fdset)) {
      scwm_safe_call0(SCM_CDR(item));
      while(SCM_BOOL_T==scm_char_ready_p(port) 
	    && SCM_CDR(prev)==cur) {
	scwm_safe_call0(proc);
      }
    }
  }
}
/* Initialization. */

void init_callbacks()
{
  DEFINE_HOOK(error_hook, "error-hook");
  gettimeofday(&last_timeval, NULL);

  timer_hooks = scm_permanent_object(gh_cons(SCM_EOL, SCM_EOL));
  input_hooks=scm_permanent_object(gh_cons(SCM_EOL,SCM_EOL));
  new_input_hooks=scm_permanent_object(gh_cons(SCM_EOL,SCM_EOL));

#ifndef SCM_MAGIC_SNARFER
#include "callbacks.x"
#endif
}

