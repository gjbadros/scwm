/* $Id$
 * Copyright (C) 1997-1999 Maciej Stachowiak and Greg J. Badros
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

#include <sys/time.h>
#include <unistd.h>
#include <limits.h>
#include <assert.h>

#include <guile/gh.h>
#include <libguile.h>
#include <libguile/fluids.h>
#include "callbacks.h"

#include "scwm.h"
#include "guile-compat.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

/* #define SCWM_DEBUG_SAFE_APPLY */
/* #define SCWM_DEBUG_RUN_HOOK */
/* #define SCWM_DEBUG_CALL_INTERACTIVELY */

SCWM_HOOK(error_hook, "error-hook", 1,
"Called on all kinds of errors and exceptions.
Whenever an error or other uncaught throw occurs on any callback,
whether a hook, a mouse binding, a key binding, a menu entry, a file
being processed, or anything else, error-hook will be invoked. Each
procedure in the hook will be called with the throw arguments; these
will generally include information about the nature of the error. ");

SCWM_HOOK(load_processing_hook,"load-processing-hook",1,
"This hook is invoked for every several top-level s-exps in the startup file.
The hook procedures are invoked with one argument, the count of the
s-expressions evaluated thus far. See also `set-load-processing-hook-frequency!'.");

SCWM_HOOK(pre_command_hook,"pre-command-hook",2,
"This hook is invoked with two arguments before every `call-interactively' execution.
The arguments are the procedure to be called, and a list of the arguments passed.
Global variable `this-command' and `this-command-args' can be modified to change 
either of these.");

SCWM_HOOK(post_command_hook,"post-command-hook",2,
"This hook is invoked with two arguments after every `call-interactively' execution.
The arguments are the procedure just called, and the list of the arguments passed.");


SCM timer_hooks;

struct scwm_body_apply_data {
  SCM proc;
  SCM args;
};


static SCM
scwm_body_apply (void *body_data)
{
  struct scwm_body_apply_data *ad = (struct scwm_body_apply_data *) body_data;
  return gh_apply(ad->proc, ad->args);
}


/* Use scm_internal_cwdr to establish a new dynamic root - this causes
   all throws to be caught and prevents continuations from exiting the
   dynamic scope of the callback. This is needed to prevent callbacks
   from disrupting scwm's flow control, which would likely cause a
   crash. Use scm_internal_stack_catch to save the stack so we can
   display a backtrace. scm_internal_stack_cwdr is the combination of
   both. Note that the current implementation causes three(!) distinct
   catch-like constructs to be used; this may have negative, perhaps
   even significantly so, performance implications. */

struct cwssdr_data
{
  SCM tag;
  scm_catch_body_t body;
  void *data;
  scm_catch_handler_t handler;
};

static SCM
cwssdr_body (void *data)
{
  struct cwssdr_data *d = (struct cwssdr_data *) data;
  return scm_internal_stack_catch (d->tag, d->body, d->data, d->handler, 
				  NULL);
}

#ifdef SCWM_DEBUG_SAFE_APPLY
char *SzNameOfProcedureNew(SCM proc)
{
  SCM sym = scm_procedure_name(proc);
  if (SCM_BOOL_F != sym) {
    SCM str = scm_symbol_to_string(sym);
    if (gh_string_p(str)) 
      return gh_scm2newstr(str,NULL);
  }
  return strdup("<anonymous proc>");
}
#endif


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
  d.handler = handler;
  return scm_internal_cwdr_no_unwind (cwssdr_body, &d, handler, handler_data, 
			    stack_item);
}


#ifdef HAVE_SCM_MAKE_HOOK
static SCM run_hook_proc;
#endif

#if 0
/* GJB:FIXME::
   this resulted in the crazy message-window-show/hide! infinite-loop
   bug.  Why is the _stack_cwdr version needed?--10/04/99 gjb */
SCM
scwm_safe_apply (SCM proc, SCM args)
{
  SCM_STACKITEM stack_item;
  struct scwm_body_apply_data apply_data;
  apply_data.proc = proc;
  apply_data.args = args;
  
  return scm_internal_cwdr(scwm_body_apply, &apply_data,
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
#endif

SCM
scwm_safe_apply (SCM proc, SCM args)
{
  SCM_STACKITEM stack_item;
  struct scwm_body_apply_data apply_data;

  apply_data.proc = proc;
  apply_data.args = args;

#ifdef SCWM_DEBUG_SAFE_APPLY
#ifdef HAVE_SCM_MAKE_HOOK
  if (proc != run_hook_proc) 
#endif
    { /* scope, or if above */
    char *sz = SzNameOfProcedureNew(proc);
    scwm_msg(DBG,"scwm_safe_apply","Calling %s",sz);
    gh_free(sz);
    }
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

#ifdef SCWM_DEBUG_SAFE_APPLY
#ifdef HAVE_SCM_MAKE_HOOK
  if (proc != run_hook_proc) 
#endif
    { /* scope, or if above */
    char *sz = SzNameOfProcedureNew(proc);
    scwm_msg(DBG,"scwm_safe_apply_message_only","Calling %s",sz);
    gh_free(sz);
  }
#endif

  return scm_internal_cwdr_no_unwind(scwm_body_apply, &apply_data,
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
  return scwm_safe_apply (proc, gh_list(arg, SCM_UNDEFINED));
}


#ifndef HAVE_SCM_MAKE_HOOK

SCM
scwm_safe_call2 (SCM proc, SCM arg1, SCM arg2)
{
  /* This means w must cons (albeit only once) on each callback of
     size two - seems lame. */
  return scwm_safe_apply (proc, gh_list(arg1, arg2, SCM_UNDEFINED));
}

SCM
scwm_safe_call3 (SCM proc, SCM arg1, SCM arg2, SCM arg3)
{
  /* This means w must cons (albeit only once) on each callback of
     size two - seems lame. */
  return scwm_safe_apply (proc, gh_list(arg1, arg2, arg3, SCM_UNDEFINED));
}

SCM
scwm_safe_call4 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  /* This means w must cons (albeit only once) on each callback of
     size two - seems lame. */
  return scwm_safe_apply (proc, gh_list(arg1, arg2, arg3, arg4, SCM_UNDEFINED));
}

SCM
scwm_safe_call5 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5)
{
  /* This means w must cons (albeit only once) on each callback of
     size two - seems lame. */
  return scwm_safe_apply (proc, gh_list(arg1, arg2, arg3, arg4, arg5, SCM_UNDEFINED));
}

SCM
scwm_safe_call6 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  /* This means w must cons (albeit only once) on each callback of
     size two - seems lame. */
  return scwm_safe_apply (proc, gh_list(arg1, arg2, arg3, arg4, arg5, arg6, SCM_UNDEFINED));
}

SCM
scwm_safe_call7 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7)
{
  /* This means w must cons (albeit only once) on each callback of
     size two - seems lame. */
  return scwm_safe_apply (proc, gh_list(arg1, arg2, arg3, arg4, arg5, arg6, arg7, SCM_UNDEFINED));
}

#endif


/* Hooks. */



/* FIXDOC: We need a way to cross-reference concepts in docs. */

/**CONCEPT: Hooks
  Hooks are used throughout scwm to provide a convenient mechanism for
user callbacks on particular events. Fundamentally, a hook is just a
variable that contains a list of procedures that are called in order
when the relevant event occurs. However, several convenience macros
are provided for manipulating hooks; see `add-hook!', `remove-hook!',
`reset-hook!', and `run-hook'. 
*/

#ifdef HAVE_SCM_MAKE_HOOK

extern ScwmWindow *pswCurrent;
extern Bool scwm_gc_often;

SCWM_INLINE SCM scwm_run_hook(SCM hook, SCM args)
{
  static Bool scwm_gc_really_often = False;
  ScwmWindow *psw = pswCurrent; /* save this value before the hooks are invoked */
  SCM answer;
  if (!SCM_HOOKP(hook)) {
    scm_display_error_message(gh_str02scm("Bad hook: %S, args = %S\n"), 
                              gh_list(hook,args,SCM_UNDEFINED),
                              scm_current_error_port());
    return SCM_UNSPECIFIED;
  }
#ifdef SCWM_DEBUG_RUN_HOOK
  scwm_message(DBG,"scwm_run_hook","Calling hook `%s'; args = %S",
               gh_list(hook,args,SCM_UNDEFINED));
#endif
  if (scwm_gc_often) scm_gc();
  answer = scwm_safe_apply(run_hook_proc, gh_cons(hook,args));
  if (scwm_gc_really_often) scm_gc();
  pswCurrent = psw;
  return answer;
}

SCWM_INLINE SCM scwm_run_hook_message_only(SCM hook, SCM args)
{
#ifdef SCWM_DEBUG_RUN_HOOK
  scwm_message(DBG,"scwm_run_hook_message_only","Calling hook `%s'; args = %S",
               gh_list(hook,args,SCM_UNDEFINED));
#endif
  return scwm_safe_apply_message_only(run_hook_proc, gh_cons(hook,args));
}


SCWM_INLINE SCM call0_hooks(SCM hook)
{
  return scwm_run_hook(hook,SCM_EOL);
}

SCWM_INLINE SCM call1_hooks(SCM hook, SCM arg1)
{
  return scwm_run_hook(hook,gh_list(arg1,SCM_UNDEFINED));
}

SCWM_INLINE SCM call2_hooks(SCM hook, SCM arg1, SCM arg2)
{
  return scwm_run_hook(hook,gh_list(arg1,arg2,SCM_UNDEFINED));
}

SCWM_INLINE SCM call3_hooks(SCM hook, SCM arg1, SCM arg2, SCM arg3)
{
  return scwm_run_hook(hook,gh_list(arg1,arg2,arg3,SCM_UNDEFINED));
}

SCWM_INLINE SCM call4_hooks(SCM hook, SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  return scwm_run_hook(hook,gh_list(arg1,arg2,arg3,arg4,SCM_UNDEFINED));
}

SCWM_INLINE SCM call5_hooks(SCM hook, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5)
{
  return scwm_run_hook(hook,gh_list(arg1,arg2,arg3,arg4,arg5,SCM_UNDEFINED));
}

SCWM_INLINE SCM call6_hooks(SCM hook, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  return scwm_run_hook(hook,gh_list(arg1,arg2,arg3,arg4,arg5,arg6,SCM_UNDEFINED));
}

SCWM_INLINE SCM call7_hooks(SCM hook, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7)
{
  return scwm_run_hook(hook,gh_list(arg1,arg2,arg3,arg4,arg5,arg6,arg7,SCM_UNDEFINED));
}

SCM
scm_empty_hook_p(SCM hook)
{
  return gh_bool2scm(!gh_pair_p(gh_cddr(hook)));
}

#else

SCM
scm_empty_hook_p(SCM hook)
{
  return gh_bool2scm(hook == SCM_EOL || UNSET_SCM(hook));
}

/* Print warning message, and reset the hook */
void
WarnBadHook(SCM hook)
{
  assert(!gh_list_p(gh_cdr(hook)));
  { /* scope */ 
    /* Warn that hook list is not a list. */
    char *szHookName = gh_scm2newstr(hook_name, NULL);
    scwm_message(WARN,"WarnBadHook","hooklist is not a list for %s; resetting it to ()!", hook_name);
    gh_set_cdr_x(hook, SCM_EOL);
  }
}


SCM call0_hooks (SCM hook)
{
  SCM p;
  SCM hook_list;

  /* Ensure hook list is a list. */
  hook_list = gh_cdr(hook);

  if (!gh_list_p(hook_list)) {
    WarnBadHook(hook);
    return SCM_UNSPECIFIED;
  }

  for (p = hook_list; p != SCM_EOL; p = gh_cdr(p)) {
    scwm_safe_call0 (gh_car(p));
  }
  
  return SCM_UNSPECIFIED;
}

SCM call1_hooks (SCM hook, SCM arg)
{
  SCM p;
  SCM hook_list;
  /* Ensure hook list is a list. */

  hook_list = gh_cdr(hook);

  if (!gh_list_p(hook_list)) {
    WarnBadHook(hook);
    return SCM_UNSPECIFIED;
  }

  for (p = hook_list; p != SCM_EOL; p = gh_cdr(p)) {
    scwm_safe_call1 (gh_car(p), arg);
  }
  
  return SCM_UNSPECIFIED;
}

SCM call2_hooks (SCM hook, SCM arg1, SCM arg2)
{
  SCM p;
  SCM hook_list;
  /* Ensure hook list is a list. */

  hook_list = gh_cdr(hook);

  if (!gh_list_p(hook_list)) {
    WarnBadHook(hook);
    return SCM_UNSPECIFIED;
  }

  for (p = hook_list; p != SCM_EOL; p = gh_cdr(p)) {
    scwm_safe_call2 (gh_car(p), arg1, arg2);
  }
  
  return SCM_UNSPECIFIED;
}

SCM call3_hooks (SCM hook, SCM arg1, SCM arg2, SCM arg3)
{
  SCM p;
  SCM hook_list;
  /* Ensure hook list is a list. */

  hook_list = gh_cdr(hook);

  if (!gh_list_p(hook_list)) {
    WarnBadHook(hook);
    return SCM_UNSPECIFIED;
  }

  for (p = hook_list; p != SCM_EOL; p = gh_cdr(p)) {
    scwm_safe_call3 (gh_car(p), arg1, arg2, arg3);
  }
  
  return SCM_UNSPECIFIED;
}

SCM call4_hooks (SCM hook, SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  SCM p;
  SCM hook_list;
  /* Ensure hook list is a list. */

  hook_list = gh_cdr(hook);

  if (!gh_list_p(hook_list)) {
    WarnBadHook(hook);
    return SCM_UNSPECIFIED;
  }

  for (p = hook_list; p != SCM_EOL; p = gh_cdr(p)) {
    scwm_safe_call4 (gh_car(p), arg1, arg2, arg3, arg4);
  }
  
  return SCM_UNSPECIFIED;
}


SCM call5_hooks (SCM hook, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5)
{
  SCM p;
  SCM hook_list;
  /* Ensure hook list is a list. */

  hook_list = gh_cdr(hook);

  if (!gh_list_p(hook_list)) {
    WarnBadHook(hook);
    return SCM_UNSPECIFIED;
  }

  for (p = hook_list; p != SCM_EOL; p = gh_cdr(p)) {
    scwm_safe_call5 (gh_car(p), arg1, arg2, arg3, arg4, arg5);
  }
  
  return SCM_UNSPECIFIED;
}

SCM call6_hooks (SCM hook, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  SCM p;
  SCM hook_list;
  /* Ensure hook list is a list. */

  hook_list = gh_cdr(hook);

  if (!gh_list_p(hook_list)) {
    WarnBadHook(hook);
    return SCM_UNSPECIFIED;
  }

  for (p = hook_list; p != SCM_EOL; p = gh_cdr(p)) {
    scwm_safe_call6 (gh_car(p), arg1, arg2, arg3, arg4, arg5, arg6);
  }
  
  return SCM_UNSPECIFIED;
}

SCM call7_hooks (SCM hook, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7)
{
  SCM p;
  SCM hook_list;
  /* Ensure hook list is a list. */

  hook_list = gh_cdr(hook);

  if (!gh_list_p(hook_list)) {
    WarnBadHook(hook);
    return SCM_UNSPECIFIED;
  }

  for (p = hook_list; p != SCM_EOL; p = gh_cdr(p)) {
    scwm_safe_call7 (gh_car(p), arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  }
  
  return SCM_UNSPECIFIED;
}


SCM scwm_run_hook(SCM hook, SCM args)
{
  SCM p;
  SCM hook_list;

  hook_list = gh_cdr(hook);

  /* Ensure hook list is a list. */
  if (!gh_list_p(hook_list)) {
    WarnBadHook(hook);
    return SCM_UNSPECIFIED;
  }

  for (p = hook_list; p != SCM_EOL; p = gh_cdr(p)) {
    scwm_safe_apply (gh_car(p), args);
  }
  
  return SCM_UNSPECIFIED;
}


/* This is needed for running error hooks - if an error hook throws an
   error, we really don't want to invoke the standard handler (which
   would invoke the error hooks again), we should just fall through
   and assume the caller is catching errors and doing something
   appropriate. */

SCM scwm_run_hook_message_only (SCM hook, SCM args)
{
  SCM p;
  SCM hook_list;

  hook_list = gh_cdr(hook);

  /* Ensure hook list is a list. */
  if (!gh_list_p(hook_list)) {
    WarnBadHook(hook);
    return SCM_UNSPECIFIED;
  }

  for (p = hook_list; p != SCM_EOL; p = gh_cdr(p)) {
    scwm_safe_apply_message_only (gh_car(p), args);
  }
    
  return SCM_UNSPECIFIED;
}

#endif

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


SCWM_INLINE static SCM 
scwm_catching_eval_x (SCM expr) {
  return scm_internal_stack_catch (SCM_BOOL_T, scwm_body_eval_x, &expr,
			  scwm_handle_error, "scwm");
}

static int clnsProcessingHook = 5;

SCWM_INLINE static SCM 
scwm_catching_load_from_port (SCM port)
{
  SCM expr;
  SCM answer = SCM_UNSPECIFIED;
  int i = 0;

  while (!SCM_EOF_OBJECT_P(expr = scm_read (port))) {  
    answer = scwm_catching_eval_x (expr);
    if (++i % clnsProcessingHook == 0) {
      call1_hooks(load_processing_hook, gh_int2scm(i));
    }
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
  char *sz = (char *) body_data;
  SCM port = scm_mkstrport (SCM_INUM0, gh_str02scm(sz), 
			    SCM_OPN | SCM_RDNG, "scwm_safe_eval_str");
  return scwm_catching_load_from_port (port);
}


SCM 
scwm_handle_error (void *data, SCM tag, SCM throw_args)
{
  SCM port = scm_set_current_error_port(make_output_strport("error-handler"));
  SCM str;
  scm_handle_by_message_noexit(data,tag,throw_args);
  port = scm_set_current_error_port(port);
  str = scm_strport_to_string(port);
  if (SCM_BOOL_T == scm_empty_hook_p(error_hook)) {
    scm_display(str,scm_def_errp);
    scm_newline(scm_def_errp);
  } else {
    scwm_run_hook_message_only(error_hook, 
                               gh_list(str, SCM_UNDEFINED));
  }
  return SCM_UNSPECIFIED;
}


SCWM_PROC(safe_load, "safe-load", 1, 0, 0,
          (SCM fname),
"Load file FNAME while trapping and displaying errors.\n\
Each individual top-level-expression is evaluated separately and all\n\
errors are trapped and displayed.  You should use this procedure if\n\
you need to make sure most of a file loads, even if it may contain\n\
errors.")
#define FUNC_NAME s_safe_load
{
  SCM_STACKITEM stack_item;
  VALIDATE_ARG_STR(1,fname);
  return scm_internal_cwdr_no_unwind(scwm_body_load, &fname,
				     scm_handle_by_message_noexit, "scwm", 
				     &stack_item);
}
#undef FUNC_NAME

SCM scwm_safe_load (char *filename)
{
  return safe_load(gh_str02scm(filename));
}

SCM scwm_safe_eval_str (char *string)
{
  SCM_STACKITEM stack_item;
  return scm_internal_cwdr_no_unwind(scwm_body_eval_str, string,
				     scm_handle_by_message_noexit, "scwm", 
				     &stack_item);
}

SCWM_PROC(set_load_processing_frequency_x, "set-load-processing-frequency!", 1, 0, 0,
          (SCM num_lines),
"Invoke hooks on `load-processing-hook' every NUM-LINES lines. \n\
Returns the old value.")
#define FUNC_NAME s_set_load_processing_frequency_x
{
  int i = clnsProcessingHook;
  VALIDATE_ARG_INT_MIN_COPY(1,num_lines,1,clnsProcessingHook);
  return gh_int2scm(i);
}
#undef FUNC_NAME

/* Timer hooks. */

/**CONCEPT: Timer Hooks 
  Timer hooks are a special form of hook that is called after a
specified amount of time has passed. They are treated differently than
normal hooks - use `add-timer-hook!' and `remove-timer-hook!' to
manipulate them. Timer hooks, unlike regular hooks, are one-shot -
once the time limit expires and the timer hook is triggered, it is
removed.
*/

SCWM_PROC(add_timer_hook_x, "add-timer-hook!", 2, 0, 0,
          (SCM msec, SCM proc),
"Add a timer hook to call PROC once sometime after MSEC milliseconds.\n\
When at least MSEC milliseconds have passed, procedure PROC will be\n\
called with no arguments. A handle suitable for passing to\n\
`remove-timer-hook!' is returned.")
#define FUNC_NAME s_add_timer_hook_x
{
  SCM newcell;
  SCM p, last;
  SCM th_list;

  VALIDATE_ARG_INT_MIN(1,msec,0);
  VALIDATE_ARG_PROC(2,proc);

  th_list=gh_cdr(timer_hooks);
  newcell=gh_cons(msec, proc);
  update_timer_hooks ();

  for (p = th_list, last = timer_hooks; p != SCM_EOL; 
       last = p, p = gh_cdr(p)) {
    SCM cur = gh_car(p);
    /* scm_gr_p is ">" */
    if (SCM_FALSEP(scm_gr_p(msec, gh_car(cur)))) {
      break;
    }
  }

  gh_set_cdr_x(last, gh_cons(newcell, p));

  return newcell;
}
#undef FUNC_NAME

SCWM_PROC(remove_timer_hook_x, "remove-timer-hook!", 1, 0, 0,
          (SCM handle),
"Remove a timer hook identified by HANDLE.\n\
The HANDLE should be an object that was returned by\n\
`add-timer-hook!'. No warning or error will occur if HANDLE is for a\n\
timer hook that has already been triggered.")
#define FUNC_NAME s_remove_timer_hook_x
{
  gh_set_cdr_x(timer_hooks,scm_delq_x (handle, gh_cdr(timer_hooks)));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(reset_timer_hook_x, "reset-timer-hook!", 0, 0, 0,
          (),
"Remove all timer-hook procedures.")
#define FUNC_NAME s_reset_timer_hook_x
{
  gh_set_cdr_x(timer_hooks,SCM_EOL);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(get_timer_hooks_list, "get-timer-hooks-list", 0, 0, 0,
          (),
"Return the timer-hooks list.")
#define FUNC_NAME s_get_timer_hooks_list
{
  return timer_hooks;
}
#undef FUNC_NAME


long shortest_timer_timeout()
{
  if (gh_cdr(timer_hooks)==SCM_EOL) {
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
  if (sdelta > LONG_MAX / 1000000) {
    sdelta = LONG_MAX;
  } else {
    sdelta = sdelta * 1000000;
  }

  if (LONG_MAX - sdelta < usdelta) {
    usdelta = LONG_MAX;
  } else {
    usdelta = usdelta + sdelta;
  }

  for (p = gh_cdr(timer_hooks); p != SCM_EOL; p = gh_cdr(p)) {
    SCM cur = gh_car(p);
    long val;

    val = 1000*gh_scm2long(gh_car(cur));
    val = max (0, val - usdelta);
    gh_set_car_x(cur, gh_long2scm(val/1000));
  }

  last_timeval = tmp;
}

void run_timed_out_timers()
{
  SCM p = gh_cdr(timer_hooks);

  while (p != SCM_EOL) {
    SCM cur = gh_car(p);
    if (gh_scm2long(gh_car(cur)) == 0) {
      p = gh_cdr(p);
      gh_set_cdr_x(timer_hooks, p);
      scwm_safe_call0(gh_cdr(cur));
    } else {
      break;
    }
  }
}

/* Input hooks. */

/**CONCEPT: Input Hooks 
  Input hooks are a special form of hook that is called whenever input
is available on a particular port. They are treated differently than
normal hooks - use `add-input-hook!' and `remove-input-hook!' to
manipulate them. Like regular hooks and unlike timer hooks, input
hooks are not one-shot - they trigger every time input is made
available on the particular port, and do not go away until explicitly
removed. An input hook may safely remove itself from within its own
invocation.
*/

static SCM input_hooks;
static SCM new_input_hooks;

SCWM_PROC(add_input_hook_x, "add-input-hook!", 2, 0, 0,
          (SCM port, SCM proc),
"Add an input hook to run PROC on input from PORT.\n\
Whenever input becomes available on PORT, procedure PROC will be called\n\
with no arguments repeatedly until no unprocessed input remains on\n\
PORT. PORT must be open, it must be an input port, and it must be a\n\
file port (this includes pipes and sockets, but not string ports or\n\
soft ports). A handle suitable for passing to `remove-input-hook!' is\n\
returned.")
#define FUNC_NAME s_add_input_hook_x
{
  SCM newcell;

  if (!SCM_OPINFPORTP(port)) {
    SCWM_WRONG_TYPE_ARG(1, port);
  }

  VALIDATE_ARG_PROC(2,proc);

  newcell=gh_cons(port, proc);

  gh_set_cdr_x(input_hooks, gh_cons(newcell, gh_cdr(input_hooks)));
  gh_set_cdr_x(new_input_hooks, gh_cons(newcell, gh_cdr(new_input_hooks)));

  return newcell;
}
#undef FUNC_NAME

SCWM_PROC(remove_input_hook_x, "remove-input-hook!", 1, 0, 0,
          (SCM handle),
"Remove an input hook identified by HANDLE.\n\
HANDLE should be an object that was returned by `add-input-hook!'. An\n\
input hook may safely remove itself.")
#define FUNC_NAME s_remove_input_hook_x
{
  gh_set_cdr_x(input_hooks,scm_delq_x (handle, gh_cdr(input_hooks)));
  /* GJB:FIXME:: should these stick around until they've been forced at
     least once?  For now I'll remove them... */
  gh_set_cdr_x(new_input_hooks,scm_delq_x (handle, gh_cdr(new_input_hooks)));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(reset_input_hook_x, "reset-input-hook!", 0, 0, 0,
          (),
"Remove all procedures from the input hook.")
#define FUNC_NAME s_reset_input_hook_x
{
  gh_set_cdr_x(input_hooks,SCM_EOL);
  /* GJB:FIXME:: should these stick around until they've been forced at
     least once?  For now I'll remove them... */
  gh_set_cdr_x(new_input_hooks,SCM_EOL);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(get_input_hooks_list, "get-input-hooks-list", 0, 0, 0,
          (),
"Return the input-hooks list.")
#define FUNC_NAME s_get_input_hooks_list
{
  return input_hooks;
}
#undef FUNC_NAME

void 
add_hook_fds_to_set(fd_set *in_fdset, int *fd_width)
{
  SCM cur;
  
  for (cur = gh_cdr(input_hooks); cur != SCM_EOL; cur= gh_cdr(cur)) {
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

  /* GJB:FIXME:: can we really make the
     assumptions that things below are lists and 
     remove the error code */
  for (cur = gh_cdr(new_input_hooks);
       cur != SCM_EOL;
       cur = gh_cdr(cur)) {
    if (gh_pair_p(cur)) {
      SCM item = gh_car(cur);
      if (gh_pair_p(item)) {
        SCM port = gh_car(item);
        SCM proc = gh_cdr(item);
        while (SCM_BOOL_F!=gh_memq(item, input_hooks) && 
               SCM_OPINFPORTP(port) &&
               SCM_BOOL_T==scm_char_ready_p(port)) {
          scwm_safe_call0(proc);
        }
      } else {
        scwm_msg(ERR,"force_new_input_hooks","item is not a list");
      }
    } else {
      scwm_msg(ERR,"force_new_input_hooks","cur is not a list");
    }
  }
  gh_set_cdr_x(new_input_hooks, SCM_EOL);
}

void 
run_input_hooks(fd_set *in_fdset)
{
  SCM prev, cur;

  for (prev=input_hooks, cur=gh_cdr(prev);
       cur != SCM_EOL;
       prev=cur, cur=gh_cdr(cur)) {
    SCM item = gh_car(cur);
    SCM port = gh_car(item);
    SCM proc = gh_cdr(item);

    if (SCM_OPINFPORTP(port) &&
	FD_ISSET(gh_scm2int(scm_fileno(port)), in_fdset)) {
      scwm_safe_call0(proc);
      while(SCM_OPINFPORTP(port) &&
	    SCM_BOOL_T==scm_char_ready_p(port) 
	    /* MS:FIXME:: Is this safe enough? */
	    && gh_cdr(prev)==cur) {
	scwm_safe_call0(proc);
      }
    }
  }
}
/* Initialization. */

extern SCM sym_interactive;

/* from window.c */
extern SCM ScmArgsFromInteractiveSpec(SCM spec, SCM proc);

static SCM *pscm_this_command;
static SCM *pscm_this_command_args;

SCWM_PROC(call_interactively, "call-interactively", 1, 1, 0,
          (SCM thunk, SCM debug),
"Invoke THUNK interactively.\n\
Write a debug message if DEBUG is #t.")
#define FUNC_NAME s_call_interactively
{
  SCM interactive_spec = SCM_BOOL_F;
  Bool fDebugThisCommand = False;
  VALIDATE_ARG_PROC(1,thunk);
  VALIDATE_ARG_BOOL_COPY_USE_F(2,debug,fDebugThisCommand);
  interactive_spec = scm_procedure_property(thunk,sym_interactive);
  if (UNSET_SCM(interactive_spec)) {
    SCM procname = scm_procedure_name(thunk);
    /* char *szProcname = strdup("<anonymous procedure>"); 
       only warn about non-anonymous non-interactive procedures;
       it's a pain to use (lambda* "" () ..) to create procs for bindings */
    if (gh_string_p(procname) || gh_symbol_p(procname)) {
      scwm_message(WARN,FUNC_NAME,"Procedure `%s' is not interactive.", 
                   SCM_LIST1(procname));
    }
  }
  { /* scope */
    SCM args = SCM_EOL;
    SCM answer;
    if (gh_string_p(interactive_spec)) {
      args = ScmArgsFromInteractiveSpec(interactive_spec,thunk);
    }
#ifndef SCWM_DEBUG_CALL_INTERACTIVELY
    if (fDebugThisCommand) {
#else
    { /* scope */
#endif
      scm_puts("call-interactively: ", scm_current_error_port());
      scm_write(gh_list(thunk,interactive_spec,args,SCM_UNDEFINED),scm_current_output_port());
      scm_newline(scm_current_output_port());
    }
    *pscm_this_command = thunk;
    *pscm_this_command_args = args;
    scwm_run_hook(pre_command_hook,gh_list(thunk,args,SCM_UNDEFINED));
    thunk = *pscm_this_command;
    args = *pscm_this_command_args;
    answer = scwm_safe_apply(thunk, args);
    scwm_run_hook(post_command_hook,gh_list(thunk,args,SCM_UNDEFINED));

    return answer;
  }
}
#undef FUNC_NAME


void init_callbacks()
{
  SCWM_VAR_INIT(this_command,"this-command",SCM_BOOL_F);
  /** The current command when executing the `pre-command-hook';  may be mutated to affect 
      what command is run.  See also `this-command-args'. */
  SCWM_VAR_INIT(this_command_args,"this-command-args",SCM_EOL);
  /** The arguments to the current command when executing the `post-command-hook'; may be
      mutated to affect the arguments when the command is run.  See also `this-command'. */

  gettimeofday(&last_timeval, NULL);

  timer_hooks = scm_permanent_object(gh_cons(SCM_EOL,SCM_EOL));
  input_hooks = scm_permanent_object(gh_cons(SCM_EOL,SCM_EOL));
  new_input_hooks = scm_permanent_object(gh_cons(SCM_EOL,SCM_EOL));

#ifdef HAVE_SCM_MAKE_HOOK
  run_hook_proc = gh_lookup("run-hook");
#endif

#ifndef SCM_MAGIC_SNARFER
#include "callbacks.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

