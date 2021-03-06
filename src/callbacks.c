/* $Id$
 * Copyright (C) 1997, 1998, 1999, 2000 Maciej Stachowiak and Greg J. Badros
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

#include <sys/time.h>
#include <unistd.h>
#include <limits.h>
#include <assert.h>

#include <libguile.h>
#include <libguile/fluids.h>
#include <libguile/ports.h>
#include "callbacks.h"

#include "scwm.h"
#include "errors.h"
#include "guile-compat.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

typedef void *(*cont_proc)(void*);

#define USE_LAZY_HANDLER 1

/* #define SCWM_DEBUG_SAFE_APPLY */
/* #define SCWM_DEBUG_RUN_HOOK */
/* #define SCWM_DEBUG_CALL_INTERACTIVELY */

SCM_VARIABLE_INIT(scm_this_command, "this-command", SCM_BOOL_F);
/** The current command when executing the `pre-command-hook'; may be
    mutated to affect what command is run.  See also
    `this-command-args'. */
SCM_VARIABLE_INIT(scm_this_command_args, "this-command-args", SCM_EOL);
/** The arguments to the current command when executing the
    `post-command-hook'; may be mutated to affect the arguments when
    the command is run.  See also `this-command'. */


SCWM_HOOK(error_hook, "error-hook", 1,
"Called on all kinds of errors and exceptions.\n\n"
"Whenever an error or other uncaught throw occurs on any callback,\n"
"whether a hook, a mouse binding, a key binding, a menu entry, a file\n"
"being processed, or anything else, error-hook will be invoked. Each\n"
"procedure in the hook will be called with the throw arguments; these\n"
"will generally include information about the nature of the error. ");

SCWM_HOOK(load_processing_hook,"load-processing-hook",1,
"This hook is invoked for every several top-level s-exps in the startup file.\n\n"
"The hook procedures are invoked with one argument, the count of the\n"
"s-expressions evaluated thus far. See also `set-load-processing-hook-frequency!'.");

SCWM_HOOK(pre_command_hook,"pre-command-hook",2,
"This hook is invoked with two arguments before every `call-interactively' execution.\n\n"
"The arguments are the procedure to be called, and a list of the arguments passed.\n"
"Global variable `this-command' and `this-command-args' can be modified to change \n"
"either of these.");

SCWM_HOOK(post_command_hook,"post-command-hook",2,
"This hook is invoked with two arguments after every `call-interactively' execution.\n\n"
"The arguments are the procedure just called, and the list of the arguments passed.");


SCM timer_hooks;

struct scwm_body_apply_data {
  SCM proc;
  SCM args;
};

static SCM scwm_handle_error (void *handler_data, SCM tag, SCM throw_args);
static SCM scwm_safe_apply_message_only (SCM proc, SCM args);

static SCM
scwm_body_apply (void *body_data)
{
  struct scwm_body_apply_data *ad = (struct scwm_body_apply_data *) body_data;
  return scm_apply_0(ad->proc, ad->args);/* DPS: Watch args */
}

/*
  scm_internal_cwdr -> scm_c_with_continuation_barrier
  scm_internal_stack_catch -> scm_intrenal_stack_catch
  scm_internal_stack_cwdr -> ?
 */

/* Use scm_internal_cwdr to establish a new dynamic root - this causes
   all throws to be caught and prevents continuations from exiting the
   dynamic scope of the callback. This is needed to prevent callbacks
   from disrupting scwm's flow control, which would likely cause a
   crash. Use scm_internal_stack_catch to save the stack so we can
   display a backtrace. scm_internal_stack_cwdr is the combination of
   both. Note that the current implementation causes three(!) distinct
   catch-like constructs to be used; this may have negative, perhaps
   even significantly so, performance implications. */

#ifdef SCWM_DEBUG_SAFE_APPLY
char *SzNameOfProcedureNew(SCM proc)
{
  SCM sym = scm_procedure_name(proc);
  if (scm_is_true(sym)) {
    SCM str = scm_symbol_to_string(sym);
    if (scm_is_string(str)) 
      return scm_to_locale_string(str);
  }
  return strdup("<anonymous proc>");
}
#endif



static SCM run_hook_proc;


SCM
scwm_safe_apply (SCM proc, SCM args)
{
  SCM_STACKITEM stack_item;
  struct scwm_body_apply_data apply_data;

  apply_data.proc = proc;
  apply_data.args = args;

#ifdef SCWM_DEBUG_SAFE_APPLY
  if (proc != run_hook_proc) 
    { /* scope, or if above */
    char *sz = SzNameOfProcedureNew(proc);
    scwm_msg(DBG,"scwm_safe_apply","Calling %s",sz);
    free(sz);
    }
#endif

  return scm_c_with_continuation_barrier((cont_proc)scwm_body_apply, &apply_data);
}

static SCM
scwm_safe_apply_message_only (SCM proc, SCM args)
{
  SCM_STACKITEM stack_item;
  struct scwm_body_apply_data apply_data;

  apply_data.proc = proc;
  apply_data.args = args;

#ifdef SCWM_DEBUG_SAFE_APPLY
  if (proc != run_hook_proc) 
    { /* scope, or if above */
    char *sz = SzNameOfProcedureNew(proc);
    scwm_msg(DBG,"scwm_safe_apply_message_only","Calling %s",sz);
    free(sz);
  }
#endif

  return scm_c_with_continuation_barrier((cont_proc)scwm_body_apply, &apply_data);
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
  return scwm_safe_apply (proc, scm_list_n(arg, SCM_UNDEFINED));
}



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

extern ScwmWindow *pswCurrent;
extern Bool scwm_gc_often;

/* Can set this in debugger to turn off running hooks */
static Bool fSuppressRunningHooks = False;

SCWM_INLINE SCM scwm_run_hook(SCM hook, SCM args)
{
  static Bool scwm_gc_really_often = False;
  ScwmWindow *psw = pswCurrent; /* save this value before the hooks are invoked */
  SCM answer;

  if (fSuppressRunningHooks)
    return SCM_BOOL_F;

  if (scm_is_false(scm_hook_p(hook))) {
    scwm_error_message(scm_from_latin1_string("Bad hook: ~S, args = ~S\n"), 
                       scm_list_n(hook,args,SCM_UNDEFINED));
    return SCM_UNSPECIFIED;
  }
#ifdef SCWM_DEBUG_RUN_HOOK
  scwm_message(DBG,"scwm_run_hook","Calling hook `~A'; args = ~S",
               scm_list_n(hook,args,SCM_UNDEFINED));
#endif
  if (scwm_gc_often) scm_gc();
  answer = scwm_safe_apply(run_hook_proc, scm_cons(hook, args));
  if (scwm_gc_really_often) scm_gc();
  pswCurrent = psw;
  return answer;
}

SCWM_INLINE SCM scwm_run_hook_message_only(SCM hook, SCM args)
{
#ifdef SCWM_DEBUG_RUN_HOOK
  scwm_message(DBG, "scwm_run_hook_message_only",
	       "Calling hook `~A'; args = ~S",
               scm_list_n(hook, args, SCM_UNDEFINED));
#endif
  return scwm_safe_apply_message_only(run_hook_proc, scm_cons(hook,args));
}


SCWM_INLINE SCM scwm_run_hook0(SCM hook)
{
  return scwm_run_hook(hook, SCM_EOL);
}

SCWM_INLINE SCM scwm_run_hook1(SCM hook, SCM arg1)
{
  return scwm_run_hook(hook, scm_cons(arg1,SCM_EOL));
}

SCWM_INLINE SCM scwm_run_hook2(SCM hook, SCM arg1, SCM arg2)
{
  return scwm_run_hook(hook, scm_list_2(arg1,arg2));
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
  SCM compile = scm_module_variable(scm_c_resolve_module("system base compile"),
				    scm_from_locale_symbol ("compile"));
  
  if (scm_is_true(compile))
    return scm_call_3(scm_variable_ref(compile), expr,
		      scm_from_locale_keyword("env"), scm_current_module());
  else
    return scm_eval(expr, scm_current_module());
}


SCWM_INLINE static SCM 
scwm_catching_eval_x (SCM expr)
{
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
      scwm_run_hook1(load_processing_hook, scm_from_int(i));
    }
  }
  scm_close_port (port);

  return answer;
}

static SCM
scwm_body_load (void *body_data)
{
  SCM filename = *(SCM *) body_data;
  SCM port = scm_open_file (filename, scm_from_latin1_string("r"));
  return scwm_catching_load_from_port (port);
}

static SCM
scwm_body_eval_str (void *body_data)
{
  char *sz = (char *) body_data;
  SCM port = scm_mkstrport (SCM_INUM0, scm_from_locale_string(sz), 
			    SCM_OPN | SCM_RDNG, "scwm_body_eval_str");
  return scwm_catching_load_from_port (port);
}

#include <stdio.h>

static SCM 
scwm_handle_error (void *data, SCM tag, SCM throw_args)
{
  SCM port = scm_set_current_error_port(scm_open_output_string());
  SCM str;

  scm_handle_by_message_noexit(data, tag, throw_args);

  port = scm_set_current_error_port(port);
  str = scm_strport_to_string(port);

  // eq? #t
  if (scm_is_true(scm_hook_empty_p(error_hook))) {
    scm_display(str, scm_current_error_port());	/* was scm_def_errp */
    scm_newline(scm_current_error_port());	/* was scm_def_errp */
  } else {
    scwm_run_hook_message_only(error_hook, 
                               scm_list_n(str, SCM_UNDEFINED));
  }
  return SCM_UNSPECIFIED;
}


SCM_DEFINE(safe_load, "safe-load", 1, 0, 0,
          (SCM fname),
"Load file FNAME while trapping and displaying errors.\n\n"
"Each individual top-level-expression is evaluated separately and all\n"
"errors are trapped and displayed.  You should use this procedure if\n"
"you need to make sure most of a file loads, even if it may contain\n"
"errors.")
#define FUNC_NAME s_safe_load
{
  void * ret;
  ret =  scm_c_with_continuation_barrier((cont_proc)scwm_body_load, &fname);
  if (ret)
    return (SCM) ret;
  return SCM_BOOL_F; /* or what? */
}
#undef FUNC_NAME

SCM scwm_safe_eval_str (char *string)
{
  void *ret;

  ret = scm_c_with_continuation_barrier((cont_proc)scwm_body_eval_str, string);
  if (ret)
    return (SCM) ret;
  return SCM_BOOL_F; /* or what? */
}

SCM_DEFINE(set_load_processing_frequency_x, "set-load-processing-frequency!", 1, 0, 0,
          (SCM num_lines),
"Invoke hooks on `load-processing-hook' every NUM-LINES lines.\n\n"
"Returns the old value.")
#define FUNC_NAME s_set_load_processing_frequency_x
{
  int i = clnsProcessingHook;
  VALIDATE_ARG_INT_MIN_COPY(1,num_lines,1,clnsProcessingHook);
  return scm_from_int(i);
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

SCM_DEFINE(add_timer_hook_x, "add-timer-hook!", 2, 0, 0,
          (SCM msec, SCM proc),
"Add a timer hook to call PROC once sometime after MSEC milliseconds.\n\n"
"When at least MSEC milliseconds have passed, procedure PROC will be\n"
"called with no arguments. A handle suitable for passing to\n"
"`remove-timer-hook!' is returned.")
#define FUNC_NAME s_add_timer_hook_x
{
  SCM newcell;
  SCM p, last;
  SCM th_list;

  VALIDATE_ARG_INT_MIN(1,msec,0);
  VALIDATE_ARG_PROC(2,proc);

  th_list=scm_cdr(timer_hooks);
  newcell=scm_cons(msec, proc);
  update_timer_hooks ();

  for (p = th_list, last = timer_hooks; !scm_is_null(p);
       last = p, p = scm_cdr(p)) {
    SCM cur = scm_car(p);
    /* scm_gr_p is ">" */
    if (scm_is_false(scm_gr_p(msec, scm_car(cur)))) {
      break;
    }
  }

  scm_set_cdr_x(last, scm_cons(newcell, p));

  return newcell;
}
#undef FUNC_NAME

SCM_DEFINE(remove_timer_hook_x, "remove-timer-hook!", 1, 0, 0,
          (SCM handle),
"Remove a timer hook identified by HANDLE.\n\n"
"The HANDLE should be an object that was returned by\n"
"`add-timer-hook!'.  No warning or error will occur if HANDLE is for a\n"
"timer hook that has already been triggered.")
#define FUNC_NAME s_remove_timer_hook_x
{
  scm_set_cdr_x(timer_hooks,scm_delq_x (handle, scm_cdr(timer_hooks)));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(reset_timer_hook_x, "reset-timer-hook!", 0, 0, 0,
          (),
"Remove all timer-hook procedures.")
#define FUNC_NAME s_reset_timer_hook_x
{
  scm_set_cdr_x(timer_hooks,SCM_EOL);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(get_timer_hooks_list, "get-timer-hooks-list", 0, 0, 0,
          (),
"Return the timer-hooks list.")
#define FUNC_NAME s_get_timer_hooks_list
{
  return timer_hooks;
}
#undef FUNC_NAME


long shortest_timer_timeout()
{
  if (scm_is_null(scm_cdr(timer_hooks))) {
    return -1;
  } else {
    return scm_to_long(SCM_CAADR(timer_hooks));
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

  for (p = scm_cdr(timer_hooks); !scm_is_null(p); p = scm_cdr(p)) {
    SCM cur = scm_car(p);
    long val;

    val = 1000*scm_to_long(scm_car(cur));
    val = max (0, val - usdelta);
    scm_set_car_x(cur, scm_from_long(val/1000));
  }

  last_timeval = tmp;
}

void run_timed_out_timers()
{
  SCM p = scm_cdr(timer_hooks);

  while (!scm_is_null(p)) {
    SCM cur = scm_car(p);
    if (scm_to_long(scm_car(cur)) == 0) {
      p = scm_cdr(p);
      scm_set_cdr_x(timer_hooks, p);
      scwm_safe_call0(scm_cdr(cur));
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

SCM_DEFINE(add_input_hook_x, "add-input-hook!", 2, 0, 0,
          (SCM port, SCM proc),
"Add an input hook to run PROC on input from PORT.\n\n"
"Whenever input becomes available on PORT, procedure PROC will be called\n"
"with no arguments repeatedly until no unprocessed input remains on\n"
"PORT. PORT must be open, it must be an input port, and it must be a\n"
"file port (this includes pipes and sockets, but not string ports or\n"
"soft ports). A handle suitable for passing to `remove-input-hook!' is\n"
"returned.")
#define FUNC_NAME s_add_input_hook_x
{
  SCM newcell;

  if (!SCM_OPINFPORTP(port)) {
    SCWM_WRONG_TYPE_ARG(1, port);
  }

  VALIDATE_ARG_PROC(2,proc);

  newcell=scm_cons(port, proc);

  scm_set_cdr_x(input_hooks, scm_cons(newcell, scm_cdr(input_hooks)));
  scm_set_cdr_x(new_input_hooks, scm_cons(newcell, scm_cdr(new_input_hooks)));

  return newcell;
}
#undef FUNC_NAME

SCM_DEFINE(remove_input_hook_x, "remove-input-hook!", 1, 0, 0,
          (SCM handle),
"Remove an input hook identified by HANDLE.\n\n"
"HANDLE should be an object that was returned by `add-input-hook!'. An\n"
"input hook may safely remove itself.")
#define FUNC_NAME s_remove_input_hook_x
{
  scm_set_cdr_x(input_hooks,scm_delq_x (handle, scm_cdr(input_hooks)));
  /* GJB:FIXME:: should these stick around until they've been forced at
     least once?  For now I'll remove them... */
  scm_set_cdr_x(new_input_hooks,scm_delq_x (handle, scm_cdr(new_input_hooks)));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE(reset_input_hook_x, "reset-input-hook!", 0, 0, 0,
          (),
"Remove all procedures from the input hook.")
#define FUNC_NAME s_reset_input_hook_x
{
  scm_set_cdr_x(input_hooks,SCM_EOL);
  /* GJB:FIXME:: should these stick around until they've been forced at
     least once?  For now I'll remove them... */
  scm_set_cdr_x(new_input_hooks,SCM_EOL);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE(get_input_hooks_list, "get-input-hooks-list", 0, 0, 0,
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
  
  for (cur = scm_cdr(input_hooks); !scm_is_null(cur); cur= scm_cdr(cur)) {
    if (SCM_OPINFPORTP(SCM_CAAR(cur))) {
      int fd = scm_to_int(scm_fileno(SCM_CAAR(cur)));
      
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
  for (cur = scm_cdr(new_input_hooks);
       !scm_is_null(cur);
       cur = scm_cdr(cur)) {
    if (scm_is_true(scm_pair_p(cur))) {
      SCM item = scm_car(cur);
      if (scm_is_true(scm_pair_p(item))) {
        SCM port = scm_car(item);
        SCM proc = scm_cdr(item);
        while (scm_is_true(scm_memq(item, input_hooks)) && 
               SCM_OPINFPORTP(port) &&
               scm_is_true(scm_char_ready_p(port))) {
          scwm_safe_call0(proc);
        }
      } else {
        scwm_msg(ERR,"force_new_input_hooks","item is not a list");
      }
    } else {
      scwm_msg(ERR,"force_new_input_hooks","cur is not a list");
    }
  }
  scm_set_cdr_x(new_input_hooks, SCM_EOL);
}

void 
run_input_hooks(fd_set *in_fdset)
{
  SCM prev, cur;

  for (prev=input_hooks, cur=scm_cdr(prev);
       !scm_is_null(cur);
       prev=cur, cur=scm_cdr(cur)) {
    SCM item = scm_car(cur);
    SCM port = scm_car(item);
    SCM proc = scm_cdr(item);

    if (SCM_OPINFPORTP(port) &&
	FD_ISSET(scm_to_int(scm_fileno(port)), in_fdset)) {
      scwm_safe_call0(proc);
      while (SCM_OPINFPORTP(port) &&
	     scm_is_true(scm_char_ready_p(port)) // eq? #t 
	     /* MS:FIXME:: Is this safe enough? */
	     && scm_cdr(prev)==cur) {
	scwm_safe_call0(proc);
      }
    }
  }
}
/* Initialization. */

extern SCM sym_interactive;

/* from window.c */
extern SCM ScmArgsFromInteractiveSpec(SCM spec, SCM proc);

SCM_DEFINE(call_interactively, "call-interactively", 1, 1, 0,
          (SCM thunk, SCM debug),
"Invoke THUNK interactively.\n\n"
"THUNK can be either a procedure or a symbol.\n"
"If it is a symbol it is dereferenced.\n"
"Write a debug message if DEBUG is #t.")
#define FUNC_NAME s_call_interactively
{
  SCM interactive_spec = SCM_BOOL_F;
  Bool fDebugThisCommand = False;
  DEREF_IF_SYMBOL(thunk);
  VALIDATE_ARG_PROC(1,thunk);
  VALIDATE_ARG_BOOL_COPY_USE_F(2,debug,fDebugThisCommand);
  interactive_spec = scm_procedure_property(thunk,scm_from_locale_symbol("interactive")); // sym_interactive
  if (UNSET_SCM(interactive_spec)) { 
    SCM procname = scm_procedure_name(thunk);
    /* char *szProcname = strdup("<anonymous procedure>"); 
       only warn about non-anonymous non-interactive procedures;
       it's a pain to use (lambda* "" () ..) to create procs for bindings */
    if (scm_is_string(procname) || scm_is_symbol(procname)) {
      scwm_message(WARN,FUNC_NAME,"Procedure `~A' is not interactive.", 
                   scm_list_1(procname));
    }
  }
  { /* scope */
    SCM args = SCM_EOL;
    SCM answer;
    if (scm_is_string(interactive_spec)) {
      args = ScmArgsFromInteractiveSpec(interactive_spec,thunk);
    }
#ifndef SCWM_DEBUG_CALL_INTERACTIVELY
    if (fDebugThisCommand) {
#else
    { /* scope */
#endif
      scm_puts("call-interactively: ", scm_current_error_port());
      scm_write(scm_list_n(thunk,interactive_spec,args,SCM_UNDEFINED),scm_current_output_port());
      scm_newline(scm_current_output_port());
    }
    scm_variable_set_x(scm_this_command, thunk);
    scm_variable_set_x(scm_this_command_args, args);
    scwm_run_hook2(pre_command_hook,thunk,args);
    thunk = scm_variable_ref(scm_this_command);
    args = scm_variable_ref(scm_this_command_args);
    answer = scwm_safe_apply(thunk, args);
    scwm_run_hook2(post_command_hook,thunk,args);

    return answer;
  }
}
#undef FUNC_NAME


void init_callbacks()
{
  gettimeofday(&last_timeval, NULL);

  timer_hooks = scm_permanent_object(scm_cons(SCM_EOL,SCM_EOL));
  input_hooks = scm_permanent_object(scm_cons(SCM_EOL,SCM_EOL));
  new_input_hooks = scm_permanent_object(scm_cons(SCM_EOL,SCM_EOL));
  run_hook_proc = scm_variable_ref(scm_c_lookup("run-hook"));

#include "callbacks.x"
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

