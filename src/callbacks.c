/* $Id$ */
/****************************************************************************
 * This module is all original code 
 * by Maciej Stachowiak and Greg J. Badros.
 * It may be used or distributed under either the FVWM license 
 * (see COPYING.FVWM) or the GNU General Public License (see COPYING.GPL and
 * the description below)
 * Copyright 1997, Maciej Stachowiak and Greg J. Badros
 ****************************************************************************/
/*      Copyright (C) 1997, Maciej Stachowiak abd Greg J. Badros

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

#include <guile/gh.h>
#include "callbacks.h"

SCM error_hook;

struct scwm_body_apply_data {
  SCM proc;
  SCM args;
};


SCM scwm_handle_error (void *handler_data, SCM tag, SCM throw_args)
{
  puts("Inside error handler.");
  scm_handle_by_message_noexit(handler_data, tag, throw_args);
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
  return scm_internal_catch (SCM_BOOL_T, scwm_body_eval_x, &expr,
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
			   scwm_handle_error, "scwm", 
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
			   scwm_handle_error, "scwm", 
			   &stack_item);
}

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



void init_callbacks()
{
  DEFINE_HOOK(error_hook, "error-hook");
  
#ifndef SCM_MAGIC_SNARFER
#include "callbacks.x"
#endif
}
