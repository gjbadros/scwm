/* $Id$ */
/*
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

#ifndef CALLBACKS_H
#define CALLBACKS_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>  /* for file descriptors */
#endif

SCM scwm_handle_error (void *handler_data, SCM tag, SCM throw_args);
SCM scwm_safe_apply_message_only (SCM proc, SCM args);

/* Individual callbacks. */

SCM scwm_safe_apply (SCM proc, SCM args);
SCM scwm_safe_call0 (SCM thunk);
SCM scwm_safe_call1 (SCM proc, SCM arg);
SCM scwm_safe_call2 (SCM proc, SCM arg1, SCM arg2);

SCM safe_load (SCM fname);
SCM scwm_safe_load (char *filename);
SCM scwm_safe_eval_str (char *string);

/* Hooks. */

SCM call0_hooks (SCM hook);
SCM call1_hooks (SCM hook_type, SCM arg);
SCM call2_hooks (SCM hook_type, SCM arg1, SCM arg2);
SCM call3_hooks (SCM hook_type, SCM arg1, SCM arg2, SCM arg3);
SCM call4_hooks (SCM hook_type, SCM arg1, SCM arg2, SCM arg3, SCM arg4);
SCM call5_hooks (SCM hook_type, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5);
SCM call6_hooks (SCM hook_type, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6);
SCM call7_hooks (SCM hook_type, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7);

SCM apply_hooks (SCM hook_type, SCM args);
SCM apply_hooks_message_only (SCM hook_type, SCM args);

#if 0
/* GJB:FIXME:: new style hooks are not yet supported */
#define call0_hooks(hook) run_hook(hook,SCM_EOL)
#define call1_hooks(hook,a1) run_hook(hook,gh_list(a1,SCM_UNDEFINED))
#define call2_hooks(hook,a1,a2) run_hook(hook,gh_list(a1,a2,SCM_UNDEFINED))
#define call3_hooks(hook,a1,a2,a3) run_hook(hook,gh_list(a1,a2,a3,SCM_UNDEFINED))
#define call4_hooks(hook,a1,a2,a3,a4) run_hook(hook,gh_list(a1,a2,a3,a4,SCM_UNDEFINED))
#define call5_hooks(hook,a1,a2,a3,a4,a5) run_hook(hook,gh_list(a1,a2,a3,a4,a5,SCM_UNDEFINED))
#define call6_hooks(hook,a1,a2,a3,a4,a5,a6) run_hook(hook,gh_list(a1,a2,a3,a4,a5,a6,SCM_UNDEFINED))
#define call7_hooks(hook,a1,a2,a3,a4,a5,a6,a7) run_hook(hook,gh_list(a1,a2,a3,a4,a5,a6,a7,SCM_UNDEFINED))

#define apply_hooks(hook,args) run_hook(hook,args)
#define apply_hooks_message_only(hook,args) run_hook(hook,args)

#endif


int /* Bool */ FEmptyHook(SCM hook);

/* Timer hooks. */

SCM add_timer_hook_x(SCM usec, SCM proc);
SCM remove_timer_hook_x(SCM handle);

long shortest_timer_timeout();
void update_timer_hooks();
void run_timed_out_timers();

/* Input hooks. */

SCM add_input_hook_x (SCM fd, SCM proc);
SCM remove_timer_hook_x(SCM handle);

void add_hook_fds_to_set(fd_set *in_fdset, int *fd_width);
void force_new_input_hooks();
void run_input_hooks(fd_set *in_fdset);

#endif /* CALLBACKS_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

