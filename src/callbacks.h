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

#ifndef CALLBACKS_H
#define CALLBACKS_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>

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

#define SCWM_DEFINE_HOOK(var, name) var=scm_sysintern(name, SCM_EOL)

SCM call0_hooks (SCM hook);
SCM call1_hooks (SCM hook_type, SCM arg);
SCM call2_hooks (SCM hook_type, SCM arg1, SCM arg2);
SCM apply_hooks (SCM hook_type, SCM args);
SCM apply_hooks_message_only (SCM hook_type, SCM args);

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

/* Initialization. */

void init_callbacks();

#endif /* CALLBACKS_H */
