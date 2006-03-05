/* $Id$ */
/*
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

#ifndef CALLBACKS_H
#define CALLBACKS_H

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>  /* for file descriptors */
#endif

//#define SCWM_MAKE_HOOK(name,args) scm_create_hook((name),(args))
#define SCWM_MAKE_HOOK(name,args) scm_c_define(name, scm_make_hook(args))

/* Individual callbacks. */

SCM call_interactively (SCM thunk, SCM debug);

SCM scwm_safe_apply (SCM proc, SCM args);
SCM scwm_safe_call0 (SCM thunk);
SCM scwm_safe_call1 (SCM proc, SCM arg);

SCM safe_load (SCM fname);
SCM scwm_safe_eval_str (char *string);

/* Hooks. */
SCM scwm_run_hook(SCM hook, SCM args);
SCM scwm_run_hook0(SCM hook);
SCM scwm_run_hook1(SCM hook, SCM arg1);
SCM scwm_run_hook2(SCM hook, SCM arg1, SCM arg2);
SCM scwm_run_hook_message_only(SCM hook, SCM args);

#ifndef HAVE_SCM_HOOK_EMPTY_P
SCM scm_hook_empty_p(SCM hook);
#endif

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

