/* $Id$
 * binding.h
 */

#ifndef BINDING_H
#define BINDING_H

SCM bind_key(SCM contexts, SCM key, SCM proc);
SCM unbind_key(SCM contexts, SCM key);
SCM bind_mouse(SCM contexts, SCM button, SCM proc);
SCM unbind_mouse(SCM contexts, SCM button);

void ungrab_button_all_windows(int button, int modifier);

void init_binding();
void find_mouse_event_type();
void clear_mouse_event_type();

SCM mouse_event_type();

SCM bind_event(SCM ev_sym, SCM proc);
void run_new_window_hook(SCM w);
void run_new_window_hint_hook(SCM w);

Bool FKeyToKeysymModifiers(SCM key, KeySym *pkeysym, int *pmodifier);

#endif /* BINDING_H */
