/* $Id$
 * binding.h
 */

#ifndef BINDING_H
#define BINDING_H


typedef struct Binding_tag {
  char IsMouse;			/* Is it a mouse or key binding 1= mouse; */
  int Button_Key;		/* Mouse Button number of Keycode */
  char *key_name;		/* In case of keycode, give the key_name too */
  int Context;			/* Contex is Scwm context, ie titlebar, frame, etc */
  int Modifier;			/* Modifiers for keyboard state */
  char *Action;			/* What to do? */
  SCM Thunk;
  struct Binding_tag *NextBinding;
} Binding;

SCM bind_key(SCM contexts, SCM key, SCM proc);
SCM unbind_key(SCM contexts, SCM key);
SCM bind_mouse(SCM contexts, SCM button, SCM proc);
SCM unbind_mouse(SCM contexts, SCM button);

void init_binding();
void find_mouse_event_type();
void clear_mouse_event_type();

SCM mouse_event_type();

SCM bind_event(SCM ev_sym, SCM proc);
void run_new_window_hook(SCM w);
void run_new_window_hint_hook(SCM w);

Bool FKeyToKeysymModifiers(SCM key, KeySym *pkeysym, int *pmodifier);

#endif /* BINDING_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
