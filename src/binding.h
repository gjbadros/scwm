/* $Id$
 * binding.h
 */

#ifndef BINDING_H
#define BINDING_H

/* contexts for button presses */
#define C_NO_CONTEXT	0
#define C_WINDOW	1
#define C_TITLE		2
#define C_ICON		4
#define C_ROOT		8
#define C_FRAME		16
#define C_SIDEBAR       32
#define C_L1            64
#define C_L2           128
#define C_L3           256
#define C_L4           512
#define C_L5          1024
#define C_R1          2048
#define C_R2          4096
#define C_R3          8192
#define C_R4         16384
#define C_R5         32768
#define C_RALL       (C_R1|C_R2|C_R3|C_R4|C_R5)
#define C_LALL       (C_L1|C_L2|C_L3|C_L4|C_L5)
#define C_ALL   (C_WINDOW|C_TITLE|C_ICON|C_ROOT|C_FRAME|C_SIDEBAR|\
                 C_L1|C_L2|C_L3|C_L4|C_L5|C_R1|C_R2|C_R3|C_R4|C_R5)


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
void init_modifiers();
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
