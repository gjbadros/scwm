/* $Id$
 * binding.h
 */

#ifndef BINDING_H
#define BINDING_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/X.h>
#include <X11/Xlib.h>

#include "window_fwd.h"

#undef EXTERN
#undef EXTERN_SET
#ifdef BINDING_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

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

/* GJB:FIXME:: this should be redone -- minimally
   a union and better names; part of the event rewrite, I'd guess */
typedef struct Binding_tag {
  char IsMouse;			/* Is it a mouse or key binding 1= mouse; */
  int Button_Key;		/* Mouse Button number or Keycode */
  char *key_name;		/* In case of keycode, give the key_name too */
  int Context;			/* Contex is Scwm context, ie titlebar, frame, etc */
  unsigned int Modifier;	/* Modifiers for keyboard state */
  SCM Thunk;
  SCM ReleaseThunk;             /* this is also the immediate clink thunk 
                                   for mouse bindings */
  struct Binding_tag *NextBinding;
} Binding;

void init_modifiers();
void init_pointer_mapping(void);
void find_mouse_event_type(XButtonEvent *ev);
void clear_mouse_event_type();

SCM mouse_event_type();

/* Used by events.c's send_key_press */
Bool FKeyToKeysymModifiers(SCM key, KeySym *pkeysym, int *pmodifier, const char *func_name,
			   Bool allow_any_p, Bool fShowError);
Bool FButtonToBnumModifiers(SCM button, int *pbnum, int *pmodifier, const char *func_name,
			    Bool allow_any_p);
const char *SzKeysymForKeyCode(KeyCode code, int index);
char *SzNewModifierStringForModMask(int modmask);
char *SzNewForModMaskKeyCode(int modmask, KeyCode code);
Binding *PBndFromMouse(int button,
                       unsigned int modifier, int context);
Binding *PBndFromKey(KeyCode keycode,
                     unsigned int modifier, int context);


void GrabKeysForPsw(ScwmWindow *psw);
void GrabButtonsForPsw(ScwmWindow *psw);
void GrabButtonWithModifiersMaskXcPm(int button, int modifier, 
                                     Window w, unsigned int event_mask,
                                     Cursor xc, int pointer_mode);
void GrabButtonWithModifiers(int button, int modifier, ScwmWindow *psw);
void UngrabButtonWithModifiersWin(int button, int modifier, Window w);
void UngrabButtonWithModifiers(int button, int modifier, ScwmWindow *psw);
void GrabKeyWithModifiers(KeyCode key, unsigned int modifier, ScwmWindow *psw);
void UngrabKeyWithModifiers(KeyCode key, unsigned int modifier, ScwmWindow *psw);
void GrabKeyWithModifiersWin(KeyCode key, unsigned int modifier, Window win);
void UngrabKeyWithModifiersWin(KeyCode key, unsigned int modifier, Window win);

#endif /* BINDING_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

