
/****************************************************************************
 * This module has been significantly modified by Maciej Stachowiak.
 * It may be used under the terms of the fvwm copyright (see COPYING.FVWM).
 * Changes Copyright 1997, Maciej stachowiak
 ****************************************************************************/
#include <config.h>

#include <guile/gh.h>
#include "scwm.h"
#include "menus.h"
#include "screen.h"
#include "window.h"
#include "errors.h"
#include "complex.h"
#include "util.h"
#include "misc.h"

struct symnum {
  SCM sym;
  int value;
};

struct symnum binding_contexts[] =
{
  {SCM_UNDEFINED, C_WINDOW},
  {SCM_UNDEFINED, C_TITLE},
  {SCM_UNDEFINED, C_ICON},
  {SCM_UNDEFINED, C_ROOT},
  {SCM_UNDEFINED, C_FRAME},
  {SCM_UNDEFINED, C_SIDEBAR},
  {SCM_UNDEFINED, C_L1},
  {SCM_UNDEFINED, C_R1},
  {SCM_UNDEFINED, C_L2},
  {SCM_UNDEFINED, C_R2},
  {SCM_UNDEFINED, C_L3},
  {SCM_UNDEFINED, C_R3},
  {SCM_UNDEFINED, C_L4},
  {SCM_UNDEFINED, C_R4},
  {SCM_UNDEFINED, C_L5},
  {SCM_UNDEFINED, C_R5},
  {SCM_UNDEFINED, C_ALL},
  {SCM_UNDEFINED, 0}
};


/*
   ** to remove a binding from the global list (probably needs more processing
   ** for mouse binding lines though, like when context is a title bar button).
 */
void 
remove_binding(int contexts, int mods, int button, KeySym keysym,
	       int mouse_binding)
{
  Binding *temp = Scr.AllBindings, *temp2, *prev = NULL;
  KeyCode keycode = 0;

  if (!mouse_binding)
    keycode = XKeysymToKeycode(dpy, keysym);

  while (temp) {
    temp2 = temp->NextBinding;
    if (temp->IsMouse == mouse_binding) {
      if ((temp->Button_Key == ((mouse_binding) ? (button) : (keycode))) &&
	  (temp->Context == contexts) &&
	  (temp->Modifier == mods)) {
	/* we found it, remove it from list */
	if (prev) {		/* middle of list */
	  prev->NextBinding = temp2;
	} else {		/* must have been first one, set new start */
	  Scr.AllBindings = temp2;
	}
	free(temp);
	temp = NULL;
      }
    }
    if (temp)
      prev = temp;
    temp = temp2;
  }
}


int 
lookup_context(SCM context)
{
  int i;

  if (!gh_symbol_p(context)) {
    return -2;
  }
  for (i = 0; binding_contexts[i].value != 0; i++) {
    if (gh_eq_p(binding_contexts[i].sym, context)) {
      return (binding_contexts[i].value);
    }
  }
  return -1;
}

int 
compute_contexts(SCM contexts)
{
  int tmp, retval;

  if (gh_list_p(contexts)) {
    for (tmp = 0, retval = 0; contexts != SCM_EOL; contexts = gh_cdr(contexts)) {
      if ((tmp = lookup_context(gh_car(contexts))) < 0) {
	return tmp;
      } else {
	retval |= tmp;
      }
    }
    return retval;
  } else {
    return lookup_context(contexts);
  }
}

/* FIXGJB: abstract out stuff-- lots of duplication
   between this and unbind_mouse */
SCM 
unbind_key(SCM contexts, SCM key)
{
  KeySym keysym;
  char *keyname, *okey;
  int len;
  int modmask = 0;
  int context = 0;
  int l = 0;

  SCM_REDEFER_INTS;
  if (!gh_string_p(key)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(__FUNCTION__, 2, key);
  }
  context = compute_contexts(contexts);

  switch (context) {
  case 0:
    SCM_ALLOW_INTS;
    scwm_error(__FUNCTION__, 8);
    break;
  case -1:
    SCM_ALLOW_INTS;
    scwm_error(__FUNCTION__, 9);
    break;
  case -2:
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(__FUNCTION__, 1, contexts);
    break;
  default:
  }

  /* FIXGJB: abstract this out */
  okey = (keyname = gh_scm2newstr(key, &len));
  do {
    l = 0;
    if (!strncmp("C-", keyname, 2)) {
      modmask |= ControlMask;
      keyname += 2;
      l = 1;
    } else if (!strncmp("M-", keyname, 2)) {
      modmask |= Mod1Mask;
      keyname += 2;
      l = 1;
    } else if (!strncmp("S-", keyname, 2)) {
      modmask |= ShiftMask;
      keyname += 2;
      l = 1;
    }
  } while (l);

  /*
   * Don't let a 0 keycode go through, since that means AnyKey to the
   * XGrabKey call in GrabKeys().
   */
  if ((keysym = XStringToKeysym(keyname)) == NoSymbol ||
      (XKeysymToKeycode(dpy, keysym)) == 0) {
    free(okey);
    gh_allow_ints();
    SCM_ALLOW_INTS;
    scwm_error(__FUNCTION__, 4);
  }
  remove_binding(context,modmask,0,keysym,False);

  free(okey);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM 
unbind_mouse(SCM contexts, SCM button)
{
  char *keyname, *okey;
  int len;
  int modmask = 0;
  int context = 0;
  int bnum = 0;
  int l = 0;

  SCM_REDEFER_INTS;
  if (!gh_string_p(button)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(__FUNCTION__, 2, button);
  }
  context = compute_contexts(contexts);

  switch (context) {
  case 0:
    SCM_ALLOW_INTS;
    scwm_error(__FUNCTION__, 8);
    break;
  case -1:
    SCM_ALLOW_INTS;
    scwm_error(__FUNCTION__, 9);
    break;
  case -2:
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(__FUNCTION__, 1, contexts);
    break;
  default:
  }

  /* FIXGJB: abstract this out */
  okey = (keyname = gh_scm2newstr(button, &len));
  do {
    l = 0;
    if (!strncmp("C-", keyname, 2)) {
      modmask |= ControlMask;
      keyname += 2;
      l = 1;
    } else if (!strncmp("M-", keyname, 2)) {
      modmask |= Mod1Mask;
      keyname += 2;
      l = 1;
    } else if (!strncmp("S-", keyname, 2)) {
      modmask |= ShiftMask;
      keyname += 2;
      l = 1;
    }
  } while (l);
  bnum = strtol(keyname, NULL, 10);

  remove_binding(context,modmask,bnum,0,True /* Mouse binding */);

  free(okey);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


/* This grabs all the defined keys on all the windows */
void
grab_all_keys_all_windows()
{
  ScwmWindow *swCurrent;

  swCurrent = Scr.ScwmRoot.next;
  while (swCurrent != NULL) {
    GrabKeys(swCurrent);
    swCurrent = swCurrent->next;
  }
}

/* Just grab a single key + modifier on all windows
   This needs to be done after a new key binding */
void
grab_key_all_windows(int key, int modifier)
{
  ScwmWindow *swCurrent;
  swCurrent = Scr.ScwmRoot.next;
  while (swCurrent != NULL) {
    XGrabKey(dpy, key, modifier, swCurrent->frame, True, 
	     GrabModeAsync, GrabModeAsync);
    if (modifier != AnyModifier) {
      XGrabKey(dpy, key, modifier | LockMask, swCurrent->frame, True,
	       GrabModeAsync, GrabModeAsync);
    }
    swCurrent = swCurrent->next;
  }
}

/* Just grab a mouse button + modifier on all windows
   This needs to be done after a new mouse binding */
void
grab_button_all_windows(int button, int modifier)
{
  ScwmWindow *swCurrent;
  swCurrent = Scr.ScwmRoot.next;
  while (swCurrent != NULL) {
    GrabButtonWithModifiers(button,modifier,swCurrent);
    swCurrent = swCurrent->next;
  }
}



SCM 
bind_key(SCM contexts, SCM key, SCM proc)
{
  Binding *temp;
  KeySym keysym;
  Bool fBoundKey = False;	/* for error checking */
  char *keyname, *okey;
  int len, i, min, max;
  int modmask = 0;
  int context = 0;
  int l = 0;

  SCM_REDEFER_INTS;
  if (!gh_string_p(key)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("bind-key", 2, key);
  }
  if (!gh_procedure_p(proc)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("bind-key", 3, proc);
  }
  context = compute_contexts(contexts);

  switch (context) {
  case 0:
    SCM_ALLOW_INTS;
    scwm_error("bind-key", 8);
    break;
  case -1:
    SCM_ALLOW_INTS;
    scwm_error("bind-key", 9);
    break;
  case -2:
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("bind-key", 1, contexts);
    break;
  default:
  }

  okey = (keyname = gh_scm2newstr(key, &len));
  do {
    l = 0;
    if (!strncmp("C-", keyname, 2)) {
      modmask |= ControlMask;
      keyname += 2;
      l = 1;
    } else if (!strncmp("M-", keyname, 2)) {
      modmask |= Mod1Mask;
      keyname += 2;
      l = 1;
    } else if (!strncmp("S-", keyname, 2)) {
      modmask |= ShiftMask;
      keyname += 2;
      l = 1;
    }
  } while (l);

  /*
   * Don't let a 0 keycode go through, since that means AnyKey to the
   * XGrabKey call in GrabKeys().
   */
  if ((keysym = XStringToKeysym(keyname)) == NoSymbol ||
      (XKeysymToKeycode(dpy, keysym)) == 0) {
    free(okey);
    gh_allow_ints();
    SCM_ALLOW_INTS;
    scwm_error("bind", 4);
  }
  /*
     ** why wasn't XKeysymToKeycode used instead of this for loop?
   */
  /* 
   * Because more than one keycode might map to the same keysym -MS
   */

  XDisplayKeycodes(dpy, &min, &max);
  for (i = min; i <= max; i++)
    if (XKeycodeToKeysym(dpy, i, 0) == keysym) {
      temp = Scr.AllBindings;
      Scr.AllBindings = (Binding *) safemalloc(sizeof(Binding));
      Scr.AllBindings->IsMouse = 0;
      Scr.AllBindings->Button_Key = i;
      Scr.AllBindings->key_name = strdup(keyname);
      Scr.AllBindings->Context = context;
      Scr.AllBindings->Modifier = modmask;
      Scr.AllBindings->Action = "Scheme";
      Scr.AllBindings->Thunk = proc;
      Scr.AllBindings->NextBinding = temp;
      scm_protect_object(proc);
      if (Scr.flags & WindowsCaptured) {
	/* only grab the key if we have already captured,
	   otherwise it's a waste of time since we will grab
	   them all later when we do the initial capture;
	   this is good, since initialization probably defines
	   lots of key bindings */
	grab_key_all_windows(i,modmask);
      }
      fBoundKey = True;
    }
  free(okey);
  SCM_REALLOW_INTS;
  if (!fBoundKey) {
    /* FIXGJB: prefer a better error mechanism allowing better
       descriptions -- would like to display the keysym/keycode */
    scwm_error_imm("bind-key", "No matching keycode!");
  }
  return SCM_UNSPECIFIED;
}


SCM 
bind_mouse(SCM contexts, SCM button, SCM proc)
{

  Binding *temp;
  char *keyname = 0;
  char *okey = 0;
  int bnum = 0;
  int bset = 0;
  int len = 0;
  int j = 0;
  int k = 0;
  int modmask = 0;
  int context = 0;
  Bool fGotModifer = False;

  SCM_REDEFER_INTS;

  if (!gh_string_p(button)) {
    if (gh_number_p(button)) {
      bnum = gh_scm2int(button);
      bset = 1;
    } else {
      SCM_ALLOW_INTS;
      scm_wrong_type_arg("bind-mouse", 2, button);
    }
  }
  if (!gh_procedure_p(proc)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("bind-mouse", 3, proc);
  }
  context = compute_contexts(contexts);
  switch (context) {
  case 0:
    SCM_ALLOW_INTS;
    scwm_error("bind-mouse", 8);
    break;
  case -1:
    SCM_ALLOW_INTS;
    scwm_error("bind-mouse", 9);
    break;
  case -2:
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("bind-mouse", 1, contexts);
    break;
  default:
  }

  if (!bset) {
    okey = (keyname = gh_scm2newstr(button, &len));
    do {
      fGotModifer = False;
      if (!strncmp("C-", keyname, 2)) {
	modmask |= ControlMask;
	keyname += 2;
	fGotModifer = True;
      } else if (!strncmp("M-", keyname, 2)) {
	modmask |= Mod1Mask;
	keyname += 2;
	fGotModifer = True;
      } else if (!strncmp("S-", keyname, 2)) {
	modmask |= ShiftMask;
	keyname += 2;
	fGotModifer = True;
      }
    } while (fGotModifer);
    bnum = strtol(keyname, NULL, 10);
  }
  if ((context != C_ALL) && (context & C_LALL)) {
    /* check for nr_left_buttons */
    k = 0;
    j = (context & C_LALL) / C_L1;
    while (j > 0) {
      k++;
      j = j >> 1;
    }
    if (Scr.nr_left_buttons < k)
      Scr.nr_left_buttons = k;
  }
  if ((context != C_ALL) && (context & C_RALL)) {
    /* check for nr_right_buttons */
    k = 0;
    j = (context & C_RALL) / C_R1;
    while (j > 0) {
      k++;
      j = j >> 1;
    }
    if (Scr.nr_right_buttons < k)
      Scr.nr_right_buttons = k;
  }
  /* XXX - we should redraw the titlebars if necessary to reflect the new
     buttons, prehaps? */

  if ((contexts & C_WINDOW) && (((modmask == 0) || modmask == AnyModifier))) {
    Scr.buttons2grab &= ~(1 << (bnum - 1));
  }
  temp = Scr.AllBindings;
  Scr.AllBindings = (Binding *) safemalloc(sizeof(Binding));
  Scr.AllBindings->IsMouse = 1;
  Scr.AllBindings->Button_Key = bnum;
  Scr.AllBindings->key_name = NULL;
  Scr.AllBindings->Context = context;
  Scr.AllBindings->Modifier = modmask;
  Scr.AllBindings->Action = "Scheme";
  Scr.AllBindings->Thunk = proc;
  Scr.AllBindings->NextBinding = temp;
  if (Scr.flags & WindowsCaptured) {
    /* only grab the button press if we have already captured,
       otherwise it's a waste of time since we will grab
       them all later when we do the initial capture;
       this is good, since initialization probably defines
       lots of mouse  bindings */
    grab_button_all_windows(bnum,modmask);
  }

  scm_protect_object(proc);

  if (!bset) {
    free(okey);
  }
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}




/* to distinguish click, double-click, move */

SCM sym_motion, sym_click, sym_one_and_a_half_clicks, sym_double_click;

SCM mouse_ev_type = SCM_BOOL_F;

int have_orig_position = 0;
int orig_x, orig_y;

void 
find_mouse_event_type()
{
  XEvent d;

  gh_defer_ints();
  XQueryPointer(dpy, Scr.Root, &JunkRoot, &JunkChild,
		&orig_x, &orig_y, &JunkX, &JunkY, &JunkMask);
  have_orig_position = 1;

  mouse_ev_type = sym_motion;
  if (IsClick(orig_x, orig_y, ButtonReleaseMask, &d)) {
    mouse_ev_type = sym_click;
    /* If it was a click, wait to see if its a double click */
    if (IsClick(orig_x, orig_y, ButtonPressMask, &d)) {
      mouse_ev_type = sym_one_and_a_half_clicks;
      if (IsClick(orig_x, orig_y, ButtonReleaseMask, &d)) {
	mouse_ev_type = sym_double_click;
      }
    }
  }
  gh_allow_ints();
}

void 
clear_mouse_event_type()
{
  have_orig_position = 0;
  mouse_ev_type = SCM_BOOL_F;
}

SCM 
mouse_event_type()
{
  return mouse_ev_type;
}


SCM sym_new_window, sym_new_window_hint
/*,sym_enter_window.sym_leave_window,sym_edge_hook */ ;


static SCM new_window_hook = SCM_BOOL_F;
static SCM new_window_hint_hook = SCM_BOOL_F;

/* static SCM window_enter_hook=SCM_UNDEFINED;
   static SCM window_leave_hook=SCM_UNDEFINED;
   static SCM edge_hook=SCM_UNDEFINED;
 */

SCM 
bind_event(SCM ev_sym, SCM proc)
{
  SCM old_handler = SCM_UNDEFINED;

  if (!gh_symbol_p(ev_sym)) {
    scm_wrong_type_arg("bind-event", 1, ev_sym);
  }
  if (!gh_procedure_p(proc) && (proc != SCM_BOOL_F)) {
    scm_wrong_type_arg("bind-event", 2, proc);
  }
  if (gh_eq_p(ev_sym, sym_new_window)) {
    old_handler = new_window_hook;
    new_window_hook = proc;
  } else if (gh_eq_p(ev_sym, sym_new_window_hint)) {
    old_handler = new_window_hint_hook;
    new_window_hint_hook = proc;
  } else {
    scwm_error("bind-event", 12);
    return SCM_UNDEFINED;
  }
  scm_unprotect_object(old_handler);
  scm_protect_object(proc);
  return old_handler;
}


void 
run_new_window_hook(SCM w)
{
  if (new_window_hook != SCM_BOOL_F) {
    set_window_context(w);
    call_thunk_with_message_handler(new_window_hook);
    unset_window_context();
  }
}


void 
run_new_window_hint_hook(SCM w)
{
  if (new_window_hint_hook != SCM_BOOL_F) {
    set_window_context(w);
    call_thunk_with_message_handler(new_window_hint_hook);
    unset_window_context();
  }
}



void 
init_binding(void)
{
  int i;
  static char *context_strings[] =
  {
    "window",
    "title",
    "icon",
    "root",
    "frame",
    "sidebar",
    "button-1",
    "button-2",
    "button-3",
    "button-4",
    "button-5",
    "button-6",
    "button-7",
    "button-8",
    "button-9",
    "button-10",
    "all",
    NULL
  };

  for (i = 0; context_strings[i] != NULL; i++) {
    binding_contexts[i].sym = gh_symbol2scm(context_strings[i]);
    scm_protect_object(binding_contexts[i].sym);
  }
  sym_motion = gh_symbol2scm("motion");
  scm_protect_object(sym_motion);
  sym_click = gh_symbol2scm("click");
  scm_protect_object(sym_click);
  sym_one_and_a_half_clicks = gh_symbol2scm("one-and-a-half-clicks");
  scm_protect_object(sym_one_and_a_half_clicks);
  sym_double_click = gh_symbol2scm("double-click");
  scm_protect_object(sym_double_click);

  sym_new_window = gh_symbol2scm("new-window");
  scm_protect_object(sym_new_window);

  sym_new_window_hint = gh_symbol2scm("new-window-hint");
  scm_protect_object(sym_new_window_hint);

  /*
     sym_new_window=gh_symbol2scm("enter-window");
     scm_protect_object(sym_enter_window);
     sym_new_window=gh_symbol2scm("leave-window");
     scm_protect_object(sym_leave_window);
     sym_new_window=gh_symbol2scm("edge");
     scm_protect_object(sym_edge);
   */
}


/* Local Variables: */
/* tab-width: 8 */
/* eval: (set-c-style "k&r") */
/* c-basic-offset: 2 */
/* End: */
