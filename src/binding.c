/* $Id$
 * binding.c
 * (C) 1997-1998 By Maciej Stachowiak and Greg J. Badros
 */

#define BINDING_IMPLEMENTATION

#include <config.h>

#include <guile/gh.h>
#include <X11/keysym.h>
#include <ctype.h>

#include "scwm.h"
#include "screen.h"
#include "window.h"
#include "decor.h"
#include "errors.h"
#include "complex.h"
#include "util.h"
#include "misc.h"
#include "miscprocs.h"
#include "add_window.h"
#include "binding.h"
#include "xmisc.h"
#include "syscompat.h"
#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

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

static int MetaMask = 0,
  AltMask = 0,
  HyperMask = 0,
  SuperMask = 0;

static unsigned char rgmapMouseButtons[XSERVER_MAX_BUTTONS];

static int cMouseButtons = 3;

/**CONCEPT: Key Specifier
   A key specifier is a string denoting a keystroke, perhaps including
modifiers.  The available modifiers include S-, C-, M-, A-, H-, and s-
for shift, control meta, alt, hyper, and super, respectively.  They
can be combined arbitrarily, and in any order, but should precide the 
*/

static const char *
PchModifiersToModmask(const char *pch, int *pmodifier)
{
  int modmask = 0;
  Bool fError = False;

  while (True) {
    if (pch[1] != '-') {
      break;
    }
    switch (pch[0]) {
    case 'S': /* Shift */
      modmask |= ShiftMask;
      break;
    case 'C': /* Control */
      modmask |= ControlMask;
      break;
    case 'M': /* Meta */
      if (!MetaMask)
	fError = True;
      modmask |= MetaMask;
      break;
    case 'A': /* Alt */
      if (!AltMask)
	fError = True;
      modmask |= AltMask;
      break;
    case 'H': /* Hyper */
      if (!HyperMask)
	fError = True;
      modmask |= HyperMask;
      break;
    case 's': /* super modifier [0x40] (emacs uses "s", so we do too) */
      if (!SuperMask)
	fError = True;
      modmask |= SuperMask;
      break;
    case 'P':
      /* FIXGJB this can get pulled out later-- I used 'P' at first to avoid
         confusion between 's-' and 'S-' (shift), but people didn't like it */
      scwm_msg(WARN,__FUNCTION__,"Unrecognized modifier P- (super is now 's-')");
      return NULL;
    default:
      scwm_msg(WARN,__FUNCTION__,"Unrecognized modifier %c-",pch[0]);
      return NULL;
    }
    if (fError)
      scwm_msg(WARN,__FUNCTION__,"Unbound modifier %c-",
	       pch[0]);
    pch += 2;
  }

  if (fError) {
    *pmodifier = -1;
  } else {
    *pmodifier = modmask;
  }
  return pch;
}


Bool 
FKeyToKeysymModifiers(SCM key, KeySym *pkeysym, int *pmodifier)
{
  Bool fOk = True;
  int len;
  char *keyname = gh_scm2newstr(key,&len);
  char *pch = (char *) PchModifiersToModmask(keyname,pmodifier);

  if (pch == 0 || *pmodifier < 0) {
    FREE(keyname);
    return False;
  }

  if (pch[1] == '\0' && isgraph(pch[0])) {  /* single character, so use tolower */
    pch[0] = tolower(pch[0]);
  }

  if ((*pkeysym = XStringToKeysym(pch)) == NoSymbol ||
	   (XKeysymToKeycode(dpy, *pkeysym)) == 0) { 
    scwm_msg(WARN,__FUNCTION__,"No symbol `%s'",keyname);
    fOk = False; 
  }
  FREE(keyname);
  return fOk;
}

/* Permit "Mouse-1", "1", "M1", "Mouse1", "mouse1" all to
   be acceptable */
static
int
BnumFromSz(const char *sz)
{
  if (sz == 0)
    return -1;

  if (tolower(*sz) == 'a'  && (strcasecmp(sz,"any") == 0 ||
			       strcasecmp(sz,"all") == 0)) {
    return 0;
  } else {
    int ichFirstDigit = strcspn(sz,"0123456789");
    if (strncasecmp(sz,"mouse-",ichFirstDigit) != 0) {
      return -1; /* no match */
    } else {
      if (strlen(sz+ichFirstDigit) != 1) return -1;
      return strtol(sz + ichFirstDigit, NULL, 10);
    }
  }
}

#ifdef FIXGJB_NOTUSED
/* This grabs all the defined keys on all the windows */
/* FIXGJB: this is not used, but I'm sure it should be used */
static void
grab_all_keys_all_windows()
{
  ScwmWindow *psw;

  psw = Scr.ScwmRoot.next;
  while (psw) {
    GrabKeys(psw);
    psw = psw->next;
  }
}
#endif

/* Just grab a single key + modifier on all windows
   This needs to be done after a new key binding */
static void
grab_key_all_windows(int key, int modifier)
{
  ScwmWindow *psw;
  psw = Scr.ScwmRoot.next;
  while (psw != NULL) {
    XGrabKey(dpy, key, modifier, psw->frame, True, 
	     GrabModeAsync, GrabModeAsync);
    if (modifier != AnyModifier) {
      XGrabKey(dpy, key, modifier | LockMask, psw->frame, True,
	       GrabModeAsync, GrabModeAsync);
    }
    psw = psw->next;
  }
}

/* Just grab a mouse button + modifier on all windows
   This needs to be done after a new mouse binding */
static void
grab_button_all_windows(int button, int modifier)
{
  ScwmWindow *psw;
  psw = Scr.ScwmRoot.next;
  while (psw != NULL) {
    GrabButtonWithModifiers(button,modifier,psw);
    psw = psw->next;
  }
}


static void
ungrab_button_all_windows(int button, int modifier)
{
  ScwmWindow *psw;
  psw = Scr.ScwmRoot.next;
  while (psw != NULL) {
    UngrabButtonWithModifiers(button,modifier,psw);
    psw = psw->next;
  }
}

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

  if (!mouse_binding) {
    keycode = XKeysymToKeycode(dpy, keysym);
  } else if (contexts & C_WINDOW) {
    ungrab_button_all_windows(button,mods);
  }

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
	FREE(temp);
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
SCWM_PROC(unbind_key, "unbind-key", 2, 0, 0,
          (SCM contexts, SCM key))
     /** Remove any bindings attached to KEY in given CONTEXTS.
CONTEXTS is a list of event-contexts (e.g., '(button1 sidebar))
KEY is a string giving the key-specifier (e.g., M-Delete for META+Delete) */
{
  KeySym keysym;
  Bool fOkayKey;
  int modmask = 0;
  int context = 0;

  SCM_REDEFER_INTS;
  if (!gh_string_p(key)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg(__FUNCTION__, 2, key);
  }
  context = compute_contexts(contexts);

  switch (context) {
  case 0:
    SCM_ALLOW_INTS;
    /* FIXGJBERROR: do not error by number */
    scwm_error(__FUNCTION__, 8);
    break;
  case -1:
    SCM_ALLOW_INTS;
    /* FIXGJBERROR: do not error by number */
    scwm_error(__FUNCTION__, 9);
    break;
  case -2:
    SCM_ALLOW_INTS;
    /* FIXGJBERROR: do not error by number */
    scm_wrong_type_arg(__FUNCTION__, 1, contexts);
    break;
  default:
    break;
  }

  fOkayKey = FKeyToKeysymModifiers(key, &keysym, &modmask);

  /*
   * Don't let a 0 keycode go through, since that means AnyKey to the
   * XGrabKey call in GrabKeys().
   */
  if (keysym == NoSymbol || !fOkayKey) {
    int len;
    char *keyname = gh_scm2newstr(key,&len);
    gh_allow_ints();
    SCM_ALLOW_INTS;
    scwm_msg(WARN,__FUNCTION__,"Ignoring key unbind request for `%s'",keyname);
    FREE(keyname);
  } else {
    remove_binding(context,modmask,0,keysym,False);
  }
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCWM_PROC(unbind_mouse, "unbind-mouse", 2, 0, 0,
          (SCM contexts, SCM button))
     /** Remove any bindings attached to mouse BUTTON in given CONTEXTS.
CONTEXTS is a list of event-contexts (e.g., '(button1 sidebar))
BUTTON is a string or integer giving the mouse button number */
{
  char *szButton = NULL;
  int cchButton = 0;
  int bnum = 0;
  int modmask = 0;
  int context = 0;

  SCM_REDEFER_INTS;
  if (!gh_string_p(button)) {
    if (gh_number_p(button)) {
      bnum = gh_scm2int(button);
      if (bnum < 0 || bnum > cMouseButtons) {
	scwm_msg(WARN,__FUNCTION__,"No button number `%d'",bnum);
	SCM_ALLOW_INTS;
	return SCM_UNSPECIFIED;
      }
    } else {
      SCM_ALLOW_INTS;
      scm_wrong_type_arg("unbind-mouse", 2, button);
    }
  } else { /* it is a string */
    szButton = gh_scm2newstr(button,&cchButton);
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
    break;
  }

  if (szButton) {
    bnum = BnumFromSz(PchModifiersToModmask(szButton,&modmask));
    if (bnum < 0) {
      scwm_msg(WARN,__FUNCTION__,"No button `%s'",szButton);
      SCM_ALLOW_INTS;
      FREE(szButton);
      return SCM_UNSPECIFIED;
    }
    if (modmask < 0) {
      scwm_msg(WARN,__FUNCTION__,"Ignoring mouse unbind request for %s",
	       szButton);
      SCM_ALLOW_INTS;
      FREE(szButton);
      return SCM_UNSPECIFIED;
    }
    FREE(szButton);
  }

  remove_binding(context,modmask,bnum,0,True /* Mouse binding */);
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCWM_PROC(bind_key, "bind-key", 3, 0, 0,
          (SCM contexts, SCM key, SCM proc))
     /** Bind the given KEY within the CONTEXTS to invoke PROC.
CONTEXTS is a list of event-contexts (e.g., '(button1 sidebar))
KEY is a string giving the key-specifier (e.g., M-Delete for META+Delete)
PROC is a procedure (possibly a thunk) that should be invoked */
{
  KeySym keysym;
  int len = 0;
  Bool fOkayKey = False;
  Bool fBoundKey = False;	/* for error checking */
  int i, min, max;
  int modmask = 0;
  int context = 0;

  SCM_REDEFER_INTS;
  if (!gh_string_p(key)) {
    SCM_ALLOW_INTS;
    scm_wrong_type_arg("bind-key", 2, key);
  }
  if (!PROCEDURE_OR_SYMBOL_P(proc)) {
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
    break;
  }

  fOkayKey = FKeyToKeysymModifiers(key,&keysym,&modmask);

  /*
   * Don't let a 0 keycode go through, since that means AnyKey to the
   * XGrabKey call in GrabKeys().
   */
  if (keysym ==  NoSymbol || !fOkayKey) {
    char *keyname = gh_scm2newstr(key,&len);
    gh_allow_ints();
    SCM_ALLOW_INTS;
    scwm_msg(WARN,__FUNCTION__,"Ignoring key binding `%s'",keyname);
    FREE(keyname);
    return SCM_BOOL_F;
  }
  /* 
   * One keycode might map to the same keysym -MS
   */
  
  XDisplayKeycodes(dpy, &min, &max);
  for (i = min; i <= max; i++)
    if (XKeycodeToKeysym(dpy, i, 0) == keysym) {
      Binding *prev_binding = Scr.AllBindings;
      Scr.AllBindings = NEW(Binding);
      Scr.AllBindings->IsMouse = 0;
      Scr.AllBindings->Button_Key = i;
      Scr.AllBindings->key_name = gh_scm2newstr(key,&len);
      Scr.AllBindings->Context = context;
      Scr.AllBindings->Modifier = modmask;
      Scr.AllBindings->Action = "Scheme";
      Scr.AllBindings->Thunk = proc;
      Scr.AllBindings->NextBinding = prev_binding;
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
  SCM_REALLOW_INTS;
  if (!fBoundKey) {
    char *keyname = gh_scm2newstr(key,&len);
    gh_allow_ints();
    SCM_ALLOW_INTS;
    scwm_msg(WARN,__FUNCTION__,"No matching keycode for symbol `%s'",keyname);
    FREE(keyname);
    return SCM_BOOL_F; /* Use False for error */
  }
  return SCM_UNSPECIFIED;
}


SCWM_PROC(bind_mouse, "bind-mouse", 3, 0, 0,
          (SCM contexts, SCM button, SCM proc))
     /** Bind the given mouse BUTTON within the CONTEXTS to invoke PROC.
CONTEXTS is a list of event-contexts (e.g., '(button1 sidebar))
BUTTON is a string or integer giving the mouse button number
PROC is a procedure (possibly a thunk) that should be invoked */
{
  Binding *temp;
  char *szButton = 0;
  int cchButton;
  int bnum = 0;
  int j = 0;
  int k = 0;
  int modmask = 0;
  int context = 0;
  Bool fChangedNumButtons = False;

  SCM_REDEFER_INTS;

  if (!gh_string_p(button)) {
    if (gh_number_p(button)) {
      bnum = gh_scm2int(button);
      if (bnum < 0 || bnum > cMouseButtons) {
	scwm_msg(WARN,__FUNCTION__,"No button number `%d'",bnum);
	SCM_ALLOW_INTS;
	return SCM_UNSPECIFIED;
      }
    } else {
      SCM_ALLOW_INTS;
      scm_wrong_type_arg("bind-mouse", 2, button);
    }
  } else { /* it is a string */
    szButton = gh_scm2newstr(button,&cchButton);
  }
  if (!PROCEDURE_OR_SYMBOL_P(proc)) {
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
    break;
  }
  
  if (szButton) {
    bnum = BnumFromSz(PchModifiersToModmask(szButton,&modmask));
    if (bnum < 0) {
      scwm_msg(WARN,__FUNCTION__,"No button `%s'",szButton);
      SCM_ALLOW_INTS;
      FREE(szButton);
      return SCM_UNSPECIFIED;
    }
    if (modmask < 0) {
      scwm_msg(WARN,__FUNCTION__,"Ignoring mouse binding for %s",szButton);
      SCM_ALLOW_INTS;
      FREE(szButton);
      return SCM_UNSPECIFIED;
    }
    FREE(szButton);
  }
  if ((context != C_ALL) && (context & C_LALL)) {
    /* check for nr_left_buttons */
    k = 0;
    j = (context & C_LALL) / C_L1;
    while (j > 0) {
      k++;
      j = j >> 1;
    }
    if (Scr.nr_left_buttons < k) {
      Scr.nr_left_buttons = k;
      fChangedNumButtons = True;
    }
  }
  if ((context != C_ALL) && (context & C_RALL)) {
    /* check for nr_right_buttons */
    k = 0;
    j = (context & C_RALL) / C_R1;
    while (j > 0) {
      k++;
      j = j >> 1;
    }
    if (Scr.nr_right_buttons < k) {
      Scr.nr_right_buttons = k;
      fChangedNumButtons = True;
    }
  }

  if ((contexts & C_WINDOW) && (((modmask == 0) || modmask == AnyModifier))) {
    Scr.buttons2grab &= ~(1 << (bnum - 1));
  }
  temp = Scr.AllBindings;
  Scr.AllBindings = NEW(Binding);
  Scr.AllBindings->IsMouse = 1;
  Scr.AllBindings->Button_Key = bnum;
  Scr.AllBindings->key_name = NULL;
  Scr.AllBindings->Context = context;
  Scr.AllBindings->Modifier = modmask;
  Scr.AllBindings->Action = "Scheme";
  Scr.AllBindings->Thunk = proc;
  Scr.AllBindings->NextBinding = temp;
  if (contexts & C_WINDOW && Scr.flags & WindowsCaptured) {
    /* only grab the button press if we have already captured,
       otherwise it's a waste of time since we will grab
       them all later when we do the initial capture;
       this is good, since initialization probably defines
       lots of mouse  bindings */
    grab_button_all_windows(bnum,modmask);
  }

  scm_protect_object(proc);
  SCM_REALLOW_INTS;
  if (fChangedNumButtons && Scr.flags & WindowsCaptured) {
  /* FIXGJB - we should redraw the titlebars if necessary to reflect the new
     buttons */
#ifdef FIXGJB /* this doesn't work, just want to redraw buttons on all windows */
    ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;
    redraw_borders(fl);
#endif
    recapture(); /* this stinks, but'll have to do for now --11/11/97 gjb */
  }
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
  XGetPointerWindowOffsets(Scr.Root, &orig_x, &orig_y);
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

/* FIXGJB: a single, slow click with no movement should
   still count as a single click, see IsClick(), too */
SCWM_PROC(mouse_event_type, "mouse-event-type", 0, 0, 0,
          ())
     /** Return a mouse-event-type corresponding to the most recent mouse event.
Return value is one of 'motion, 'click, 'one-and-a-half-clicks, 'double-clicks */
{
  return mouse_ev_type;
}


SCWM_PROC(mod_mask_meta,"mod-mask-meta", 0, 0, 0, ())
     /** Return the bitmask for the META modifier key, or #f.
Returns #f iff there is no key bound to act as META, otherwise
returns a power of two corresponding to the bitmask of the modifier */
{ return MetaMask == 0? SCM_BOOL_F : gh_int2scm(MetaMask); }

SCWM_PROC(mod_mask_alt, "mod-mask-alt", 0, 0, 0, ())
     /** Return the bitmask for the ALT modifier key, or #f.
Returns #f iff there is no key bound to act as ALT, otherwise
returns a power of two corresponding to the bitmask of the modifier */
{ return AltMask == 0? SCM_BOOL_F : gh_int2scm(AltMask); }

SCWM_PROC(mod_mask_hyper, "mod-mask-hyper", 0, 0, 0, ())
     /** Return the bitmask for the HYPER modifier key, or #f.
Returns #f iff there is no key bound to act as HYPER, otherwise
returns a power of two corresponding to the bitmask of the modifier */
{ return HyperMask == 0? SCM_BOOL_F : gh_int2scm (HyperMask); }


SCWM_PROC(mod_mask_super, "mod-mask-super", 0, 0, 0,())
     /** Return the bitmask for the SUPER modifier key, or #f.
Returns #f iff there is no key bound to act as SUPER, otherwise
returns a power of two corresponding to the bitmask of the modifier */
{ return SuperMask == 0? SCM_BOOL_F : gh_int2scm (SuperMask); }


SCWM_PROC(pointer_mapping, "X-pointer-mapping", 0, 0, 0,
          ())
     /** Return the mapping of physical->logical pointer buttons as a list.
The length of the returned list is the number of buttons available.  Each
element in the list is an integer.  E.g., '(1 2 3) is a normally mapped
3-button mouse, whereas '(3 2 1) is a 3-button mouse where the rightmost
physical button acts as logical button 1, and the leftmost acts as button 3. */
{
  SCM mapping = SCM_EOL;
  int imap = cMouseButtons - 1;
  while (imap >= 0) {
    mapping = gh_cons(gh_int2scm(rgmapMouseButtons[imap]), mapping);
    imap--;
  }
  return mapping;
}

void
init_pointer_mapping(void)
{
  cMouseButtons = XGetPointerMapping(dpy, rgmapMouseButtons, XSERVER_MAX_BUTTONS);
}

void
init_modifiers(void)
{
  int i, j, num;
  XModifierKeymap *mod;
  KeyCode *codes;
  KeySym *syms;

  MetaMask = AltMask = HyperMask = SuperMask = 0;

  mod = XGetModifierMapping(dpy);
  if (mod) {
    codes = mod->modifiermap;
    for (i = 0; i < 8; i++)
      for (j = 0; j < mod->max_keypermod; j++, codes++)
	if (*codes) {
	  syms = XGetKeyboardMapping(dpy, *codes, 1, &num);
	  if (syms) {
	    while (num--)
	      switch (syms[num]) {
	      case XK_Meta_L:
	      case XK_Meta_R:
		MetaMask = 1<<i;
		break;
	      case XK_Alt_L:
	      case XK_Alt_R:
		AltMask = 1<<i;
		break;
	      case XK_Super_L:
	      case XK_Super_R:
		SuperMask = 1<<i;
		break;
	      case XK_Hyper_L:
	      case XK_Hyper_R:
		HyperMask = 1<<i;
		break;
	      }
	    XFree(syms);
	  }
	}
    XFreeModifiermap(mod);
  }
}


void 
init_binding(void)
{
  int i;
  /* FIXGJB: buttons should have symbolic names, not numbered
     physically */
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

#ifndef SCM_MAGIC_SNARFER
#include "binding.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
