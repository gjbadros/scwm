/* $Id$
 * binding.c
 * Copyright (C) 1997-1999 By Maciej Stachowiak and Greg J. Badros
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <ctype.h>
#include <X11/keysym.h>

#include <guile/gh.h>

#define BINDING_IMPLEMENTATION
#include "binding.h"

#include "scwm.h"
#include "screen.h"
#include "window.h"
#include "decor.h"
#include "errors.h"
#include "util.h"
#include "add_window.h"
#include "xmisc.h"
#include "syscompat.h"
#include "cursor.h"
#include "focus.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

Bool fQuotingKeystrokes = False;

/* also used by window.c's set-window-focus! */
SCWM_GLOBAL_SYMBOL(sym_click,"click");

SCWM_SYMBOL(sym_motion,"motion");
SCWM_SYMBOL(sym_one_and_a_half_clicks,"one-and-a-half-clicks");
SCWM_SYMBOL(sym_single_click,"single-click");
SCWM_SYMBOL(sym_double_click,"double-click");

SCWM_SYMBOL(sym_shift,"shift");
SCWM_SYMBOL(sym_control,"control");
SCWM_SYMBOL(sym_meta,"meta");
SCWM_SYMBOL(sym_alt,"alt");
SCWM_SYMBOL(sym_hyper,"hyper");
SCWM_SYMBOL(sym_super,"super");
SCWM_SYMBOL(sym_any_modifier,"any-modifier");

struct symnum {
  SCM sym;
  char *sz;
  int value;
};

/* this array is parallel to the below context_strings array */
static
struct symnum binding_contexts[] =
{
  {SCM_UNDEFINED, "window", C_WINDOW},
  {SCM_UNDEFINED, "title", C_TITLE},
  {SCM_UNDEFINED, "icon", C_ICON},
  {SCM_UNDEFINED, "root", C_ROOT},
  {SCM_UNDEFINED, "frame-corners", C_FRAME},
  {SCM_UNDEFINED, "frame-sides", C_SIDEBAR},
  {SCM_UNDEFINED, "left-button-1", C_L1},
  {SCM_UNDEFINED, "right-button-1", C_R1},
  {SCM_UNDEFINED, "left-button-2", C_L2},
  {SCM_UNDEFINED, "right-button-2", C_R2},
  {SCM_UNDEFINED, "left-button-3", C_L3},
  {SCM_UNDEFINED, "right-button-3", C_R3},
  {SCM_UNDEFINED, "left-button-4", C_L4},
  {SCM_UNDEFINED, "right-button-4", C_R4},
  {SCM_UNDEFINED, "left-button-5", C_L5},
  {SCM_UNDEFINED, "right-button-5", C_R5},
  {SCM_UNDEFINED, "all", C_ALL},
  /* below are deprecated or alternate names */
#define IBC_MAX_UNDEPRECATED_SYMBOL 16  /* 16 entries above */
  /* the undeprecated version must be at index-(IBC_MAX_UNDEPRECATED_SYMBOL+1) */
  {SCM_UNDEFINED, "client-window", C_WINDOW},
  {SCM_UNDEFINED, "titlebar", C_TITLE},
  {SCM_UNDEFINED, "icon-window", C_ICON},
  {SCM_UNDEFINED, "root-window", C_ROOT},
  {SCM_UNDEFINED, "frame", C_FRAME},
  {SCM_UNDEFINED, "sidebar", C_SIDEBAR},
  {SCM_UNDEFINED, "button-1", C_L1},
  {SCM_UNDEFINED, "button-2", C_R1},
  {SCM_UNDEFINED, "button-3", C_L2},
  {SCM_UNDEFINED, "button-4", C_R2},
  {SCM_UNDEFINED, "button-5", C_L3},
  {SCM_UNDEFINED, "button-6", C_R3},
  {SCM_UNDEFINED, "button-7", C_L4},
  {SCM_UNDEFINED, "button-8", C_R4},
  {SCM_UNDEFINED, "button-9", C_L5},
  {SCM_UNDEFINED, "button-10", C_R5},
  {SCM_UNDEFINED, "any", C_ALL},
  {SCM_UNDEFINED, NULL, 0}
};


static unsigned 
int MetaMask = 0,
  AltMask = 0,
  HyperMask = 0,
  SuperMask = 0,
  numlock_mask = 0, 
  scrollock_mask = 0;

static unsigned int mask_mod_combos[8];

static unsigned char rgmapMouseButtons[XSERVER_MAX_BUTTONS];

static int cMouseButtons = 3;


/**CONCEPT: Key Specifier
   A key specifier is a string denoting a keystroke, perhaps including
modifiers.  The available modifiers include S-, C-, M-, A-, H-, and s-
for Shift, Control, Meta, Alt, Hyper, and Super, respectively.  They
can be combined arbitrarily, and in any order, but should precede the
key name. They may also be combined without the dash separator;  e.g.,
CSM-Left refers to the keysym "Left" with the control, shift, and meta
modifiers.

When a key specifier is being used to indicate a binding, the
additional special modifier *- may be used; it indicates that the key
should be bound with every possible modifier combination, including
possibly no modifiers. *- may not be combined with any other modifier.  */


static const char *
PchModifiersToModmask(const char *pch, int *pmodifier, char *func_name, Bool allow_any_p)
{
  int modmask = 0;
  Bool fError = False;
  /* C-S-M-Left and CSM-Left should both be okay */
  char *pchLastDash = strrchr(pch,'-');

  while (True) {
    /* do not look at the keysym -- if there was no last dash
       then leave right away, too. */
    if (pch[0] == '\0' || pchLastDash == NULL || pch >= pchLastDash)
      break;
    switch (pch[0]) {
    case 'S': /* Shift */
      if (modmask == AnyModifier) {
	fError = True;
      } else {
	modmask |= ShiftMask;
      }
      break;
    case 'C': /* Control */
      if (modmask == AnyModifier) {
	fError = True;
      } else {	
	modmask |= ControlMask;
      }
      break;
    case 'M': /* Meta */
      if (!MetaMask || modmask == AnyModifier) {
	fError = True;
      } else {	
	modmask |= MetaMask;
      }
      break;
    case 'A': /* Alt */
      if (!AltMask|| modmask == AnyModifier) {
	fError = True;
      } else {	
	modmask |= AltMask;
      }
      break;
    case 'H': /* Hyper */
      if (!HyperMask || modmask == AnyModifier) {
	fError = True;
      } else {
	modmask |= HyperMask;
      }
      break;
    case 's': /* Super (emacs uses "s", so we do too) */
      if (!SuperMask || modmask == AnyModifier) {
	fError = True;
      } else {
	modmask |= SuperMask;
      }
      break;
    case '*': /* AnyModifier */
      if (modmask != 0 || !allow_any_p) {
	fError = True;
      } else {
	modmask |= AnyModifier;
      }
      break;
    default:
      scwm_msg(WARN,func_name,"Unrecognized modifier %c-",pch[0]);
      return NULL;
    }
    if (fError)
      scwm_msg(WARN,func_name,"Unbound modifier %c-",
	       pch[0]);
    /* go to next char, skipping over '-' if necessary */
    if (*(++pch) == '-')
      ++pch;
  }

  if (fError) {
    *pmodifier = -1;
  } else {
    *pmodifier = modmask;
  }
  return pch;
}

/* Returns True if KEY is a string holding a keysym name + possible
   modifier prefixes.  Set *pmodifier to be the modifiers, and *pkeysym
   to the keysym.
*/
/*SCWM_VALIDATE: key */
Bool 
FKeyToKeysymModifiers(SCM key, KeySym *pkeysym, int *pmodifier, char *func_name, 
		      Bool allow_any_p, Bool fShowError)
#define FUNC_NAME func_name
{
  Bool fOk = True;
  char *pch;
  char *keyname;

  int ikey_arg = 2; /* GJB:FIXME:: HACK ALERT! 
                       Works around scwmdoc restriction */

  VALIDATE_ARG_STR_NEWCOPY(ikey_arg,key,keyname);

  pch = (char *) PchModifiersToModmask(keyname,pmodifier, func_name, allow_any_p);

  if (pch == 0 || *pmodifier < 0) {
    FREE(keyname);
    return False;
  }

  if (pch[1] == '\0' && isgraph(pch[0])) {  /* single character, so use tolower */
    pch[0] = tolower(pch[0]);
  }

  if ((*pkeysym = XStringToKeysym(pch)) == NoSymbol ||
	   (XKeysymToKeycode(dpy, *pkeysym)) == 0) { 
    if (fShowError) {
      if (ispunct(pch[0]))
        scwm_msg(WARN,func_name,"No symbol `%s' -- punctuation must be spelled out as a keysym",pch);
      else
        scwm_msg(WARN,func_name,"No symbol `%s'",pch);
    }
    fOk = False; 
  }
  FREE(keyname);
  return fOk;
}
#undef FUNC_NAME

/* No not free the returned char * */
const char *
SzKeysymForKeyCode(KeyCode code, int index)
{
  KeySym keysym = XKeycodeToKeysym(dpy,code,index);
  return XKeysymToString(keysym);
}

/* must FREE the returned string */
char *
SzNewModifierStringForModMask(int modmask)
{
  /* 6 modifiers, each M-, + NULL byte */
  char *sz = NEWC(6*2+1,char);
  *sz = 0;
  if (modmask & ShiftMask) strcat(sz,"S-");
  if (modmask & ControlMask) strcat(sz,"C-");
  if (modmask & MetaMask) strcat(sz,"M-");
  if (modmask & AltMask) strcat(sz,"A-");
  if (modmask & HyperMask) strcat(sz,"H-");
  if (modmask & SuperMask) strcat(sz,"s-");
  return sz;
}

/* must FREE the returned string */
char *
SzNewForModMaskKeyCode(int modmask, KeyCode code)
{
  char *sz = SzNewModifierStringForModMask(modmask);
  KeySym keysym = XKeycodeToKeysym(dpy,code,0);
  /* GJB:FIXME:: is this portable? Want to not list modifier
     keys as keysym strings */
  if (keysym >= XK_Shift_L && keysym <= XK_Hyper_R) {
    /* Just return, e.g., "S-C-M-" */
    return sz;
  } else {
    const char *szKeysym = SzKeysymForKeyCode(code,0);
    if (szKeysym) {
      char *szFull = NEWC(strlen(sz)+strlen(szKeysym)+1,char);
      *szFull = 0;
      strcat(szFull,sz);
      strcat(szFull,szKeysym);
      FREE(sz);
      return szFull;
    } else {
      /* error looking up the keysym */
      return NULL;
    }
  }
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

Bool
FButtonToBnumModifiers(SCM button, int *pbnum, int *pmodifier, char *func_name, 
		       Bool allow_any_p)
{
  Bool fOk = True;
  int len;
  char *button_name = NULL;

  if (!gh_string_p(button)) {
    if (gh_number_p(button)) {
      *pbnum = gh_scm2int(button);
      if (*pbnum < 0 || *pbnum > cMouseButtons) {
	scwm_msg(WARN,func_name,"No button number `%d'",*pbnum);
	return False;
      }
    } else {
      scm_wrong_type_arg(func_name, 2, button);
    }
  } else { /* it is a string */
    button_name = gh_scm2newstr(button,&len);
  }

  if (NULL!=button_name) {
    *pbnum = BnumFromSz(PchModifiersToModmask(button_name, pmodifier, func_name, 
					      allow_any_p));
    if (*pbnum < 0) {
      scwm_msg(WARN,func_name,"No button `%s'",button_name);
      fOk=False;
    }
    if (*pmodifier < 0) {
      scwm_msg(WARN,func_name,"Ignoring mouse bind/unbind request for %s",
	       button_name);
      fOk=False;
    }
    gh_free(button_name);
  }

  return fOk;
}


void
GrabButtonWithModifiersMaskXcPm(int button, int modifier, 
                                Window w, unsigned int event_mask,
                                Cursor xc, int pointer_mode)
{
  if (button > 0) {
    XGrabButton(dpy, button, modifier, w,
		True, event_mask,
		pointer_mode, GrabModeAsync, None, xc);
    if (modifier != AnyModifier) {
      int i = 0;
      for (; i<8; ++i) {
        XGrabButton(dpy, button, (modifier | mask_mod_combos[i]), w,
                    True, event_mask,
                    pointer_mode, GrabModeAsync, None, xc);
      }
    }
  } else {
    int i = 1;
    for ( ; i <= XSERVER_MAX_BUTTONS; ++i) {
      GrabButtonWithModifiersMaskXcPm(i,modifier,w,event_mask,xc,pointer_mode);
    }
  }
}

__inline__ void
GrabButtonWithModifiers(int button, int modifier, 
			ScwmWindow *psw)
{ 
  GrabButtonWithModifiersMaskXcPm(button,modifier,psw->w,
                                  ButtonPressMask | ButtonReleaseMask,
                                  XCursorByNumber(XC_top_left_arrow),
                                  GrabModeAsync);
}


void
UngrabButtonWithModifiersWin(int button, int modifier, Window w)
{
  if (button > 0) {
    XUngrabButton(dpy, button, modifier, w);
    if (modifier != AnyModifier) {
      int i = 0;
      for (; i<8; ++i) {
        XUngrabButton(dpy, button, (modifier | mask_mod_combos[i]), w);
      }
    }
  } else {
    int i = 1;
    for ( ; i <= XSERVER_MAX_BUTTONS; ++i) {
      UngrabButtonWithModifiersWin(i,modifier,w);
    }
  }
}

__inline__ void
UngrabButtonWithModifiers(int button, int modifier, ScwmWindow *psw)
{ UngrabButtonWithModifiersWin(button,modifier,psw->w); }


void 
GrabKeyWithModifiersWin(KeyCode key, unsigned int modifier, Window w)
{
  XGrabKey(dpy, key, modifier, w, True,
           GrabModeAsync, GrabModeAsync);
  if (modifier != AnyModifier) {
    int i = 0;
    for (; i<8; ++i) {
      XGrabKey(dpy, key, modifier | mask_mod_combos[i],
               w, True, GrabModeAsync, GrabModeAsync);
    }
  }
  return;
}

__inline__ void 
GrabKeyWithModifiers(KeyCode key, unsigned int modifier, ScwmWindow *psw)
{ GrabKeyWithModifiersWin(key,modifier,psw->frame); }

void 
UngrabKeyWithModifiersWin(KeyCode key, unsigned int modifier, Window w)
{
  XUngrabKey(dpy, key, modifier, w);
  if (modifier != AnyModifier) {
    int i = 0;
    for (; i<8; ++i) {
      XUngrabKey(dpy, key, modifier | mask_mod_combos[i], w);
    }
  }
  return;
}

__inline__ void 
UngrabKeyWithModifiers(KeyCode key, unsigned int modifier, ScwmWindow *psw)
{ UngrabKeyWithModifiersWin(key,modifier,psw->frame); }


/* GrabButtonsForPsw - grab needed buttons for the window
 *
 * psw - the scwm window structure that needs the grabs
 * (a window just being added)
 */
void 
GrabButtonsForPsw(ScwmWindow * psw)
{
  Binding *pbnd;

  for (pbnd = Scr.AllBindings; pbnd; pbnd = pbnd->NextBinding) {
    if ((pbnd->Context & C_WINDOW) && 
        pbnd->IsMouse) {
      GrabButtonWithModifiers(pbnd->Button_Key,pbnd->Modifier,psw);
    }
  }
  return;
}

void 
UngrabButtonsForPsw(ScwmWindow * psw)
{
  Binding *pbnd;

  for (pbnd = Scr.AllBindings; pbnd; pbnd = pbnd->NextBinding) {
    if ((pbnd->Context & C_WINDOW) &&
        pbnd->IsMouse) {
      UngrabButtonWithModifiers(pbnd->Button_Key,pbnd->Modifier,psw);
    }
  }
  return;
}

/*
 * GrabKeysForPsw - grab needed keys for the window
 *
 * psw - the scwm window structure that needs the grabs
 * (a window just being added)
 */
void 
GrabKeysForPsw(ScwmWindow *psw)
{
  Binding *pbnd;

  for (pbnd = Scr.AllBindings; pbnd; pbnd = pbnd->NextBinding) {
    if ((pbnd->Context & C_WINDOW) &&
	!pbnd->IsMouse) {
      GrabKeyWithModifiers(pbnd->Button_Key,pbnd->Modifier,psw);
    }
  }
  return;
}

void 
UngrabKeysForPsw(ScwmWindow *psw)
{
  Binding *pbnd;

  for (pbnd = Scr.AllBindings; pbnd; pbnd = pbnd->NextBinding) {
    if ((pbnd->Context & C_WINDOW) &&
	!pbnd->IsMouse) {
      UngrabKeyWithModifiers(pbnd->Button_Key,pbnd->Modifier,psw);
    }
  }
  return;
}



/* This grabs all the defined keys on all the windows */
static void
grab_all_keys_all_buttons_all_windows()
{
  ScwmWindow *psw;
  for (psw = Scr.ScwmRoot.next; psw; psw = psw->next) {
    GrabKeysForPsw(psw);
    GrabButtonsForPsw(psw);
  }
}

/* This grabs all the defined keys on all the windows */
static void
ungrab_all_keys_all_buttons_all_windows()
{
  ScwmWindow *psw;
  for (psw = Scr.ScwmRoot.next; psw; psw = psw->next) {
    UngrabKeysForPsw(psw);
    UngrabButtonsForPsw(psw);
  }
}


/* Just grab a single key + modifier on all windows
   This needs to be done after a new key binding */
static void
grab_key_all_windows(int key, int modifier)
{
  ScwmWindow *psw;
  for (psw = Scr.ScwmRoot.next; psw != NULL; psw = psw->next) {
    GrabKeyWithModifiers(key,modifier,psw);
  }
}


static void
ungrab_key_all_windows(int key, int modifier)
{
  ScwmWindow *psw;
  for (psw = Scr.ScwmRoot.next; psw != NULL; psw = psw->next) {
    UngrabKeyWithModifiers(key,modifier,psw);
  }
}


/* Just grab a mouse button + modifier on all windows
   This needs to be done after a new mouse binding */
static void
grab_button_all_windows(int button, int modifier)
{
  ScwmWindow *psw;
  for (psw = Scr.ScwmRoot.next; psw != NULL; psw = psw->next) {
    GrabButtonWithModifiers(button,modifier,psw);
  }
}


static void
ungrab_button_all_windows(int button, int modifier)
{
  ScwmWindow *psw;
  for (psw = Scr.ScwmRoot.next; psw != NULL; psw = psw->next) {
    UngrabButtonWithModifiers(button,modifier,psw);
  }
}


/* to remove a binding from the global list (probably needs more processing
   for mouse binding lines though, like when context is a title bar button).
*/
void 
remove_binding(int context, unsigned int mods, int button, KeySym keysym,
	       int mouse_binding)
{
  Binding *pbnd = Scr.AllBindings, *pbndNext, *prev = NULL;
  KeyCode keycode = 0;

  if (!mouse_binding) {
    keycode = XKeysymToKeycode(dpy, keysym);
    ungrab_key_all_windows(keycode, mods);
  } else if (context & C_WINDOW) {
    ungrab_button_all_windows(button,mods);
  }

  while (pbnd) {
    pbndNext = pbnd->NextBinding;
    if (pbnd->IsMouse == mouse_binding) {
      if ((pbnd->Button_Key == ((mouse_binding) ? (button) : (keycode))) &&
	  (pbnd->Context == context) &&
	  (pbnd->Modifier == mods)) {
	/* we found it, remove it from list */
	if (prev) {		/* middle of list */
	  prev->NextBinding = pbndNext;
	} else {		/* must have been first one, set new start */
	  Scr.AllBindings = pbndNext;
	}
        if (pbnd->key_name)
          gh_free(pbnd->key_name);
        if (!UNSET_SCM(pbnd->Thunk))
          scm_unprotect_object(pbnd->Thunk);
        if (!UNSET_SCM(pbnd->ReleaseThunk))
          scm_unprotect_object(pbnd->ReleaseThunk);
	FREE(pbnd);
	pbnd = NULL;
      }
    }
    if (pbnd)
      prev = pbnd;
    pbnd = pbndNext;
  }
}

void 
add_binding(int context, int modmask, int bnum_or_keycode, int mouse_p, 
	    SCM proc, SCM release_proc, char *name)
{
  Binding *prev_binding = Scr.AllBindings;
  Scr.AllBindings = NEW(Binding);

  Scr.AllBindings->IsMouse = mouse_p;
  Scr.AllBindings->Button_Key = bnum_or_keycode;
  Scr.AllBindings->key_name = name;
  Scr.AllBindings->Context = context;
  Scr.AllBindings->Modifier = modmask;
  Scr.AllBindings->Thunk = proc;
  Scr.AllBindings->ReleaseThunk = release_proc;
  Scr.AllBindings->NextBinding = prev_binding;

  /* have to protect these objects so they do not get GCd --
     will not need this when bindings are first class and they
     get recursively marked */
  if (!UNSET_SCM(proc)) 
    scm_protect_object(proc);
  if (!UNSET_SCM(release_proc)) 
    scm_protect_object(release_proc);

  if (mouse_p) {
    if ( (context & C_WINDOW) && Scr.fWindowsCaptured) {
      /* only grab the button press if we have already captured,
	 otherwise it's a waste of time since we will grab
	 them all later when we do the initial capture;
	 this is good, since initialization probably defines
	 lots of mouse  bindings */
      grab_button_all_windows(bnum_or_keycode, modmask);
    } 
  } else {
    if (Scr.fWindowsCaptured) {
      /* only grab the key if we have already captured,
	 otherwise it's a waste of time since we will grab
	 them all later when we do the initial capture;
	 this is good, since initialization probably defines
	 lots of key bindings */
      grab_key_all_windows(bnum_or_keycode, modmask);
    }
  }
}


/**CONCEPT: Event Contexts

There are various event contexts that are used as arguments
to the binding procedures.  Among these are:

  'window
  'titlebar (or 'title)
  'icon
  'root
  'frame-corners (or 'frame)
  'frame-sides (or 'sidebar)
  'client-window
  'root-window
  'left-button-N  (N=1-5)
  'right-button-N (N=1-5)
  'button-N (N=1-10) [deprecated]

GJB:FIXME:: This should be a definition list or a table, and give real
explanations of what these contexts mean!
 */

int 
lookup_context(SCM context)
{
  int i;

  if (!gh_symbol_p(context)) {
    return -2;
  }
  for (i = 0; binding_contexts[i].value != 0; i++) {
    if (gh_eq_p(binding_contexts[i].sym, context)) {
      if ( i > IBC_MAX_UNDEPRECATED_SYMBOL ) {
        scwm_msg(ERR,"lookup_context","Context name `%s' has been deprecated; please use new name %s",
                 binding_contexts[i].sz,
                 binding_contexts[i-(IBC_MAX_UNDEPRECATED_SYMBOL+1)].sz);
      }
      return (binding_contexts[i].value);
    }
  }
  return -1;
}

int 
compute_contexts(SCM contexts, char *func_name)
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
  } else {
   retval = lookup_context(contexts);
  }

  switch (retval) {
  case 0:
    scwm_error(func_name, "No binding contexts specified.");
    break;
  case -1:
    scwm_error(func_name, "Invalid binding context.");
    break;
  case -2:
    scm_wrong_type_arg(func_name, 1, contexts);
    break;
  default:
    break;
  }
  
  return retval;
}

/* Return NULL if no binding is applicable */
Binding *
PBindingFromKey(KeyCode keycode,
                unsigned int modifier, int context)
{
  Binding *pbnd;
  const unsigned int mask =
    (ShiftMask | ControlMask | Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask |
     Mod5Mask) & (~(numlock_mask | scrollock_mask | LockMask));

  for (pbnd = Scr.AllBindings; pbnd != NULL; pbnd = pbnd->NextBinding) {
    if (!pbnd->IsMouse &&
        (pbnd->Button_Key == keycode) &&
	((pbnd->Modifier == (modifier & mask)) ||
	 (pbnd->Modifier == AnyModifier)) &&
	(pbnd->Context & context)) {
      return pbnd;
    }
  }
  return NULL;
}

/* Return NULL if no binding is applicable */
Binding *
PBindingFromMouse(int button,
                  unsigned int modifier, int context)
{
  Binding *pbnd;
  const unsigned int mask =
    (ShiftMask | ControlMask | Mod1Mask | Mod2Mask | Mod3Mask | Mod4Mask |
     Mod5Mask) & (~(numlock_mask | scrollock_mask | LockMask));

  for (pbnd = Scr.AllBindings; pbnd != NULL; pbnd = pbnd->NextBinding) {
    if (pbnd->IsMouse &&
        ((pbnd->Button_Key == button) ||
         (pbnd->Button_Key == 0)) &&
	((pbnd->Modifier == (modifier & mask)) ||
         (pbnd->Modifier == AnyModifier)) &&
	(pbnd->Context & context)) {
      return pbnd;
    }
  }
  return NULL;
}

SCWM_PROC(set_quote_key_events_x, "set-quote-key-events!", 1, 0, 0,
          (SCM quoting_on_p))
     /** Set key event quoting to QUOTING-ON?. */
#define FUNC_NAME s_set_quote_key_events_x
{
  VALIDATE_ARG_BOOL_COPY(1,quoting_on_p,fQuotingKeystrokes);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(quote_key_events_p, "quote-key-events?", 0, 0, 0,
          ())
     /** Return #t iff key events are being qutoed.
See also `set-quote-key-events!'. */
#define FUNC_NAME s_quote_key_events_p
{
  return gh_bool2scm(fQuotingKeystrokes);
}
#undef FUNC_NAME



SCWM_PROC(lookup_key, "lookup-key", 2, 0, 0,
          (SCM contexts, SCM key))
     /** Return the procedures bound to KEY within the CONTEXTS.
KEY is a modifiers and keysym string.
CONTEXTS is a list of event-contexts (e.g., '(button1 sidebar))
The return value is a list: (press-proc release-proc), or #f
if there is no matching binding. */
#define FUNC_NAME s_lookup_key
{
  KeySym keysym;
  Bool fOkayKey = False;
  int i, min, max;
  int modmask = 0;
  int context = 0;
  Binding *pbnd = NULL;

  context = compute_contexts(contexts, FUNC_NAME);
  fOkayKey = FKeyToKeysymModifiers(key,&keysym,&modmask, FUNC_NAME, True, True);

  /*
   * Don't let a 0 keycode go through, since that means AnyKey to the
   * XGrabKey call in GrabKeys().
   */
  if (keysym ==  NoSymbol || !fOkayKey) {
    return SCM_BOOL_F;
  }

  /* 
   * More than one keycode might map to the same keysym -MS
   */
  
  XDisplayKeycodes(dpy, &min, &max);
  for (i = min; i <= max; i++) {
    if (XKeycodeToKeysym(dpy, i, 0) == keysym) {
      pbnd = PBindingFromKey(i,modmask,context);
      break;
    }
  }

  if (pbnd) {
    return gh_list(pbnd->Thunk,pbnd->ReleaseThunk,SCM_UNDEFINED);
  }

  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCWM_PROC(unbind_key, "unbind-key", 2, 0, 0,
          (SCM contexts, SCM key))
     /** Remove any bindings attached to KEY in given CONTEXTS.
CONTEXTS is a list of event-contexts (e.g., '(button1 sidebar))
KEY is a string giving the key-specifier (e.g., M-Delete for Meta+Delete) */
#define FUNC_NAME s_unbind_key
{
  KeySym keysym;
  Bool fOkayKey;
  int modmask = 0;
  int context = 0;

  context = compute_contexts(contexts, FUNC_NAME);
  fOkayKey = FKeyToKeysymModifiers(key, &keysym, &modmask, FUNC_NAME, True, True);

  /*
   * Don't let a 0 keycode go through, since that means AnyKey to the
   * XGrabKey call in GrabKeys().
   */
  if (keysym == NoSymbol || !fOkayKey) {
    int len;
    char *keyname = gh_scm2newstr(key,&len);
    scwm_msg(WARN,FUNC_NAME,"Ignoring key unbind request for `%s'",keyname);
    gh_free(keyname);
  } else {
    remove_binding(context,modmask,0,keysym,False);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(keysym_to_keycode, "keysym->keycode", 1, 0, 0,
          (SCM keysym_name))
     /** Returns a list of X/11 keycodes that generate the keysym, KEYSYM-NAME.
KEYSYM-NAME should be a string.  E.g., "Control_L".  Return #f if KEYSYM-NAME
is not a valid keysym. */
#define FUNC_NAME s_keysym_to_keycode
{
  SCM answer = SCM_EOL;
  int min, max;
  KeySym keysym;
  int modmask;
  int i;
  Bool fOkayKey;

  /* GJB:FIXME:: check the arg here, since FKeyToKeysymModifiers will
     report errors in position 2, not 1 */
  VALIDATE_ARG_STR(1,keysym_name);

  /* GJB:FIXME:: This shouldn't really accept modifiers in front, but
     FKeyToKeysymModifiers does permit them */
  fOkayKey = FKeyToKeysymModifiers(keysym_name,&keysym,&modmask, FUNC_NAME, False, False);

  if (!fOkayKey)
    return SCM_BOOL_F;

  XDisplayKeycodes(dpy, &min, &max);
  for (i = max; i >= min; --i) {
    if (XKeycodeToKeysym(dpy, i, 0) == keysym) {
      answer = gh_cons(gh_int2scm(i),answer);
    }
  }
  return answer;
}
#undef FUNC_NAME

SCWM_PROC(unbind_mouse, "unbind-mouse", 2, 0, 0,
          (SCM contexts, SCM button))
     /** Remove any bindings attached to mouse BUTTON in given CONTEXTS.
CONTEXTS is a list of event-contexts (e.g., '(button1 sidebar))
BUTTON is a string or integer giving the mouse button number */
#define FUNC_NAME s_unbind_mouse
{
  int bnum = 0;
  int modmask = 0;
  int context = 0;
  int fButtonOK = True;

  fButtonOK = FButtonToBnumModifiers(button, &bnum, &modmask, FUNC_NAME, True);
  context = compute_contexts(contexts, FUNC_NAME);

  if (!fButtonOK) {
    /* Need a better error */
    SCWM_WRONG_TYPE_ARG(2,button);
  }

  remove_binding(context,modmask,bnum,0,True /* Mouse binding */);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(bind_key, "bind-key", 3, 1, 0,
          (SCM contexts, SCM key, SCM proc, SCM release_proc))
     /** Bind the given KEY within the CONTEXTS to invoke PROC.
CONTEXTS is a list of event-contexts (e.g., '(button1 sidebar)) KEY is
a string giving the key-specifier (e.g., M-Delete for Meta+Delete)
PROC is a procedure that will be invoked (with no arguments) when the
specified key is pressed in the specified context. The optional
argument RELEASE-PROC, if specified, is a procedure that will be
invoked when the key is released.  The contexts include:

'window
'titlebar (or 'title)
'icon
'root
'frame-corners (or 'frame)
'frame-sides (or 'sidebar)
'client-window
'root-window
'left-button-N  (N=1-5)
'right-button-N (N=1-5)

*/
#define FUNC_NAME s_bind_key
{
  KeySym keysym;
  int len = 0;
  Bool fOkayKey = False;
  Bool fBoundKey = False;	/* for error checking */
  int i, min, max;
  int modmask = 0;
  int context = 0;

  VALIDATE_ARG_PROC(3,proc);
  VALIDATE_ARG_PROC_USE_F(4,release_proc);

  context = compute_contexts(contexts, FUNC_NAME);

  fOkayKey = FKeyToKeysymModifiers(key,&keysym,&modmask, FUNC_NAME, True, True);

  /*
   * Don't let a 0 keycode go through, since that means AnyKey to the
   * XGrabKey call in GrabKeys().
   */
  if (keysym ==  NoSymbol || !fOkayKey) {
    char *keyname = gh_scm2newstr(key,&len);
    scwm_msg(WARN,FUNC_NAME,"Ignoring key binding `%s'",keyname);
    gh_free(keyname);
    return SCM_BOOL_F;
  }
  /* 
   * More than one keycode might map to the same keysym -MS
   */
  
  XDisplayKeycodes(dpy, &min, &max);
  for (i = min; i <= max; i++) {
    if (XKeycodeToKeysym(dpy, i, 0) == keysym) {
      add_binding(context, modmask, i, 0, proc, release_proc, gh_scm2newstr(key,&len));
      fBoundKey = True;
    }
  }

  if (!fBoundKey) {
    char *keyname = gh_scm2newstr(key,&len);
    scwm_msg(WARN,FUNC_NAME,"No matching keycode for symbol `%s'",keyname);
    gh_free(keyname);
    return SCM_BOOL_F; /* Use False for error */
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(bind_keycode, "bind-keycode", 4, 1, 0,
          (SCM contexts, SCM keycode, SCM modifier_mask, SCM proc, SCM release_proc))
     /** Bind the given KEYCODE within the CONTEXTS to invoke PROC.
CONTEXTS is a list of event-contexts (e.g., '(button1 sidebar))
KEYCODE is an X/11 key code, MODIFIER-MASK is the bitmask of modifiers,
PROC is a procedure that will be invoked (with no arguments) when the 
specified key is pressed in the specified context. 
RELEASE-PROC is a procedure that will be invoked (with no arguments) when the
specified key is released in the specified context, or #f or omitted if 
nothing should be done on key release.
*/
#define FUNC_NAME s_bind_keycode
{
  int min, max;
  int context, keycd, modmask;

  XDisplayKeycodes(dpy, &min, &max);

  VALIDATE_ARG_INT_RANGE_COPY(2,keycode,min,max,keycd);
  VALIDATE_ARG_INT_RANGE_COPY(3,modifier_mask,0,255,modmask);

  VALIDATE_ARG_PROC_USE_F(4,proc);
  VALIDATE_ARG_PROC_USE_F(5,release_proc);

  context = compute_contexts(contexts, FUNC_NAME);
  add_binding(context, modmask, keycd, 0, proc, release_proc, NULL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(bind_mouse, "bind-mouse", 3, 1, 0,
          (SCM contexts, SCM button, SCM proc, SCM immediate_proc))
     /** Bind the given mouse BUTTON within the CONTEXTS to invoke PROC.
CONTEXTS is a list of event-contexts (e.g., '(button1 sidebar))
BUTTON is a string or integer giving the mouse button number
PROC is a procedure that will be invoked (with no arguments) when the 
specified button is pressed in the specified context. See `bind-key'
for a list of the contexts. If IMMEDIATE-PROC is given, it will be
executed immediately on a button-click.  If IMMEDIATE-PROC returns
#f, then PROC will still get executed after determining the mouse click
type. */
#define FUNC_NAME s_bind_mouse
{
  int bnum = 0;
  int j = 0;
  int k = 0;
  int modmask = 0;
  int context = 0;
  Bool fChangedNumButtons = False;

  int fButtonOK = True;

  VALIDATE_ARG_PROC_USE_F(3,proc);
  VALIDATE_ARG_PROC_USE_F(4,immediate_proc);

  context = compute_contexts(contexts, FUNC_NAME);
  fButtonOK = FButtonToBnumModifiers(button, &bnum, &modmask, FUNC_NAME, True);


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

  if ((context & C_WINDOW) && ((modmask == 0) || modmask == AnyModifier)) {
    Scr.buttons2grab &= ~(1 << (bnum - 1));
  }

  add_binding(context, modmask, bnum, 1, proc, immediate_proc, NULL);

  if (fChangedNumButtons && Scr.fWindowsCaptured) {
    /* GJB:FIXME:: does this work? 
       just want to redraw buttons on all windows 
       (used to use a recapture(), but that seems heavy handed) */
    ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;
    redraw_borders(fl);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCWM_PROC(lookup_mouse, "lookup-mouse", 2, 0, 0,
          (SCM contexts, SCM button))
     /** Return the procedure bound to mouse BUTTON within the CONTEXTS.
BUTTON is a string that may contain modifier prefixes, e.g.,
"C-S-M-1". 
CONTEXTS is a list of event-contexts (e.g., '(button1 sidebar))
BUTTON is a string or integer giving the mouse button number and any
modifiers as a prefix.
The return values is the procedure object, or #f if there is no
matching binding. */
#define FUNC_NAME s_lookup_mouse
{
  int bnum = 0;
  int modmask = 0;
  int context = 0;
  Binding *pbnd = NULL;

  int fButtonOK = True;

  context = compute_contexts(contexts, FUNC_NAME);
  fButtonOK = FButtonToBnumModifiers(button, &bnum, &modmask, FUNC_NAME, True);

  pbnd = PBindingFromMouse(bnum, modmask, context);

  if (pbnd) {
    return gh_list(pbnd->Thunk,pbnd->ReleaseThunk,SCM_UNDEFINED);
  }

  return SCM_BOOL_F;
}
#undef FUNC_NAME

static SCM
ScmContextsFromContextMask(int context)
{
  SCM answer = SCM_EOL;
  int i = 0;
  for (; i <= IBC_MAX_UNDEPRECATED_SYMBOL; ++i) {
    if (binding_contexts[i].value & context) {
      answer = gh_cons(binding_contexts[i].sym,answer);
    }
  }
  return answer;
}

static SCM
ScmBindingDescriptionFromPbnd(const Binding *pbnd)
{
  SCM mouse_p = gh_bool2scm(pbnd->IsMouse);
  SCM contexts = ScmContextsFromContextMask(pbnd->Context);
  SCM modmask = gh_int2scm(pbnd->Modifier);
  SCM keybut = gh_int2scm(pbnd->Button_Key);
  SCM proc1 = pbnd->Thunk;
  SCM proc2 = pbnd->ReleaseThunk;

  return gh_list(mouse_p,contexts,modmask,keybut,proc1,proc2,SCM_UNDEFINED);
}

SCWM_PROC(lookup_procedure_bindings, "lookup-procedure-bindings", 0, 1, 0,
          (SCM proc))
     /** Return any bindings that invoke PROC.
If PROC is omitted, return all bindings.
The return value is a list of binding descriptions.  Each binding
description is a list: (mouse? contexts modmask keycode-or-butnum press-proc
release-or-immediate-proc).  mouse? is a boolean, contexts is a list of
symbols. */
#define FUNC_NAME s_lookup_procedure_bindings
{
  SCM bindings = SCM_EOL;
  Binding *pbnd;
  VALIDATE_ARG_PROC_USE_F(1,proc);

  for (pbnd = Scr.AllBindings; pbnd != NULL; pbnd = pbnd->NextBinding) {
    if (SCM_BOOL_F == proc || 
        (pbnd->Thunk == proc || pbnd->ReleaseThunk == proc)) {
      /* got a hit */
      SCM bind = ScmBindingDescriptionFromPbnd(pbnd);
      bindings = gh_cons(bind,bindings);
    }
  }
  return bindings;
}
#undef FUNC_NAME




/*
 * IsClick(...)
 * Waits Scr.ClickTime, or until it is evident that the user is not
 * clicking, but is moving the cursor
 * This function is derived from code by Robert Nation
 */

/* GJB:FIXME:: a single, slow click with no movement should
   still count as a single click */
Bool 
IsClick(int x, int y, unsigned EndMask, XEvent * d)
{
  int xcurrent, ycurrent;
  unsigned int total = 0;
  Time t0;

  xcurrent = x;
  ycurrent = y;
  t0 = lastTimestamp;

  while ((total < Scr.ClickTime) &&
	 (x - xcurrent < 3) && (x - xcurrent > -3) &&
	 (y - ycurrent < 3) && (y - ycurrent > -3) &&
	 ((lastTimestamp - t0) < Scr.ClickTime)) {
    ms_sleep(20);
    total += 20;
    if (XCheckMaskEvent(dpy, EndMask, d)) {
      StashEventTime(d);
      return True;
    }
    if (XCheckMaskEvent(dpy, ButtonMotionMask | PointerMotionMask, d)) {
      xcurrent = d->xmotion.x_root;
      ycurrent = d->xmotion.y_root;
      StashEventTime(d);
    }
  }
  return False;
}


/* to distinguish click, double-click, move */

SCM mouse_ev_type = SCM_BOOL_F;

Bool have_orig_position = False;
int orig_x, orig_y;

void 
find_mouse_event_type()
{
  XEvent d;

  gh_defer_ints();
  WXGetPointerWindowOffsets(Scr.Root, &orig_x, &orig_y);
  have_orig_position = True;

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
  have_orig_position = False;
  mouse_ev_type = SCM_BOOL_F;
}

SCWM_PROC(mouse_event_type, "mouse-event-type", 0, 0, 0,
          ())
     /** Return a symbol corresponding to the type of the most recent mouse event.
Return value is one of 'motion, 'click, 'one-and-a-half-clicks, 'double-click.
You can `case' on this symbol in a procedure bound to a mouse event
to determine, e.g., whether the user single clicked or double clicked. */
#define FUNC_NAME s_mouse_event_type
{
  return mouse_ev_type;
}
#undef FUNC_NAME


SCWM_PROC(mod_mask_shift,"mod-mask-shift", 0, 0, 0, ())
     /** Return the bit-mask for the Shift modifier key, or #f.
Returns #f if and only if there is no key bound to act as Shift, otherwise
returns a power of two corresponding to the bit-mask of the modifier */
#define FUNC_NAME s_mod_mask_shift
{ return ShiftMask == 0? SCM_BOOL_F : gh_int2scm(ShiftMask); }
#undef FUNC_NAME

SCWM_PROC(mod_mask_control,"mod-mask-control", 0, 0, 0, ())
     /** Return the bit-mask for the Control modifier key, or #f.
Returns #f if and only if there is no key bound to act as Control, otherwise
returns a power of two corresponding to the bit-mask of the modifier */
#define FUNC_NAME s_mod_mask_control
{ return ControlMask == 0? SCM_BOOL_F : gh_int2scm(ControlMask); }
#undef FUNC_NAME

SCWM_PROC(mod_mask_meta,"mod-mask-meta", 0, 0, 0, ())
     /** Return the bit-mask for the Meta modifier key, or #f.
Returns #f if and only if there is no key bound to act as Meta, otherwise
returns a power of two corresponding to the bit-mask of the modifier */
#define FUNC_NAME s_mod_mask_meta
{ return MetaMask == 0? SCM_BOOL_F : gh_int2scm(MetaMask); }
#undef FUNC_NAME

SCWM_PROC(mod_mask_alt, "mod-mask-alt", 0, 0, 0, ())
     /** Return the bit-mask for the Alt modifier key, or #f.
Returns #f if and only if there is no key bound to act as Alt, otherwise
returns a power of two corresponding to the bit-mask of the modifier */
#define FUNC_NAME s_mod_mask_alt
{ return AltMask == 0? SCM_BOOL_F : gh_int2scm(AltMask); }
#undef FUNC_NAME

SCWM_PROC(mod_mask_hyper, "mod-mask-hyper", 0, 0, 0, ())
     /** Return the bit-mask for the Hyper modifier key, or #f.
Returns #f if and only if there is no key bound to act as Hyper, otherwise
returns a power of two corresponding to the bit-mask of the modifier */
#define FUNC_NAME s_mod_mask_hyper
{ return HyperMask == 0? SCM_BOOL_F : gh_int2scm (HyperMask); }
#undef FUNC_NAME


SCWM_PROC(mod_mask_super, "mod-mask-super", 0, 0, 0, ())
     /** Return the bit-mask for the Super modifier key, or #f.
Returns #f if and only if there is no key bound to act as Super, otherwise
returns a power of two corresponding to the bit-mask of the modifier */
#define FUNC_NAME s_mod_mask_super
{ return SuperMask == 0? SCM_BOOL_F : gh_int2scm (SuperMask); }
#undef FUNC_NAME


SCWM_PROC(mod_mask_numlock, "mod-mask-numlock", 0, 0, 0, ())
     /** Return the bit-mask for the NumLock modifier key, or #f.
Returns #f if and only if there is no key bound to act as NumLock, otherwise
returns a power of two corresponding to the bit-mask of the modifier */
#define FUNC_NAME s_mod_mask_numlock
{ return numlock_mask == 0? SCM_BOOL_F : gh_int2scm (numlock_mask); }
#undef FUNC_NAME


SCWM_PROC(mod_mask_scrolllock, "mod-mask-scrolllock", 0, 0, 0, ())
     /** Return the bit-mask for the ScrollLock modifier key, or #f.
Returns #f if and only if there is no key bound to act as ScrollLock, otherwise
returns a power of two corresponding to the bit-mask of the modifier */
#define FUNC_NAME s_mod_mask_scrolllock
{ return scrollock_mask == 0? SCM_BOOL_F : gh_int2scm (scrollock_mask); }
#undef FUNC_NAME

SCWM_PROC(set_mod_mask_numlock_x, "set-mod-mask-numlock!", 1, 0, 0, 
          (SCM mask))
     /** Set the bit-mask for the NumLock modifier key.
MASK must be a power of 2. The NumLock modifier mask is
set automatically, but you can use this procedure if you
need to override the built-in algorithm. */
#define FUNC_NAME s_set_mod_mask_numlock_x
{
  VALIDATE_ARG_BOOL_COPY(1,mask,numlock_mask);
  return SCM_UNSPECIFIED;
} 
#undef FUNC_NAME

SCWM_PROC(set_mod_mask_scrolllock_x, "set-mod-mask-scrolllock!", 1, 0, 0, 
          (SCM mask))
     /** Set the bit-mask for the ScrollLock modifier key.
MASK must be a power of 2. The ScrollLock modifier mask is
set automatically, but you can use this procedure if you
need to override the built-in algorithm. */
#define FUNC_NAME s_set_mod_mask_scrolllock_x
{
  VALIDATE_ARG_BOOL_COPY(1,mask,scrollock_mask);
  return SCM_UNSPECIFIED;
} 
#undef FUNC_NAME


SCWM_PROC(undo_all_passive_grabs, "undo-all-passive-grabs", 0, 0, 0,
          ())
     /** Remove all passive grabs of keys and buttons of bindings.
See `redo-all-passive-grabs' for re-establishing those bindings.
This procedure can be useful for quoting numerous keystrokes or
mouse events. Beware that it can take several seconds to execute. */
#define FUNC_NAME s_undo_all_passive_grabs
{
  ungrab_all_keys_all_buttons_all_windows();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(redo_all_passive_grabs, "redo-all-passive-grabs", 0, 0, 0,
          ())
     /** Re-instate all passive grabs of keys and buttons of bindings.
See `undo-all-passive-grabs' for temporarily removing those bindings.
This procedure might be useful for re-establishing bindings after
quoting numerous keystrokes or mouse events.  Beware that it can
take several seconds to execute. */
#define FUNC_NAME s_redo_all_passive_grabs
{
  grab_all_keys_all_buttons_all_windows();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(undo_passive_grab, "undo-passive-grab", 2, 1, 0,
          (SCM modmask, SCM keycode_or_butnum, SCM mouse_p))
     /** Remove the passive grabs of KEYCODE-OR-BUTNUM with MODMASK on all windows. 
If MOUSE? is #t, then treat KEYCODE-OR-BUTNUM as a button number and remove
a grabe of a mouse binding.  Otherwise remove a keyboard passive grab. */
#define FUNC_NAME s_undo_passive_grab
{
  unsigned int mask;
  int key_or_but;
  Bool fMouse;
  VALIDATE_ARG_INT_COPY(1,modmask,mask);
  VALIDATE_ARG_INT_COPY(2,keycode_or_butnum,key_or_but);
  VALIDATE_ARG_BOOL_COPY_USE_F(3,mouse_p,fMouse);
  if (fMouse)
    ungrab_button_all_windows(key_or_but,mask);
  else
    ungrab_key_all_windows(key_or_but,mask);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(redo_passive_grab, "redo-passive-grab", 2, 1, 0,
          (SCM modmask, SCM keycode_or_butnum, SCM mouse_p))
     /** Re-instate the passive grab of KEYCODE-OR-BUTNUM with MODMASK on all windows. 
If MOUSE? is #t, then treat KEYCODE-OR-BUTNUM as a button number and remove
a grabe of a mouse binding.  Otherwise remove a keyboard passive grab. */
#define FUNC_NAME s_redo_passive_grab
{
  unsigned int mask;
  int key_or_but;
  Bool fMouse;
  VALIDATE_ARG_INT_COPY(1,modmask,mask);
  VALIDATE_ARG_INT_COPY(2,keycode_or_butnum,key_or_but);
  VALIDATE_ARG_BOOL_COPY_USE_F(3,mouse_p,fMouse);
  if (fMouse)
    grab_button_all_windows(key_or_but,mask);
  else
    grab_key_all_windows(key_or_but,mask);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCWM_PROC(X_pointer_mapping, "X-pointer-mapping", 0, 0, 0,
          ())
     /** Return the mapping of physical->logical pointer buttons as a list.
The length of the returned list is the number of buttons available.  Each
element in the list is an integer.  E.g., '(1 2 3) is a normally mapped
3-button mouse, whereas '(3 2 1) is a 3-button mouse where the rightmost
physical button acts as logical button 1, and the leftmost acts as button 3. */
#define FUNC_NAME s_X_pointer_mapping
{
  SCM mapping = SCM_EOL;
  int imap = cMouseButtons - 1;
  while (imap >= 0) {
    mapping = gh_cons(gh_int2scm(rgmapMouseButtons[imap]), mapping);
    imap--;
  }
  return mapping;
}
#undef FUNC_NAME

SCWM_PROC (keymask_to_string, "keymask->string", 1, 0, 0,
           (SCM keymask))
     /** Return a string representing KEYMASK.
E.g., (keymask->string 4) => "C-". Returns #f on an error. */
#define FUNC_NAME s_keymask_to_string
{
  int mask;
  char *sz;
  VALIDATE_ARG_INT_RANGE_COPY(1,keymask,0,255,mask);

  sz = SzNewModifierStringForModMask(mask);
  if (sz) {
    SCM answer = gh_str02scm(sz);
    FREE(sz);
    return answer;
  }
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCWM_PROC (keymask_keycode_to_string, "keymask-keycode->string", 2, 0, 0,
           (SCM keymask, SCM keycode))
     /** Return a string representing the key press with mask KEYMASK, code KEYCODE.
E.g., (keymask-keycode->string 4 44) => "C-j". Returns #f on an error. */
#define FUNC_NAME s_keymask_keycode_to_string
{
  int mask;
  char *sz;
  KeyCode code;
  VALIDATE_ARG_INT_RANGE_COPY(1,keymask,0,255,mask);
  VALIDATE_ARG_INT_COPY(2,keycode,code);
  
  sz = SzNewForModMaskKeyCode(mask,code);
  if (sz) {
    SCM answer = gh_str02scm(sz);
    FREE(sz);
    return answer;
  }
  return SCM_BOOL_F;
}
#undef FUNC_NAME


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
    for (i = 0; i < 8; i++) {
      for (j = 0; j < mod->max_keypermod; j++, codes++) {
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
      }
    }

    { /* scope */
      /* Modified from Enlightenment, setup.c:  GJB:SHAREDCODE:: */
      int nl, sl;
      unsigned int masks[8] = {
        ShiftMask, LockMask, ControlMask, Mod1Mask, Mod2Mask, Mod3Mask,
        Mod4Mask, Mod5Mask
      };

      nl = XKeysymToKeycode(dpy, XK_Num_Lock);
      sl = XKeysymToKeycode(dpy, XK_Scroll_Lock);
      if ((mod) && (mod->max_keypermod > 0)) {
        for (i = 0; i < (8 * mod->max_keypermod); i++) {
          if ((nl) && (mod->modifiermap[i] == nl))
            numlock_mask = masks[i / mod->max_keypermod];
          else if ((sl) && (mod->modifiermap[i] == sl))
            scrollock_mask = masks[i / mod->max_keypermod];
        }
      }
      mask_mod_combos[0] = 0;
      mask_mod_combos[1] = LockMask;
      mask_mod_combos[2] = numlock_mask;
      mask_mod_combos[3] = scrollock_mask;
      mask_mod_combos[4] = numlock_mask | scrollock_mask;
      mask_mod_combos[5] = LockMask | numlock_mask;
      mask_mod_combos[6] = LockMask | scrollock_mask;
      mask_mod_combos[7] = LockMask | numlock_mask | scrollock_mask;
    }
    XFreeModifiermap(mod);
  }
}

void 
init_binding(void)
{
  int i;
  for (i = 0; binding_contexts[i].sz != NULL; i++) {
    binding_contexts[i].sym = gh_symbol2scm(binding_contexts[i].sz);
    scm_permanent_object(binding_contexts[i].sym);
  }

#ifndef SCM_MAGIC_SNARFER
#include "binding.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

