/* $Id$
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 *
 * This module is based on code by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>

#define DECOR_IMPLEMENTATION
#include "decor.h"

#include "scwm.h"
#include "screen.h"
#include "window.h"
#include "color.h"
#include "font.h"
#include "face.h"
#include "borders.h"
#include "font.h"
#include "guile-compat.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

SCM set_current_decor_x(SCM decor);
SCM current_decor();

/*
 *  DestroyScwmDecor -- frees all memory assocated with an ScwmDecor
 *	structure, but does not free the ScwmDecor itself
 */
static void 
DestroyScwmDecor(ScwmDecor * fl)
{
  if (fl->tag) {
    FREE(fl->tag);
    fl->tag = NULL;
  }
  if (fl->HiReliefGC != NULL) {
    XFreeGC(dpy, fl->HiReliefGC);
    fl->HiReliefGC = NULL;
  }
  if (fl->HiShadowGC != NULL) {
    XFreeGC(dpy, fl->HiShadowGC);
    fl->HiShadowGC = NULL;
  }
}

/**CONCEPT: Decors

  Decors are a means of managing the abundance of visual appearance
options for windows. In the original Fvwm code, there were many
options that could only be set globally, although they affected window
appearance. Decors are a compromise between leaving them global and
making them fully settable per window. These quasi-global options may
be set in a particular decor, and a decor may be attached to one or
more windows.

  Having to use decors to change certain aspects of the look and feel
is confusing. Scwm will probably move to some way of making these
options directly settable per-window at some point, especially if we
can figure out a way to not increase the memory overhead much.
*/


size_t 
free_decor(SCM obj)
{
  DestroyScwmDecor(SCWMDECOR(obj));
  FREE(SCWMDECOR(obj));
  FREE(DECOR(obj));
  return 0;
};

int 
print_decor(SCM obj, SCM port, scm_print_state * pstate)
{
  char *name;

  name = SCWMDECOR(obj)->tag;
  scm_puts("#<decor ", port);
  if (NULL == name) {
    scm_write(gh_long2scm((long) DECOR(obj)), port);
  } else {
    scm_puts(name, port);
  }
  scm_putc('>', port);

  return 1;
};


SCM 
mark_decor(SCM obj)
{
  ScwmDecor *fl;
  int i,j;
  
  /* Mark the decor */
  SCM_SETGC8MARK(obj);

  fl=SCWMDECOR(obj);

  /* also protect all of the face objects attached to this decor. */
  for (i=0; i< 5; i++) {
    /* protect the titlebar buttons */
    for (j=0;j< MaxButtonState; j++) {
      if (fl->right_buttons[i].state[j]) {
	GC_MARK_SCM_IF_SET(fl->right_buttons[i].state[j]->sface);
      }
      if (fl->left_buttons[i].state[j]) {
	GC_MARK_SCM_IF_SET(fl->left_buttons[i].state[j]->sface);
      }
    }
  }

  /* Mark the titlebar faces */
  for (j=0;j< MaxButtonState; j++) {
    if (fl->titlebar.state[j]) {
      GC_MARK_SCM_IF_SET(fl->titlebar.state[j]->sface);
    }
  }

  /* Mark the border faces */

  if (fl->BorderStyle.inactive) {
    GC_MARK_SCM_IF_SET(fl->BorderStyle.inactive->sface);
  }
  if (fl->BorderStyle.active) {
    GC_MARK_SCM_IF_SET(fl->BorderStyle.active->sface);
  }
  
  /* Mark the window font. */
  GC_MARK_SCM_IF_SET(fl->window_font);

  /* Mark the hilight colors and relief colors */
  GC_MARK_SCM_IF_SET(fl->HiColors.fg);
  GC_MARK_SCM_IF_SET(fl->HiColors.bg);
  GC_MARK_SCM_IF_SET(fl->HiRelief.fg);
  GC_MARK_SCM_IF_SET(fl->HiRelief.bg);

  return SCM_BOOL_F;
}


extern ScwmDecor *cur_decor;


SCM 
decor2scm(ScwmDecor * fl)
{
  SCM answer;
  SCM tmpd;
  scwm_decor *dec;
  int i,j;

  dec = NEW(scwm_decor);
  dec->refcnt = 0;
  dec->sd = fl;
  gh_defer_ints();
  SCWM_NEWCELL_SMOB(answer,scm_tc16_scwm_decor,dec);
  fl->scmdecor = answer;

  fl->HiReliefGC = NULL;
  fl->HiShadowGC = NULL;

  fl->next = NULL;
 
  tmpd = current_decor();
  set_current_decor_x(answer);
  fl->hilight_factor = 1.2;
  fl->shadow_factor = 0.5;

  set_hilight_foreground_x(BLACK_COLOR);
  set_hilight_background_x(gh_str02scm("grey"));
  set_title_font_x(str_fixed);
  set_current_decor_x(tmpd);

  /* initialize buttons */
  for (i = 0; i < 5; ++i) {
    for (j = 0; j < MaxButtonState; ++j) {
      fl->right_buttons[i].state[j] = BUTTONFACE(default_rbutton_face[i]);
      fl->left_buttons[i].state[j] = BUTTONFACE(default_lbutton_face[i]);
    }
  }
  fl->right_buttons[0].flags |= MWMButton;

  /* initialize title-bar styles */
  fl->titlebar.flags = 0;

  for (i = 0; i < MaxButtonState; ++i) {
    fl->titlebar.state[i] = BUTTONFACE(default_titlebar_face);
  }

  fl->BorderStyle.active=BUTTONFACE(default_border_face);
  fl->BorderStyle.inactive=BUTTONFACE(default_border_face);


  gh_allow_ints();

  return (answer);
};


SCWM_PROC(make_decor, "make-decor", 0, 1, 0,
          (SCM name))
     /** Create a new decor object. NAME optionally provides a string
that is used to name the decor, and is displayed when the decor is
printed. */
#define FUNC_NAME s_make_decor
{
  char *tag;
  int dummy;
  ScwmDecor *newdec;

  if (gh_string_p(name)) {
    tag = gh_scm2newstr(name, &dummy);
  } else if (UNSET_SCM(name)) {
    tag = NULL;
  } else {
    scm_wrong_type_arg(FUNC_NAME, 1, name);
  }

  /* make the decor */
  newdec = NEW(ScwmDecor);
  newdec->tag = tag;

  return decor2scm(newdec);
}
#undef FUNC_NAME


SCWM_PROC(default_decor, "default-decor", 0, 0, 0,
          ())
     /** Return the default decor. */
#define FUNC_NAME s_default_decor
{
  return Scr.DefaultDecor.scmdecor;
}
#undef FUNC_NAME


SCWM_PROC(set_current_decor_x, "set-current-decor!", 1, 0, 0,
          (SCM decor))
     /** Set the current decor to DECOR. Operations described as
setting options "in the current decor" will now operate on this
one. */
#define FUNC_NAME s_set_current_decor_x
{
  ScwmDecor *new_cur;

  if (decor == SCM_BOOL_F) {
    new_cur = NULL;
  } else if (DECORP(decor)) {
    new_cur = SCWMDECOR(decor);
  } else {
    scm_wrong_type_arg(FUNC_NAME, 1, decor);
  }

  if (cur_decor != NULL) {
    DECORUNREF(cur_decor->scmdecor);
  }
  if (new_cur != NULL) {
    DECORREF(decor);
  }
  cur_decor = new_cur;

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(current_decor, "current-decor", 0, 0, 0,
          ())
     /** Return the current decor. */
#define FUNC_NAME s_current_decor
{
  if (cur_decor == NULL) {
    return SCM_BOOL_F;
  } else {
    return cur_decor->scmdecor;
  }
}
#undef FUNC_NAME


SCWM_PROC(set_window_decor_x, "set-window-decor!", 2, 0, 0,
          (SCM win, SCM decor))
     /** Set WIN's decor to DECOR, updating its decorations appropriately. */
#define FUNC_NAME s_set_window_decor_x
{
  int old_height, extra_height;
  ScwmDecor *fl;
  ScwmWindow *psw;

  if (!WINDOWP(win) || !VALIDWINP(win)) {
    scm_wrong_type_arg (FUNC_NAME, 1, win);
  }
  psw = PSWFROMSCMWIN(win);

  if (DECORP(decor)) {
    fl = SCWMDECOR(decor);
  } else {
    scm_wrong_type_arg(FUNC_NAME, 1, decor);
  }

  old_height = psw->fl->TitleHeight;

  psw->fl = fl;

  extra_height = psw->fl->TitleHeight - old_height;

  set_window_internal_title_height(psw, psw->title_height + extra_height);

  SetBorderX(psw, Scr.Hilite == psw, True, True, None, True);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(window_decor, "window-decor", 1, 0, 0,
          (SCM win))
     /** Return WIN's decor. */
#define FUNC_NAME s_window_decor
{
  ScwmWindow *psw;

  if (!WINDOWP(win) || !VALIDWINP(win)) {
    scm_wrong_type_arg (FUNC_NAME, 1, win);
  }
  psw = PSWFROMSCMWIN(win);

  return (psw->fl->scmdecor);
}
#undef FUNC_NAME



MAKE_SMOBFUNS(decor);

void
init_decor()
{
  REGISTER_SCWMSMOBFUNS(decor);
#ifndef SCM_MAGIC_SNARFER
#include "decor.x"
#endif
}



/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
