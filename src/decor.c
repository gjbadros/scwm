/* $Id$ */
/****************************************************************************
 * This module has been significantly modified by Maciej Stachowiak
 * and Greg J Badros.
 * It may be used under the terms of the fvwm copyright (see COPYING.FVWM).
 * Changes Copyright 1997, Maciej Stachowiak and Greg J. Badros
 ****************************************************************************/
/****************************************************************************
 * This module based on original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/


#define DECOR_IMPLEMENTATION
#include <config.h>
#include <guile/gh.h>
#include "scwm.h"
#include "screen.h"
#include "window.h"
#include "decor.h"
#include "color.h"
#include "font.h"
#include "face.h"
#include "borders.h"

size_t 
free_decor(SCM obj)
{
  DestroyScwmDecor(SCWMDECOR(obj));
  free(SCWMDECOR(obj));
  free(DECOR(obj));
  return 0;
};

int 
print_decor(SCM obj, SCM port, scm_print_state * pstate)
{
  char *name;

  name = SCWMDECOR(obj)->tag;
#ifdef HAVE_SCM_PUTS
  scm_puts("#<decor ", port);
  if (NULL == name) {
    scm_write(gh_int2scm((int) DECOR(obj)), port);
  } else {
    scm_puts(name, port);
  }
  scm_putc('>', port);
#else /* !HAVE_SCM_PUTS */
  scm_gen_puts(scm_regular_port, "#<decor ", port);
  if (NULL == name) {
    scm_write(gh_int2scm((int) DECOR(obj)), port);
  } else {
    scm_gen_puts(scm_regular_port, name, port);
  }
  scm_gen_putc('>', port);
#endif /* HAVE_SCM_PUTS */

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
      if (fl->right_buttons[i].state[j] != NULL) {
	scm_gc_mark(fl->right_buttons[i].state[j]->sface);
      }
      if (fl->left_buttons[i].state[j] !=NULL) {
	scm_gc_mark(fl->left_buttons[i].state[j]->sface);
      }
    }
  }

  /* Mark the titlebar faces */
  for (j=0;j< MaxButtonState; j++) {
    if (fl->titlebar.state[j] !=NULL) {
      scm_gc_mark(fl->titlebar.state[j]->sface);
    }
  }

  /* Mark the border faces */

  if (fl->BorderStyle.inactive !=NULL) {
    scm_gc_mark(fl->BorderStyle.inactive->sface);
  }
  if (fl->BorderStyle.active !=NULL) {
    scm_gc_mark(fl->BorderStyle.active->sface);
  }
  
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

  dec = (scwm_decor *) safemalloc(sizeof(scwm_decor));
  dec->refcnt = 0;
  dec->sd = fl;
  SCM_DEFER_INTS;
  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, scm_tc16_scwm_decor);
  SCM_SETCDR(answer, (SCM) dec);
  fl->scmdecor = answer;

  InitScwmDecor(fl);

  
  tmpd = current_decor();
  set_current_decor_x(answer);
  set_hilight_colors(gh_str02scm("black"),
		     gh_str02scm("grey"));
  set_window_font(gh_str02scm("fixed"));
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


  SCM_ALLOW_INTS;

  return (answer);
};


SCM 
make_decor(SCM name)
{
  char *tag;
  int dummy;
  ScwmDecor *newdec;

  if (gh_string_p(name)) {
    tag = gh_scm2newstr(name, &dummy);
  } else if (name == SCM_UNDEFINED) {
    tag = NULL;
  } else {
    scm_wrong_type_arg("make-decor", 1, name);
  }

  /* make the decor */
  newdec = (ScwmDecor *) safemalloc(sizeof(ScwmDecor));
  newdec->tag = tag;

  return decor2scm(newdec);
}

SCM 
default_decor()
{
  return Scr.DefaultDecor.scmdecor;
}


SCM 
set_current_decor_x(SCM decor)
{
  ScwmDecor *new_cur;

  if (decor == SCM_BOOL_F) {
    new_cur = NULL;
  } else if (SCM_NIMP(decor) && DECORP(decor)) {
    new_cur = SCWMDECOR(decor);
  } else {
    scm_wrong_type_arg("set-current-decor!", 1, decor);
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

SCM 
current_decor()
{
  if (cur_decor == NULL) {
    return SCM_BOOL_F;
  } else {
    return cur_decor->scmdecor;
  }
}


SCM 
set_window_decor_x(SCM decor, SCM win)
{
  int x, y, width, height, old_height, extra_height;
  ScwmDecor *fl;
  ScwmWindow *tmp_win;

  if (SCM_NIMP(decor) && DECORP(decor)) {
    fl = SCWMDECOR(decor);
  } else {
    scm_wrong_type_arg("set-window-decor!", 1, decor);
  }

  VALIDATEN(win, 2, "set-window-decor!");
  tmp_win = SCWMWINDOW(win);

  old_height = tmp_win->fl->TitleHeight;

  tmp_win->fl = fl;

  extra_height = (tmp_win->flags & TITLE) ?
    (old_height - tmp_win->fl->TitleHeight) : 0;
  x = tmp_win->frame_x;
  y = tmp_win->frame_y;
  width = tmp_win->frame_width;
  height = tmp_win->frame_height - extra_height;
  tmp_win->frame_x = 0;
  tmp_win->frame_y = 0;
  tmp_win->frame_height = 0;
  tmp_win->frame_width = 0;
  SetupFrame(tmp_win, x, y, width, height, True);
  SetBorder(tmp_win, Scr.Hilite == tmp_win, True, True, None);

  return SCM_UNSPECIFIED;
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
