/* $Id$
 * menuitem.c
 * By Greg J. Badros -- Nov. 14, 1997
 *
 */

#define SCWMMENU_IMPLEMENTATION

#include <config.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <X11/keysym.h>
#include <sys/types.h>
#include <sys/time.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <guile/gh.h>
#include "scwm.h"
#include "font.h"
#include "system.h"
#include "scwmmenu.h"


SCM 
mark_scwmmenu(SCM obj)
{
  Scwm_Menu *mi = SCWM_SCWMMENU(obj);

  SCM_SETGC8MARK(obj);
  if (mi->scmMenuItems != SCM_UNDEFINED) {
    scm_gc_mark(mi->scmMenuItems);
  }
  if (mi->scmSideBGColor != SCM_UNDEFINED) {
    scm_gc_mark(mi->scmSideBGColor);
  }
  if (mi->scmBGColor != SCM_UNDEFINED) {
    scm_gc_mark(mi->scmBGColor);
  }
  if (mi->scmTextColor != SCM_UNDEFINED) {
    scm_gc_mark(mi->scmTextColor);
  }
  if (mi->scmFont != SCM_UNDEFINED) {
    scm_gc_mark(mi->scmFont);
  }
  return SCM_BOOL_F;
}

size_t 
free_scwmmenu(SCM obj)
{
  Scwm_Menu *menu = SCWM_SCWMMENU(obj);
  if (menu->picSide) {
    DestroyPicture(dpy,menu->picSide);
  }
  if (menu->picBackground) {
    DestroyPicture(dpy,menu->picBackground);
  }
  if (menu->pchUsedShortcutKeys) {
    free(menu->pchUsedShortcutKeys);
  }
  free(menu);
  return(0);
}

int 
print_scwmmenu(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<scwmmenu ", port);
  if (SCWM_MENU_P(obj)) {
    Scwm_Menu *menu = SCWM_SCWMMENU(obj);
    scm_write(gh_car(menu->scmMenuItems), port);
  } else {
    scm_puts("(invalid)", port);
  }
  scm_putc('>', port);

  return 1;
}

SCM 
scwmmenu_p(SCM obj)
{
  return ((SCM_NIMP(obj) && SCWM_MENU_P(obj)) ? SCM_BOOL_T : SCM_BOOL_F);
}


SCM make_scwmmenu(SCM list_of_menuitems,
		  SCM picture_side, SCM side_bg_color,
		  SCM bg_color, SCM text_color,
		  SCM picture_bg, SCM font)
{
  Scwm_Menu *menu = safemalloc(sizeof(Scwm_Menu));
  SCM answer;
  int iarg = 1;

  if (!gh_list_p(list_of_menuitems)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,list_of_menuitems);
  }
  menu->scmMenuItems = list_of_menuitems;

  iarg++;
  if (picture_side == SCM_UNDEFINED) {
    menu->picSide = NULL;
  } else if (!PICTURE_P(picture_side)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,picture_side);
  } else {
    menu->picSide = PICTURE(picture_side)->pic;
  }

  iarg++;
  if (side_bg_color != SCM_UNDEFINED && !COLORP(side_bg_color)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,side_bg_color);
  }
  menu->scmSideBGColor = side_bg_color;

  iarg++;
  if (bg_color != SCM_UNDEFINED && !COLORP(bg_color)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,bg_color);
  }
  menu->scmBGColor = bg_color;

  iarg++;
  if (text_color != SCM_UNDEFINED && !COLORP(text_color)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,text_color);
  }
  menu->scmTextColor = text_color;

  iarg++;
  if (picture_bg == SCM_UNDEFINED) {
    menu->picBackground = NULL;
  } else if (!PICTURE_P(picture_bg)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,picture_bg);
  } else {
    menu->picBackground = PICTURE(picture_bg)->pic;
  }

  iarg++;
  if (font != SCM_UNDEFINED && !FONTP(font)) {
    scm_wrong_type_arg(__FUNCTION__,iarg,font);
  }
  menu->scmFont = font;

  /* FIXGJB: initialize this properly */
  menu->pchUsedShortcutKeys = NULL;

  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, scm_tc16_scwm_scwmmenu);
  SCM_SETCDR(answer, (SCM) menu);
  return answer;
}
