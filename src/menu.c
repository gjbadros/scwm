




/****************************************************************************
 * This module is all original code 
 * by Maciej Stachowiak.
 * It may be used or distributed under either the FVWM license 
 * (see COPYING.fvwm) or the GNU General Public License (see COPYING.GPL and
 * the description below)
 * Copyright 1997, Maciej Stachowiak
 ****************************************************************************/
/*      Copyright (C) 1997, Maciej Stachowiak

 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.GPL.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, this file may alternatively be distributed under 
 * the fvwm license (see COPYING.FVWM).
 *
 */

#include <guile/gh.h>
#include <config.h>
#include "scwm.h"
#include "menu.h"
#include "screen.h"

long scm_tc16_scwm_menu;

extern XContext MenuContext;

void 
rem_menu_from_list(MenuRoot * mr)
{
  MenuRoot *tmp, *last, *tmp2;

  tmp = Scr.SchemeMenus;
  if (Scr.SchemeMenus == mr) {
    Scr.SchemeMenus = Scr.SchemeMenus->next;
  } else {
    while (NULL != tmp && tmp != mr) {
      tmp2 = tmp;
      tmp = tmp->next;
      last = tmp2;
    }
    if (tmp == mr) {
      last->next = tmp->next;
    }
  }
}

void 
add_menu_to_list(MenuRoot * mr)
{
  mr->next = Scr.SchemeMenus;
  Scr.SchemeMenus = mr;
}

size_t 
free_menu(SCM obj)
{
  MenuItem *mi, *tmp2;
  MenuRoot *mr = MENUROOT(obj);

  rem_menu_from_list(mr);
  XDestroyWindow(dpy, mr->w);
  XDeleteContext(dpy, mr->w, MenuContext);

  /* need to free the window list ? */
  mi = mr->first;
  while (mi != NULL) {
    tmp2 = mi->next;
    if (mi->item != NULL)
      free(mi->item);
    if (mi->item2 != NULL)
      free(mi->item2);
    if (mi->action != NULL)
      free(mi->action);
    if (mi->picture)
      DestroyPicture(dpy, mi->picture);
    if (mi->lpicture)
      DestroyPicture(dpy, mi->lpicture);
    free(mi);
    mi = tmp2;
  }
  free(mr);
  free(SCWMMENU(obj));
  return (0);
}


SCM 
mark_menu(SCM obj)
{
  MenuItem *mi, *tmp2;
  MenuRoot *mr = MENUROOT(obj);

  SCM_SETGC8MARK(obj);
  mi = mr->first;
  while (mi != NULL) {
    if (mi->thunk != SCM_UNDEFINED) {
      scm_gc_mark(mi->thunk);
    }
    mi = mi->next;
  }
  return SCM_BOOL_F;
}

int 
print_menu(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_gen_puts(scm_regular_port, "#<menu \"", port);
  scm_gen_puts(scm_regular_port, MENUROOT(obj)->name, port);
  scm_gen_puts(scm_regular_port, "\">", port);
  return 1;
}

SCM sym_title, sym_separator;

SCM 
make_menu(SCM title, SCM args)
{
  SCM cur, centry;
  SCM answer;
  scwm_menu *em;
  int dummy, procp;

  if (!gh_string_p(title)) {
    scm_wrong_type_arg("make-menu", 1, title);
  }
  gh_defer_ints();
  em = malloc(sizeof(scwm_menu));
  if (NULL == em) {
    gh_allow_ints();
    scm_memory_error("make-menu");
  }
  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, scm_tc16_scwm_menu);
  SCM_SETCDR(answer, (SCM) (em));
  MENUROOT(answer) = (MenuRoot *) malloc(sizeof(MenuRoot));
  if (NULL == MENUROOT(answer)) {
    free(em);
    gh_allow_ints();
    scm_memory_error("make-menu");
  }
  MENUROOT(answer)->func = 19;
  MENUROOT(answer)->name = gh_scm2newstr(title, &dummy);
  MENUROOT(answer)->first = NULL;
  MENUROOT(answer)->items = 0;
  MENUROOT(answer)->width = 0;
  MENUROOT(answer)->width2 = 0;
  MENUROOT(answer)->w = None;
  MENUROOT(answer)->next = NULL;

  for (cur = args; cur != SCM_EOL; cur = SCM_CDR(cur)) {
    centry = SCM_CAR(cur);
    if (gh_eq_p(centry, sym_title)) {
      AddToMenu(MENUROOT(answer),
		MENUROOT(answer)->name, "Title");
      MENUROOT(answer)->last->thunk = SCM_UNDEFINED;
    } else if (gh_eq_p(centry, sym_separator)) {
      AddToMenu(MENUROOT(answer),
		"", "Nop");
      MENUROOT(answer)->last->thunk = SCM_UNDEFINED;
    } else if (gh_pair_p(centry) && gh_string_p(gh_car(centry)) &&
	       gh_pair_p(gh_cdr(centry)) &&
	       (gh_procedure_p(gh_cadr(centry))
		|| (SCM_NIMP(gh_cadr(centry)) &&
		    MENUP(gh_cadr(centry)))) &&
	       (gh_cddr(centry) == SCM_EOL)) {
      procp = gh_procedure_p(gh_cadr(centry));
      AddToMenu(MENUROOT(answer),
		gh_scm2newstr(SCM_CAR(centry), &dummy), procp ? "Scheme" : "SchemeMenu");
      MENUROOT(answer)->last->thunk = SCM_CAR(SCM_CDR(centry));
    } else {
      scwm_error("make-menu", 11);
    }
  }
  gh_allow_ints();
  add_menu_to_list(MENUROOT(answer));
  MakeMenu(MENUROOT(answer));
  return answer;
}

SCM 
popup(SCM menu, SCM sticks)
{
  extern int menuFromFrameOrWindowOrTitlebar;

  if (!(SCM_NIMP(menu) && MENUP(menu))) {
    scm_wrong_type_arg("popup", 1, menu);
  }
  if (sticks == SCM_UNDEFINED) {
    sticks = SCM_BOOL_F;
  }
  if (!gh_boolean_p(sticks)) {
    scm_wrong_type_arg("popup", 2, sticks);
  }
  ActiveItem = NULL;
  ActiveMenu = NULL;
  menuFromFrameOrWindowOrTitlebar = FALSE;
  do_menu(MENUROOT(menu), (sticks == SCM_BOOL_T ? 1 : 0));
  return SCM_UNSPECIFIED;
}

SCM 
menu_p(SCM obj)
{
  return (SCM_NIMP(obj) && MENUP(obj) ? SCM_BOOL_T : SCM_BOOL_F);
}


void 
init_menu()
{
  sym_title = gh_symbol2scm("title");
  scm_protect_object(sym_title);
  sym_separator = gh_symbol2scm("separator");
  scm_protect_object(sym_separator);
}
