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
#include <libguile.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>
#include "scwm.h"
#include "font.h"
#include "color.h"
#include "window.h"
#include "menu.h"
#include "decor.h"

static scm_smobfuns font_smobfuns =
{
  &scm_mark0,
  &free_font,
  &print_font,
  0
};

static scm_smobfuns color_smobfuns =
{
  &scm_mark0,
  &scm_free0,
  &print_color,
  0
};

static scm_smobfuns window_smobfuns =
{
  &mark_window,
  &free_window,
  &print_window,
  0
};

static scm_smobfuns menu_smobfuns =
{
  &mark_menu,
  &free_menu,
  &print_menu,
  0
};

static scm_smobfuns decor_smobfuns =
{
  &scm_mark0,
  &free_decor,
  &print_decor,
  0
};


void 
init_scwm_types(void)
{
  scm_tc16_scwm_font = scm_newsmob(&font_smobfuns);
  scm_tc16_scwm_color = scm_newsmob(&color_smobfuns);
  scm_tc16_scwm_window = scm_newsmob(&window_smobfuns);
  scm_tc16_scwm_menu = scm_newsmob(&menu_smobfuns);
  scm_tc16_scwm_decor = scm_newsmob(&decor_smobfuns);
}
