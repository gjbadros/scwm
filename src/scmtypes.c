#include <libguile.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>
#include "scwm.h"
#include "font.h"
#include "color.h"
#include "window.h"
#include "menu.h"

static scm_smobfuns font_smobfuns= {
  &scm_mark0,
  &free_font,
  &print_font,
  0
};

static scm_smobfuns color_smobfuns= {
  &scm_mark0,
  &scm_free0,
  &print_color,
  0
};

static scm_smobfuns window_smobfuns= {
  &scm_mark0,
  &free_window,
  &print_window,
  0
};

static scm_smobfuns menu_smobfuns= {
  &mark_menu,
  &free_menu,
  &print_menu,
  0
};


void init_scwm_types(void)
{
  scm_tc16_scwm_font = scm_newsmob (&font_smobfuns);
  scm_tc16_scwm_color = scm_newsmob (&color_smobfuns);
  scm_tc16_scwm_window = scm_newsmob (&window_smobfuns);
  scm_tc16_scwm_menu = scm_newsmob (&menu_smobfuns);
}




