#include <stdio.h>
#include <guile/gh.h>
#include <libguile.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "font.h"
#include "color.h"
#include "binding.h"
#include "window.h"
#include "miscprocs.h"
#include "menu.h"

void init_scwm_procs(void)
{
  gh_new_procedure1_0("load-font", load_font);
  gh_new_procedure1_0("set-icon-font!", set_icon_font);
  gh_new_procedure1_0("set-window-font!", set_window_font);
  gh_new_procedure1_0("set-menu-font!", set_menu_font);
  gh_new_procedure1_0("load-color", load_color);
  gh_new_procedure2_0("set-hilight-colors!", set_hilight_colors);
  gh_new_procedure("set-menu-colors!", set_menu_colors,0,3,0);
  gh_new_procedure0_1("set-menu-mwm-style!", set_menu_mwm_style);
  gh_new_procedure1_0("set-xor-value!", set_xor_value);
  gh_new_procedure3_0("bind-key",bind_key);
  gh_new_procedure3_0("bind-mouse",bind_mouse);
  gh_new_procedure0_0("mouse-event-type",mouse_event_type);
  gh_new_procedure0_1("set-title-justify!",set_title_justify);
  gh_new_procedure0_0("get-window",get_window);
  gh_new_procedure0_0("select-window",select_window);
  /* maybe set-window-context! and unset-window-context! will be needed.. */
  gh_new_procedure0_1("delete-window",delete_window);
  gh_new_procedure0_1("destroy-window",destroy_window);
  gh_new_procedure0_1("window-deletable?",window_deletable_p);
  gh_new_procedure0_1("focus",focus);
  gh_new_procedure0_1("warp-to-window",warp_to_window);
  gh_new_procedure0_1("raise-window",raise_window);
  gh_new_procedure0_1("lower-window",lower_window);
  gh_new_procedure0_1("raised?",raised_p);
  gh_new_procedure0_1("iconify",iconify);
  gh_new_procedure0_1("deiconify",deiconify);
  gh_new_procedure0_1("iconified?",iconified_p);
  gh_new_procedure0_1("stick",stick);
  gh_new_procedure0_1("unstick",unstick);
  gh_new_procedure0_1("sticky?",sticky_p);
#ifdef WINDOWSHADE
  gh_new_procedure0_1("window-shade",window_shade);
  gh_new_procedure0_1("un-window-shade",un_window_shade);
  gh_new_procedure0_1("window-shaded?",window_shaded_p);
#endif /* WINDOWSHADE */
  gh_new_procedure2_1("move-to",move_to);
  gh_new_procedure0_1("interactive-move",interactive_move);
  gh_new_procedure2_1("resize-to",resize_to);
  gh_new_procedure0_1("interactive-resize",interactive_resize);
  gh_new_procedure0_1("refresh-window",refresh_window); 
  gh_new_procedure0_0("refresh",refresh); 
  gh_new_procedure1_0("set-click-time!",set_click_time_x);
  gh_new_procedure1_0("set-colormap-focus!",set_colormap_focus_x);
  gh_new_procedure1_0("set-opaque-move-size!",set_opaque_move_size_x);
  gh_new_procedure("make-menu",make_menu,1,0,1);
  gh_new_procedure1_1("popup",popup);
  gh_new_procedure1_0("menu?",menu_p);
  gh_new_procedure1_0("color?",color_p);
  gh_new_procedure1_0("font?",font_p);
  gh_new_procedure1_0("window?",window_p);
  gh_new_procedure0_0("scwm-quit",scwm_quit);
}







