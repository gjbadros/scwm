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
#include "paths.h"
#include "deskpage.h"

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
  gh_new_procedure0_0("get-pointer-position",get_pointer_position);
  gh_new_procedure2_0("move-pointer-to",move_pointer_to);
  gh_new_procedure0_0("recapture",recapture);
  gh_new_procedure1_0("restart",restart);
  gh_new_procedure1_0("wait-for-window",wait_for_window);
  gh_new_procedure1_0("set-pixmap-path!",set_pixmap_path_x);
  gh_new_procedure1_0("set-icon-path!",set_icon_path_x);
  gh_new_procedure1_1("move-window-to-desk",move_window_to_desk);
  gh_new_procedure1_0("set-current-desk!",set_current_desk_x);
  gh_new_procedure2_0("set-viewport-position!",set_viewport_position_x);
  gh_new_procedure2_0("set-edge-scroll!",set_edge_scroll_x);
  gh_new_procedure2_0("set-edge-wrap!",set_edge_wrap_x);
  gh_new_procedure2_0("set-edge-resistance!",set_edge_resistance_x);
  gh_new_procedure2_0("set-desk-size!",set_desk_size_x);
  gh_new_procedure0_0("get-display-size",get_display_size);
  gh_new_procedure0_0("get-desk-size",get_desk_size);
  gh_new_procedure0_0("get-viewport-position",get_viewport_position);
  gh_new_procedure0_0("get-current-desk",get_current_desk);
  gh_new_procedure0_1("get-window-position",get_window_position);
  gh_new_procedure0_1("get-window-size",get_window_size);
  gh_new_procedure0_1("get-window-id",get_window_id);
  gh_new_procedure0_1("get-window-desk",get_window_desk);
  gh_new_procedure0_1("get-window-title",get_window_title);
  gh_new_procedure0_0("get-window-list",get_window_list);
}







