/****************************************************************************
 * This module is all original code 
 * by Maciej Stachowiak.
 * It may be used or distributed under either the FVWM license 
 * (see COPYING.fvwm) or the GNU General Public License (see COPYING.GPL and
 * the description below)
 * Copyright 1997, Maciej Stachowiak
 ****************************************************************************/
/*	Copyright (C) 1997, Maciej Stachowiak
 * 
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

#include <config.h>
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
  gh_new_procedure1_0("set-title-justify!",set_title_justify);
  gh_new_procedure1_0("set-title-height!",set_title_height);
  gh_new_procedure0_2("get-window",get_window);
  gh_new_procedure0_1("select-window",select_window);
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
  gh_new_procedure0_0("pointer-position",pointer_position);
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
  gh_new_procedure0_0("display-size",display_size);
  gh_new_procedure0_0("desk-size",desk_size);
  gh_new_procedure0_0("viewport-position",viewport_position);
  gh_new_procedure0_0("current-desk",current_desk);
  gh_new_procedure0_1("window-position",window_position);
  gh_new_procedure0_1("window-size",window_size);
  gh_new_procedure0_1("window-id",window_id);
  gh_new_procedure0_1("window-desk",window_desk);
  gh_new_procedure0_1("window-title",window_title);
  gh_new_procedure0_0("list-all-windows",list_all_windows);
  gh_new_procedure0_1("keep-on-top",keep_on_top);
  gh_new_procedure0_1("un-keep-on-top",un_keep_on_top);
  gh_new_procedure0_1("kept-on-top?",kept_on_top_p);
  gh_new_procedure0_1("show-titlebar",show_titlebar);
  gh_new_procedure0_1("hide-titlebar",hide_titlebar);
  gh_new_procedure0_1("titlebar-shown?",titlebar_shown_p);
  gh_new_procedure0_1("normal-border",normal_border);
  gh_new_procedure0_1("plain-border",plain_border);
  gh_new_procedure0_1("border-normal?",border_normal_p);
  gh_new_procedure1_1("set-border-width!",set_border_width_x);
  gh_new_procedure0_1("stick-icon",stick_icon);
  gh_new_procedure0_1("unstick-icon",unstick_icon);
  gh_new_procedure0_1("icon-sticky?",icon_sticky_p);
  gh_new_procedure("set-icon-box!",set_icon_box_x,4,1,0);
  gh_new_procedure1_1("set-window-focus!",set_window_focus_x);
  gh_new_procedure("set-window-colors!",set_window_colors_x,0,3,0);
  gh_new_procedure1_1("set-icon-title!",set_icon_title_x);
  gh_new_procedure2_0("bind-event",bind_event);
  gh_new_procedure1_1("set-random-placement!",set_random_placement_x);
  gh_new_procedure1_1("set-smart-placement!",set_smart_placement_x);
  gh_new_procedure2_1("set-window-button!",set_window_button_x);
  gh_new_procedure1_1("set-mwm-buttons!",set_mwm_buttons_x);
  gh_new_procedure1_1("set-mwm-border!",set_mwm_border_x);
  gh_new_procedure1_1("set-icon!",set_icon_x);
  gh_new_procedure1_1("set-mini-icon!",set_mini_icon_x);
  gh_new_procedure1_1("set-hint-override!",set_hint_override_x);
  gh_new_procedure1_1("set-decorate-transient!",set_decorate_transient_x);
  gh_new_procedure1_1("set-mwm-decor-hint!",set_mwm_decor_hint_x);
  gh_new_procedure1_1("set-mwm-func-hint!",set_mwm_func_hint_x);
  gh_new_procedure1_1("set-PPosition-hint!",set_PPosition_hint_x);
  gh_new_procedure1_1("set-OL-decor-hint!",set_OL_decor_hint_x);
  gh_new_procedure1_1("set-start-on-desk!",set_start_on_desk_x);
  gh_new_procedure1_1("set-skip-mapping!",set_skip_mapping_x);
  gh_new_procedure0_1("window-class",window_class);
  gh_new_procedure0_1("window-resource",window_resource);
  gh_new_procedure0_0("beep",beep);
  gh_new_procedure1_1("set-lenience!",set_lenience_x);
}







