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

#include <config.h>
#include <stdio.h>
#include <guile/gh.h>
#include <libguile.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "font.h"
#include "binding.h"
#include "window.h"
#include "events.h"
#include "miscprocs.h"
#include "menuitem.h"
#include "scwmmenu.h"
#include "deskpage.h"
#include "decor.h"
#include "face.h"

void 
init_scwm_procs(void)
{
  gh_new_procedure("set-menu-mwm-style!", set_menu_mwm_style, 0, 1, 0);
  gh_new_procedure("set-rubber-band-mask!", set_rubber_band_mask_x, 1, 0, 0);
  gh_new_procedure("set-animation!", set_animation_x, 1,0,0);
  gh_new_procedure("send-button-press", send_button_press, 2,4,0);
  gh_new_procedure("send-key-press", send_key_press, 1,4,0);
  gh_new_procedure("bind-key", bind_key, 3, 0, 0);
  gh_new_procedure("unbind-key", unbind_key, 2, 0, 0);
  gh_new_procedure("bind-mouse", bind_mouse, 3, 0, 0);
  gh_new_procedure("unbind-mouse", unbind_mouse, 2, 0, 0);
  gh_new_procedure("mouse-event-type", mouse_event_type, 0, 0, 0);
  gh_new_procedure("set-title-justify!", set_title_justify, 1, 0, 0);
  gh_new_procedure("set-title-height!", set_title_height, 1, 0, 0);
  gh_new_procedure("get-window", get_window, 0, 3, 0);
  gh_new_procedure("current-window-with-focus", 
		   current_window_with_focus, 0, 0, 0);
  gh_new_procedure("current-window-with-pointer", 
		   current_window_with_pointer, 0, 0, 0);
  gh_new_procedure("select-window-interactively",
		   select_window_interactively, 0, 0, 0);
  gh_new_procedure("select-window", select_window, 0, 2, 0);
  /* maybe set-window-context! and unset-window-context! will be needed.. */
  gh_new_procedure("delete-window", delete_window, 0, 1, 0);
  gh_new_procedure("destroy-window", destroy_window, 0, 1, 0);
  gh_new_procedure("window-deletable?", window_deletable_p, 0, 1, 0);
  gh_new_procedure("focus", focus, 0, 1, 0);
  gh_new_procedure("unfocus", unfocus, 0, 0, 0);
  gh_new_procedure("warp-to-window", warp_to_window, 0, 1, 0);
  gh_new_procedure("raise-window", raise_window, 0, 1, 0);
  gh_new_procedure("lower-window", lower_window, 0, 1, 0);
  gh_new_procedure("raised?", raised_p, 0, 1, 0);
  gh_new_procedure("iconify", iconify, 0, 1, 0);
  gh_new_procedure("deiconify", deiconify, 0, 1, 0);
  gh_new_procedure("iconified?", iconified_p, 0, 1, 0);
  gh_new_procedure("stick", stick, 0, 1, 0);
  gh_new_procedure("unstick", unstick, 0, 1, 0);
  gh_new_procedure("sticky?", sticky_p, 0, 1, 0);
  gh_new_procedure("window-shade", window_shade, 0, 2, 0);
  gh_new_procedure("un-window-shade", un_window_shade, 0, 2, 0);
  gh_new_procedure("window-shaded?", window_shaded_p, 0, 1, 0);
  gh_new_procedure("move-to", move_to, 2, 3, 0);
  gh_new_procedure("interactive-move", interactive_move, 0, 1, 0);
  gh_new_procedure("resize-to", resize_to, 2, 1, 0);
  gh_new_procedure("interactive-resize", interactive_resize, 0, 1, 0);
  gh_new_procedure("refresh-window", refresh_window, 0, 1, 0);
  gh_new_procedure("refresh", refresh, 0, 0, 0);
  gh_new_procedure("set-click-time!", set_click_time_x, 1, 0, 0);
  gh_new_procedure("set-colormap-focus!", set_colormap_focus_x, 1, 0, 0);
  gh_new_procedure("set-opaque-move-size!", set_opaque_move_size_x, 1, 0, 0);
  gh_new_procedure("font?", font_p, 1, 0, 0);
  gh_new_procedure("window?", window_p, 1, 0, 0);
  gh_new_procedure("scwm-quit", scwm_quit, 0, 0, RESTP_SCM);
  gh_new_procedure("pointer-position", pointer_position, 0, 0, 0);
  gh_new_procedure("move-pointer-to", move_pointer_to, 2, 0, 0);
  gh_new_procedure("recapture", recapture, 0, 0, 0);
  gh_new_procedure("restart", restart, 1, 0, 0);
  gh_new_procedure("wait-for-window", wait_for_window, 1, 0, 0);
  gh_new_procedure("move-window-to-desk", move_window_to_desk, 1, 1, 0);
  gh_new_procedure("set-current-desk!", set_current_desk_x, 1, 0, 0);
  gh_new_procedure("set-viewport-position!", set_viewport_position_x, 2, 0, 0);
  gh_new_procedure("set-edge-scroll!", set_edge_scroll_x, 2, 0, 0);
  gh_new_procedure("set-edge-wrap!", set_edge_wrap_x, 2, 0, 0);
  gh_new_procedure("set-edge-resistance!", set_edge_resistance_x, 2, 0, 0);
  gh_new_procedure("set-desk-size!", set_desk_size_x, 2, 0, 0);
  gh_new_procedure("display-size", display_size, 0, 0, 0);
  gh_new_procedure("desk-size", desk_size, 0, 0, 0);
  gh_new_procedure("viewport-position", viewport_position, 0, 0, 0);
  gh_new_procedure("current-desk", current_desk, 0, 0, 0);
  gh_new_procedure("window-position", window_position, 0, 1, 0);
  gh_new_procedure("window-size", window_size, 0, 1, 0);
  gh_new_procedure("window-id", window_id, 0, 1, 0);
  gh_new_procedure("window-frame-id", window_frame_id, 0, 1, 0);
  gh_new_procedure("window-from-window-id", window_from_window_id, 0, 1, 0);
  gh_new_procedure("window-desk", window_desk, 0, 1, 0);
  gh_new_procedure("window-title", window_title, 0, 1, 0);
  gh_new_procedure("list-all-windows", list_all_windows, 0, 0, 0);
  gh_new_procedure("keep-on-top", keep_on_top, 0, 1, 0);
  gh_new_procedure("un-keep-on-top", un_keep_on_top, 0, 1, 0);
  gh_new_procedure("kept-on-top?", kept_on_top_p, 0, 1, 0);
  gh_new_procedure("show-titlebar", show_titlebar, 0, 1, 0);
  gh_new_procedure("hide-titlebar", hide_titlebar, 0, 1, 0);
  gh_new_procedure("titlebar-shown?", titlebar_shown_p, 0, 1, 0);
  gh_new_procedure("normal-border", normal_border, 0, 1, 0);
  gh_new_procedure("plain-border", plain_border, 0, 1, 0);
  gh_new_procedure("border-normal?", border_normal_p, 0, 1, 0);
  gh_new_procedure("set-border-width!", set_border_width_x, 1, 1, 0);
  gh_new_procedure("stick-icon", stick_icon, 0, 1, 0);
  gh_new_procedure("unstick-icon", unstick_icon, 0, 1, 0);
  gh_new_procedure("icon-sticky?", icon_sticky_p, 0, 1, 0);
  gh_new_procedure("set-icon-box!", set_icon_box_x, 4, 1, 0);
  gh_new_procedure("set-window-focus!", set_window_focus_x, 1, 1, 0);
  gh_new_procedure("set-window-foreground!", set_window_foreground_x, 1, 1, 0);
  gh_new_procedure("set-window-background!", set_window_background_x, 1, 1, 0);
  gh_new_procedure("set-icon-title!", set_icon_title_x, 1, 1, 0);
  gh_new_procedure("bind-event", bind_event, 2, 0, 0);
  gh_new_procedure("set-random-placement!", set_random_placement_x, 1, 1, 0);
  gh_new_procedure("set-smart-placement!", set_smart_placement_x, 1, 1, 0);
  gh_new_procedure("set-window-button!", set_window_button_x, 2, 1, 0);
  gh_new_procedure("set-mwm-buttons!", set_mwm_buttons_x, 1, 1, 0);
  gh_new_procedure("set-mwm-border!", set_mwm_border_x, 1, 1, 0);
  gh_new_procedure("set-force-icon!", set_force_icon_x, 1, 1, 0);
  gh_new_procedure("set-show-icon!", set_show_icon_x, 1, 1, 0); 
  gh_new_procedure("set-icon!", set_icon_x, 1, 1, 0);
  gh_new_procedure("set-mini-icon!", set_mini_icon_x, 1, 1, 0);
  gh_new_procedure("set-hint-override!", set_hint_override_x, 1, 1, 0);
  gh_new_procedure("set-decorate-transient!", set_decorate_transient_x, 1, 1, 0);
  gh_new_procedure("set-mwm-decor-hint!", set_mwm_decor_hint_x, 1, 1, 0);
  gh_new_procedure("set-mwm-func-hint!", set_mwm_func_hint_x, 1, 1, 0);
  gh_new_procedure("set-PPosition-hint!", set_PPosition_hint_x, 1, 1, 0);
  gh_new_procedure("set-OL-decor-hint!", set_OL_decor_hint_x, 1, 1, 0);
  gh_new_procedure("set-start-on-desk!", set_start_on_desk_x, 1, 1, 0);
  gh_new_procedure("set-skip-mapping!", set_skip_mapping_x, 1, 1, 0);
  gh_new_procedure("window-class", window_class, 0, 1, 0);
  gh_new_procedure("window-resource", window_resource, 0, 1, 0);
  gh_new_procedure("beep", beep, 0, 0, 0);
  gh_new_procedure("set-lenience!", set_lenience_x, 1, 1, 0);
  gh_new_procedure("make-decor", make_decor, 0, 1, 0);
  gh_new_procedure("default-decor", default_decor, 0, 0, 0);
  gh_new_procedure("set-current-decor!", set_current_decor_x, 1, 0, 0);
  gh_new_procedure("current-decor", current_decor, 0, 0, 0);
  gh_new_procedure("set-window-decor!", set_window_decor_x, 1, 1, 0);
  gh_new_procedure("set-smart-placement-is-really-smart!",
		   set_smart_placement_is_really_smart_x, 1, 0, 0);
  gh_new_procedure("set-click-to-focus-passes-click!", 
		   set_click_to_focus_passes_click_x, 1, 0, 0);
  gh_new_procedure("set-click-to-focus-raises!", 
		   set_click_to_focus_raises_x, 1, 0, 0);
  gh_new_procedure("set-mouse-focus-click-raises!", 
		   set_mouse_focus_click_raises_x, 1, 0, 0);
  gh_new_procedure("restarted?", restarted_p, 0, 0, 0);
  gh_new_procedure("make-face",make_face,2,0,0);
  gh_new_procedure("set-title-face!", set_title_face_x, 1 , 2, 0);
  gh_new_procedure("set-button-face!", set_button_face_x, 2, 2, 0);
  gh_new_procedure("set-button-mwm-flag!", set_button_face_x, 2, 0, 0);
  gh_new_procedure("set-border-face!",set_border_face_x, 1, 1, 0); 
  gh_new_procedure("add-input-hook",add_input_hook,2,0,0);
  gh_new_procedure("scwm-version", scwm_version, 0, 0, 0);
  gh_new_procedure("X-version-information", x_version_information, 0, 0, 0);
  gh_new_procedure("X-display-information", x_display_information, 0, 0, 0);
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
