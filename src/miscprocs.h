#ifndef MISCPROCS_H
#define MISCPROCS_H


SCM set_menu_mwm_style(SCM should);
SCM set_rubber_band_mask_x(SCM value);
SCM set_title_justify(SCM just);
SCM set_title_height(SCM height);
void init_miscprocs();
SCM refresh();

SCM set_click_time_x(SCM ctime);
SCM set_colormap_focus_x(SCM ftype);
SCM set_opaque_move_size_x(SCM size);

SCM scwm_quit(SCM args);

SCM pointer_position();
SCM move_pointer_to(SCM sx, SCM sy);
SCM recapture();
SCM restart(SCM command);
SCM wait_for_window(SCM name);

SCM beep();

SCM set_smart_placement_is_really_smart_x(SCM val);
SCM set_click_to_focus_passes_click_x(SCM val);
SCM set_click_to_focus_raises_x(SCM val);
SCM set_mouse_focus_click_raises_x(SCM val);

SCM scwm_version();

#endif	/* MISCPROCS_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
