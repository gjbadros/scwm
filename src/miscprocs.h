#ifndef MISCPROCS_H
#define MISCPROCS_H


SCM set_menu_mwm_style(SCM should);
SCM set_xor_value(SCM value);
SCM set_title_justify(SCM just);
SCM set_title_height(SCM height);
void init_miscprocs();
SCM refresh();

SCM set_click_time_x(SCM ctime);
SCM set_colormap_focus_x(SCM ftype);
SCM set_opaque_move_size_x(SCM size);

SCM scwm_quit();

SCM pointer_position();
SCM move_pointer_to(SCM sx, SCM sy);
SCM recapture();
SCM restart(SCM command);
SCM wait_for_window(SCM name);

#endif MISCPROCS_H
