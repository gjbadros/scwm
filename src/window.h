/* $Id$ */

#ifndef WINDOW_H
#define WINDOW_H

#undef EXTERN
#undef EXTERN_SET
#ifdef WINDOW_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

#include <libguile.h>
#include "scwm.h"

SCM  ensure_valid(SCM win, int n, char *subr, SCM kill_p, SCM release_p);

#define VALIDATE(win,subr)  if(((win=ensure_valid(win,1,subr,SCM_BOOL_F, SCM_BOOL_T)))==SCM_BOOL_F) return SCM_BOOL_F

#define VALIDATEKILL(win,subr)  if(((win=ensure_valid(win,1,subr,SCM_BOOL_T, SCM_BOOL_T)))==SCM_BOOL_F) return SCM_BOOL_F

#define VALIDATEN(win,n,subr)  if(((win=ensure_valid(win,n,subr,SCM_BOOL_F, SCM_BOOL_T)))==SCM_BOOL_F) return SCM_BOOL_F

#define VALIDATE_PRESS_ONLY(win,subr)  if(((win=ensure_valid(win,1,subr,SCM_BOOL_F, SCM_BOOL_F)))==SCM_BOOL_F) return SCM_BOOL_F

typedef struct {
  ScwmWindow *sw;
  int valid;
} scwm_window;

EXTERN long scm_tc16_scwm_window;
EXTERN_SET(SCM window_context,SCM_UNDEFINED);

#define WINDOWP(X) ((X) && (SCM_CAR(X) == (SCM)scm_tc16_scwm_window))
#define WINDOW(X)  ((scwm_window *)SCM_CDR(X))
#define SCWMWINDOW(X) (((scwm_window *)SCM_CDR(X))->sw)
#define VALIDWINP(X) (((scwm_window *)SCM_CDR(X))->valid)


#define set_window_context(X) window_context=X;
#define unset_window_context() window_context=SCM_UNDEFINED;

size_t free_window(SCM obj);
SCM mark_window(SCM obj);
int print_window(SCM obj, SCM port, scm_print_state * pstate);

void init_window();

ScwmWindow *SwFromWindow(Display *dpy, Window w);
ScwmWindow *SwFromPointerLocation(Display *dpy);
ScwmWindow *SwSelectInteractively(Display *dpy);

SCM make_window(ScwmWindow * win);
void invalidate_window(SCM schwin);
SCM window_p(SCM obj);

SCM get_window(SCM kill_p, SCM select_p, SCM release_p);
SCM current_window_with_focus();
SCM current_window_with_pointer();
SCM select_window_interactively();

SCM select_window(SCM kill_p, SCM release_p);
SCM delete_window(SCM win);
SCM destroy_window(SCM win);
SCM window_deletable_p(SCM win);
SCM focus(SCM win);
SCM warp_to_window(SCM win);
SCM raise_window(SCM win);
SCM lower_window(SCM win);
SCM raised_p(SCM win);
SCM deiconify(SCM win);
SCM iconify(SCM win);
SCM iconified_p(SCM win);
SCM stick(SCM win);
SCM unstick(SCM win);
SCM sticky_p(SCM win);

SCM window_shade(SCM win, SCM animated_p);
SCM un_window_shade(SCM win, SCM animated_p);
SCM window_shaded_p(SCM win);


SCM set_animation_x(SCM vector);
SCM move_to(SCM x, SCM y, SCM win, SCM animated, SCM move_pointer_too);
SCM interactive_move(SCM win);
SCM resize_to(SCM w, SCM h, SCM win);
SCM interactive_resize(SCM win);
SCM refresh_window(SCM win);

SCM move_window_to_desk(SCM which, SCM win);
SCM window_position(SCM win);
SCM window_size(SCM win);
SCM window_id(SCM win);
SCM window_desk(SCM win);
SCM window_title(SCM win);

SCM window_class(SCM win);
SCM window_resource(SCM win);

SCM list_all_windows();

SCM keep_on_top(SCM win);
SCM un_keep_on_top(SCM win);
SCM kept_on_top_p(SCM win);

SCM show_titlebar(SCM win);
SCM hide_titlebar(SCM win);
SCM titlebar_shown_p(SCM win);

SCM normal_border(SCM win);
SCM plain_border(SCM win);
SCM border_normal_p(SCM win);

SCM set_border_width_x(SCM width, SCM win);
SCM stick_icon(SCM win);
SCM unstick_icon(SCM win);
SCM icon_sticky_p(SCM win);

SCM set_random_placement_x(SCM val, SCM win);
SCM set_smart_placement_x(SCM val, SCM win);
SCM set_window_button_x(SCM butt, SCM val, SCM win);

SCM set_icon_box_x(SCM sx, SCM sy, SCM sw, SCM sh, SCM win);
SCM set_window_focus_x(SCM sym, SCM win);
SCM set_window_colors_x(SCM fg, SCM bg, SCM win);
SCM set_icon_title_x(SCM title, SCM win);

SCM set_mwm_buttons_x(SCM val, SCM win);
SCM set_mwm_border_x(SCM val, SCM win);
SCM set_icon_x(SCM val, SCM win);
SCM set_mini_icon_x(SCM val, SCM win);
SCM set_hint_override_x(SCM val, SCM w);
SCM set_decorate_transient_x(SCM val, SCM w);
SCM set_mwm_decor_hint_x(SCM val, SCM w);
SCM set_mwm_func_hint_x(SCM val, SCM w);
SCM set_PPosition_hint_x(SCM val, SCM w);
SCM set_OL_decor_hint_x(SCM val, SCM w);
SCM set_start_on_desk_x(SCM desk, SCM w);
SCM set_skip_mapping_x(SCM val, SCM w);
SCM set_lenience_x(SCM val, SCM win);

#endif /* WINDOW_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
