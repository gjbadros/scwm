#ifndef WINDOW_H
#define WINDOW_H

#include <libguile.h>
#include "scwm.h"


typedef struct {
  ScwmWindow *sw;
  int valid;
} scwm_window;

extern long scm_tc16_scwm_window;

#define WINDOWP(X) (SCM_CAR(X) == (SCM)scm_tc16_scwm_window)
#define WINDOW(X)  ((scwm_window *)SCM_CDR(X))
#define SCWMWINDOW(X) (((scwm_window *)SCM_CDR(X))->sw)
#define VALIDWINP(X) (((scwm_window *)SCM_CDR(X))->valid)

extern SCM window_context;

#define set_window_context(X) window_context=X;
#define unset_window_context() window_context=SCM_UNDEFINED;

size_t free_window (SCM obj);
int print_window (SCM obj, SCM port, scm_print_state *pstate);

SCM make_window(ScwmWindow *win);
void invalidate_window(SCM schwin);
SCM window_p(SCM obj);

SCM get_window(void);
SCM select_window(void); 
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

#ifdef WINDOWSHADE
SCM window_shade(SCM win);
SCM un_window_shade(SCM win);
SCM window_shaded_p(SCM win);
#endif /* WINDOWSHADE */

SCM move_to(SCM x, SCM y, SCM win);
SCM interactive_move(SCM win);
SCM resize_to(SCM w, SCM h, SCM win);
SCM interactive_resize(SCM win);
SCM refresh_window(SCM win);

SCM move_window_to_desk(SCM which, SCM win);
SCM get_window_position(SCM win);
SCM get_window_size(SCM win);
SCM get_window_id(SCM win);
SCM get_window_desk(SCM win);
SCM get_window_title(SCM win);
SCM get_window_list();

#endif /* WINDOW_H */



