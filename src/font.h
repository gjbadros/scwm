

#ifndef FONT_H
#define FONT_H

#include <libguile.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

typedef struct {
  XFontStruct *xfs;
  char *name;
} scwm_font;

extern long scm_tc16_scwm_font;

#define FONTP(X) (SCM_CAR(X) == (SCM)scm_tc16_scwm_font)
#define FONT(X)  ((scwm_font *)SCM_CDR(X))
#define XFONT(X) (((scwm_font *)SCM_CDR(X))->xfs)
#define FONTNAME(X) (((scwm_font *)SCM_CDR(X))->name)

size_t free_font(SCM obj);
int print_font(SCM obj, SCM port, scm_print_state * pstate);
SCM load_font(SCM fname);
SCM set_icon_font(SCM font);
SCM set_window_font(SCM font);
SCM set_menu_font(SCM font);
SCM font_p(SCM obj);

#endif /* FONT_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
