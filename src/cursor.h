/* $Id$
 * Copyright (C) 1999, 2000 Toby Sargeant and Greg J. Badros
 */

#ifndef CURSOR_H__
#define CURSOR_H__

#include <X11/cursorfont.h>
#include <X11/Xlib.h>
#include "scwm.h"

#undef EXTERN
#undef EXTERN_SET
#ifdef CURSOR_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif


#define CURSOR(X)  ((scwm_cursor *)(gh_cdr(X)))
#define IS_CURSOR(X) (SCM_NIMP(X) && gh_car(X) == (SCM)scm_tc16_scwm_cursor)
#define XCURSOR(X)  (IS_CURSOR((X))?CURSOR(X)->cursor:None)

EXTERN SCM *pscm_cursor_set_focus;
#define XCURSOR_SET_FOCUS XCURSOR(*pscm_cursor_set_focus)

EXTERN SCM *pscm_cursor_move;
#define XCURSOR_MOVE XCURSOR(*pscm_cursor_move)

EXTERN SCM *pscm_cursor_icon;
#define XCURSOR_ICON XCURSOR(*pscm_cursor_icon)

EXTERN SCM *pscm_cursor_kill;
#define XCURSOR_KILL XCURSOR(*pscm_cursor_kill)

EXTERN SCM *pscm_cursor_select;
#define XCURSOR_SELECT XCURSOR(*pscm_cursor_select)

EXTERN SCM *pscm_cursor_menu;
#define XCURSOR_MENU XCURSOR(*pscm_cursor_menu)


EXTERN long scm_tc16_scwm_cursor;

typedef struct {
  Cursor cursor;
  PackedBool(is_x_cursor);
  const char *szName; /* pointer to static if is_x_cursor, 
                         must be FREEd otherwise */
} scwm_cursor;


Cursor XCursorByNumber(int);
SCM get_scm_cursor_by_number(int);
SCM get_scm_cursor_by_name(const char *);

void init_cursor();
void CreateScmGlobalCursors();

#define VALIDATE_ARG_CURSOR(pos,arg) \
  do { if (!IS_CURSOR(arg)) scm_wrong_type_arg(FUNC_NAME,pos,arg); } while (0)

#define VALIDATE_ARG_CURSOR_USE_F(pos,arg) \
  do { if (UNSET_SCM(arg)) arg = SCM_BOOL_F; \
       else if (!IS_CURSOR(arg)) scm_wrong_type_arg(FUNC_NAME,pos,arg); } while (0)

#define VALIDATE_ARG_CURSOR_COPY_USE_KILLORCIRCLE(pos,arg,cvar) \
  do { if (UNSET_SCM(arg)) { arg = SCM_BOOL_F; cvar = XCURSOR_SELECT; } \
       else if (SCM_BOOL_T == arg) { cvar = XCURSOR_KILL; } \
       else if (!IS_CURSOR(arg)) scm_wrong_type_arg(FUNC_NAME,pos,arg); \
       else cvar = XCURSOR(arg); } while (0)


#endif


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */
