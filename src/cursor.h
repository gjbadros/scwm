/* $Id$
 * (C) 1999 Toby Sargeant and Greg J. Badros
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

EXTERN long scm_tc16_scwm_cursor;

typedef struct {
  Cursor cursor;
  PackedBool(is_x_cursor);
} scwm_cursor;


Cursor XCursorByNumber(int);
SCM get_scm_cursor_by_number(int);
SCM get_scm_cursor_by_name(const char *);

void init_cursor();

#endif
