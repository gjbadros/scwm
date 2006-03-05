/* $Id$ 
 * message-window.h
 * Copyright (C) 1997, 1998, 1999, 2000 Jeffrey Nichols, Greg J. Badros, and Maciej Stachowiak
 */


#ifndef MSGWINDOW_H
#define MSGWINDOW_H

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>

#undef EXTERN
#undef EXTERN_SET
#ifdef MESSAGE_WINDOW_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

void init_message_window();

EXTERN long scm_tc16_scwm_msgwindow;

#define MSGWINDOW_P(X) (SCM_SMOB_PREDICATE(scm_tc16_scwm_msgwindow, X))
#define MSGWINDOW(X)   ((scwm_msgwindow *)SCM_SMOB_DATA(X))
#define MSGWINDOW_IMAGE(X)     (MSGWINDOW(X)->bg_image)


/* JWN: Variable Struct   -- stores whatever variables are 
   necessary for a msgwindow */

typedef struct {
  SCM font;    /* font object */
  SCM fg_color; /* colors for message window (color object) */
  SCM bg_color;
  SCM shadow_color; /* relief colors for message window (color object) */
  SCM highlight_color;
  SCM bg_image; /* bg image, or SCM_BOOL_F if none */
  char *sz;     /* the current message */
  Bool fRelief;  /* draw with relief? */
  int x, y;        /* current position */
  double x_align, y_align;  /* alignment (e.g., -.5,-.5 is centered*/
  int width, height; /* -1 means auto-sized in that direction */
  Window win;   /* X Window obj */
} scwm_msgwindow;

/* JWN: msgwindow interface functions */

/* Constructor */

SCM make_message_window( SCM msg );

/* Some stuff for determining whether the object is a msgwindow or not */

#define MSGWINDOW_OR_SYMBOL_P(x) (MSGWINDOW_P(x) || scm_is_symbol(x))

#define DYNAMIC_MSGWINDOW_P(X) (scm_is_symbol(X)? \
			        MSGWINDOW_P(scm_variable_ref(scm_lookup(X))) : \
			        MSGWINDOW_P(X))

/* Context for expose event handling */

EXTERN XContext ExposeWindowProcContext;
EXTERN XContext MsgWindowContext;

/* Object Variables setters */

SCM message_window_show( SCM mwn );
SCM message_window_hide( SCM mwn );
SCM message_window_set_message_x( SCM mwn, SCM str );
SCM message_window_set_position_x( SCM mwn, SCM x, SCM y, SCM x_align, SCM y_align);
SCM message_window_set_font_x( SCM mwn, SCM fnt );
SCM message_window_set_colors_x( SCM mwn, SCM fg_color, SCM bg_color );
SCM message_window_set_relief_x( SCM mwn, SCM relief );

/* Object Variable Getters */

SCM message_window_visible_p( SCM mwn );
SCM message_window_message( SCM mwn );
SCM message_window_position( SCM mwn );
SCM message_window_font( SCM mwn );
SCM message_window_colors( SCM mwn );
SCM message_window_relief_p( SCM mwn );


#define VALIDATE_ARG_MSGWINDOW(pos,arg) \
  do { if (!MSGWINDOW_P(arg)) scm_wrong_type_arg(FUNC_NAME,pos,arg); } while (0)

#define VALIDATE_ARG_MSGWINDOW_COPY(pos,arg,cvar) \
  do { if (!MSGWINDOW_P(arg)) scm_wrong_type_arg(FUNC_NAME,pos,arg); \
       else cvar = MSGWINDOW(arg); } while (0)

 
#endif /* MSGWINDOW_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */


