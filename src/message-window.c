/* $Id$
 * Copyright (C) 1997-1999 Jeffrey Nichols, Greg J. Badros, and Maciej Stachowiak
 * 
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>
#include <libguile.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>
#include <assert.h>

#define MESSAGE_WINDOW_IMPLEMENTATION
#include "message-window.h"

#include "scwm.h"
#include "screen.h"
#include "errors.h"
#include "borders.h"
#include "color.h"
#include "decor.h"
#include "colors.h"
#include "guile-compat.h"
#include "xmisc.h"
#include "font.h"
#include "events.h"

SCM
mark_msgwindow(SCM obj)
{
  scwm_msgwindow* msg = MSGWINDOW( obj );

  GC_MARK_SCM_IF_SET(msg->font);
  GC_MARK_SCM_IF_SET(msg->fg_color);
  GC_MARK_SCM_IF_SET(msg->bg_color);
  GC_MARK_SCM_IF_SET(msg->shadow_color);
  GC_MARK_SCM_IF_SET(msg->highlight_color);
  GC_MARK_SCM_IF_SET(msg->message);

  return SCM_BOOL_F;
}

SCM message_window_hide_x(SCM mwn);

size_t 
free_msgwindow(SCM obj)
{
  scwm_msgwindow *msg = MSGWINDOW(obj);
  message_window_hide_x(obj);

  FREE(msg);

  return 0;
}

int 
print_msgwindow(SCM obj, SCM port, scm_print_state *ARG_IGNORE(pstate))
{
  scm_puts("#<msgwindow ", port);
  scm_write(MSGWINDOW_MESSAGE(obj), port);
  scm_putc('>', port);
  return 1;
}

SCWM_PROC(message_window_p, "message-window?", 1, 0, 0, 
           (SCM mwn))
     /** Returns #t if MWN is a msgwindow object, otherwise #f. */
#define FUNC_NAME s_message_window_p
{
  return SCM_BOOL_FromBool(MSGWINDOW_P(mwn));
}
#undef FUNC_NAME


/* Create the X window object for the message window */

const double message_highlight_factor = 1.2;  /* defined like this from move.c */
const double message_shadow_factor = 0.5;


/* DrawWindow

   Draws the contents of a message window.  Takes a pointer to
the scwm_msgwindow to be drawn.
 */
void
DrawWindow( scwm_msgwindow* msg ) {

  GC gcMsg = Scr.ScratchGC2;
  GC gcHilite = Scr.ScratchGC2;
  GC gcShadow = Scr.ScratchGC3;
  SCM scmFgRelief, scmBgRelief;
  int cch;
  char *const sz = gh_scm2newstr(msg->message, &cch);
  int textwidth = ComputeXTextWidth(XFONT(msg->font),sz, -1);
  int winwidth = textwidth + SIZE_HINDENT*2;
  int textheight = FONTHEIGHT(msg->font);
  int winheight = textheight + SIZE_VINDENT*2;

  scmFgRelief = msg->highlight_color; 
  scmBgRelief = msg->shadow_color; 

  if (msg->fRelief) {
    if (scmFgRelief != SCM_BOOL_F)
      SetGCFg(gcHilite,XCOLOR(scmFgRelief));
    else
      SetGCFg(gcHilite,WhitePixel(dpy,Scr.screen));
    
    if (scmBgRelief != SCM_BOOL_F)
      SetGCFg(gcShadow,XCOLOR(scmBgRelief));
    else
      SetGCFg(gcShadow,BlackPixel(dpy,Scr.screen));

    XClearWindow(dpy, msg->win);
    if (Scr.d_depth >= 2) {
      RelieveRectangle(msg->win, 0, 0, winwidth, winheight,
                       gcHilite, gcShadow);
    }
  } else {
    XClearArea(dpy, msg->win, 0, 0, 
               textwidth, textheight, False);
  }

  NewFontAndColor(gcMsg,XFONTID(msg->font),
                  XCOLOR(msg->fg_color), XCOLOR(msg->bg_color)); 
  
#ifdef I18N
  XmbDrawString(dpy, msg->win, XFONT(msg->font),
#else
  XDrawString(dpy, msg->win, 
#endif
	  gcMsg, SIZE_HINDENT, FONTY(msg->font) + SIZE_VINDENT,
	  sz, cch);
#if 0
  );  /* for paren matching of above to keep (x)emacs happy */
#endif                
  FREE(sz);
}

/* ResizeMessageWindow

   Resizes the scwm_msgwindow pointed at by msg and calls
DrawWindow to redraw it.
 */
void
ResizeMessageWindow( scwm_msgwindow* msg ) {

  char *const sz = gh_scm2newstr(msg->message,NULL);
  int textwidth = ComputeXTextWidth(XFONT(msg->font),sz, -1);
  int winwidth = textwidth + SIZE_HINDENT*2;
  int textheight = FONTHEIGHT(msg->font);
  int winheight = textheight + SIZE_VINDENT*2;
  int win_x, win_y;

  win_x = msg->x + (msg->x_align * winwidth);
  win_y = msg->y + (msg->y_align * winheight);

  XMoveResizeWindow(dpy, msg->win, win_x, win_y, winwidth, winheight);

  DrawWindow( msg );

  FREE(sz);
}

/* OnExposeEvent

   Called when an expose event is issued by the X server to
the message window.  Redraws the window with any appropriate
size changes.
*/
void
OnExposeEvent( Window w ) {
  scwm_msgwindow* msg;
  if ( XFindContext(dpy, w, MsgWindowContext, (XPointer*)&msg) == 0 && msg != NULL )
    ResizeMessageWindow( msg );
}


/* CreateMessageWindow

   Creates the X Window object that will be associated with
a message window.
 */
Window
CreateMessageWindow( scwm_msgwindow* msg ) {

  const int width = 50; /* just some starting place-- we'll figure it out later */
  XSetWindowAttributes attributes;
  unsigned long valuemask = (CWBorderPixel | CWBackPixel | CWBitGravity | CWEventMask);

  attributes.border_pixel = XCOLOR(msg->fg_color);
  attributes.background_pixel = XCOLOR(msg->bg_color);
  attributes.bit_gravity = NorthWestGravity;
  attributes.event_mask = ExposureMask;

  return msg->win = XCreateWindow(dpy, Scr.Root,
                                  0, 0, width, 
                                  (FONTHEIGHT(msg->font) + SIZE_VINDENT * 2),
                                  0, 0, CopyFromParent, (Visual *) CopyFromParent,
                                  valuemask, &attributes);
}


/* MapMessageWindow

   Map a message window to the screen.
*/
void
MapMessageWindow(scwm_msgwindow* msg)
{
  int w, h;
  int x, y;
  
  if (!FXGetWindowSize(msg->win,&w,&h))
    assert(False);

  x = msg->x + (msg->x_align * w);
  y = msg->y + (msg->y_align * h);

  XMoveWindow(dpy, msg->win, x, y);
  XMapRaised(dpy, msg->win);
}


/* UnmapMessageWindow

   Unmap a message window.
*/
void
UnmapMessageWindow(scwm_msgwindow* msg)
{
  XUnmapWindow(dpy, msg->win);
}

/*  GJB:FIXME:: below should get defaults from a prototype object, not
    the ScreenInfo struct */
SCWM_PROC (make_message_window, "make-message-window", 1, 0, 0,
           (SCM message))
     /** Returns a newly created message window object with string MESSAGE.
MESSAGE is the initial string for the message window. 
Uses defaults from the ScreenInfo struct for the other values. */
#define FUNC_NAME s_make_message_window
{
  SCM answer;
  scwm_msgwindow* msg;

  if (!gh_string_p(message)) {
    SCWM_WRONG_TYPE_ARG(1, message);
  }

  msg = NEW(scwm_msgwindow);

  if (SCM_UNDEFINED == message)
    message = SCM_BOOL_F;

  msg->message = message;

  /* JWN:FIXME:: For now, I'll assume the ScreenInfo struct contains good defaults
     for the message window params.  Is there another better place? */

  msg->x = Scr.msg_window_x;
  msg->y = Scr.msg_window_y;
  msg->x_align = Scr.msg_window_x_align;
  msg->y_align = Scr.msg_window_y_align;
  msg->fg_color = Scr.msg_window_fg;
  msg->bg_color = Scr.msg_window_bg;
  msg->shadow_color = SCM_BOOL_F;
  msg->highlight_color = SCM_BOOL_F;

  msg->font = Scr.msg_window_font;
  msg->fRelief = TRUE;

  CreateMessageWindow(msg);
  XSaveContext(dpy, msg->win, ExposeWindowProcContext, (caddr_t) OnExposeEvent);
  XSaveContext(dpy, msg->win, MsgWindowContext, (caddr_t) msg);

  gh_defer_ints();
  SCWM_NEWCELL_SMOB(answer, scm_tc16_scwm_msgwindow, msg);
  gh_allow_ints();

  return answer;
}
#undef FUNC_NAME

SCWM_PROC(id_to_message_window, "id->message-window", 1, 0, 0,
          (SCM winid))
     /** Return the message-window of an X/11 window id (a long integer).
Returns #f if WINID does not correspond to a message-window.
You can use the xwininfo program to get the window id of an arbitrary
window on your X/11 display. */
#define FUNC_NAME s_id_to_message_window
{
  Window w;
  scwm_msgwindow* msg = NULL;
  SCM answer = SCM_BOOL_F;

  if (!gh_number_p(winid)) {
    SCWM_WRONG_TYPE_ARG(1,winid);
  }
  w = (Window) gh_scm2ulong(winid);
  
  if ( XFindContext(dpy, w, MsgWindowContext, (XPointer*)&msg) == 0 && msg != NULL ) {
    SCWM_NEWCELL_SMOB(answer, scm_tc16_scwm_msgwindow, msg);
  }
  return answer;
}
#undef FUNC_NAME


SCWM_PROC(message_window_set_message_x, "message-window-set-message!", 2, 0, 0,
          (SCM mwn, SCM message))
    /** Changes the message displayed by the message window MWN.
The message will be MESSAGE.*/
#define FUNC_NAME s_message_window_set_message_x
{
  scwm_msgwindow* msg = MSGWINDOW(mwn);

  if (!MSGWINDOW_P(mwn) ) {
    SCWM_WRONG_TYPE_ARG(1, mwn);
  }

  if (!gh_string_p(message)) {
    SCWM_WRONG_TYPE_ARG(2, message);
  }

  msg->message = message;

  ResizeMessageWindow( msg );

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(message_window_set_font_x, "message-window-set-font!", 2, 0, 0,
          (SCM mwn, SCM fnt))
    /** Set the font to be used for the message window MWN.
The font will be FNT.*/
#define FUNC_NAME s_message_window_set_font_x
{
  scwm_msgwindow* msg = MSGWINDOW(mwn);

  if (!MSGWINDOW_P(mwn) ) {
    SCWM_WRONG_TYPE_ARG(1, mwn);
  }

  if (gh_string_p(fnt)) {
    fnt = make_font(fnt);
  }

  if (!FONT_P(fnt)) {
    SCWM_WRONG_TYPE_ARG(2, fnt);
  }
  msg->font=fnt;

  ResizeMessageWindow( msg );

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(message_window_set_colors_x, "message-window-set-colors!", 3, 0, 0,
          (SCM mwn, SCM fg_color, SCM bg_color))
    /** Set the fore- and background colors to be used for the message window MWN.
The foreground color will be FG-COLOR and the background color will be BG-COLOR.*/
#define FUNC_NAME s_message_window_set_colors_x
{
  scwm_msgwindow* msg = MSGWINDOW(mwn);

  if (!MSGWINDOW_P(mwn) ) {
    SCWM_WRONG_TYPE_ARG(1, mwn);
  }

  if ( fg_color != SCM_BOOL_F ) {
    VALIDATE_ARG_COLOR(2,fg_color);
    msg->fg_color = fg_color;
  }

  if ( bg_color != SCM_BOOL_F ) {
    VALIDATE_ARG_COLOR(3,bg_color);
    msg->bg_color = bg_color;
    msg->shadow_color = adjust_brightness(msg->bg_color, 
					  message_shadow_factor);
    msg->highlight_color = adjust_brightness(msg->bg_color, 
                                             message_highlight_factor);
  }


  XSetWindowBorder(dpy,msg->win,XCOLOR(msg->fg_color));
  XSetWindowBackground(dpy,msg->win,XCOLOR(msg->bg_color));

  DrawWindow(msg);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* GJB:FIXME:: it'd be nice to add an option to have the message window follow
   the pointer around! --07/25/98 gjb
   This might best be done in the message-window code, as a special option
   to a message-window's position (it'd have to track pointer movement events
 */
SCWM_PROC(message_window_set_position_x, "message-window-set-position!", 3, 2, 0,
          (SCM mwn, SCM x, SCM y, SCM x_align, SCM y_align))
    /** Set the position to be used for the message window MWN.
X and Y specify the position of the control point of the window,
while X-ALIGN and Y-ALIGN specify a fraction of the width and
height of the window to offset the window for alignment.
X-ALIGN and Y-ALIGN should each be in the range [0,-1].*/
#define FUNC_NAME s_message_window_set_position_x
{
  scwm_msgwindow* msg = MSGWINDOW(mwn);

  if (!MSGWINDOW_P(mwn) ) {
    SCWM_WRONG_TYPE_ARG(1, mwn);
  }

  SCM_REDEFER_INTS;

  if ( !UNSET_SCM(x) ) {
    if (!gh_number_p(x)) {
      gh_allow_ints();
      SCWM_WRONG_TYPE_ARG(2, x);
    }
    msg->x = gh_scm2long(x);
  }
  if ( !UNSET_SCM(y) ) {
    if (!gh_number_p(y)) {
      gh_allow_ints();
      SCWM_WRONG_TYPE_ARG(3, y);
    }
    msg->y = gh_scm2long(y);
  }
  if ( !UNSET_SCM(x_align) ) {
    if (!gh_number_p(x_align)) {
      gh_allow_ints();
      SCWM_WRONG_TYPE_ARG(4, x_align);
    }
    msg->x_align = gh_scm2double(x_align);
  }
  if ( !UNSET_SCM(y_align) ) {
    if (!gh_number_p(y)) {
      gh_allow_ints();
      SCWM_WRONG_TYPE_ARG(5, y_align);
    }
    msg->y_align = gh_scm2double(y_align);
  }

  SCM_REALLOW_INTS;

  ResizeMessageWindow(msg);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC (message_window_set_relief_x, "message-window-set-relief!", 2, 0, 0,
           (SCM mwn, SCM rlf))
  /** Sets the relief for the window MWN. Relief will be RLF. */
#define FUNC_NAME s_message_window_set_relief_x
{
  if (!MSGWINDOW_P(mwn) ) {
    SCWM_WRONG_TYPE_ARG(1, mwn);
  }

  if ( gh_boolean_p(rlf) )
    MSGWINDOW(mwn)->fRelief = rlf;

  DrawWindow(MSGWINDOW(mwn));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC (message_window_show_x, "message-window-show!", 1, 0, 0,
           (SCM mwn))
     /** Displays the message window MWN on the screen. 
Be sure to keep the displayed message window somewhere
do that you can call `message-window-hide!' (otherwise
the window will not ever disappear). */
#define FUNC_NAME s_message_window_show_x
{
  if (! MSGWINDOW_P(mwn) ) {
    SCWM_WRONG_TYPE_ARG(1,mwn);
  }

  /* if it's already visible, we need do nothing;
     in particular, scm_protect_object may track
     protections, and we want a single hide to
     eliminate the affects of all show!-s */
  if (FXIsWindowMapped(dpy,MSGWINDOW(mwn)->win))
    return SCM_UNSPECIFIED;

  /* do not let GC collect the object when it is visible */
  scm_protect_object(mwn);

  MapMessageWindow(MSGWINDOW(mwn));
  ResizeMessageWindow( MSGWINDOW(mwn) );

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC (message_window_hide_x, "message-window-hide!", 1, 0, 0,
           (SCM mwn))
     /** Hide the message window MWN.
See also `message-window-show'. */
#define FUNC_NAME s_message_window_hide_x
{
  if (!MSGWINDOW_P(mwn) ) {
    SCWM_WRONG_TYPE_ARG(1, mwn);
  }

  UnmapMessageWindow(MSGWINDOW(mwn));
  XFlush(dpy);

  scm_unprotect_object(mwn);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC (message_window_visible_p, "message-window-visible?", 1, 0, 0,
           (SCM mwn))
     /** Return #t if the message window MWN is visible, #f otherwise.
See also `message-window-show', `message-window-hide'. */
#define FUNC_NAME s_message_window_visible_p
{
  if (!MSGWINDOW_P(mwn) ) {
    SCWM_WRONG_TYPE_ARG(1, mwn);
  }

  return SCM_BOOL_FromBool(FXIsWindowMapped(dpy,MSGWINDOW(mwn)->win));
}
#undef FUNC_NAME


SCWM_PROC (message_window_message, "message-window-message", 1, 0, 0,
           (SCM mwn))
     /** Returns the message that message window MWN displays. */
#define FUNC_NAME s_message_window_message
{
  if (!MSGWINDOW_P(mwn) ) {
    SCWM_WRONG_TYPE_ARG(1, mwn);
  }

  return MSGWINDOW_MESSAGE(mwn);
}
#undef FUNC_NAME


SCWM_PROC (message_window_position, "message-window-position", 1, 0, 0,
           (SCM mwn))
     /** Returns the position that message window MWN is/will be displayed at. This is 
returned as a four element list of (x,y,x_align,y_align) */
#define FUNC_NAME s_message_window_position
{
  scwm_msgwindow* msg = MSGWINDOW(mwn);

  if (!MSGWINDOW_P(mwn) ) {
    SCWM_WRONG_TYPE_ARG(1, mwn);
  }

  return gh_list( msg->x, msg->y, msg->x_align, msg->y_align, SCM_UNDEFINED );
}
#undef FUNC_NAME


SCWM_PROC (message_window_font, "message-window-font", 1, 0, 0,
           (SCM mwn))
     /** Returns the font that the message window MWN uses for displaying text. */
#define FUNC_NAME s_message_window_font
{
  if (!MSGWINDOW_P(mwn) ) {
    SCWM_WRONG_TYPE_ARG(1, mwn);
  }

  return MSGWINDOW(mwn)->font;
}
#undef FUNC_NAME


SCWM_PROC (message_window_colors, "message-window-colors", 1, 0, 0,
           (SCM mwn))
     /** Returns the colors that the message window MWN is displayed with.  
These are returned in a list of the form (fg_color,bg_color). */
#define FUNC_NAME s_message_window_colors
{
  scwm_msgwindow* msg = NULL;

  if (!MSGWINDOW_P(mwn) ) {
    SCWM_WRONG_TYPE_ARG(1, mwn);
  }

  msg = MSGWINDOW(mwn);

  return gh_list( msg->fg_color, msg->bg_color, SCM_UNDEFINED );
}
#undef FUNC_NAME

SCWM_PROC (message_window_relief_p, "message-window-relief?", 1, 0, 0,
           (SCM mwn))
     /** Returns the relief setting for the message window MWN. */
#define FUNC_NAME s_message_window_relief_p
{
  if (!MSGWINDOW_P(mwn) ) {
    SCWM_WRONG_TYPE_ARG(1, mwn);
  }

  return SCM_BOOL_FromBool(MSGWINDOW(mwn)->fRelief);
}
#undef FUNC_NAME


MAKE_SMOBFUNS(msgwindow);

void 
init_message_window()
{
  REGISTER_SCWMSMOBFUNS(msgwindow);

#ifndef SCM_MAGIC_SNARFER
#include "message-window.x"
#endif
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */

