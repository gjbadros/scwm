/* $Id$
 * Copyright (C) 1997, 1998, 1999, 2000 Jeffrey Nichols, Greg J. Badros, and Maciej Stachowiak
 *
 * Background image extension added --07/01/99 gjb
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#define MESSAGE_WINDOW_IMPLEMENTATION
#include "message-window.h"

#include <guile/gh.h>
#include <libguile.h>
#include <X11/X.h>
#ifdef HAVE_SHAPE
#include <X11/extensions/shape.h>
#endif
#include <assert.h>

#include "scwm.h"
#include "screen.h"
#include "errors.h"
#include "borders.h"
#include "color.h"
#include "decor.h"
#include "guile-compat.h"
#include "xmisc.h"
#include "font.h"
#include "events.h"

static SCM
mark_msgwindow(SCM obj)
{
  scwm_msgwindow* msg = MSGWINDOW( obj );

  GC_MARK_SCM_IF_SET(msg->font);
  GC_MARK_SCM_IF_SET(msg->fg_color);
  GC_MARK_SCM_IF_SET(msg->bg_color);
  GC_MARK_SCM_IF_SET(msg->shadow_color);
  GC_MARK_SCM_IF_SET(msg->highlight_color);
  GC_MARK_SCM_IF_SET(msg->bg_image);

  return SCM_BOOL_F;
}

/* UnmapMessageWindow
   Unmap a message window.
*/
static void
UnmapMessageWindow(scwm_msgwindow* msg)
{
  XUnmapWindow(dpy, msg->win);
}


static size_t 
free_msgwindow(SCM obj)
{
  scwm_msgwindow *msg = MSGWINDOW(obj);
#ifndef NDEBUG
  Window w = msg->win;
  if (FXIsWindowMapped(dpy,w)) {
    scwm_msg(WARN,"free_msgwindow","Message window %s is visible when freeing.",
             msg->sz);
    UnmapMessageWindow(msg);
    XFlush(dpy);
  }
#endif

  FREE(msg);

  return 0;
}

static int 
print_msgwindow(SCM obj, SCM port, scm_print_state *ARG_IGNORE(pstate))
{
  scwm_msgwindow *msg = MSGWINDOW(obj);
  scm_puts("#<msgwindow ", port);
  scm_puts(msg->sz, port);
  if (!UNSET_SCM(MSGWINDOW_IMAGE(obj))) {
    scm_puts("; ", port);
    scm_write(MSGWINDOW_IMAGE(obj),port);
  }
  scm_putc('>', port);
  return 1;
}

SCWM_PROC(message_window_p, "message-window?", 1, 0, 0, 
          (SCM mwn),
"Returns #t if MWN is a msgwindow object, otherwise #f.")
#define FUNC_NAME s_message_window_p
{
  return SCM_BOOL_FromBool(MSGWINDOW_P(mwn));
}
#undef FUNC_NAME


const double message_highlight_factor = 1.2;  /* defined like this from move.c */
const double message_shadow_factor = 0.5;


static SCWM_INLINE int
MessageWindowWidth(scwm_msgwindow *msg)
{
  if (msg->width > 0)
    return msg->width;
  else
    return ComputeXTextWidth(XFONT(msg->font),msg->sz, -1) + SIZE_HINDENT*2;
}


static SCWM_INLINE int
MessageWindowHeight(scwm_msgwindow *msg)
{
  if (msg->height > 0)
    return msg->height;
  else
    return FONTHEIGHT(msg->font) + SIZE_VINDENT*2;
}

/* DrawWindow:
   Draws the contents of a message window.  Takes a pointer to
the scwm_msgwindow to be drawn. */
static void
DrawWindow( scwm_msgwindow* msg ) 
{

  GC gcMsg = Scr.ScratchGC2;
  GC gcHilite = Scr.ScratchGC2;
  GC gcShadow = Scr.ScratchGC3;
  SCM scmFgRelief, scmBgRelief;
  int cch = strlen(msg->sz);
  int winwidth = MessageWindowWidth(msg);
  int winheight = MessageWindowHeight(msg);

  scmFgRelief = msg->highlight_color; 
  scmBgRelief = msg->shadow_color; 

  XClearWindow(dpy, msg->win);
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
  } 

  NewFontAndColor(gcMsg,XFONTID(msg->font),
                  XCOLOR(msg->fg_color), XCOLOR(msg->bg_color)); 
  
#ifdef I18N
  XmbDrawString(dpy, msg->win, XFONT(msg->font),
#else
  XDrawString(dpy, msg->win, 
#endif
	  gcMsg, SIZE_HINDENT, FONTY(msg->font) + SIZE_VINDENT,
	  msg->sz, cch);
#if 0
  );  /* for paren matching of above to keep (x)emacs happy */
#endif                
}

/* ResizeMessageWindow

   Resizes the scwm_msgwindow pointed at by msg and calls
DrawWindow to redraw it.
 */
static void
ResizeMessageWindow( scwm_msgwindow* msg ) 
{
  int winwidth = MessageWindowWidth(msg);
  int winheight = MessageWindowHeight(msg);
  int win_x = msg->x + (msg->x_align * winwidth);
  int win_y = msg->y + (msg->y_align * winheight);

  XMoveResizeWindow(dpy, msg->win, win_x, win_y, winwidth, winheight);

  DrawWindow( msg );
}

/* OnExposeEvent

   Called when an expose event is issued by the X server to
the message window.  Redraws the window with any appropriate
size changes.
*/
static void
OnExposeEvent( Window w ) 
{
  scwm_msgwindow* msg;
  if ( XFindContext(dpy, w, MsgWindowContext, (XPointer*)&msg) == 0 && msg != NULL )
    ResizeMessageWindow( msg );
}


/* CreateMessageWindow:

   Creates the X Window object that will be associated with
a message window. */
static Window
CreateMessageWindow( scwm_msgwindow* msg ) 
{

  const int width = 50; /* just some starting place-- we'll figure it out later */
  XSetWindowAttributes attributes;
  unsigned long valuemask = (CWBorderPixel | CWBackPixel | CWBitGravity | 
                             CWEventMask);

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
static void
MapMessageWindow(scwm_msgwindow* msg)
{
  int w, h;
  int x, y;
  
  if (!FXGetWindowSize(msg->win,&w,&h)) {
    /* GJB:FIXME:: do something to avoid this */
    scwm_msg(WARN,"MapMessageWindow","Bitten by race condition in getting window size");
  } else {
    
    x = msg->x + (msg->x_align * w);
    y = msg->y + (msg->y_align * h);
    
    XMoveWindow(dpy, msg->win, x, y);
  }
  XMapRaised(dpy, msg->win);
}


/*  GJB:FIXME:: below should get defaults from a prototype object, not
    the ScreenInfo struct */
SCWM_PROC (make_message_window, "make-message-window", 1, 0, 0,
           (SCM message),
"Returns a newly created message window object with string MESSAGE.\n\
MESSAGE is the initial string for the message window. \n\
Uses defaults from the ScreenInfo struct for the other values.")
#define FUNC_NAME s_make_message_window
{
  SCM answer;
  scwm_msgwindow* msg;

  VALIDATE_ARG_STR(1,message);

  msg = NEW(scwm_msgwindow);

  if (SCM_UNDEFINED == message)
    msg->sz = NULL;
  else
    msg->sz = gh_scm2newstr(message,NULL);

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
  msg->bg_image = SCM_BOOL_F;
  msg->width = msg->height = -1;   /* auto-sized */

  msg->font = Scr.msg_window_font;
  msg->fRelief = TRUE;

  CreateMessageWindow(msg);
  XSaveContext(dpy, msg->win, ExposeWindowProcContext, (caddr_t) OnExposeEvent);
  XSaveContext(dpy, msg->win, MsgWindowContext, (caddr_t) msg);

  SCWM_NEWCELL_SMOB(answer, scm_tc16_scwm_msgwindow, msg);

  return answer;
}
#undef FUNC_NAME

SCWM_PROC(id_to_message_window, "id->message-window", 1, 0, 0,
          (SCM winid),
"Return the message-window of an X/11 window id (a long integer).\n\
Returns #f if WINID does not correspond to a message-window.\n\
You can use the xwininfo program to get the window id of an arbitrary\n\
window on your X/11 display.")
#define FUNC_NAME s_id_to_message_window
{
  Window w;
  scwm_msgwindow* msg = NULL;
  SCM answer = SCM_BOOL_F;
  unsigned long i;

  VALIDATE_ARG_INT_COPY(1,winid,i);
  w = (Window) i;
  
  if ( XFindContext(dpy, w, MsgWindowContext, (XPointer*)&msg) == 0 && msg != NULL ) {
    SCWM_NEWCELL_SMOB(answer, scm_tc16_scwm_msgwindow, msg);
  }
  return answer;
}
#undef FUNC_NAME


SCWM_PROC(message_window_set_message_x, "message-window-set-message!", 2, 0, 0,
          (SCM mwn, SCM message),
"Changes the message displayed by the message window MWN.\n\
The message will be MESSAGE")
#define FUNC_NAME s_message_window_set_message_x
{
  scwm_msgwindow *msg;

  VALIDATE_ARG_MSGWINDOW_COPY(1,mwn,msg);
  VALIDATE_ARG_STR(2,message);

  if (msg->sz) {
    gh_free(msg->sz);
  }
  msg->sz = gh_scm2newstr(message,NULL);
  ResizeMessageWindow( msg );

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(message_window_set_image_x, "message-window-set-image!", 2, 3, 0,
          (SCM mwn, SCM image, SCM fg_color, SCM bg_color, SCM shaped_p),
"Changes the background image for the message window MWN to IMAGE. \n\
FG-COLOR, BG-COLOR are the colors for the image, SHAPED? is whether it\n\
should use a shaped message window. If IMAGE is #f, then no image\n\
is used for MWN.")
#define FUNC_NAME s_message_window_set_image_x
{
  scwm_msgwindow *msg;
  scwm_image *pimg;
  Bool fShaped;

  VALIDATE_ARG_MSGWINDOW_COPY(1,mwn,msg);
  VALIDATE_ARG_IMAGE_USE_F(2,image);
  VALIDATE_ARG_COLOR_OR_SYM_USE_WHITE(3,fg_color);
  VALIDATE_ARG_COLOR_OR_SYM_USE_BLACK(4,bg_color);
  VALIDATE_ARG_BOOL_COPY_USE_F(5,shaped_p,fShaped);

  msg->bg_image = image;

  if (SCM_BOOL_F != image) {
    Pixel fg = XCOLOR(fg_color);
    Pixel bg = XCOLOR(bg_color);
    Pixmap mask;
    pimg = IMAGE(image);
    XSetWindowBackgroundPixmap(dpy, msg->win, pimg->image);
#ifdef HAVE_SHAPE
    if (fShaped && pimg->mask) {
      mask = Pixmap1DeepFromPixmap(pimg->mask,fg,bg);
      XShapeCombineMask(dpy,msg->win,ShapeBounding,0,0,mask,ShapeSet);
    }
#endif
  } else {
    XSetWindowBackgroundPixmap(dpy,msg->win, None);
  }
    
  ResizeMessageWindow( msg );

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(message_window_set_font_x, "message-window-set-font!", 2, 0, 0,
          (SCM mwn, SCM fnt),
"Set the font to be used for the message window MWN.\n\
The font will be FNT")
#define FUNC_NAME s_message_window_set_font_x
{
  scwm_msgwindow *msg;

  VALIDATE_ARG_MSGWINDOW_COPY(1,mwn,msg);

  if (gh_string_p(fnt)) {
    fnt = make_font(fnt);
  }

  VALIDATE_ARG_FONT(2,fnt);
  msg->font=fnt;

  ResizeMessageWindow( msg );

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(message_window_set_colors_x, "message-window-set-colors!", 3, 0, 0,
          (SCM mwn, SCM fg_color, SCM bg_color),
"Set the fore- and background colors to be used for the message window MWN.\n\
The foreground color will be FG-COLOR and the background color will be BG-COLOR")
#define FUNC_NAME s_message_window_set_colors_x
{
  scwm_msgwindow *msg;
  VALIDATE_ARG_MSGWINDOW_COPY(1,mwn,msg);

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
          (SCM mwn, SCM x, SCM y, SCM x_align, SCM y_align),
"Set the position to be used for the message window MWN.\n\
X and Y specify the position of the control point of the window,\n\
while X-ALIGN and Y-ALIGN specify a fraction of the width and\n\
height of the message window to offset it from the specified position.\n\
X-ALIGN and Y-ALIGN should each be in the range [-1,0].  E.g.,\n\
If X-ALIGN and Y-ALIGN are both -0.5, the window will be\n\
centered at viewport pixel position X, Y.  Any of the parameters\n\
excep MWN can be #f to mean not to change the existing value.")
#define FUNC_NAME s_message_window_set_position_x
{
  scwm_msgwindow* msg;

  VALIDATE_ARG_MSGWINDOW_COPY(1,mwn,msg);
  VALIDATE_ARG_INT_COPY_USE_DEF(2,x,msg->x,msg->x);
  VALIDATE_ARG_INT_COPY_USE_DEF(3,y,msg->y,msg->y);
  VALIDATE_ARG_DBL_COPY_USE_DEF(4,x_align,msg->x_align,msg->x_align);
  VALIDATE_ARG_DBL_COPY_USE_DEF(5,y_align,msg->y_align,msg->y_align);

  ResizeMessageWindow(msg);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(message_window_set_size_x, "message-window-set-size!", 3, 0, 0,
          (SCM mwn, SCM width, SCM height),
"Set the size of message window MWN to WIDTH pixels by HEIGHT pixels.\n\
If WIDTH or HEIGHT is #f, that direction is automatically sized\n\
based on the message content. This procedure is especially useful when a message window\n\
is used to display an image.")
#define FUNC_NAME s_message_window_set_size_x
{
  scwm_msgwindow* msg;
  int w, h;

  VALIDATE_ARG_MSGWINDOW_COPY(1,mwn,msg);
  VALIDATE_ARG_INT_COPY_USE_DEF(2,width,w,-1);
  VALIDATE_ARG_INT_COPY_USE_DEF(3,height,h,-1);

  msg->width = w;
  msg->height = h;

  ResizeMessageWindow(msg);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC (message_window_set_relief_x, "message-window-set-relief!", 2, 0, 0,
           (SCM mwn, SCM draw_relief_p),
"Sets the relief for the window MWN.\n\
Relief will be drawn if and only if DRAW-RELIEF? is #t.")
#define FUNC_NAME s_message_window_set_relief_x
{
  Bool f;
  VALIDATE_ARG_MSGWINDOW(1,mwn);
  VALIDATE_ARG_BOOL_COPY(2,draw_relief_p,f);
  MSGWINDOW(mwn)->fRelief = f;
  DrawWindow(MSGWINDOW(mwn));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC (message_window_show_x, "message-window-show!", 1, 0, 0,
           (SCM mwn),
"Displays the message window MWN on the screen. \n\
Be sure to keep the displayed message window somewhere\n\
do that you can call `message-window-hide!' (otherwise\n\
the window will not ever disappear).")
#define FUNC_NAME s_message_window_show_x
{
  VALIDATE_ARG_MSGWINDOW(1,mwn);

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
           (SCM mwn),
"Hide the message window MWN.\n\
See also `message-window-show'.")
#define FUNC_NAME s_message_window_hide_x
{
  VALIDATE_ARG_MSGWINDOW(1,mwn);
  UnmapMessageWindow(MSGWINDOW(mwn));
  XFlush(dpy);

  scm_unprotect_object(mwn);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC (message_window_visible_p, "message-window-visible?", 1, 0, 0,
           (SCM mwn),
"Return #t if the message window MWN is visible, #f otherwise.\n\
See also `message-window-show', `message-window-hide'.")
#define FUNC_NAME s_message_window_visible_p
{
  VALIDATE_ARG_MSGWINDOW(1,mwn);

  return SCM_BOOL_FromBool(FXIsWindowMapped(dpy,MSGWINDOW(mwn)->win));
}
#undef FUNC_NAME


SCWM_PROC (message_window_message, "message-window-message", 1, 0, 0,
           (SCM mwn),
"Returns the message that message window MWN displays.")
#define FUNC_NAME s_message_window_message
{
  VALIDATE_ARG_MSGWINDOW(1,mwn);
  return gh_str02scm(MSGWINDOW(mwn)->sz);
}
#undef FUNC_NAME


SCWM_PROC (message_window_image, "message-window-image", 1, 0, 0,
           (SCM mwn),
"Returns the image that message window MWN displays.")
#define FUNC_NAME s_message_window_image
{
  VALIDATE_ARG_MSGWINDOW(1,mwn);
  return MSGWINDOW_IMAGE(mwn);
}
#undef FUNC_NAME


SCWM_PROC (message_window_position, "message-window-position", 1, 0, 0,
           (SCM mwn),
"Returns the position that message window MWN is/will be displayed at. \n\
This is returned as a four element list: (x y x-align y-align).")
#define FUNC_NAME s_message_window_position
{
  scwm_msgwindow* msg;
  VALIDATE_ARG_MSGWINDOW_COPY(1,mwn,msg);

  return gh_list(gh_int2scm(msg->x), gh_int2scm(msg->y), 
		 gh_double2scm(msg->x_align), gh_double2scm(msg->y_align), 
		 SCM_UNDEFINED );
}
#undef FUNC_NAME


SCWM_PROC (message_window_size, "message-window-size", 1, 0, 0,
           (SCM mwn),
"Returns the size of message window MWN in pixels.\n\
Returns as a two element list: (width height).")
#define FUNC_NAME s_message_window_size
{
  scwm_msgwindow* msg;
  int w, h;

  VALIDATE_ARG_MSGWINDOW_COPY(1,mwn,msg);

  w = MessageWindowWidth(msg);
  h = MessageWindowHeight(msg);

  return gh_list(gh_int2scm(w), gh_int2scm(h), SCM_UNDEFINED);
}
#undef FUNC_NAME


SCWM_PROC (message_window_font, "message-window-font", 1, 0, 0,
           (SCM mwn),
"Returns the font that the message window MWN uses for displaying text.")
#define FUNC_NAME s_message_window_font
{
  VALIDATE_ARG_MSGWINDOW(1,mwn);
  return MSGWINDOW(mwn)->font;
}
#undef FUNC_NAME


SCWM_PROC (message_window_id, "message-window-id", 1, 0, 0,
           (SCM mwn),
"Returns the X11 id of message window MWN.")
#define FUNC_NAME s_message_window_id
{
  VALIDATE_ARG_MSGWINDOW(1,mwn);
  return gh_long2scm(MSGWINDOW(mwn)->win);
}
#undef FUNC_NAME


SCWM_PROC (message_window_colors, "message-window-colors", 1, 0, 0,
           (SCM mwn),
"Returns the colors that the message window MWN is displayed with.  \n\
These are returned in a list of the form (fg_color,bg_color).")
#define FUNC_NAME s_message_window_colors
{
  scwm_msgwindow* msg;
  VALIDATE_ARG_MSGWINDOW_COPY(1,mwn,msg);
  return gh_list( msg->fg_color, msg->bg_color, SCM_UNDEFINED );
}
#undef FUNC_NAME

SCWM_PROC (message_window_relief_p, "message-window-relief?", 1, 0, 0,
           (SCM mwn),
"Returns the relief setting for the message window MWN.")
#define FUNC_NAME s_message_window_relief_p
{
  VALIDATE_ARG_MSGWINDOW(1,mwn);

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
/* vim:ts=8:sw=2:sta 
 */
