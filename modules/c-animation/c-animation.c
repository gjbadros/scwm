/* $Id$
 * Copyright (C) 1998-1999, Maciej Stachowiak and Greg J. Badros
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.GPL.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/Xlib.h>

#include <guile/gh.h>
#include "guile-compat.h"

#include "scwm.h"
#include "screen.h"
#include "borders.h"
#include "decorations.h"
#include "resize.h"
#include "xmisc.h"
#include "util.h"
#include "events.h"
#include "module-interface.h"
#include "window.h"
#include "winprop.h"


/* From window.c */
extern SCM sym_shaded;


/* TODO: Animated resizes? Animated iconifies of various flavors? 
   what else could be animated? */

extern XEvent Event;

/* MS:FIXME:: Add variable and concept documentation. */

SCM_VCELL(animation_delay, "animation-delay");

float rgpctMovementDefault[32] = {
    -.01, 0, .01, .03,.08,.18,.3,.45,.60,.75,.85,.90,.94,.97,.99,1.0 
    /* must end in 1.0 */
};

int cmsDelayDefault = 10; /* milliseconds */

/* Perform the movement of the window. ppctMovement *must* have a 1.0 entry
   somewhere in ins list of floats, and movement will stop when it hits a 1.0 entry
   The positions given are viewport positions (not virtual) */
void 
AnimatedMoveWindow(Window w,int startX,int startY,int endX, int endY, /* viewport posns */
		   Bool fWarpPointerToo, int cmsDelay, float *ppctMovement )
{
  int pointerX, pointerY;
  int currentX, currentY;
  int lastX, lastY;
  int deltaX, deltaY;

  /* set our defaults */
  if (ppctMovement == NULL) ppctMovement = rgpctMovementDefault;
  if (cmsDelay < 0)         cmsDelay     = cmsDelayDefault;

  deltaX = endX - startX;
  deltaY = endY - startY;
  lastX = startX;
  lastY = startY;

  if (deltaX == 0 && deltaY == 0)
    return;

  do {
    currentX = (int) (startX + deltaX * (*ppctMovement));
    currentY = (int) (startY + deltaY * (*ppctMovement));
    XMoveWindow(dpy,w,currentX,currentY);
    if (fWarpPointerToo) {
      WXGetPointerWindowOffsets(Scr.Root,&pointerX,&pointerY);
      pointerX += currentX - lastX;
      pointerY += currentY - lastY;
      XWarpPointer(dpy,None,Scr.Root,0,0,0,0,
		   pointerX,pointerY);
    }
    XFlush(dpy);
    /* handle expose events as we're animating the window move */
    while (XCheckMaskEvent(dpy,  ExposureMask, &Event))
      {
	DispatchEvent();
      }

    ms_sleep(cmsDelay);
#ifdef FIXGJB_ALLOW_ABORTING_ANIMATED_MOVES
    /* this didn't work for me -- maybe no longer necessary since
       we warn the user when they use > .5 seconds as a between-frame delay
       time */
    if (XCheckMaskEvent(dpy, 
			ButtonPressMask|ButtonReleaseMask|
			KeyPressMask,
			&Event)) {
      /* finish the move immediately */
      XMoveWindow(dpy,w,endX,endY);
      XFlush(dpy);
      return;
    }
#endif
    lastX = currentX;
    lastY = currentY;
    }
  while (*ppctMovement != 1.0 && ppctMovement++);

}

void SendClientConfigureNotify(const ScwmWindow *psw);

/* GJB:FIXME:: can drop AnimatedMoveWindow since this is more general */
/* Perform the resizing of the window. ppctMovement *must* have a 1.0 entry
   somewhere in ins list of floats, and movement will stop when it hits a 1.0 entry
   The positions given are viewport positions (not virtual) */
void 
AnimatedResizeWindow(ScwmWindow *psw, Window w, int startW,int startH,int endW, int endH,
                     int startX, int startY, int endX, int endY,
		     Bool fWarpPointerToo, int cmsDelay, float *ppctMovement )
{
  int currentW, currentH;
  int lastW, lastH;
  int deltaW, deltaH;

  int pointerX, pointerY;
  int currentX, currentY;
  int lastX, lastY;
  int deltaX, deltaY;

  /* set our defaults */
  if (ppctMovement == NULL) ppctMovement = rgpctMovementDefault;
  if (cmsDelay < 0)         cmsDelay     = cmsDelayDefault;

  if (startW < 0 || startH < 0) {
    FXGetWindowSize(w, &currentW, &currentH);
    if (startW < 0) startW = currentW;
    if (startH < 0) startH = currentH;
  }

  deltaW = endW - startW;
  deltaH = endH - startH;
  lastW = startW;
  lastH = startH;

  deltaX = endX - startX;
  deltaY = endY - startY;
  lastX = startX;
  lastY = startY;

  if ((deltaX == 0 && deltaY == 0) && \
      (deltaW == 0 && deltaH == 0))
    return;

  do {
    currentX = (int) (startX + deltaX * (*ppctMovement));
    currentY = (int) (startY + deltaY * (*ppctMovement));

    currentW = (int) (startW + deltaW * (*ppctMovement));
    currentH = (int) (startH + deltaH * (*ppctMovement));
    /* XResizeWindow(dpy, w, currentW, currentH); */

    SET_CVALUE(psw,frame_width,currentW);
    SET_CVALUE(psw,frame_height,currentH);

    XMoveWindow(dpy,w,currentX,currentY);
    if (fWarpPointerToo) {
      WXGetPointerWindowOffsets(Scr.Root,&pointerX,&pointerY);
      pointerX += currentX - lastX;
      pointerY += currentY - lastY;
      XWarpPointer(dpy,None,Scr.Root,0,0,0,0,
		   pointerX,pointerY);
    }
    SendClientConfigureNotify(psw);
    SetupFrame(psw, currentX, currentY, currentW, currentH, 
               WAS_MOVED, WAS_RESIZED);


    XFlush(dpy);
    /* handle expose events as we're animating the window move */
    while (XCheckMaskEvent(dpy,  ExposureMask, &Event))
      {
	DispatchEvent();
      }

    ms_sleep(cmsDelay);
#ifdef FIXGJB_ALLOW_ABORTING_ANIMATED_MOVES
    /* this didn't work for me -- maybe no longer necessary since
       we warn the user when they use > .5 seconds as a between-frame delay
       time */
    if (XCheckMaskEvent(dpy, 
			ButtonPressMask|ButtonReleaseMask|
			KeyPressMask,
			&Event)) {
      /* finish the move immediately */
      XResizeWindow(dpy,w,endW,endH);
      XFlush(dpy);
      return;
    }
#endif
    lastW = currentW;
    lastH = currentH;
    lastX = currentX;
    lastY = currentY;
    }
  while (*ppctMovement != 1.0 && ppctMovement++);

}



/* AnimatedShadeWindow handles animating of window shades
   note that the first argument to this is a ScwmWindow *, since
   the frame needs to be manipulated; the last two args are like
   AnimatedMoveWindow, above --11/09/97 gjb */
/* Note that this does not allow animations to overshoot target-- it
   stops at first pctMovement >= 1.0 --11/25/97 gjb */
void 
AnimatedShadeWindow(ScwmWindow *psw, Bool fRollUp, 
		    int cmsDelay, float *ppctMovement)
{
  Window w = psw->w;
  Window wFrame = psw->frame;
  int width = FRAME_WIDTH(psw);
  int shaded_height = psw->title_height + psw->boundary_width;
  /* FIXGJB above was psw->title_height + 2 * (psw->boundary_width + psw->bw); */
  int normal_height = psw->orig_height;
  int client_height = normal_height - shaded_height;
  /* set our defaults */
  if (ppctMovement == NULL) ppctMovement = rgpctMovementDefault;
  if (cmsDelay < 0)         cmsDelay     = cmsDelayDefault;
  
  if (fRollUp) {
    XLowerWindow(dpy,w);
    do {
      XMoveWindow(dpy, w, 0, (int) (-client_height * (*ppctMovement)));
      XResizeWindow(dpy, wFrame, width, 
		    (int) (shaded_height + client_height * (1 - *ppctMovement)));
      XFlush(dpy);
      /* handle expose events as we're rolling up the window shade */
      while (XCheckMaskEvent(dpy,  ExposureMask, &Event))
	DispatchEvent();
      ms_sleep(cmsDelay);
    } while (*ppctMovement < 1.0 && ppctMovement++);
    XMoveWindow(dpy,w,0,-client_height);
    XResizeWindow(dpy,wFrame,width,shaded_height);
  } else {  /* roll down the window shade */
    do {
      XResizeWindow(dpy, wFrame, width, 
		    (int) (shaded_height + client_height * (*ppctMovement)));
      XMoveWindow(dpy, w, 0, (int) (-client_height * (1 - *ppctMovement)));
      XFlush(dpy);
      while (XCheckMaskEvent(dpy,  ExposureMask, &Event))
	DispatchEvent();
      ms_sleep(cmsDelay);
    } while (*ppctMovement < 1.0 && ppctMovement++);
    XResizeWindow(dpy,wFrame,width,shaded_height+client_height);
    XMoveWindow(dpy,w,0,0);
  }
  XFlush(dpy);
}





/* MS:FIXME:GJB: Let's think more about this - does the vector really
   need to get turned into a C array, or might it be adequate to
   convert on the fly, and just make it a variable? */

/* set animation parameters */
SCWM_PROC(set_animation_x, "set-animation!", 1,0,0,
          (SCM vector))
     /** Set the animation parameters to VECTOR. VECTOR is a vector of
floats which give the fractions of the final position that the window
should appear at. For instance, #(0.0 0.25 0.5 0.75 1.0 1.1 1.0) would
make the window appear at the initial position, 1/4 of the way, 1/2 of
the way, 3/4 of the way, overshoot the final position slightly, and
finally slide back into place. This parameter is used for both
animated window shades and animated moves. */
#define FUNC_NAME s_set_animation_x
{
  int citems;
  int i;
/*
  FIXGJB: make a scheme-variable move-animation-delay get used instead
  if (!gh_int_p(delay) && !gh_boolean_p(delay)) {
    SCWM_WRONG_TYPE_ARG(arg,delay);
  } */
  if (!gh_vector_p(vector)) {
    SCWM_WRONG_TYPE_ARG(1,vector);
  }
/*
  if (gh_int_p(delay)) {
    cmsDelayDefault = gh_scm2int(delay);
  }
  */
  citems = gh_vector_length(vector);
  for (i=0; i<citems; i++) {
    SCM val = gh_vector_ref(vector,gh_int2scm(i));    
    if (!gh_number_p(val)) {
      SCWM_WRONG_TYPE_ARG(1,vector);
    }
    /* FIXGJB: also check < 2, perhaps (don't want to
      check < 1, since we might want to overshoot and then come back) */
    rgpctMovementDefault[i] = (float) gh_scm2double(val);
  }
  /* Ensure that we end up 100% of the way to our destination */
  if (i>0 && rgpctMovementDefault[i-1] != 1.0) {
    rgpctMovementDefault[i++] = 1.0;
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(animated_window_shade, "animated-window-shade", 0, 1, 0,
          (SCM win))
     /** Cause WIN to become "window-shaded".
That is, to roll up into just a titlebar.  The window will be animated
as it rolls up, producing a pleasing visual effect. WIN defaults to
the window context in the usual way if not specified. */
#define FUNC_NAME s_animated_window_shade
{
  ScwmWindow *psw;
  Bool old;

  VALIDATE_WIN_USE_CONTEXT(win);
  psw = PSWFROMSCMWIN(win);
  old = psw->fWindowShaded;

  /* MS:FIXME:: Good idea to forbid maximization of shaded windows? */

  if (!psw->fTitle) {
    return SCM_BOOL_F;
  }

  SET_SHADED(psw);
  
  AnimatedShadeWindow(psw,True /* roll up */, -1, NULL);
  /* discard resize events */
  while (XCheckMaskEvent(dpy,  ResizeRedirectMask, &Event))
    { }
  /* We discard events so we don't propagate a resize
     event that will call SetupFrame again */
  /* Note sometimes the event we're trying to discard won't be
     generated in time for the above to discard it, so I had to hack
     the HandleConfigureNotify() routine to avoid resizing the
     frame; I left the XSync in for performance, since there's no
     reason to propagate that event if we can avoid it; perhaps
     substructure redirection is a solution here, but I don't know
     much about it --11/11/97 gjb */

  /* need to reset the client window offset so that if
     if it's un-window-shaded w/o animation, things are ok */
  XMoveWindow(dpy,psw->w,0,0);

  /*  FIXGJB: ideally, avoid this call when animated,
     but we need it to ensure that different combinations of 
     animated/unanimated shading do the right thing */
  
  SetupFrame(psw, FRAME_X_VP(psw), FRAME_Y_VP(psw), FRAME_WIDTH(psw),
	     psw->title_height + (psw->fSquashedTitlebar? 2:1) * psw->boundary_width,
	     NOT_MOVED, WAS_RESIZED);

  CoerceEnterNotifyOnCurrentWindow();
  Broadcast(M_WINDOWSHADE, 1, psw->w, 0, 0, 0, 0, 0, 0);

  signal_window_property_change(win, sym_shaded, SCM_BOOL_T,
                                SCM_BOOL_FromBool(old));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(animated_window_unshade, "animated-window-unshade", 0, 1, 0,
          (SCM win))
    /** Reverse the effect of `window-shade' on WIN.
The window will be animated as it rolls down, producing a pleasing
visual effect. WIN defaults to the window context in the usual way if
not specified. See also `window-unshade', `animated-window-shade'. */
#define FUNC_NAME s_animated_window_unshade
{
  ScwmWindow *psw;
  Bool old;

  VALIDATE_WIN_USE_CONTEXT(win);
  psw = PSWFROMSCMWIN(win);
  old = psw->fWindowShaded;

  SET_UNSHADED(psw);

  if (ShapesSupported) {
    if (psw->fSquashedTitlebar) {
      SetShapedTitlebar(psw, psw->tbar_right - psw->xboundary_width);
    }
  }

  AnimatedShadeWindow(psw,False /* !roll up */, -1, NULL);
  SetupFrame(psw, FRAME_X_VP(psw), FRAME_Y_VP(psw), 
	     psw->orig_width, psw->orig_height,
	     NOT_MOVED, WAS_RESIZED);
  
  Broadcast(M_DEWINDOWSHADE, 1, psw->w, 0, 0, 0, 0, 0, 0);

  signal_window_property_change(win, sym_shaded, SCM_BOOL_F,
                                SCM_BOOL_FromBool(old));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(animated_move_window, "animated-move-window", 2, 2, 0,
          (SCM x, SCM y, SCM win, SCM move_pointer_too_p))
     /** Move WIN to virtual coordinates X, Y with animation.  
If X or Y is #f, then do not change that coordinate during the move. 
If MOVE-POINTER-TOO? is specified and true, move the mouse pointer by
the same amount as the window, animating the motion of the pointer
along with the window. WIN defaults to the window context in the usual
way if not specified. */
#define FUNC_NAME s_animated_move_window
{
  ScwmWindow *psw;
  Window w;
  Bool fMovePointer = False;
  int startX, startY;
  int destX, destY;

  if (SCM_BOOL_F==
      convert_move_data(x,y,win,FUNC_NAME,
			&startX,&startY,&destX, &destY, &psw, &w)) {
    return SCM_BOOL_F;
  };

  VALIDATE_ARG_BOOL_COPY_USE_F(4, move_pointer_too_p, fMovePointer);

  { /* scope */
    SCM animation_ms_delay = SCM_CDR(animation_delay);
    int cmsDelay = -1;
    
    if (animation_ms_delay != SCM_BOOL_F &&
	gh_number_p(animation_ms_delay)) {
      cmsDelay = gh_scm2int(animation_ms_delay);
    }

    /* use viewport coordinates */
    AnimatedMoveWindow(w,
                       startX - WIN_VP_OFFSET_X(psw),
                       startY - WIN_VP_OFFSET_Y(psw),
		       destX - WIN_VP_OFFSET_X(psw),
		       destY - WIN_VP_OFFSET_Y(psw),
		       fMovePointer,cmsDelay,NULL);
  } /* scope */
  move_finalize_virt(w, psw, destX, destY);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/*SCWM_VALIDATE: w, h, win, x, y, move_pointer_too_p */
SCM 
animated_resize_common(SCM w, SCM h, SCM win, SCM x, SCM y, SCM move_pointer_too_p,
                       const char *func_name, Bool frame_p)
{
  ScwmWindow *psw;
  int width, height;
  int startX, startY, destX, destY;
  Bool fWarpPointerToo;

#define FUNC_NAME func_name
  VALIDATE_ARG_INT_COPY(1,w,width);
  VALIDATE_ARG_INT_COPY(2,h,height);
  VALIDATE_ARG_WIN_COPY_USE_CONTEXT(3, win,psw);
  VALIDATE_ARG_INT_OR_UNDEF(4,x);
  VALIDATE_ARG_INT_OR_UNDEF(5,y);
  VALIDATE_ARG_BOOL_COPY_USE_F(6,move_pointer_too_p, fWarpPointerToo);
#undef FUNC_NAME
  if (check_allowed_function(F_RESIZE, psw) == 0
      || SHADED_P(psw)) {
    return SCM_BOOL_F;
  }

  /* can't resize icons */
  if (psw->fIconified) {
    return SCM_BOOL_F;
  }

  if (!frame_p) {
    width += (2*psw->xboundary_width);
    height += (psw->title_height + 2*psw->boundary_width);
  }

  { /* scope */
    SCM animation_ms_delay = SCM_CDR(animation_delay);
    int cmsDelay = -1;
    Window x_win;
    
    if (animation_ms_delay != SCM_BOOL_F &&
	gh_number_p(animation_ms_delay)) {
      cmsDelay = gh_scm2int(animation_ms_delay);
    }

    if (SCM_BOOL_F==
        convert_move_data(x,y,win,func_name,
                          &startX,&startY,&destX, &destY, &psw, &x_win)) {
      /* destX, destY are viewport */
      return SCM_BOOL_F;
    };

    ConstrainSize(psw, 0, 0, &width, &height);
    ComputePositionForResize(psw, &destX, &destY, width, height);

    /* use viewport coordinates */
    AnimatedResizeWindow(psw, psw->frame, 
			 FRAME_WIDTH(psw), FRAME_HEIGHT(psw),
			 width, height,
                         startX - WIN_VP_OFFSET_X(psw),
                         startY - WIN_VP_OFFSET_Y(psw),
                         destX - WIN_VP_OFFSET_X(psw),
                         destY - WIN_VP_OFFSET_Y(psw),
			 fWarpPointerToo,cmsDelay,NULL);
  } /* scope */

  /* endX, endY are virtual */
  MoveResizeTo(psw,destX,destY,width,height);

  return SCM_UNSPECIFIED;
}

SCWM_PROC(animated_resize_window, "animated-resize-window", 2, 4, 0,
          (SCM w, SCM h, SCM win, SCM x, SCM y, SCM move_pointer_too_p))
     /** Resize the client area of WIN to size W, H (pixels) with animation.  
WIN defaults to the window context in the usual way if not
specified.  If X and Y are given, they are a new virtual position for the northwest
corder of the window. If MOVE-POINTER-TOO? is specified and true, move the mouse pointer by
the same amount as the window, animating the motion of the pointer
along with the window. */
#define FUNC_NAME s_animated_resize_window
{
  return animated_resize_common(w, h, win, x, y, move_pointer_too_p, FUNC_NAME, False);
}
#undef FUNC_NAME


SCWM_PROC(animated_resize_frame, "animated-resize-frame", 2, 4, 0,
          (SCM w, SCM h, SCM win, SCM x, SCM y, SCM move_pointer_too_p))
     /** Resize the frame of WIN to size W, H (pixels) with animation.  
WIN defaults to the window context in the usual way if not
specified.  If X and Y are given, they are a new virtual position for the northwest
corder of the window. If MOVE-POINTER-TOO? is specified and true, move the mouse pointer by
the same amount as the window, animating the motion of the pointer
along with the window. */
#define FUNC_NAME s_animated_resize_frame
{
  return animated_resize_common(w, h, win, x, y, move_pointer_too_p, FUNC_NAME, True);
}
#undef FUNC_NAME


static
void
init_c_animation()
{
#ifndef SCM_MAGIC_SNARFER
 #include "c-animation.x"
#endif
}

void scm_init_app_scwm_c_animation_module()
{
  scm_register_module_xxx("app scwm c-animation", init_c_animation);
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta */
