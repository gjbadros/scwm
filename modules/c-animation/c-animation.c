/* $Id$
 * Copyright (C) 1998, 1999, 2000 Maciej Stachowiak and Greg J. Badros
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
#include "scwmconfig.h"
#endif

#include <X11/Xlib.h>

#include <libguile.h>
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

extern XEvent Event;

static SCM *pscm_animation_delay;

float rgpctMovementDefault[32] = {
    -.01, 0, .01, .03,.08,.18,.3,.45,.60,.75,.85,.90,.94,.97,.99,1.0 
    /* must end in 1.0 */
};

int cmsDelayDefault = 10; /* milliseconds */

void SendClientConfigureNotify(const ScwmWindow *psw);


/* Perform the resizing of the window. ppctMovement *must* have a 1.0 entry
   somewhere in ins list of floats, and movement will stop when it hits a 1.0 entry
   The positions given are viewport positions (not virtual) */
/* This gets passed a first argument that is a list of lists: 
    ((window frame? (start-width . start-height) (end-width . end-height)
                   (start-x . start-y) (end-x . end-y) (set-end-x? . set-end-y?))
     ... ;; more lists like the above
     ... )
    If fWarpPointerToo is True, the pointer warps with the first window in the
    xforms list.
 */
void 
AnimatedResizeWindows(SCM xforms,
                      Bool fWarpPointerToo, int cmsDelay, float *ppctMovement,
                      Bool fUseSolver)
{
  int currentW, currentH;
  int pointerX, pointerY;
  int currentX, currentY;
  int lastX = 0, lastY = 0;
  int cStepsAllowed = 100;
  Bool fFirstAnimationIteration = True;

  /* set our defaults */
  if (ppctMovement == NULL) ppctMovement = rgpctMovementDefault;
  if (cmsDelay < 0)         cmsDelay     = cmsDelayDefault;

  do {
    float pctMovement = *ppctMovement;
  window_loop:
    { /* scope */
    SCM xform_iter = xforms;
    Bool fFirst = True;
    for (; xform_iter != SCM_EOL; xform_iter = scm_cdr(xform_iter)) {
      SCM xform = scm_car(xform_iter);
      ScwmWindow *psw = NULL;
      Window w;
      Bool fFrame = False;
      Bool fSetEndX = False, fSetEndY = False;
      SCM cns;
      int startW, startH, deltaW, deltaH;
      int endW, endH;
      int startX, startY, deltaX, deltaY;
      int endX, endY;
      
      psw = PSWFROMSCMWIN(scm_car(xform)); xform = scm_cdr(xform);
      fFrame = scm_to_bool(scm_car(xform)); 
      if (fFrame) w = psw->frame;
      else w = psw->icon_w;

      xform = scm_cdr(xform); cns = scm_car(xform);
      startW = scm_to_int(scm_car(cns));
      startH = scm_to_int(scm_cdr(cns));
      xform = scm_cdr(xform); cns = scm_car(xform);
      endW = scm_to_int(scm_car(cns));
      endH = scm_to_int(scm_cdr(cns));
      deltaW = endW - startW;
      deltaH = endH - startH;

      xform = scm_cdr(xform); cns = scm_car(xform);
      startX = scm_to_int(scm_car(cns));
      startY = scm_to_int(scm_cdr(cns));
      xform = scm_cdr(xform); cns = scm_car(xform);
      endX = scm_to_int(scm_car(cns));
      endY = scm_to_int(scm_cdr(cns));
      deltaX = endX - startX;
      deltaY = endY - startY;

      xform = scm_cdr(xform); cns = scm_car(xform);
      fSetEndX = scm_to_bool(scm_car(cns));
      fSetEndY = scm_to_bool(scm_cdr(cns));
      
#ifdef SCWM_DEBUG_ANIM_RESIZE
      fprintf(stderr,"startX = %d, startY = %d, endX = %d, endY = %d, deltaX = %d, deltaY = %d\n",
              startX, startY, endX, endY, deltaX, deltaY);
#endif

      if (!fSetEndX || !fSetEndY) {
        int grav_dx, grav_dy;
        ComputeDeltaForResizeWithOrigSize(psw,&grav_dx,&grav_dy,endW,endH,startW,startH);
        if (!fSetEndX) {
          deltaX -= grav_dx;
        }
        if (!fSetEndY) {
          deltaY -= grav_dy;
        }
#ifdef SCWM_DEBUG_ANIM_RESIZE
        fprintf(stderr,"now deltaX = %d, deltaY = %d\n", deltaX, deltaY);
#endif
      }

      if (startW < 0 || startH < 0) {
        FXGetWindowSize(w, &currentW, &currentH);
        if (startW < 0) startW = currentW;
        if (startH < 0) startH = currentH;
      }

      if ((deltaX == 0 && deltaY == 0) && \
          (deltaW == 0 && deltaH == 0))
        continue;
      
      currentX = (int) (startX + deltaX * pctMovement);
      currentY = (int) (startY + deltaY * pctMovement);
      
      currentW = (int) (startW + deltaW * pctMovement);
      currentH = (int) (startH + deltaH * pctMovement);

#ifdef SCWM_DEBUG_ANIM_RESIZE
      fprintf(stderr,"currentX = %d, currentY = %d, currentW = %d, currentH = %d\n",
              currentX, currentY, currentW, currentH);
#endif

      if (fUseSolver) {
        CassowaryEditSize(psw);
        
        if (SuggestSizeWindowTo(psw,
                                WIN_VP_OFFSET_X(psw) + currentX,
                                WIN_VP_OFFSET_Y(psw) + currentY,
                                currentW, currentH, True)) {
          /* it was resized/moved */
          /* empty */
        }
        
        CassowaryEndEdit(psw);
      } else {
        SetScwmWindowGeometry(psw,
                              WIN_VP_OFFSET_X(psw) + currentX,
                              WIN_VP_OFFSET_Y(psw) + currentY,
                              currentW, currentH, True);
      }

      if (fFirstAnimationIteration) {
        fFirstAnimationIteration = False;
        lastX = startX;
        lastY = startY;
      }

      if (fFirst && fWarpPointerToo) {
        fFirst = False;
        WXGetPointerWindowOffsets(Scr.Root,&pointerX,&pointerY);
        pointerX += currentX - lastX;
        pointerY += currentY - lastY;
        XWarpPointer(dpy,None,Scr.Root,0,0,0,0,
                     pointerX,pointerY);
        lastX = currentX;
        lastY = currentY;
      }
    } /* for each window */

    /* XFlush(dpy); */
    
    /* handle expose events as we're animating the window move */
    while (XCheckMaskEvent(dpy,  ExposureMask, &Event)) {
      DispatchEvent(); 
    }
  
    ms_sleep(cmsDelay);
    if (XCheckMaskEvent(dpy, 
			KeyPressMask,
			&Event)) {
      /* finish the move immediately */
      pctMovement = 1.0;
      cStepsAllowed = 0;
      goto window_loop;
    }
    } /* scope */
  } while (*ppctMovement != 1.0 && ppctMovement++ && --cStepsAllowed > 0);
}


void 
AnimatedResizeWindow(ScwmWindow *psw, Window w, int startW,int startH,int endW, int endH,
                     int startX, int startY, int endX, int endY, 
                     Bool fSetEndX, Bool fSetEndY,
		     Bool fWarpPointerToo, int cmsDelay, float *ppctMovement)
{
  SCM xforms = scm_cons(ScmWindowDelta(psw,w,startW,startH,endW,endH,
                                      startX,startY,endX,endY,fSetEndX,fSetEndY),
                       SCM_EOL);
  AnimatedResizeWindows(xforms,fWarpPointerToo,cmsDelay,ppctMovement, True);
}



/* AnimatedShadeWindow handles animating of window shades
   note that the first argument to this is a ScwmWindow *, since
   the frame needs to be manipulated; the last two args are like
   AnimatedResizeindow, above */
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
SCM_DEFINE(set_animation_x, "set-animation!", 1,0,0,
          (SCM vector),
"Set the animation parameters to VECTOR. VECTOR is a vector of\n\
floats which give the fractions of the final position that the window\n\
should appear at. For instance, #(0.0 0.25 0.5 0.75 1.0 1.1 1.0) would\n\
make the window appear at the initial position, 1/4 of the way, 1/2 of\n\
the way, 3/4 of the way, overshoot the final position slightly, and\n\
finally slide back into place. This parameter is used for both\n\
animated window shades and animated moves.")
#define FUNC_NAME s_set_animation_x
{
  int citems;
  int i;

  if (!scm_to_bool(scm_vector_p(vector))) {
    SCWM_WRONG_TYPE_ARG(1,vector);
  }

  citems = scm_to_int(scm_vector_length(vector));
  for (i=0; i<citems; i++) {
    SCM val = scm_vector_ref(vector,scm_from_int(i));    
    if (!scm_to_bool(scm_number_p(val))) {
      SCWM_WRONG_TYPE_ARG(1,vector);
    }
    /* FIXGJB: also check < 2, perhaps (don't want to
      check < 1, since we might want to overshoot and then come back) */
    rgpctMovementDefault[i] = (float) scm_to_double(val);
  }
  /* Ensure that we end up 100% of the way to our destination */
  if (i>0 && rgpctMovementDefault[i-1] != 1.0) {
    rgpctMovementDefault[i++] = 1.0;
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE(animated_window_shade, "animated-window-shade", 0, 1, 0,
          (SCM win),
"Cause WIN to become \"window-shaded\".\n\
That is, to roll up into just a titlebar.  The window will be animated\n\
as it rolls up, producing a pleasing visual effect. WIN defaults to\n\
the window context in the usual way if not specified.")
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
                                scm_from_bool(old));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(animated_window_unshade, "animated-window-unshade", 0, 1, 0,
          (SCM win),
"Reverse the effect of `window-shade' on WIN.\n\
The window will be animated as it rolls down, producing a pleasing\n\
visual effect. WIN defaults to the window context in the usual way if\n\
not specified. See also `window-unshade', `animated-window-shade'.")
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
                                scm_from_bool(old));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

extern Bool fInResolveHook;

SCM_DEFINE(animate_windows, "animate-windows", 1, 1, 0,
          (SCM xforms, SCM move_pointer_too_p),
"Animate multiple windows. \n\
XFORMS is a list of transform operations where each xform operation\n\
describes how a single window should move and resize by giving its\n\
old and new configuration.  Each xfrom element of the XFORMS list\n\
should look like:\n\
(window frame? (start-width . start-height) (end-width . end-height) (start-x . start-y) (end-x . end-y) (set-end-x? . set-end-y?))\n\
If MOVE-POINTER-TOO? is #t, then the X11 pointer will move in\n\
conjunction with the first window in the XFORMS list;  defaults to #f.")
#define FUNC_NAME s_animate_windows
{
  Bool fMovePointer;
  SCM xform_iter = xforms;
  int i = 0;
  for (; xform_iter != SCM_EOL; xform_iter = scm_cdr(xform_iter), ++i) {
    if (!FScmIsWindowDelta(scm_car(xform_iter))) {
      scm_misc_error(FUNC_NAME,"Element %S of xforms argument list is bad: %s.",
                     scm_list_n(scm_from_int(i),scm_car(xform_iter),SCM_UNDEFINED));
      scm_wrong_type_arg(FUNC_NAME,1,xforms);
    }
  }
  VALIDATE_ARG_BOOL_COPY_USE_F(2,move_pointer_too_p,fMovePointer);

  { /* scope */
    int cmsDelay = -1;
    
    if (scm_is_number(*pscm_animation_delay)) {
      cmsDelay = scm_to_int(*pscm_animation_delay);
    }
    AnimatedResizeWindows(xforms, fMovePointer, 
                          cmsDelay, NULL, !fInResolveHook);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE(animated_move_window, "animated-move-window", 2, 2, 0,
          (SCM x, SCM y, SCM win, SCM move_pointer_too_p),
"Move WIN to virtual coordinates X, Y with animation.\n\
If X or Y is #f, then do not change that coordinate during the move. \n\
If MOVE-POINTER-TOO? is specified and true, move the mouse pointer by\n\
the same amount as the window, animating the motion of the pointer\n\
along with the window. WIN defaults to the window context in the usual\n\
way if not specified.")
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
    int cmsDelay = -1;
    
    if (scm_is_number(*pscm_animation_delay)) {
      cmsDelay = scm_to_int(*pscm_animation_delay);
    }

    /* use viewport coordinates */
    AnimatedResizeWindow(psw, w, FRAME_WIDTH(psw), FRAME_HEIGHT(psw),
                         FRAME_WIDTH(psw), FRAME_HEIGHT(psw),
                         startX - WIN_VP_OFFSET_X(psw),
                         startY - WIN_VP_OFFSET_Y(psw),
                         destX - WIN_VP_OFFSET_X(psw),
                         destY - WIN_VP_OFFSET_Y(psw),
                         !UNSET_SCM(x),!UNSET_SCM(y),
                         fMovePointer,cmsDelay,NULL);
  } /* scope */

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
  VALIDATE_ARG_WIN_COPY_USE_CONTEXT(3, win,psw);
  VALIDATE_ARG_INT_COPY_USE_DEF(1,w,width,FRAME_WIDTH(psw));
  VALIDATE_ARG_INT_COPY_USE_DEF(2,h,height,FRAME_HEIGHT(psw));
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
    int cmsDelay = -1;
    Window x_win;
    if (scm_to_bool(scm_number_p(*pscm_animation_delay))) {
      cmsDelay = scm_to_int(*pscm_animation_delay);
    }

    if (SCM_BOOL_F==
        convert_move_data(x,y,win,func_name,
                          &startX,&startY,&destX, &destY, &psw, &x_win)) {
      /* destX, destY are viewport */
      return SCM_BOOL_F;
    };

    ConstrainSize(psw, 0, 0, &width, &height);

    /* use viewport coordinates */
    AnimatedResizeWindow(psw, psw->frame, 
			 FRAME_WIDTH(psw), FRAME_HEIGHT(psw),
			 width, height,
                         startX - WIN_VP_OFFSET_X(psw),
                         startY - WIN_VP_OFFSET_Y(psw),
                         destX - WIN_VP_OFFSET_X(psw),
                         destY - WIN_VP_OFFSET_Y(psw),
                         !UNSET_SCM(x), !UNSET_SCM(y),
			 fWarpPointerToo,cmsDelay,NULL);
  } /* scope */

  return SCM_UNSPECIFIED;
}

SCM_DEFINE(animated_resize_window, "animated-resize-window", 2, 4, 0,
          (SCM w, SCM h, SCM win, SCM x, SCM y, SCM move_pointer_too_p),
"Resize the client area of WIN to size W, H (pixels) with animation.\n\
WIN defaults to the window context in the usual way if not\n\
specified.  If X and Y are given, they are a new virtual position for the northwest\n\
corder of the window. If MOVE-POINTER-TOO? is specified and true, move the mouse pointer by\n\
the same amount as the window, animating the motion of the pointer\n\
along with the window.")
#define FUNC_NAME s_animated_resize_window
{
  return animated_resize_common(w, h, win, x, y, move_pointer_too_p, FUNC_NAME, False);
}
#undef FUNC_NAME


SCM_DEFINE(animated_resize_frame, "animated-resize-frame", 2, 4, 0,
          (SCM w, SCM h, SCM win, SCM x, SCM y, SCM move_pointer_too_p),
"Resize the frame of WIN to size W, H (pixels) with animation.\n\
WIN defaults to the window context in the usual way if not\n\
specified.  If X and Y are given, they are a new virtual position for the northwest\n\
corder of the window. If MOVE-POINTER-TOO? is specified and true, move the mouse pointer by\n\
the same amount as the window, animating the motion of the pointer\n\
along with the window.")
#define FUNC_NAME s_animated_resize_frame
{
  return animated_resize_common(w, h, win, x, y, move_pointer_too_p, FUNC_NAME, True);
}
#undef FUNC_NAME


void
init_c_animation()
{
 #include "c-animation.x"
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta */
