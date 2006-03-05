/* $Id$
 * screen.h
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
 *
 * This module derived from modified by Rob Nation, originally code based on Twm
 */

/*
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#ifndef SCREEN_H__
#define SCREEN_H__

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "binding.h"
#include "window.h"

#undef EXTERN
#undef EXTERN_SET
#ifdef DECOR_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

#define SIZE_HINDENT 5
#define SIZE_VINDENT 3
#define MAX_WINDOW_WIDTH 32767
#define MAX_WINDOW_HEIGHT 32767

typedef struct {
  Window win;
  int isMapped;
} PanFrame;


typedef enum {
  /* button types */
  VectorButton,
  SimpleButton,
  HGradButton,
  VGradButton,
  PixmapButton,
  TiledPixmapButton,
  MiniIconButton,
  SolidButton
  /* max button is 15 (0xF) */
} ButtonFaceType;

typedef int ButtonFaceStyle;

#define ButtonFaceTypeMask      0x000F

/* button style flags (per-state) */
enum {
  /* specific style flags */
  /* justification bits (3.17 -> 4 bits) */
  HOffCenter = (1 << 4),
  HRight = (1 << 5),
  VOffCenter = (1 << 6),
  VBottom = (1 << 7),

  /* general style flags */
  UseTitleStyle = (1 << 8),
  UseBorderStyle = (1 << 9),
  FlatButton = (1 << 10),
  SunkButton = (1 << 11)
};

/* border style flags (uses ButtonFace) */
enum {
  HiddenHandles = (1 << 8),
  NoInset = (1 << 9)
};

struct vector_coords {
  int num;
  int x[20];
  int y[20];
  int line_style[20];
};

typedef struct ButtonFace {
  ButtonFaceStyle style;   /* a ButtonFaceType + some flags in the high order bits */
  /* was just the enum ButtonFaceType, but that isn't the right thing
     and causes problems with C++'s more picky enum semantics */
  union {
    SCM image;
    SCM back;
    struct {
      int npixels;
      Pixel *pixels;
    } grad;
  } u;

  struct vector_coords vector;

  struct ButtonFace *next;
  SCM sface;
} ButtonFace;

/* button style flags (per title button) */
enum {
  MWMButton = (1 << 0)
};

enum ButtonState {
  ActiveUp,
  ActiveDown,
  Inactive,
  MaxButtonState
};

typedef struct {
  int flags;
  ButtonFace *state[MaxButtonState];
} TitleButton;

typedef struct ColorPair {
  SCM fg;
  SCM bg;
} ColorPair;

typedef struct ScwmDecor {
  char *tag;			/* general style tag */
  ColorPair HiColors;		/* standard fore/back colors */
  ColorPair HiRelief;
  GC HiReliefGC;		/* GC for highlighted window relief */
  GC HiShadowGC;		/* GC for highlighted window shadow */

  int TitleHeight;		/* height of the title bar window */

  SCM window_font;              /* Font for drawing the window */
  int window_font_y;            /* Real y position for the font. */

  /* titlebar buttons */
  TitleButton left_buttons[5];
  TitleButton right_buttons[5];
  TitleButton titlebar;
  struct BorderStyle {
    ButtonFace *active, *inactive;
  } BorderStyle;

  double highlight_factor;
  double shadow_factor;
  struct ScwmDecor *next;	/* additional user-defined styles */
  SCM scmdecor;                 /* self pointer */
} ScwmDecor;


EXTERN long scm_tc16_scwm_screen;

struct ScwmScreenConstraintInfo;

/* Holds all the per-screen information;
   these are the global variables and options */
typedef struct ScreenInfo {

  unsigned long screen;
  int d_depth;			/* copy of DefaultDepth(dpy, screen) */
  int NumberOfScreens;		/* number of screens on display */
  int DisplayWidth;		/* my copy of DisplayWidth(dpy, screen) */
  int DisplayHeight;		/* my copy of DisplayHeight(dpy, screen) */

  ScwmWindow ScwmRoot;		/* the head of the scwm window list */
  Window Root;			/* the root window */
  Window NoFocusWin;		/* Window which will own focus when no other
				 * windows have it */
  PanFrame PanFrameTop, PanFrameLeft, PanFrameRight, PanFrameBottom;

  Pixmap gray_bitmap;		/*dark gray pattern for shaded out menu items*/
  Pixmap gray_pixmap;		/* dark gray pattern for inactive borders */
  Pixmap light_gray_pixmap;	/* light gray pattern for inactive borders */
  Pixmap sticky_gray_pixmap;	/* light gray pattern for sticky borders */

  Binding *AllBindings;

  int root_pushes;		/* current push level to install root
				   colormap windows */
  ScwmWindow *pushed_window;	/* saved window to install when pushes drops
				   to zero */
  char *DefaultIcon;		/* Icon to use when no other icons are found */

  struct ScwmScreenConstraintInfo *pssci; /* Constraint information for this screen */
                                /* NULL if built w/o cassowary support */


  /* GJB:FIXME:: these should be renamed, or separated out
     -- they are the colors used for various things throughout the code */
  ColorPair NotMenuColors;
  ColorPair NotMenuRelief;

  SCM schscreen;                /* back pointer to the scheme-level screen obj */

  SCM icon_font;                /* for icon labels */
  SCM msg_window_font;          /* font for the size/position window */
  SCM msg_window_fg;            /* fg color for the size/position window */
  SCM msg_window_bg;            /* bg color for the size/position window */
  SCM msg_window_highlight;        /* highlight color for the size/position window */
  SCM msg_window_shadow;        /* shadow color for the size/position window */
  int msg_window_x;		/* x position of the size/position window */
  int msg_window_y;		/* y position of the size/position window */
  double msg_window_x_align;	/* offset msg window by multiple of size */
  double msg_window_y_align;	/* offset msg window by multiple of size */
  
  GC ScratchGC1;
  GC ScratchGC2;
  GC ScratchGC3;

  int BoundaryWidth;		/* frame width for decorated windows */
  int NoBoundaryWidth;		/* frame width for decorated windows */

  ScwmDecor DefaultDecor;	/* decoration style(s) */

  int nr_left_buttons;		/* number of left-side title-bar buttons */
  int nr_right_buttons;		/* number of right-side title-bar buttons */

  ScwmWindow *Hilite;		/* the scwm window that is highlighted 
				 * except for networking delays, this is the
				 * window which REALLY has the focus */
  ScwmWindow *Focus;		/* Last window which Scwm gave the focus to 
				 * NOT the window that really has the focus */
  ScwmWindow *Ungrabbed;
  ScwmWindow *PreviousFocus;	/* Window which had focus before scwm stole it
				 * to do moves/menus/etc. */
  int EdgeScrollX;		/* #pixels to scroll on screen edge */
  int EdgeScrollY;		/* #pixels to scroll on screen edge */
  unsigned char buttons2grab;	/* buttons to grab in click to focus mode */

  int randomx;			/* values used for randomPlacement */
  int randomy;
  ScwmWindow *LastWindowRaised;	/* Last window which was raised. Used for raise
				 * lower func. */

  /* virtual screen information */
  int VxMax;			/* Max location for top left of virt desk */
  int VyMax;
  int Vx;			/* Current loc for top left of virt desk */
  int Vy;
  int CurrentDesk;		/* The current desktop number */

  /* There aren't really enough of these to justify using a PackedBool
     but what the heck... --07/26/98 gjb */
  PackedBool(fWindowsCaptured); /* have the windows already been captured? */

  /* global options */
  PackedBool(fEdgeWrapX);       /* does the pointer wrap horizontally? */
  PackedBool(fEdgeWrapY);       /* does the pointer wrap vertically? */
  /*  PackedBool(fMWMMenus);        MWM menu style (not really --07/26/98 gjb */
  PackedBool(fColormapFollowsMouse);
  /* these global options might better be window-specific options */
  PackedBool(fClickToFocusPassesClick);
  PackedBool(fClickToFocusRaises);
  PackedBool(fMouseFocusClickRaises);

  /* Global options */
  unsigned int ClickTime;		/* Max button-click delay for distinguishing clicks/drags */
  int ScrollResistance;		/* resistance to scrolling in desktop */
  int MoveResistance;		/* res to moving windows over viewport edge */

  SCM nonant_highlight_color;
} ScreenInfo;

extern ScreenInfo Scr;

SCM ScmFromPScreenInfo(ScreenInfo *psi);

#define SCREENP(X) (SCM_SMOB_PREDICATE(scm_tc16_scwm_screen, X))
#define SCREEN(X)  ((ScreenInfo *)SCM_SMOB_DATA(X))


/* 
   Macro which gets specific decor or default decor.
   This saves an indirection in case you don't want
   the UseDecor mechanism.
 */
#define GET_DECOR(window,part) ((window)->fl->part)

#endif /* SCREEN_H__ */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

