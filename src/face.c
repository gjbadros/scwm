/* $Id$
 * face.c
 * (C) 1998 Maciej Stachowiak and Greg J. Badros
 * 
 * This module has been significantly modified by Maciej Stachowiak
 * and Greg J Badros.
 * It may be used under the terms of the fvwm copyright (see COPYING.FVWM).
 * Changes Copyright 1997, 1998 Maciej Stachowiak and Greg J. Badros
 *
 *
 * This module based on original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_CONFIG_H
#include <guile/gh.h>
#endif
#include "scwm.h"
#include "screen.h"
#include "window.h"
#include "face.h"
#include "color.h"
#include "colors.h"
#include "util.h"
#include "image.h"
#include "guile-compat.h"
#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

/**CONCEPT: Faces 
  Faces are a data type used to specify in detail the way in which
window decorations like the titlebar, the border and titlebar buttons
will be drawn. They are currently somewhat kludgey and
ad-hoc. However, they offer a great deal of flexibility. All faces are
set in the current decor, so multiple decors must be used to use
different faces for different windows. The low-level functionality
offered in the face primitives will rarely be needed; the
`button-style', `title-style' and `border-style' procedures in the
(app scwm face) module provide a more convenient interface to this
functionality.
*/


/*
 *  LoadDefaultLeftButton -- loads default left button # into 
 *		assumes associated button memory is already free
 */
void 
LoadDefaultLeftButton(ButtonFace * bf, int i)
{
  bf->style = VectorButton;
  switch (i % 5) {
  case 0:
  case 4:
    bf->vector.x[0] = 22;
    bf->vector.y[0] = 39;
    bf->vector.line_style[0] = 1;
    bf->vector.x[1] = 78;
    bf->vector.y[1] = 39;
    bf->vector.line_style[1] = 1;
    bf->vector.x[2] = 78;
    bf->vector.y[2] = 61;
    bf->vector.line_style[2] = 0;
    bf->vector.x[3] = 22;
    bf->vector.y[3] = 61;
    bf->vector.line_style[3] = 0;
    bf->vector.x[4] = 22;
    bf->vector.y[4] = 39;
    bf->vector.line_style[4] = 1;
    bf->vector.num = 5;
    break;
  case 1:
    bf->vector.x[0] = 32;
    bf->vector.y[0] = 45;
    bf->vector.line_style[0] = 0;
    bf->vector.x[1] = 68;
    bf->vector.y[1] = 45;
    bf->vector.line_style[1] = 0;
    bf->vector.x[2] = 68;
    bf->vector.y[2] = 55;
    bf->vector.line_style[2] = 1;
    bf->vector.x[3] = 32;
    bf->vector.y[3] = 55;
    bf->vector.line_style[3] = 1;
    bf->vector.x[4] = 32;
    bf->vector.y[4] = 45;
    bf->vector.line_style[4] = 0;
    bf->vector.num = 5;
    break;
  case 2:
    bf->vector.x[0] = 49;
    bf->vector.y[0] = 49;
    bf->vector.line_style[0] = 1;
    bf->vector.x[1] = 51;
    bf->vector.y[1] = 49;
    bf->vector.line_style[1] = 1;
    bf->vector.x[2] = 51;
    bf->vector.y[2] = 51;
    bf->vector.line_style[2] = 0;
    bf->vector.x[3] = 49;
    bf->vector.y[3] = 51;
    bf->vector.line_style[3] = 0;
    bf->vector.x[4] = 49;
    bf->vector.y[4] = 49;
    bf->vector.line_style[4] = 1;
    bf->vector.num = 5;
    break;
  case 3:
    bf->vector.x[0] = 32;
    bf->vector.y[0] = 45;
    bf->vector.line_style[0] = 1;
    bf->vector.x[1] = 68;
    bf->vector.y[1] = 45;
    bf->vector.line_style[1] = 1;
    bf->vector.x[2] = 68;
    bf->vector.y[2] = 55;
    bf->vector.line_style[2] = 0;
    bf->vector.x[3] = 32;
    bf->vector.y[3] = 55;
    bf->vector.line_style[3] = 0;
    bf->vector.x[4] = 32;
    bf->vector.y[4] = 45;
    bf->vector.line_style[4] = 1;
    bf->vector.num = 5;
    break;
  }
}

/*
 *  LoadDefaultRightButton -- loads default left button # into
 *		assumes associated button memory is already free
 */
void 
LoadDefaultRightButton(ButtonFace * bf, int i)
{
  bf->style = VectorButton;
  switch (i % 5) {
  case 0:
  case 3:
    bf->vector.x[0] = 25;
    bf->vector.y[0] = 25;
    bf->vector.line_style[0] = 1;
    bf->vector.x[1] = 75;
    bf->vector.y[1] = 25;
    bf->vector.line_style[1] = 1;
    bf->vector.x[2] = 75;
    bf->vector.y[2] = 75;
    bf->vector.line_style[2] = 0;
    bf->vector.x[3] = 25;
    bf->vector.y[3] = 75;
    bf->vector.line_style[3] = 0;
    bf->vector.x[4] = 25;
    bf->vector.y[4] = 25;
    bf->vector.line_style[4] = 1;
    bf->vector.num = 5;
    break;
  case 1:
    bf->vector.x[0] = 39;
    bf->vector.y[0] = 39;
    bf->vector.line_style[0] = 1;
    bf->vector.x[1] = 61;
    bf->vector.y[1] = 39;
    bf->vector.line_style[1] = 1;
    bf->vector.x[2] = 61;
    bf->vector.y[2] = 61;
    bf->vector.line_style[2] = 0;
    bf->vector.x[3] = 39;
    bf->vector.y[3] = 61;
    bf->vector.line_style[3] = 0;
    bf->vector.x[4] = 39;
    bf->vector.y[4] = 39;
    bf->vector.line_style[4] = 1;
    bf->vector.num = 5;
    break;
  case 2:
    bf->vector.x[0] = 49;
    bf->vector.y[0] = 49;
    bf->vector.line_style[0] = 1;
    bf->vector.x[1] = 51;
    bf->vector.y[1] = 49;
    bf->vector.line_style[1] = 1;
    bf->vector.x[2] = 51;
    bf->vector.y[2] = 51;
    bf->vector.line_style[2] = 0;
    bf->vector.x[3] = 49;
    bf->vector.y[3] = 51;
    bf->vector.line_style[3] = 0;
    bf->vector.x[4] = 49;
    bf->vector.y[4] = 49;
    bf->vector.line_style[4] = 1;
    bf->vector.num = 5;
    break;
  case 4:
    bf->vector.x[0] = 36;
    bf->vector.y[0] = 36;
    bf->vector.line_style[0] = 1;
    bf->vector.x[1] = 64;
    bf->vector.y[1] = 36;
    bf->vector.line_style[1] = 1;
    bf->vector.x[2] = 64;
    bf->vector.y[2] = 64;
    bf->vector.line_style[2] = 0;
    bf->vector.x[3] = 36;
    bf->vector.y[3] = 64;
    bf->vector.line_style[3] = 0;
    bf->vector.x[4] = 36;
    bf->vector.y[4] = 36;
    bf->vector.line_style[4] = 1;
    bf->vector.num = 5;
    break;
  }
}

/*
 *  LoadDefaultButton -- loads default button # into button structure
 *		assumes associated button memory is already free
 */
void 
LoadDefaultButton(ButtonFace * bf, int i)
{
  int n = i / 2;

  if ((n * 2) == i) {
    if (--n < 0)
      n = 4;
    LoadDefaultRightButton(bf, n);
  } else
    LoadDefaultLeftButton(bf, n);
}

void FreeButtonFace(Display * dpy, ButtonFace * bf);

/*
 *  ResetAllButtons -- resets all buttons to defaults
 *                 destroys existing buttons
 */
void 
ResetAllButtons(ScwmDecor * fl)
{
  int i = 0;

  for (; i < 5; ++i) {
    int j;

    FreeButtonFace(dpy, fl->left_buttons[i].state[0]);
    FreeButtonFace(dpy, fl->right_buttons[i].state[0]);

    LoadDefaultLeftButton(fl->left_buttons[i].state[0], i);
    LoadDefaultRightButton(fl->right_buttons[i].state[0], i);

    for (j = 1; j < MaxButtonState; ++j) {
      /* FreeButtonFace(dpy, fl->left_buttons[i].state[j]); */
      /* FreeButtonFace(dpy, fl->right_buttons[i].state[j]); */

      fl->left_buttons[i].state[j] = fl->left_buttons[i].state[0];
      fl->right_buttons[i].state[j] = fl->right_buttons[i].state[0];
    }
  }
  fl->right_buttons[0].flags |= MWMButton;
}

long scm_tc16_scwm_face;

int 
print_face(SCM obj, SCM port, scm_print_state * pstate)
{
  scm_puts("#<face ", port);
  scm_write(gh_int2scm((int)FACE(obj)), port);
  scm_putc('>', port);

  return 1;
}

size_t
free_face(SCM obj)
{
  FreeButtonFace(dpy,BUTTONFACE(obj));
  FREE(BUTTONFACE(obj));
  FREE(FACE(obj));
  return (0);
}

SCM
mark_face(SCM obj)
{
  ButtonFace *bf;
  
  SCM_SETGC8MARK(obj);
  
  for (bf=BUTTONFACE(obj); bf != NULL; bf = bf->next) {
    if (((bf->style & ButtonFaceTypeMask) == PixmapButton) || 
	((bf->style & ButtonFaceTypeMask) == TiledPixmapButton)) {
      GC_MARK_SCM_IF_SET(bf->u.image);
    }
    if ((bf->style & ButtonFaceTypeMask) == SolidButton) {
      GC_MARK_SCM_IF_SET(bf->u.back);
    }
  }
  
  return SCM_BOOL_F;
}

SCM default_titlebar_face;
SCM default_border_face;
SCM default_lbutton_face[5];
SCM default_rbutton_face[5];


void add_spec_to_face_x(SCM face, SCM spec, SCM arg);

/* These three symbols are also used by msicprocs.c's
   set-title-justify! */
SCWM_GLOBAL_SYMBOL(sym_left , "left");
SCWM_GLOBAL_SYMBOL(sym_right , "right");
SCWM_GLOBAL_SYMBOL(sym_center , "center");

SCWM_SYMBOL(sym_clear , "clear");
SCWM_SYMBOL(sym_justify , "justify");
SCWM_SYMBOL(sym_vertical_justify , "vertical-justify");
SCWM_SYMBOL(sym_relief , "relief");
SCWM_SYMBOL(sym_use_style_of , "use-style-of");
SCWM_SYMBOL(sym_hidden_handles , "hidden-handles");
SCWM_SYMBOL(sym_no_inset , "no-inset");
SCWM_SYMBOL(sym_top , "top");
SCWM_SYMBOL(sym_bottom , "bottom");
SCWM_SYMBOL(sym_flat , "flat");
SCWM_SYMBOL(sym_sunk , "sunk");
SCWM_SYMBOL(sym_raised , "raised");
SCWM_SYMBOL(sym_title , "title");
SCWM_SYMBOL(sym_border , "border");


/* FIXMS Probably the right way to do this is to keep a hash table of
pointers to functions that know how to set each individual flag, but
this ugly code is almost certainly more compact and quite possibly
faster. If only C had closures... */
SCWM_PROC(set_face_flag_x,"set-face-flag!", 3, 0, 0,
          (SCM face, SCM flag, SCM flagval))
     /** Set the given FLAG to the given FLAGVAL for face FACE.
See the section on the face-specification-flags concept. */
#define FUNC_NAME s_set_face_flag_x
{
  ButtonFace *bf;

  if (!FACEP(face)) {
    scm_wrong_type_arg(FUNC_NAME,1,face);
  }

  bf=BUTTONFACE(face);

  if (flag==sym_justify) {
    if (flagval==sym_left) {
      bf->style |= HOffCenter;
      bf->style &= ~HRight;
    } else if (flagval==sym_right) {
      bf->style |= HOffCenter | HRight;
    } else if (flagval==sym_center) {
      bf->style &= ~HOffCenter;
    } 
    else {
      /* FIXMS: use something more accurate. */
      scm_wrong_type_arg(__FUNCTION__,3,flagval);    
    }

  } else if (flag==sym_vertical_justify) {
    if (flagval==sym_top) {
      bf->style |= VOffCenter;
      bf->style &= ~VBottom;
    } else if (flagval==sym_bottom) {
      bf->style |= VOffCenter;
      bf->style &= ~VBottom;
    } else if (flagval==sym_center) {
      bf->style &= ~VOffCenter;
    } 
    else {
      /* FIXMS: use something more accurate. */
      scm_wrong_type_arg(__FUNCTION__,3,flagval);    
    }

  } else if (flag==sym_relief) {
    if (flagval==sym_raised) {
      bf->style &= ~FlatButton;
      bf->style &= ~SunkButton;
    }  else if (flagval==sym_sunk) {
      bf->style &= ~FlatButton;
      bf->style |= SunkButton;
    } else if (flagval==sym_flat) {
      bf->style &= ~SunkButton;
      bf->style |= FlatButton;
    } else {
      /* FIXMS: use something more accurate. */
      scm_wrong_type_arg(__FUNCTION__,3,flagval);    
    }

  } else if (flag==sym_use_style_of) {
    if (flagval==sym_title) {
      bf->style |= UseTitleStyle;
      bf->style &= ~UseBorderStyle;
    } else if (flagval==sym_border) {
      bf->style |= UseBorderStyle;
      bf->style &= ~UseTitleStyle;
    } else if (flagval==SCM_BOOL_F) {
      bf->style &= ~UseBorderStyle & ~UseTitleStyle;
    } 
    else {
      /* FIXMS: use something more accurate. */
      scm_wrong_type_arg(__FUNCTION__,3,flagval);    
    }

  } else if (flag==sym_hidden_handles) {
    if (flagval==SCM_BOOL_T) {
      bf->style |= HiddenHandles;
    } else if (flagval==SCM_BOOL_F) {
      bf->style &= ~HiddenHandles;
    } else {
      /* FIXMS: use something more accurate. */
      scm_wrong_type_arg(__FUNCTION__,3,flagval);    
    }

  } else if (flag==sym_no_inset) {
    if (flagval==SCM_BOOL_T) {
      bf->style |= NoInset;
    } else if (flagval==SCM_BOOL_F) {
      bf->style &= ~NoInset;
    } else {
      /* FIXMS: use something more accurate. */
      scm_wrong_type_arg(__FUNCTION__,3,flagval);    
    }

  } else {
    /* FIXMS: use something more accurate. */
    scm_wrong_type_arg(__FUNCTION__,2,flagval);    
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCWM_PROC(make_face, "make-face",2,0,0,
          (SCM flags, SCM specs) )
	  /** Create a new face.
FLAGS is a list of face flags (see concept) and
SPECS is a list of face specifiers. */
#define FUNC_NAME s_make_face
{
  SCM answer;
  scwm_face *sf;
  ButtonFace *bf;
  SCM p,flag,spec;

  if (!gh_list_p(flags)) {
    scm_wrong_type_arg(FUNC_NAME,1,flags);
  }

  if (!gh_list_p(specs)) {
    scm_wrong_type_arg(FUNC_NAME,2,specs);
  }

  sf = NEW(scwm_face);
  sf->bf = bf = NEW(ButtonFace);

  SCM_DEFER_INTS;
  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, scm_tc16_scwm_face);
  SCM_SETCDR(answer, (SCM)(sf));
  bf->sface = answer;
  bf->style = SimpleButton;
  bf->next = NULL;
  SCM_ALLOW_INTS;

  for (p=flags; p!=SCM_EOL; p=SCM_CDR(p)) {
    flag = gh_car(p);
    if (gh_list_p(flag) && gh_length(flag)==2) {
      set_face_flag_x(answer,gh_car(flag),gh_cadr(flag));
    } else {
      /* Bad flag specifier error */
      scm_wrong_type_arg(FUNC_NAME,1,flags);          
    }
  }

  for (p=specs; p!=SCM_EOL; p=SCM_CDR(p)) {
    spec = gh_car(p);
 
    if (gh_list_p(spec) && gh_length(spec)==2) {
      add_spec_to_face_x(answer,gh_car(spec),gh_cadr(spec));
    } else {
      /* Bad flag specifier error */
      scm_wrong_type_arg(FUNC_NAME,1,specs);          
    }
  }

  return answer;
}
#undef FUNC_NAME

/* FIXDOC: I need to make a table. How do I do that? For now using
   ad-hoc formatting. */
/**CONCEPT: Face Flags
   Face flags are two-element lists that specify certain properties
that are set once and only once for a give face (as opposed to specs,
which may be chained arbitrarily). Nearly all flags may be used for
button faces. Exceptions, as well as the flags that may be used for
title and border faces, are indicated below.


  Key               | Possible values      | Explanation
  ------------------|----------------------|----------------
  'justify          | 'left 'right 'center | How should the contents of the
                    |                      | face (pixmap, relief pattern, etc)
                    |                      | be justified?
  'vertical-justify | 'top 'bottom 'center | How should the contents of the
                    |                      | face (pixmap, relief pattern, etc)
                    |                      | be justified vertically?
  'relief           | 'flat 'sunk 'raised  | Should the face appear flat, 
                    |                      | raised or sunk? This is the only
                    |                      | face flag that may be used for 
                    |                      | titles. It may be used for buttons
                    |                      | as well, of course.
  'use-style-of     | 'title 'border #f    | Before handling this faces specs,
                    |                      | possibly apply the faces of the
                    |                      | title or the border first.
  'hidden-handles   | #t #f                | This flag may only be used for
                    |                      | border faces; it indicates that
                    |                      | the corner `handles' of a window
                    |                      | should not be visually separated.
  'no-inset         | #t #f                | This flag may only be used for
                    |                      | border faces; it indicates that
                    |                      | the border should be relieved only
                    |                      | on the outside, not on the inside.
                    |                      | This gives a Win9x-like effect.
*/

/*   'clear            : #t      ; not implemented for now, of questionable
                               ; usefulness when button styles are not
			       ; mutable.
*/



ButtonFace *append_new_face(ButtonFace *bf);

/**CONCEPT: Face Specification flags
  Face specification flags are two-element lists that specify certain
properties that may be chained to indicate how a face is drawn. Face
specs may be fully or partially destructive. A fully destructive spec
indicates how the whole area of the element is to be drawn, making
previous specs irrelevant. A partially destructive spec overlays part,
but not all, of the drawing area.

  All specs may be used for button faces. All but non-tiled pixmaps may
be used for titlebars, and only tiled pixmaps may be used for borders.

  Format                                    : Explanation
  ------------------------------------------:----------------------------
 '(relief-pattern ((X Y BOOL) ...))   : Draw a relief pattern using
                                            : the list of triples, each of 
                                            : which indicates a pair of X,Y
                                            : coordinates given as a 
                                            : percentage of the button size,
                                            : and a boolean value indicating
                                            : whether to use the lighter or
                                            : darker color. This spec is 
                                            : partially destructive.
 '(solid COLOR)                             : Use COLOR as the color for 
                                            : this element; fully destructive.
 '(gradient (horizontal|vertical            : Draw a gradient in this element.
    NCOLORS (COLOR PERCENT)* FINAL))        : The gradient may be horizontal
                                            : or vertical. The number of colors
                                            : is specified, followed by a 
                                            : number of colors with percentages
                                            : and a final color. The 
                                            : percentages must add to 100.
                                            : Fully destructive.
 '(pixmap mini-icon|IMAGE|                  : Specify a pixmap to use, either
   (tiled IMAGE))                           : the window's mini-icon, an image
                                            : object or image specifier string,
                                            : or a list of tiled and an image,
                                            : indicating the image should be
                                            : tiled. Partially destructive, 
                                            : except when tiled, which makes 
                                            : it fully destructive.  
*/

/* * '(simple #t)                                ;; non-destructive
 *                                               ;; perhaps non-pointful?
 * '(default N)                                  ;; fully destructive
 *                                               ;; perhaps there should
 *                                               ;; be variables for
 *                                               ;; these instead or
 *  
 * In any case, skip simple and default for now.
 *                                               ;; something?
 */

/*
 * FIXMS gradients should be exposed as scheme types, that would eliminate 
 * a lot of the hackery here. Not sure if relief patterns
 * are fundamental enough to be a type...
 */


SCWM_SYMBOL(sym_relief_pattern , "relief-pattern");
SCWM_SYMBOL(sym_solid , "solid");
SCWM_SYMBOL(sym_gradient , "gradient");
SCWM_SYMBOL(sym_horizontal , "horizontal");
SCWM_SYMBOL(sym_vertical , "vertical");
SCWM_SYMBOL(sym_pixmap , "pixmap");
SCWM_SYMBOL(sym_mini_program_icon , "mini-program-icon");
SCWM_SYMBOL(sym_tiled , "tiled");

/* FIXMS this function is horrible, functions should never be this
   huge, but I did not see an easier way to translate it. */

void add_spec_to_face_x(SCM face, SCM spec, SCM arg)
{
  ButtonFace *bf;

  bf=BUTTONFACE(face);

  if (spec==sym_relief_pattern) {
    int l;
    /* FIXMS arbitrary limit of 20 points in relief patterns is silly */
    if (gh_list_p(arg) &&((l=gh_length(arg)) >= 2)
	&& (l <= 20)) {
      struct vector_coords vc;
      SCM p, pel;
      vc.num=0;
      
      for (p=arg; p != SCM_EOL; p = SCM_CDR(p)) {
	pel=SCM_CAR(p);
	if ((gh_list_p(pel) && (gh_length(pel)==3))) {
	  vc.x[vc.num]=gh_scm2int(gh_car(pel));
	  vc.y[vc.num]=gh_scm2int(gh_cadr(pel));
	  vc.line_style[vc.num]=gh_scm2bool(gh_caddr(pel));
	  vc.num++;
	}
      }

      /* paritally destructive, so find the last face and 
	 append (or mutate if its style is SimpleButton) */
      bf = append_new_face(bf);
      /* copy the vector to bf */
      /* FIXMS stylistically poor to memcpy structs, but should DTRT,
       redo this correctly later. */
      memcpy(&(bf->vector), &vc, sizeof(struct vector_coords));
      bf->style &= ~ButtonFaceTypeMask;
      bf->style |= VectorButton;
    } else {
      /* FIXMS give a better error message */
      scm_wrong_type_arg(__FUNCTION__,3,arg);
    }

  } else if (spec==sym_solid) {
    /* FIXMS give a better error message */
    VALIDATE_COLOR(arg, __FUNCTION__, arg);

    /* fully destructive, so free the face and 
       mutate it */
    FreeButtonFace(dpy,bf);
    bf->u.back = arg;
    bf->style &= ~ButtonFaceTypeMask;
    bf->style |= SolidButton;

  } else if (spec==sym_gradient) {
    int vert_p;
    if (gh_list_p(arg) && gh_length(arg) > 2
	&& ((vert_p=(gh_car(arg)==sym_vertical)) ||
	    gh_car(arg)==sym_horizontal)) {
      char **s_colors;
      int npixels, nsegs, i, sum;
      int *perc;
      Pixel *pixels;
      SCM p;

      npixels=gh_scm2int(gh_cadr(arg));
      arg=gh_cddr(arg);
      nsegs=gh_length(arg)-1;

      if (nsegs < 1 || nsegs > 128) {
	/* FIXMS give a better error message */
	scm_wrong_type_arg(__FUNCTION__,3,arg);
      }
      perc = NEWC(nsegs,int);
      s_colors = NEWC(nsegs+1,char *);

      sum=0;
      for (i = 0, p=arg; i <= nsegs; ++i, p=SCM_CDR(p)) {
	int dummy;
	SCM item_i=gh_car(p);

	if (i< nsegs-1 || gh_list_p(item_i)) {
	  perc[i] = gh_scm2int(gh_cadr(item_i));
	  sum += perc[i];
	  item_i=gh_car(item_i);
	} else if (i < nsegs) {
	  perc[i] = abs(100-sum);
	  sum += perc[i];
	} 
	s_colors[i]=gh_scm2newstr(item_i, &dummy);
      }
      
      if (sum!=100) {
	for (i = 0; i <= nsegs; ++i) {
	  FREE(s_colors[i]);
	}
	FREEC(s_colors);
	FREEC(perc);
	/* FIXMS give a better error message */
	scm_wrong_type_arg(__FUNCTION__,3,arg);
      }

      pixels = AllocNonlinearGradient(s_colors, perc, nsegs, npixels);
      for (i = 0; i <= nsegs; ++i) {
	FREE(s_colors[i]);
      }
      FREEC(perc);
      FREEC(s_colors);

      if (!pixels) {
	/* error: couldn't allocate gradient */
	/* FIXMS give a better error message */
	scm_wrong_type_arg(__FUNCTION__,3,arg);
      }

      /* fully destructive, so free the face and 
	 mutate it */
      FreeButtonFace(dpy,bf);
      
      bf->u.grad.pixels = pixels;
      bf->u.grad.npixels = npixels;
      
      bf->style &= ~ButtonFaceTypeMask;
      if (vert_p) {
	bf->style |= VGradButton;
      } else {
	bf->style |= HGradButton;
      }
    } else {
      /* FIXMS give a better error message */
      scm_wrong_type_arg(__FUNCTION__,3,arg);
    }
  } else if (spec==sym_pixmap) {
    int tiled_p;
    int mini_p=0;
    
    tiled_p=(gh_list_p(arg) && gh_length(arg) == 2 &&
	     gh_car(arg)==sym_tiled &&
	     (gh_string_p(gh_cadr(arg)) || IMAGE_P(gh_cadr(arg))));
    if (tiled_p || gh_string_p(arg) || (mini_p=(arg==sym_mini_program_icon)) ||
	IMAGE_P(arg)) {
      SCM image = SCM_BOOL_F;
      
      if (tiled_p) {
	arg=gh_cadr(arg);
      }

      /* FIXGJB: I think this should only take pixmap objects;
	 if we want scheme sugar for converting strings into
	 pixmaps, that's fine, too;  this works well now, though,
	 so maybe it's only work changing when make-face is
	 wrapped to use keyword arguments */
      if (!mini_p) {
	if (IMAGE_P(arg)) {
	  image = arg;
	} else {
	  image = make_image(arg);
	}
	if (image==SCM_BOOL_F) {
	  /* signal an error: couldn't load picture */
	  /* FIXMS give a better error message */
	  scwm_msg(WARN,__FUNCTION__, "Image not found for argument #%d",3);
	}
      }
      
      if (tiled_p) {
	/* fully destructive, so free the face and 
	   mutate it */
	FreeButtonFace(dpy,bf);
      } else {
	/* paritally destructive, so find the last face and 
	   append (or mutate if its style is SimpleButton) */
	bf=append_new_face(bf);
      }
      bf->u.image = image;
      bf->style &= ~ButtonFaceTypeMask;
      if (mini_p) {
	bf->style |= MiniIconButton;
      } else if (tiled_p) {
	bf->style |= TiledPixmapButton;
      } else {
	bf->style |= PixmapButton;
      }
    } else {
      /* FIXMS give a better error message */
      scwm_msg(WARN,__FUNCTION__, "Image not found for argument #%d",3);
    }
  } else {
    /* FIXMS give a better error message */
    scm_wrong_type_arg(__FUNCTION__,2,arg);
  }

}


ButtonFace *append_new_face(ButtonFace *bf) {
  while (bf->next!=NULL) {
    bf=bf->next;
  }
  if ((bf->style & ButtonFaceTypeMask) == SimpleButton) {
    return bf;
  } else {
    ButtonFace *retval = NEW(ButtonFace);
    bf->next=retval;
    retval->style=SimpleButton;
    retval->next=NULL;
    return retval;
  }
}

extern ScwmDecor *cur_decor;


SCWM_PROC(set_title_face_x, "set-title-face!", 1 , 2, 0,
          (SCM active_up, SCM active_down, SCM inactive))
     /** Set the titlebar faces for the various window states.
In the current decor, use ACTIVE-UP as the face for the title
bar when active and not pressed in. Use ACTIVE-DOWN when the title bar
is active and pressed in, and INACTIVE when the window is
inactive. Both INACTIVE and ACTIVE-DOWN default to ACTIVE-UP when not
specified. Note that ACTIVE-DOWN will magically reverse the sense of
the relief flag, so if your titlebar bar is raised in the ACTIVE-UP
state, it will be sunk in the ACTIVE-DOWN state by default.  */
#define FUNC_NAME s_set_title_face_x
{
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (!FACEP(active_up)) {
    scm_wrong_type_arg(FUNC_NAME,1,active_up);
  }

  if (active_down==SCM_UNDEFINED) {
    active_down=active_up;
  } else if (!FACEP(active_down)) {
    scm_wrong_type_arg(FUNC_NAME,2,active_down);
  }

  if (inactive==SCM_UNDEFINED) {
    inactive=active_up;
  } else if (!FACEP(inactive)) {
    scm_wrong_type_arg(FUNC_NAME,3,inactive);
  }

  fl->titlebar.state[ActiveUp]=BUTTONFACE(active_up);
  fl->titlebar.state[ActiveDown]=BUTTONFACE(active_down);
  fl->titlebar.state[Inactive]=BUTTONFACE(inactive);

  redraw_titlebars(fl,0);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(set_button_face_x, "set-button-face!", 2, 2, 0,
          (SCM button, SCM active_up, SCM active_down, SCM inactive) )
     /** Set the button faces for the various window states.
In the current decor, use ACTIVE-UP as the face for the
button specified by the integer BUTTON when active and not pressed
in. Use ACTIVE-DOWN when BUTTON is active and pressed in, and INACTIVE
when the window is inactive. Both INACTIVE and ACTIVE-DOWN default to
ACTIVE-UP when not specified. Note that ACTIVE-DOWN will magically
reverse the sense of the relief flag, so if the button is raised in
the ACTIVE-UP state, it will be sunk in the ACTIVE-DOWN state by
default.  */
#define FUNC_NAME s_set_button_face_x
{
  int n;
  int left_p;

  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  n=0;

  if (!gh_number_p(button) || (n=gh_scm2int(button)) < 1 || n > 10) {
    scm_wrong_type_arg(FUNC_NAME,1,button);
  }

  if (!FACEP(active_up)) {
    scm_wrong_type_arg(FUNC_NAME,2,active_up);
  }

  if (active_down==SCM_UNDEFINED) {
    active_down=active_up;
  } else if (!FACEP(active_down)) {
    scm_wrong_type_arg(FUNC_NAME,3,active_down);
  }

  if (inactive==SCM_UNDEFINED) {
    inactive=active_up;
  } else if (!FACEP(inactive)) {
    scm_wrong_type_arg(FUNC_NAME,4,inactive);
  }

  left_p = n % 2;
  n = n / 2;

  if (left_p) {
    fl->left_buttons[n].state[ActiveUp]=BUTTONFACE(active_up);
    fl->left_buttons[n].state[ActiveDown]=BUTTONFACE(active_down);
    fl->left_buttons[n].state[Inactive]=BUTTONFACE(inactive);
  } else {
    n = n - 1;
    fl->right_buttons[n].state[ActiveUp]=BUTTONFACE(active_up);
    fl->right_buttons[n].state[ActiveDown]=BUTTONFACE(active_down);
    fl->right_buttons[n].state[Inactive]=BUTTONFACE(inactive);    
  }

  redraw_borders(fl); 

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_button_mwm_flag_x, "set-button-mwm-flag!", 2, 0, 0,
          (SCM button, SCM flag) )
     /** Specify the Mwm flag for BUTTON.
If FLAG is #t, the button's relief pattern (if any) will appear to
reverse in depth sense (i.e., flip from sunken in to extruding out)
when the window is maximized. */
#define FUNC_NAME s_set_button_mwm_flag_x
{
  int n;
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  n=0;

  if (!gh_number_p(button) || (n=gh_scm2int(button)) < 1 ||
      n > 10) {
    scm_wrong_type_arg(FUNC_NAME,1,button);
  }

  if(flag==SCM_BOOL_T) {
    fl->left_buttons[n].flags |= MWMButton;    
  } else if (flag==SCM_BOOL_F) {
    fl->left_buttons[n].flags &= ~MWMButton;    
  } else {
    scm_wrong_type_arg(FUNC_NAME,1,flag);
  }

  redraw_borders(fl);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(set_border_face_x, "set-border-face!", 1, 1, 0,
          (SCM active, SCM inactive) )
     /** Set the face for the border In the current decor.
Use ACTIVE as the face for the border when the window is active. Use
INACTIVE when the window is inactive. INACTIVE defaults to the same as
ACTIVE when not specified. */
#define FUNC_NAME s_set_border_face_x
{
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (!FACEP(active)) {
    scm_wrong_type_arg(FUNC_NAME,1,active);
  }

  if (inactive==SCM_UNDEFINED) {
    inactive=active;
  } else if (!FACEP(inactive)) {
    scm_wrong_type_arg(FUNC_NAME,2,inactive);
  }

  fl->BorderStyle.active=BUTTONFACE(active);
  fl->BorderStyle.inactive=BUTTONFACE(inactive);

  redraw_borders(fl); 

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* fvwm's FreeButton face was buggy and breaks with garbage collection
   and other stuff */
void 
FreeButtonFace(Display * dpy, ButtonFace * bf)
{
  switch (bf->style & ButtonFaceTypeMask) {
  case HGradButton:
  case VGradButton:
    /* - should we check visual is not TrueColor before doing this? 

       XFreeColors(dpy, Scr.ScwmRoot.attr.colormap, 
       bf->u.grad.pixels, bf->u.grad.npixels,
       AllPlanes); */
    FREEC(bf->u.grad.pixels);
    bf->u.grad.pixels = NULL;
    break;

  case PixmapButton:
  case TiledPixmapButton:
    bf->u.image=SCM_UNDEFINED;
    break;
  default:
    break;
  }
  /* delete any compound styles */
  if (bf->next) {
    FreeButtonFace(dpy, bf->next);
    FREE(bf->next);
  }
  bf->next = NULL;
  bf->style &= ~ButtonFaceTypeMask;
  bf->style |= SimpleButton;
}

MAKE_SMOBFUNS(face);

void 
init_face()
{
  int i;
  /* This needs to be done before the faces are created, below */
  REGISTER_SCWMSMOBFUNS(face);

  /* these should probably be exported as Scheme variables */
  default_titlebar_face=make_face(SCM_EOL, SCM_EOL);
  scm_protect_object(default_titlebar_face);

  default_border_face=make_face(SCM_EOL, SCM_EOL);
  scm_protect_object(default_border_face);

  for (i=0; i<5; i++) {
    default_lbutton_face[i]= make_face(SCM_EOL, SCM_EOL);
    LoadDefaultLeftButton(BUTTONFACE(default_lbutton_face[i]),i);
    scm_protect_object(default_lbutton_face[i]);
    default_rbutton_face[i]= make_face(SCM_EOL, SCM_EOL);
    LoadDefaultRightButton(BUTTONFACE(default_rbutton_face[i]),i);
    scm_protect_object(default_rbutton_face[i]);
  }

#ifndef SCM_MAGIC_SNARFER
#include "face.x"
#endif
}

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
