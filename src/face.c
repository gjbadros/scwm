
#include <config.h>
#include <guile/gh.h>
#include "scwm.h"
#include "misc.h"
#include "screen.h"
#include "window.h"
#include "face.h"
#include "color.h"

#ifndef HAVE_GH_LENGTH
#define gh_length gh_list_length
#endif /* HAVE_GH_LENGTH */

long scm_tc16_scwm_face;

int 
print_face(SCM obj, SCM port, scm_print_state * pstate)
{
#ifdef HAVE_SCM_PUTS
  scm_puts("#<face ", port);
  scm_write(gh_int2scm((int)FACE(obj)), port);
  scm_putc('>', port);
#else /* !HAVE_SCM_PUTS */
  scm_gen_puts(scm_regular_port, "#<face ", port);
  scm_write(gh_int2scm((int)FACE(obj)), port);
  scm_gen_putc('>', port);
#endif /* HAVE_SCM_PUTS */

  return 1;
}

void FreeButtonFace(Display * dpy, ButtonFace * bf);


size_t
free_face(SCM obj)
{
  FreeButtonFace(dpy,BUTTONFACE(obj));
  free(BUTTONFACE(obj));
  free(FACE(obj));
  return (0);
}


SCM default_titlebar_face;
SCM default_border_face;
SCM default_lbutton_face[5];
SCM default_rbutton_face[5];


void set_face_flag_x(SCM face, SCM flag, SCM flagval);
void add_spec_to_face_x(SCM face, SCM spec, SCM arg);

SCM 
make_face(SCM flags, SCM specs) {
  SCM answer;
  scwm_face *sf;
  ButtonFace *bf;
  SCM p,flag,spec;

  if (!gh_list_p(flags)) {
    scm_wrong_type_arg("make-face",1,flags);
  }

  if (!gh_list_p(specs)) {
    scm_wrong_type_arg("make-face",2,specs);
  }

  bf=(ButtonFace *)calloc(1,sizeof(ButtonFace));
  sf=(scwm_face *)malloc(sizeof(scwm_face));
  sf->bf=bf;

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
      scm_wrong_type_arg("make-face",1,flags);          
    }
  }

  for (p=specs; p!=SCM_EOL; p=SCM_CDR(p)) {
    spec = gh_car(p);
 
    if (gh_list_p(spec) && gh_length(spec)==2) {
      add_spec_to_face_x(answer,gh_car(spec),gh_cadr(spec));
    } else {
      /* Bad flag specifier error */
      scm_wrong_type_arg("make-face",1,specs);          
    }
  }

  return answer;
}

/* 
 * possible flags and values: 
 * 'clear            : #t      ; not implemented for now, of questionable
                               ; usefulness when button styles are not
			       ; mutable.
 * 'justify          : 'left|'right|'center
 * 'vertical-justify : 'top|'bottom|'center
 * 'relief           : 'flat|'sunk|'raised
 * 'use-style-of     : 'title|'border|#f
 * 'hidden-handles   : #t|#f
 * 'no-inset         : #t|#f
 */

SCM sym_clear, sym_justify, sym_vertical_justify, sym_relief,
  sym_use_style_of, sym_hidden_handles, sym_no_inset, sym_left,
  sym_right, sym_center, sym_top, sym_bottom, sym_flat, sym_sunk,
  sym_raised, sym_title, sym_border;


/* FIXMS Probably the right way to do this is to keep a hash table of
pointers to functions that know how to set each individual flag, but
this ugly code is almost certainly more compact and quite possibly
faster. If only C had closures... */

void set_face_flag_x(SCM face, SCM flag, SCM
flagval) { ButtonFace *bf;

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
      scm_wrong_type_arg("set_flag_in_face",3,flagval);    
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
      scm_wrong_type_arg("set_flag_in_face",3,flagval);    
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
      scm_wrong_type_arg("set_flag_in_face",3,flagval);    
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
      scm_wrong_type_arg("set_flag_in_face",3,flagval);    
    }

  } else if (flag==sym_hidden_handles) {
    if (flagval==SCM_BOOL_T) {
      bf->style |= HiddenHandles;
    } else if (flagval==SCM_BOOL_F) {
      bf->style &= ~HiddenHandles;
    } else {
      /* FIXMS: use something more accurate. */
      scm_wrong_type_arg("set_flag_in_face",3,flagval);    
    }

  } else if (flag==sym_no_inset) {
    if (flagval==SCM_BOOL_T) {
      bf->style |= NoInset;
    } else if (flagval==SCM_BOOL_F) {
      bf->style &= ~NoInset;
    } else {
      /* FIXMS: use something more accurate. */
      scm_wrong_type_arg("set_flag_in_face",3,flagval);    
    }

  } else {
    /* FIXMS: use something more accurate. */
    scm_wrong_type_arg("set_flag_in_face",2,flagval);    
  }
}


#ifdef XPM
extern char *PixmapPath;
#endif

extern char *IconPath;

ButtonFace *append_new_face(ButtonFace *bf);

/*
 * Face specifiers and possible values :
 *  
 * '(simple #t)                                  ;; non-destructive
 *                                               ;; perhaps non-pointful?
 * '(default <N>)                                ;; fully destructive
 *                                               ;; perhaps there should
 *                                               ;; be variables for
 *                                               ;; these instead or
 *                                               ;; something?
 * In any case, skip simple and default for now.
 * '(relief-pattern ((<X> <Y> <BOOL>) ...))      ;; partially destructive
 * '(solid COLOR)                                ;; fully destructive
 * '(gradient (horizontal|vertical <NCOLORS> (<COLOR> <PERCENT>)* <FINAL>))
 *                                               ;; fully destructive
 * '(pixmap mini-icon|<PIXMAP>|(tiled <PIXMAP>)) ;; partially destructive,
 *                                               ;; except tiled (fully)
 */

/*
 * FIXMS gradients and pictures should be exposed as scheme types, that
 * would eliminate a lot of the hackery here. Not sure if relief patterns
 * are fundamental enough to be a type...
 */


SCM sym_relief_pattern, sym_solid, sym_gradient, sym_horizontal,
  sym_vertical, sym_pixmap, sym_mini_icon, sym_tiled;


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
	if (!(gh_list_p(pel) && (gh_length(pel)==3))) {
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
      scm_wrong_type_arg("add_spec_to_face_x",3,arg);
    }

  } else if (spec==sym_solid) {
    if (gh_string_p(arg)) {
      arg=load_color(arg);
    } else if (!(SCM_NIMP(arg) && COLORP(arg))) {
      /* FIXMS give a better error message */
      scm_wrong_type_arg("add_spec_to_face_x",3,arg);
    }

    /* fully destructive, so free the face and 
       mutate it */
    FreeButtonFace(dpy,bf);
    bf->u.back = COLOR(arg);
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
	scm_wrong_type_arg("add_spec_to_face_x",3,arg);
      }
      perc = (int *) safemalloc(nsegs*sizeof(int));
      s_colors = (char **) safemalloc((nsegs+1)*sizeof(char *));

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
	  free(s_colors[i]);
	}
	free(s_colors);
	free(perc);
	/* FIXMS give a better error message */
	scm_wrong_type_arg("add_spec_to_face_x",3,arg);
      }

      pixels = AllocNonlinearGradient(s_colors, perc, nsegs, npixels);
      for (i = 0; i <= nsegs; ++i) {
	free(s_colors[i]);
      }
      free(s_colors);
      free(perc);

      if (!pixels) {
	/* error: couldn't allocate gradient */
	/* FIXMS give a better error message */
	scm_wrong_type_arg("add_spec_to_face_x",3,arg);
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
      scm_wrong_type_arg("add_spec_to_face_x",3,arg);
    }
  } else if (spec==sym_pixmap) {
    int tiled_p;
    int mini_p=0;
    
    tiled_p=(gh_list_p(arg) && gh_length(arg) ==2 &&
	     gh_car(arg)==sym_tiled && gh_string_p(gh_cadr(arg)));
    if (tiled_p || gh_string_p(arg) || (mini_p=(arg==sym_mini_icon))) {
      Picture *p = NULL;
      
      if (tiled_p) {
	arg=gh_cadr(arg);
      }

      if (!mini_p) {
	char *pixmap;
	int dummy;

	pixmap=gh_scm2newstr(arg,&dummy);
	p = CachePicture(dpy, Scr.Root,
			 IconPath,
#ifdef XPM
			 PixmapPath,
#else
			 NULL,
#endif
			 pixmap);
	if (p==NULL) {
	  /* signal an error: couldn't load picture */
	  /* FIXMS give a better error message */
	  scm_wrong_type_arg("add_spec_to_face_x",3,arg);
	}
	free(pixmap);
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
      bf->u.p = p;
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
      scm_wrong_type_arg("add_spec_to_face_x",3,arg);
    }
  } else {
    /* FIXMS give a better error message */
    scm_wrong_type_arg("add_spec_to_face_x",2,arg);
  }

}


ButtonFace *append_new_face(ButtonFace *bf) {
  while (bf->next!=NULL) {
    bf=bf->next;
  }
  if ((bf->style & ButtonFaceTypeMask) == SimpleButton) {
    return bf;
  } else {
    ButtonFace *retval;
    retval=(ButtonFace *)calloc(1,sizeof(ButtonFace));
    bf->next=retval;
    retval->style=SimpleButton;
    retval->next=NULL;
    return retval;
  }
}



void 
init_face()
{
  int i;

  sym_clear = gh_symbol2scm("clear");
  scm_protect_object(sym_clear);
  sym_justify = gh_symbol2scm("justify");
  scm_protect_object(sym_justify);
  sym_vertical_justify = gh_symbol2scm("vertical-justify");
  scm_protect_object(sym_vertical_justify);
  sym_relief = gh_symbol2scm("relief");
  scm_protect_object(sym_relief);
  sym_use_style_of = gh_symbol2scm("use-style-of");
  scm_protect_object(sym_use_style_of);
  sym_hidden_handles = gh_symbol2scm("hidden-handles");
  scm_protect_object(sym_hidden_handles);
  sym_no_inset = gh_symbol2scm("no-inset");
  scm_protect_object(sym_no_inset);
  sym_left = gh_symbol2scm("left");
  scm_protect_object(sym_left);
  sym_right = gh_symbol2scm("right");
  scm_protect_object(sym_right);
  sym_center = gh_symbol2scm("center");
  scm_protect_object(sym_center);
  sym_top = gh_symbol2scm("top");
  scm_protect_object(sym_top);
  sym_bottom = gh_symbol2scm("bottom");
  scm_protect_object(sym_bottom);
  sym_flat = gh_symbol2scm("flat");
  scm_protect_object(sym_flat);
  sym_sunk = gh_symbol2scm("sunk");
  scm_protect_object(sym_sunk);
  sym_raised = gh_symbol2scm("raised");
  scm_protect_object(sym_raised);
  sym_title = gh_symbol2scm("title");
  scm_protect_object(sym_title);
  sym_border = gh_symbol2scm("border");
  scm_protect_object(sym_border);

  sym_relief_pattern =gh_symbol2scm("relief-pattern");
  scm_protect_object(sym_relief_pattern);
  sym_solid =gh_symbol2scm("solid");
  scm_protect_object(sym_solid);
  sym_gradient =gh_symbol2scm("gradient");
  scm_protect_object(sym_gradient);
  sym_horizontal =gh_symbol2scm("horizontal");
  scm_protect_object(sym_horizontal);
  sym_vertical =gh_symbol2scm("vertical");
  scm_protect_object(sym_vertical);
  sym_pixmap =gh_symbol2scm("pixmap");
  scm_protect_object(sym_pixmap);
  sym_mini_icon =gh_symbol2scm("mini-icon");
  scm_protect_object(sym_mini_icon);
  sym_tiled = gh_symbol2scm("tiled");
  scm_protect_object(sym_tiled);

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


}


/* fvwm's FreeButton face was buggy and breaks with garbage collection
   and other stuff */
void 
FreeButtonFace(Display * dpy, ButtonFace * bf)
{
  switch (bf->style & ButtonFaceTypeMask) {
#ifdef GRADIENT_BUTTONS
  case HGradButton:
  case VGradButton:
    /* - should we check visual is not TrueColor before doing this? 

       XFreeColors(dpy, Scr.ScwmRoot.attr.colormap, 
       bf->u.grad.pixels, bf->u.grad.npixels,
       AllPlanes); */
    free(bf->u.grad.pixels);
    bf->u.grad.pixels = NULL;
    break;
#endif

#ifdef PIXMAP_BUTTONS
  case PixmapButton:
  case TiledPixmapButton:
    if (bf->u.p)
      DestroyPicture(dpy, bf->u.p);
    bf->u.p = NULL;
    break;
#endif
  default:
    break;
  }
#ifdef MULTISTYLE
  /* delete any compound styles */
  if (bf->next) {
    FreeButtonFace(dpy, bf->next);
    free(bf->next);
  }
  bf->next = NULL;
#endif
  bf->style &= ~ButtonFaceTypeMask;
  bf->style |= SimpleButton;
}




#if 0

Boolean ReadButtonFace(char *s, ButtonFace * bf, int button, int verbose);
void FreeButtonFace(Display * dpy, ButtonFace * bf);

#ifdef BORDERSTYLE
/****************************************************************************
 *
 *  Sets the border style (veliaa@rpi.edu)
 *
 ****************************************************************************/
void 
SetBorderStyle(XEvent * eventp, Window w, ScwmWindow * tmp_win,
	       unsigned long context, char *action, int *Module)
{
  char *parm = NULL, *prev = action;

#ifdef USEDECOR
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

#else
  ScwmDecor *fl = &Scr.DefaultDecor;

#endif

  action = GetNextToken(action, &parm);
  while (parm && parm[0]) {
    if (mystrncasecmp(parm, "active", 6) == 0
	|| mystrncasecmp(parm, "inactive", 8) == 0) {
      int len;
      char *end, *tmp;
      ButtonFace tmpbf, *bf;

      tmpbf.style = SimpleButton;
#ifdef MULTISTYLE
      tmpbf.next = NULL;
#endif
      if (mystrncasecmp(parm, "active", 6) == 0)
	bf = &fl->BorderStyle.active;
      else
	bf = &fl->BorderStyle.inactive;
      while (isspace(*action))
	++action;
      if ('(' != *action) {
	if (!*action) {
	  scwm_msg(ERR, "SetBorderStyle",
		   "error in %s border specification", parm);
	  free(parm);
	  return;
	}
	free(parm);
	if (ReadButtonFace(action, &tmpbf, -1, True)) {
	  FreeButtonFace(dpy, bf);
	  *bf = tmpbf;
	}
	break;
      }
      end = strchr(++action, ')');
      if (!end) {
	scwm_msg(ERR, "SetBorderStyle",
		 "error in %s border specification", parm);
	free(parm);
	return;
      }
      len = end - action + 1;
      tmp = safemalloc(len);
      strncpy(tmp, action, len - 1);
      tmp[len - 1] = 0;
      ReadButtonFace(tmp, bf, -1, True);
      free(tmp);
      action = end + 1;
    } else if (strcmp(parm, "--") == 0) {
      if (ReadButtonFace(prev, &fl->BorderStyle.active, -1, True))
	ReadButtonFace(prev, &fl->BorderStyle.inactive, -1, False);
      free(parm);
      break;
    } else {
      ButtonFace tmpbf;

      tmpbf.style = SimpleButton;
#ifdef MULTISTYLE
      tmpbf.next = NULL;
#endif
      if (ReadButtonFace(prev, &tmpbf, -1, True)) {
	FreeButtonFace(dpy, &fl->BorderStyle.active);
	fl->BorderStyle.active = tmpbf;
	ReadButtonFace(prev, &fl->BorderStyle.inactive, -1, False);
      }
      free(parm);
      break;
    }
    free(parm);
    prev = action;
    action = GetNextToken(action, &parm);
  }
}
#endif

char *ReadTitleButton(char *s, TitleButton * tb, Boolean append, int button);

#if defined(MULTISTYLE) && defined(EXTENDED_TITLESTYLE)
/*****************************************************************************
 * 
 * Appends a titlestyle (veliaa@rpi.edu)
 *
 ****************************************************************************/
void 
AddTitleStyle(XEvent * eventp, Window w, ScwmWindow * tmp_win,
	      unsigned long context, char *action, int *Module)
{
#ifdef USEDECOR
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

#else
  ScwmDecor *fl = &Scr.DefaultDecor;

#endif
  char *parm = NULL, *prev;

  prev = action;
  action = GetNextToken(action, &parm);
  while (parm && parm[0] != '\0') {
    if (!(action = ReadTitleButton(prev, &fl->titlebar, True, -1))) {
      free(parm);
      break;
    }
    prev = action;
    action = GetNextToken(action, &parm);
  }
}
#endif /* MULTISTYLE && EXTENDED_TITLESTYLE */

void 
SetTitleStyle(XEvent * eventp, Window w, ScwmWindow * tmp_win,
	      unsigned long context, char *action, int *Module)
{
  char *parm = NULL, *prev = action;

#ifdef USEDECOR
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

#else
  ScwmDecor *fl = &Scr.DefaultDecor;

#endif

  action = GetNextToken(action, &parm);
  while (parm && parm[0] != '\0') {
    if (mystrncasecmp(parm, "centered", 8) == 0) {
      fl->titlebar.flags &= ~HOffCenter;
    } else if (mystrncasecmp(parm, "leftjustified", 13) == 0) {
      fl->titlebar.flags |= HOffCenter;
      fl->titlebar.flags &= ~HRight;
    } else if (mystrncasecmp(parm, "rightjustified", 14) == 0) {
      fl->titlebar.flags |= HOffCenter | HRight;
    }
#ifdef EXTENDED_TITLESTYLE
    else if (mystrncasecmp(parm, "height", 6) == 0) {
      int height, next;

      if (sscanf(action, "%d%n", &height, &next) > 0
	  && height > 4
	  && height <= 256) {
	int x, y, w, h, extra_height;
	ScwmWindow *tmp = Scr.ScwmRoot.next, *hi = Scr.Hilite;

	extra_height = fl->TitleHeight;
	fl->TitleHeight = height;
	extra_height -= fl->TitleHeight;

	fl->WindowFont.y = fl->WindowFont.font->ascent
	  + (height - (fl->WindowFont.font->ascent
		       + fl->WindowFont.font->descent + 3)) / 2;
	if (fl->WindowFont.y < fl->WindowFont.font->ascent)
	  fl->WindowFont.y = fl->WindowFont.font->ascent;

	tmp = Scr.ScwmRoot.next;
	hi = Scr.Hilite;
	while (tmp != NULL) {
	  if (!(tmp->flags & TITLE)
#ifdef USEDECOR
	      || (tmp->fl != fl)
#endif
	    ) {
	    tmp = tmp->next;
	    continue;
	  }
	  x = tmp->frame_x;
	  y = tmp->frame_y;
	  w = tmp->frame_width;
	  h = tmp->frame_height - extra_height;
	  tmp->frame_x = 0;
	  tmp->frame_y = 0;
	  tmp->frame_height = 0;
	  tmp->frame_width = 0;
	  SetupFrame(tmp, x, y, w, h, True);
	  SetTitleBar(tmp, True, True);
	  SetTitleBar(tmp, False, True);
	  tmp = tmp->next;
	}
	SetTitleBar(hi, True, True);
      } else
	scwm_msg(ERR, "SetTitleStyle",
		 "bad height argument (height must be from 5 to 256)");
      action += next;
    } else {
      if (!(action = ReadTitleButton(prev, &fl->titlebar, False, -1))) {
	free(parm);
	break;
      }
    }
#else /* ! EXTENDED_TITLESTYLE */
    else if (strcmp(parm, "--") == 0) {
      if (!(action = ReadTitleButton(prev, &fl->titlebar, False, -1))) {
	free(parm);
	break;
      }
    }
#endif /* EXTENDED_TITLESTYLE */
    free(parm);
    prev = action;
    action = GetNextToken(action, &parm);
  }
}				/* SetTitleStyle */

/*****************************************************************************
 * 
 * Changes a button decoration style (changes by veliaa@rpi.edu)
 *
 ****************************************************************************/
void 
ButtonStyle(XEvent * eventp, Window junk, ScwmWindow * tmp_win,
	    unsigned long context, char *action, int *Module)
{
  int button = 0, n;
  int multi = 0;
  char *text = action, *prev;
  char *parm = NULL;
  TitleButton *tb = NULL;

#ifdef USEDECOR
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

#else
  ScwmDecor *fl = &Scr.DefaultDecor;

#endif

  text = GetNextToken(text, &parm);
  if (parm && isdigit(*parm))
    button = atoi(parm);

  if ((parm == NULL) || (button > 10) || (button < 0)) {
    scwm_msg(ERR, "ButtonStyle", "Bad button style (1) in line %s", action);
    free(parm);
    return;
  }
  if (!isdigit(*parm)) {
    if (mystrcasecmp(parm, "left") == 0)
      multi = 1;		/* affect all left buttons */
    else if (mystrcasecmp(parm, "right") == 0)
      multi = 2;		/* affect all right buttons */
    else if (mystrcasecmp(parm, "all") == 0)
      multi = 3;		/* affect all buttons */
    else {
      /* we're either resetting buttons or
         an invalid button set was specified */
      if (mystrcasecmp(parm, "reset") == 0)
	ResetAllButtons(fl);
      else
	scwm_msg(ERR, "ButtonStyle", "Bad button style (2) in line %s", action);
      free(parm);
      return;
    }
  }
  free(parm);
  if (multi == 0) {
    /* a single button was specified */
    if (button == 10)
      button = 0;
    /* which arrays to use? */
    n = button / 2;
    if ((n * 2) == button) {
      /* right */
      n = n - 1;
      if (n < 0)
	n = 4;
      tb = &fl->right_buttons[n];
    } else {
      /* left */
      tb = &fl->left_buttons[n];
    }
  }
  prev = text;
  text = GetNextToken(text, &parm);
  while (parm && parm[0] != '\0') {
    if (strcmp(parm, "-") == 0) {
      char *tok;

      text = GetNextToken(text, &tok);
      while (tok && tok[0]) {
	int set = 1;

	if (*tok == '!') {	/* flag negate */
	  set = 0;
	  ++tok;
	}
	if (mystrncasecmp(tok, "Clear", 5) == 0) {
	  int i;

	  if (multi) {
	    if (multi & 1)
	      for (i = 0; i < 5; ++i)
		if (set)
		  fl->left_buttons[i].flags = 0;
		else
		  fl->left_buttons[i].flags = ~0;
	    if (multi & 2)
	      for (i = 0; i < 5; ++i)
		if (set)
		  fl->right_buttons[i].flags = 0;
		else
		  fl->right_buttons[i].flags = ~0;
	  } else if (set)
	    tb->flags = 0;
	  else
	    tb->flags = ~0;
	} else if (mystrncasecmp(tok, "MWMButton", 9) == 0) {
	  if (!multi)
	    if (set)
	      tb->flags |= MWMButton;
	    else
	      tb->flags &= ~MWMButton;
	} else
	  scwm_msg(ERR, "ButtonStyle",
		   "unknown title button flag %s -- line: %s", tok, text);
	if (set)
	  free(tok);
	else
	  free(tok - 1);
	text = GetNextToken(text, &tok);
      }
      free(parm);
      break;
    } else {
      if (multi) {
	int i;

	if (multi & 1)
	  for (i = 0; i < 5; ++i)
	    text = ReadTitleButton(prev, &fl->left_buttons[i], False, i * 2 + 1);
	if (multi & 2)
	  for (i = 0; i < 5; ++i)
	    text = ReadTitleButton(prev, &fl->right_buttons[i], False, i * 2);
      } else if (!(text = ReadTitleButton(prev, tb, False, button))) {
	free(parm);
	break;
      }
    }
    free(parm);
    prev = text;
    text = GetNextToken(text, &parm);
  }
}

#ifdef MULTISTYLE
/*****************************************************************************
 * 
 * Appends a button decoration style (veliaa@rpi.edu)
 *
 ****************************************************************************/
void 
AddButtonStyle(XEvent * eventp, Window junk, ScwmWindow * tmp_win,
	       unsigned long context, char *action, int *Module)
{
  int button = 0, n;
  int multi = 0;
  char *text = action, *prev;
  char *parm = NULL;
  TitleButton *tb = NULL;

#ifdef USEDECOR
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

#else
  ScwmDecor *fl = &Scr.DefaultDecor;

#endif

  text = GetNextToken(text, &parm);
  if (parm && isdigit(*parm))
    button = atoi(parm);

  if ((parm == NULL) || (button > 10) || (button < 0)) {
    scwm_msg(ERR, "ButtonStyle", "Bad button style (1) in line %s", action);
    free(parm);
    return;
  }
  if (!isdigit(*parm)) {
    if (mystrcasecmp(parm, "left") == 0)
      multi = 1;		/* affect all left buttons */
    else if (mystrcasecmp(parm, "right") == 0)
      multi = 2;		/* affect all right buttons */
    else if (mystrcasecmp(parm, "all") == 0)
      multi = 3;		/* affect all buttons */
    else {
      /* we're either resetting buttons or
         an invalid button set was specified */
      if (mystrcasecmp(parm, "reset") == 0)
	ResetAllButtons(fl);
      else
	scwm_msg(ERR, "ButtonStyle", "Bad button style (2) in line %s", action);
      free(parm);
      return;
    }
  }
  free(parm);
  if (multi == 0) {
    /* a single button was specified */
    if (button == 10)
      button = 0;
    /* which arrays to use? */
    n = button / 2;
    if ((n * 2) == button) {
      /* right */
      n = n - 1;
      if (n < 0)
	n = 4;
      tb = &fl->right_buttons[n];
    } else {
      /* left */
      tb = &fl->left_buttons[n];
    }
  }
  prev = text;
  text = GetNextToken(text, &parm);
  while (parm && parm[0] != '\0') {
    if (multi) {
      int i;

      if (multi & 1)
	for (i = 0; i < 5; ++i)
	  text = ReadTitleButton(prev, &fl->left_buttons[i], True, i * 2 + 1);
      if (multi & 2)
	for (i = 0; i < 5; ++i)
	  text = ReadTitleButton(prev, &fl->right_buttons[i], True, i * 2);
    } else if (!(text = ReadTitleButton(prev, tb, True, button))) {
      free(parm);
      break;
    }
    free(parm);
    prev = text;
    text = GetNextToken(text, &parm);
  }
}
#endif /* MULTISTYLE */




void 
FreeButtonFace(Display * dpy, ButtonFace * bf)
{
  switch (bf->style) {
#ifdef GRADIENT_BUTTONS
  case HGradButton:
  case VGradButton:
    /* - should we check visual is not TrueColor before doing this? 

       XFreeColors(dpy, Scr.ScwmRoot.attr.colormap, 
       bf->u.grad.pixels, bf->u.grad.npixels,
       AllPlanes); */
    free(bf->u.grad.pixels);
    bf->u.grad.pixels = NULL;
    break;
#endif

#ifdef PIXMAP_BUTTONS
  case PixmapButton:
  case TiledPixmapButton:
    if (bf->u.p)
      DestroyPicture(dpy, bf->u.p);
    bf->u.p = NULL;
    break;
#endif
  default:
    break;
  }
#ifdef MULTISTYLE
  /* delete any compound styles */
  if (bf->next) {
    FreeButtonFace(dpy, bf->next);
    free(bf->next);
  }
  bf->next = NULL;
#endif
  bf->style = SimpleButton;
}

/*****************************************************************************
 * 
 * Reads a button face line into a structure (veliaa@rpi.edu)
 *
 ****************************************************************************/
Boolean 
ReadButtonFace(char *s, ButtonFace * bf, int button, int verbose)
{
  int offset;
  char style[256], *file;
  char *action = s;

  if (sscanf(s, "%s%n", style, &offset) < 1) {
    if (verbose)
      scwm_msg(ERR, "ReadButtonFace", "error in face: %s", action);
    return False;
  }
  if (mystrncasecmp(style, "--", 2) != 0) {
    s += offset;

    FreeButtonFace(dpy, bf);

    /* determine button style */
    if (mystrncasecmp(style, "Simple", 6) == 0) {
      bf->style = SimpleButton;
    } else if (mystrncasecmp(style, "Default", 7) == 0) {
      int b = -1, n = sscanf(s, "%d%n", &b, &offset);

      if (n < 1) {
	if (button == -1) {
	  if (verbose)
	    scwm_msg(ERR, "ReadButtonFace",
		     "need default button number to load");
	  return False;
	}
	b = button;
      }
      s += offset;
      if ((b > 0) && (b <= 10))
	LoadDefaultButton(bf, b);
      else {
	if (verbose)
	  scwm_msg(ERR, "ReadButtonFace",
		   "button number out of range: %d", b);
	return False;
      }
    }
#ifdef VECTOR_BUTTONS
    else if (mystrncasecmp(style, "Vector", 6) == 0 ||
	     (strlen(style) <= 2 && isdigit(*style))) {
      /* normal coordinate list button style */
      int i, num_coords, num;
      struct vector_coords *vc = &bf->vector;

      /* get number of points */
      if (mystrncasecmp(style, "Vector", 6) == 0) {
	num = sscanf(s, "%d%n", &num_coords, &offset);
	s += offset;
      } else
	num = sscanf(style, "%d", &num_coords);

      if ((num != 1) || (num_coords > 20) || (num_coords < 2)) {
	if (verbose)
	  scwm_msg(ERR, "ReadButtonFace",
		   "Bad button style (2) in line: %s", action);
	return False;
      }
      vc->num = num_coords;

      /* get the points */
      for (i = 0; i < vc->num; ++i) {
	/* X x Y @ line_style */
	num = sscanf(s, "%dx%d@%d%n", &vc->x[i], &vc->y[i], &vc->line_style[i],
		     &offset);
	if (num != 3) {
	  if (verbose)
	    scwm_msg(ERR, "ReadButtonFace",
		     "Bad button style (3) in line %s", action);
	  return False;
	}
	s += offset;
      }
      bf->style = VectorButton;
    }
#endif
    else if (mystrncasecmp(style, "Solid", 5) == 0) {
      s = GetNextToken(s, &file);
      if (file && *file) {
	bf->style = SolidButton;
	bf->u.back = GetColor(file);
      } else {
	if (verbose)
	  scwm_msg(ERR, "ReadButtonFace",
		   "no color given for Solid face type: %s",
		   action);
	return False;
      }
      free(file);
    }
#ifdef GRADIENT_BUTTONS
    else if (mystrncasecmp(style, "HGradient", 9) == 0
	     || mystrncasecmp(style, "VGradient", 9) == 0) {
      char *item, **s_colors;
      int npixels, nsegs, i, sum, *perc;
      Pixel *pixels;

      if (!(s = GetNextToken(s, &item)) || (item == NULL)) {
	if (verbose)
	  scwm_msg(ERR, "ReadButtonFace",
		   "expected number of colors to allocate in gradient");
	return False;
      }
      npixels = atoi(item);
      free(item);

      if (!(s = GetNextToken(s, &item)) || (item == NULL)) {
	if (verbose)
	  scwm_msg(ERR, "ReadButtonFace",
		   "incomplete gradient style");
	return False;
      }
      if (!(isdigit(*item))) {
	s_colors = (char **) safemalloc(sizeof(char *) * 2);
	perc = (int *) safemalloc(sizeof(int));

	nsegs = 1;
	s_colors[0] = item;
	s = GetNextToken(s, &s_colors[1]);
	perc[0] = 100;
      } else {
	nsegs = atoi(item);
	free(item);
	if (nsegs < 1)
	  nsegs = 1;
	if (nsegs > 128)
	  nsegs = 128;
	s_colors = (char **) safemalloc(sizeof(char *) * (nsegs + 1));
	perc = (int *) safemalloc(sizeof(int) * nsegs);

	for (i = 0; i <= nsegs; ++i) {
	  s = GetNextToken(s, &s_colors[i]);
	  if (i < nsegs) {
	    s = GetNextToken(s, &item);
	    if (item)
	      perc[i] = atoi(item);
	    else
	      perc[i] = 0;
	    free(item);
	  }
	}
      }

      for (i = 0, sum = 0; i < nsegs; ++i)
	sum += perc[i];

      if (sum != 100) {
	if (verbose)
	  scwm_msg(ERR, "ReadButtonFace",
		   "multi gradient lenghts must sum to 100");
	for (i = 0; i <= nsegs; ++i)
	  free(s_colors[i]);
	free(s_colors);
	return False;
      }
      if (npixels < 2)
	npixels = 2;
      if (npixels > 128)
	npixels = 128;

      pixels = AllocNonlinearGradient(s_colors, perc, nsegs, npixels);
      for (i = 0; i <= nsegs; ++i)
	free(s_colors[i]);
      free(s_colors);

      if (!pixels) {
	if (verbose)
	  scwm_msg(ERR, "ReadButtonFace",
		   "couldn't create gradient");
	return False;
      }
      bf->u.grad.pixels = pixels;
      bf->u.grad.npixels = npixels;

      if (mystrncasecmp(style, "H", 1) == 0)
	bf->style = HGradButton;
      else
	bf->style = VGradButton;
    }
#endif /* GRADIENT_BUTTONS */
#ifdef PIXMAP_BUTTONS
    else if (mystrncasecmp(style, "Pixmap", 6) == 0
	     || mystrncasecmp(style, "TiledPixmap", 11) == 0) {
      s = GetNextToken(s, &file);
      bf->u.p = CachePicture(dpy, Scr.Root,
			     IconPath,
#ifdef XPM
			     PixmapPath,
#else
			     NULL,
#endif
			     file);
      if (bf->u.p == NULL) {
	if (verbose)
	  scwm_msg(ERR, "ReadButtonFace",
		   "couldn't load pixmap %s",
		   file);
	free(file);
	return False;
      }
      free(file);
      file = NULL;

      if (mystrncasecmp(style, "Tiled", 5) == 0)
	bf->style = TiledPixmapButton;
      else
	bf->style = PixmapButton;
    }
#ifdef MINI_ICONS
    else if (mystrncasecmp(style, "MiniIcon", 8) == 0) {
      bf->style = MiniIconButton;
      bf->u.p = NULL;		/* pixmap read in when the window is created */
    }
#endif
#endif /* PIXMAP_BUTTONS */
    else {
      if (verbose)
	scwm_msg(ERR, "ReadButtonFace",
		 "unknown style %s: %s", style, action);
      return False;
    }
  }
  /* Process button flags ("--" signals start of flags,
     it is also checked for above) */
  s = GetNextToken(s, &file);
  if (file && (strcmp(file, "--") == 0)) {
    char *tok;

    s = GetNextToken(s, &tok);
    while (tok && tok[0]) {
      int set = 1;

      if (*tok == '!') {	/* flag negate */
	set = 0;
	++tok;
      }
      if (mystrncasecmp(tok, "Clear", 5) == 0) {
	if (set)
	  bf->style &= ButtonFaceTypeMask;
	else
	  bf->style |= ~ButtonFaceTypeMask;	/* ? */
      } else if (mystrncasecmp(tok, "Left", 4) == 0) {
	if (set) {
	  bf->style |= HOffCenter;
	  bf->style &= ~HRight;
	} else
	  bf->style |= HOffCenter | HRight;
      } else if (mystrncasecmp(tok, "Right", 5) == 0) {
	if (set)
	  bf->style |= HOffCenter | HRight;
	else {
	  bf->style |= HOffCenter;
	  bf->style &= ~HRight;
	}
      } else if (mystrncasecmp(tok, "Centered", 8) == 0) {
	bf->style &= ~HOffCenter;
	bf->style &= ~VOffCenter;
      } else if (mystrncasecmp(tok, "Top", 3) == 0) {
	if (set) {
	  bf->style |= VOffCenter;
	  bf->style &= ~VBottom;
	} else
	  bf->style |= VOffCenter | VBottom;

      } else if (mystrncasecmp(tok, "Bottom", 6) == 0) {
	if (set)
	  bf->style |= VOffCenter | VBottom;
	else {
	  bf->style |= VOffCenter;
	  bf->style &= ~VBottom;
	}
      } else if (mystrncasecmp(tok, "Flat", 4) == 0) {
	if (set) {
	  bf->style &= ~SunkButton;
	  bf->style |= FlatButton;
	} else
	  bf->style &= ~FlatButton;
      } else if (mystrncasecmp(tok, "Sunk", 4) == 0) {
	if (set) {
	  bf->style &= ~FlatButton;
	  bf->style |= SunkButton;
	} else
	  bf->style &= ~SunkButton;
      } else if (mystrncasecmp(tok, "Raised", 6) == 0) {
	if (set) {
	  bf->style &= ~FlatButton;
	  bf->style &= ~SunkButton;
	} else {
	  bf->style |= SunkButton;
	  bf->style &= ~FlatButton;
	}
      }
#ifdef EXTENDED_TITLESTYLE
      else if (mystrncasecmp(tok, "UseTitleStyle", 13) == 0) {
	if (set) {
	  bf->style |= UseTitleStyle;
#ifdef BORDERSTYLE
	  bf->style &= ~UseBorderStyle;
#endif
	} else
	  bf->style &= ~UseTitleStyle;
      }
#endif
#ifdef BORDERSTYLE
      else if (mystrncasecmp(tok, "HiddenHandles", 13) == 0) {
	if (set)
	  bf->style |= HiddenHandles;
	else
	  bf->style &= ~HiddenHandles;
      } else if (mystrncasecmp(tok, "NoInset", 7) == 0) {
	if (set)
	  bf->style |= NoInset;
	else
	  bf->style &= ~NoInset;
      } else if (mystrncasecmp(tok, "UseBorderStyle", 14) == 0) {
	if (set) {
	  bf->style |= UseBorderStyle;
#ifdef EXTENDED_TITLESTYLE
	  bf->style &= ~UseTitleStyle;
#endif
	} else
	  bf->style &= ~UseBorderStyle;
      }
#endif
      else if (verbose)
	scwm_msg(ERR, "ReadButtonFace",
		 "unknown button face flag %s -- line: %s", tok, action);
      if (set)
	free(tok);
      else
	free(tok - 1);
      s = GetNextToken(s, &tok);
    }
  }
  return True;
}


/*****************************************************************************
 * 
 * Reads a title button description (veliaa@rpi.edu)
 *
 ****************************************************************************/
char *
ReadTitleButton(char *s, TitleButton * tb, Boolean append, int button)
{
  char *end = NULL, *spec;
  ButtonFace tmpbf;
  enum ButtonState bs = MaxButtonState;
  int i = 0, all = 0, pstyle = 0;

  while (isspace(*s))
    ++s;
  for (; i < MaxButtonState; ++i)
    if (mystrncasecmp(button_states[i], s,
		      strlen(button_states[i])) == 0) {
      bs = i;
      break;
    }
  if (bs != MaxButtonState)
    s += strlen(button_states[bs]);
  else
    all = 1;
  while (isspace(*s))
    ++s;
  if ('(' == *s) {
    int len;

    pstyle = 1;
    if (!(end = strchr(++s, ')'))) {
      scwm_msg(ERR, "ReadTitleButton",
	       "missing parenthesis: %s", s);
      return NULL;
    }
    len = end - s + 1;
    spec = safemalloc(len);
    strncpy(spec, s, len - 1);
    spec[len - 1] = 0;
  } else
    spec = s;

  while (isspace(*spec))
    ++spec;
  /* setup temporary in case button read fails */
  tmpbf.style = SimpleButton;
#ifdef MULTISTYLE
  tmpbf.next = NULL;
#endif

  if (strncmp(spec, "--", 2) == 0) {
    /* only change flags */
    if (ReadButtonFace(spec, &tb->state[all ? 0 : bs], button, True) && all) {
      for (i = 0; i < MaxButtonState; ++i)
	ReadButtonFace(spec, &tb->state[i], -1, False);
    }
  } else if (ReadButtonFace(spec, &tmpbf, button, True)) {
    int b = all ? 0 : bs;

#ifdef MULTISTYLE
    if (append) {
      ButtonFace *tail = &tb->state[b];

      while (tail->next)
	tail = tail->next;
      tail->next = (ButtonFace *) safemalloc(sizeof(ButtonFace));
      *tail->next = tmpbf;
      if (all)
	for (i = 1; i < MaxButtonState; ++i) {
	  tail = &tb->state[i];
	  while (tail->next)
	    tail = tail->next;
	  tail->next = (ButtonFace *) safemalloc(sizeof(ButtonFace));
	  tail->next->style = SimpleButton;
	  tail->next->next = NULL;
	  ReadButtonFace(spec, tail->next, button, False);
	}
    } else {
#endif
      FreeButtonFace(dpy, &tb->state[b]);
      tb->state[b] = tmpbf;
      if (all)
	for (i = 1; i < MaxButtonState; ++i)
	  ReadButtonFace(spec, &tb->state[i], button, False);
#ifdef MULTISTYLE
    }
#endif

  }
  if (pstyle) {
    free(spec);
    ++end;
    while (isspace(*end))
      ++end;
  }
  return end;
}

#endif
