/****************************************************************************
 * This module has been significantly modified by Maciej Stachowiak
 * and Greg J Badros.
 * It may be used under the terms of the fvwm copyright (see COPYING.FVWM).
 * Changes Copyright 1997, Maciej Stachowiak and Greg J. Badros
 ****************************************************************************/
/****************************************************************************
 * This module based on original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/

#include <config.h>
#include <guile/gh.h>
#include "scwm.h"
#include "misc.h"
#include "screen.h"
#include "window.h"
#include "face.h"
#include "color.h"
#include "colors.h"
#include "util.h"

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
  sf=(scwm_face *)safemalloc(sizeof(scwm_face));
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
  sym_vertical, sym_pixmap, sym_mini_program_icon, sym_tiled;


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
    if (tiled_p || gh_string_p(arg) || (mini_p=(arg==sym_mini_program_icon)) ||
	PICTURE_P(arg)) {
      Picture *p = NULL;
      
      if (tiled_p) {
	arg=gh_cadr(arg);
      }

      /* FIXGJB: I think this should only take pixmap objects;
	 if we want scheme sugar for converting strings into
	 pixmaps, that's fine, too;  this works well now, though,
	 so maybe it's only work changing when make-face is
	 wrapped to use keyword arguments */
      if (!mini_p) {
	int len;

	if (PICTURE_P(arg)) {
	  p = PICTURE(arg)->pic;
	  if (p==NULL) {
	    scwm_msg(ERR,"add_spec_to_face_x","Picture object has no pic==NULL");
	  }
	} else {
	  char *pixmap = gh_scm2newstr(arg,&len);
	  p = CachePicture(dpy, Scr.Root,
			   szPicturePath, pixmap);
	  if (pixmap) free(pixmap);
	}
	if (p==NULL) {
	  /* signal an error: couldn't load picture */
	  /* FIXMS give a better error message */
	  scm_wrong_type_arg("add_spec_to_face_x",3,arg);
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

extern ScwmDecor *cur_decor;


SCM
set_title_face_x (SCM active_up, SCM active_down, SCM inactive)
{
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (!(SCM_NIMP(active_up) && FACEP(active_up))) {
    scm_wrong_type_arg("set-title-face!",1,active_up);
  }

  if (active_down==SCM_UNDEFINED) {
    active_down=active_up;
  } else if (!(SCM_NIMP(active_down) && FACEP(active_down))) {
    scm_wrong_type_arg("set-title-face!",2,active_down);
  }

  if (inactive==SCM_UNDEFINED) {
    inactive=active_up;
  } else if (!(SCM_NIMP(inactive) && FACEP(inactive))) {
    scm_wrong_type_arg("set-title-face!",3,inactive);
  }

  fl->titlebar.state[ActiveUp]=BUTTONFACE(active_up);
  fl->titlebar.state[ActiveDown]=BUTTONFACE(active_down);
  fl->titlebar.state[Inactive]=BUTTONFACE(inactive);

  redraw_titlebars(fl,0);

  return SCM_UNSPECIFIED;
}


SCM
set_button_face_x (SCM button, SCM active_up, SCM active_down, SCM inactive) 
{
  int n;
  int left_p;

  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  n=0;

  if (!gh_number_p(button) || (n=gh_scm2int(button)) < 1 || n > 10) {
    scm_wrong_type_arg("set-button-face!",1,button);
  }

  if (!(SCM_NIMP(active_up) && FACEP(active_up))) {
    scm_wrong_type_arg("set-button-face!",2,active_up);
  }

  if (active_down==SCM_UNDEFINED) {
    active_down=active_up;
  } else if (!(SCM_NIMP(active_down) && FACEP(active_down))) {
    scm_wrong_type_arg("set-button-face!",3,active_down);
  }

  if (inactive==SCM_UNDEFINED) {
    inactive=active_up;
  } else if (!(SCM_NIMP(inactive) && FACEP(inactive))) {
    scm_wrong_type_arg("set-button-face!",4,inactive);
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

SCM
set_button_mwm_flag_x(SCM button, SCM flag) 
{
  int n;
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  n=0;

  if (!gh_number_p(button) || (n=gh_scm2int(button)) < 1 ||
      n > 10) {
    scm_wrong_type_arg("set-button-face!",1,button);
  }

  if(flag==SCM_BOOL_T) {
    fl->left_buttons[n].flags |= MWMButton;    
  } else if (flag==SCM_BOOL_F) {
    fl->left_buttons[n].flags &= ~MWMButton;    
  } else {
    scm_wrong_type_arg("set-button-mwm-flag!",1,flag);
  }

  redraw_borders(fl);

  return SCM_UNSPECIFIED;
}

SCM
set_border_face_x(SCM active, SCM inactive) 
{
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;

  if (!(SCM_NIMP(active) && FACEP(active))) {
    scm_wrong_type_arg("set-border-face!",1,active);
  }

  if (inactive==SCM_UNDEFINED) {
    inactive=active;
  } else if (!(SCM_NIMP(inactive) && FACEP(inactive))) {
    scm_wrong_type_arg("set-border-face!",2,inactive);
  }

  fl->BorderStyle.active=BUTTONFACE(active);
  fl->BorderStyle.inactive=BUTTONFACE(inactive);

  redraw_borders(fl); 

  return SCM_UNSPECIFIED;
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
  sym_mini_program_icon =gh_symbol2scm("mini-program-icon");
  scm_protect_object(sym_mini_program_icon);
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
  case HGradButton:
  case VGradButton:
    /* - should we check visual is not TrueColor before doing this? 

       XFreeColors(dpy, Scr.ScwmRoot.attr.colormap, 
       bf->u.grad.pixels, bf->u.grad.npixels,
       AllPlanes); */
    free(bf->u.grad.pixels);
    bf->u.grad.pixels = NULL;
    break;

  case PixmapButton:
  case TiledPixmapButton:
    if (bf->u.p)
      DestroyPicture(dpy, bf->u.p);
    bf->u.p = NULL;
    break;
  default:
    break;
  }
  /* delete any compound styles */
  if (bf->next) {
    FreeButtonFace(dpy, bf->next);
    free(bf->next);
  }
  bf->next = NULL;
  bf->style &= ~ButtonFaceTypeMask;
  bf->style |= SimpleButton;
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
