#include <config.h>
#include <guile/gh.h>
#include "scwm.h"
#include "screen.h"
#include "window.h"
#include "decor.h"
#include "color.h"
#include "font.h"

long scm_tc16_scwm_decor;

size_t free_decor (SCM obj) {
  DestroyScwmDecor(SCWMDECOR(obj));
  free(SCWMDECOR(obj));
  free(DECOR(obj));
  return 0;
};

int print_decor (SCM obj, SCM port, scm_print_state *pstate) {
  char *name;

  name = SCWMDECOR(obj)->tag;
  scm_gen_puts(scm_regular_port, "#<decor ", port);
  if (NULL==name) {
    scm_write(gh_int2scm((int) DECOR(obj)), port);
  } else {
    scm_gen_puts(scm_regular_port, name, port);
  }
  scm_gen_putc('>', port);
  return 1;
};


#ifdef USEDECOR
extern ScwmDecor *cur_decor;
#endif


SCM decor2scm(ScwmDecor *fl) {
  SCM answer;
  SCM tmpd;
  scwm_decor *dec;

  dec=(scwm_decor *) malloc(sizeof(scwm_decor));
  dec->refcnt=0;
  dec->sd=fl;
  SCM_DEFER_INTS;
  SCM_NEWCELL (answer);
  SCM_SETCAR (answer, scm_tc16_scwm_decor);
  SCM_SETCDR (answer, (SCM)dec);
  fl->scmdecor=answer;
  SCM_ALLOW_INTS;
  
  tmpd = current_decor();
  set_current_decor_x(answer);
  set_hilight_colors(gh_str02scm("black"),
		     gh_str02scm("grey"));
  set_window_font(gh_str02scm("fixed"));
  set_current_decor_x(tmpd);

  return (answer);
};


SCM make_decor(SCM name) {
  char *tag;
  int dummy;
  ScwmDecor *newdec;

  if (gh_string_p(name)) {
    tag=gh_scm2newstr(name,&dummy);
  } else if (name==SCM_UNDEFINED) {
    tag=NULL;
  } else {
    scm_wrong_type_arg("make-decor",1,name);
  }

  /* make the decor */
  newdec=(ScwmDecor *) malloc(sizeof(ScwmDecor));
  InitScwmDecor(newdec);
  newdec->tag=tag;

  return decor2scm(newdec);
}

SCM default_decor() {
  return Scr.DefaultDecor.scmdecor;
}


SCM set_current_decor_x(SCM decor) {
  ScwmDecor *new_cur;

  if (decor==SCM_BOOL_F) {
    new_cur=NULL;
  } else if (SCM_NIMP(decor) && DECORP(decor)) {
    new_cur=SCWMDECOR(decor);
  } else {
    scm_wrong_type_arg("set-current-decor!",1,decor);
  }

  if (cur_decor!=NULL) {
    DECORUNREF(cur_decor->scmdecor);
  } 
  if (new_cur!=NULL) {
    DECORREF(decor);
  }
  
  cur_decor=new_cur;
  
  return SCM_UNSPECIFIED;
}

SCM current_decor() {
  if (cur_decor==NULL) {
    return SCM_BOOL_F;
  } else {
    return cur_decor->scmdecor;
  }
}


SCM ensure_valid(SCM win, int n, char *subr, SCM kill_p);

#define VALIDATEN(win,n,subr)  if(((win=ensure_valid(win,n,subr,SCM_BOOL_F)))==SCM_BOOL_F) return SCM_BOOL_F


SCM set_window_decor_x(SCM decor, SCM win) {
  int x,y,width,height,old_height,extra_height;
  ScwmDecor *fl;
  ScwmWindow *tmp_win;

  if (SCM_NIMP(decor) && DECORP(decor)) {
    fl=SCWMDECOR(decor);
  } else {
    scm_wrong_type_arg("set-window-decor!",1,decor);
  }

  VALIDATEN(win,2,"set-window-decor!");
  tmp_win=SCWMWINDOW(win);

  old_height = tmp_win->fl->TitleHeight;

  printf("%x %x \n",(int)tmp_win->fl,(int)fl);
  tmp_win->fl = fl;

  extra_height = (tmp_win->flags & TITLE) ?
    (old_height - tmp_win->fl->TitleHeight) : 0;
  x = tmp_win->frame_x;
  y = tmp_win->frame_y;
  width = tmp_win->frame_width;
  height = tmp_win->frame_height - extra_height;
  tmp_win->frame_x = 0;
  tmp_win->frame_y = 0;
  tmp_win->frame_height = 0;
  tmp_win->frame_width = 0;
  SetupFrame(tmp_win,x,y,width,height,True);
  SetBorder(tmp_win,Scr.Hilite == tmp_win,True,True,None);

  return SCM_UNSPECIFIED;
}


#if 0

/*****************************************************************************
 * 
 * Updates window decoration styles (veliaa@rpi.edu)
 *
 ****************************************************************************/
void UpdateDecor(XEvent *eventp,Window junk,ScwmWindow *tmp_win,
		 unsigned long context, char *action,int* Module)
{
    ScwmWindow *fw = Scr.ScwmRoot.next;
#ifdef USEDECOR
    ScwmDecor *fl = &Scr.DefaultDecor, *found = NULL;
    char *item = NULL;
    action = GetNextToken(action, &item);
    if (item) {
	/* search for tag */
	for (; fl; fl = fl->next)
	    if (fl->tag)
		if (mystrcasecmp(item, fl->tag)==0) {
		    found = fl;
		    break;
		}
	free(item);
    }
#endif
    
    for (; fw != NULL; fw = fw->next)
    {
#ifdef USEDECOR
	/* update specific decor, or all */
	if (found) {
	    if (fw->fl == found) {
		SetBorder(fw,True,True,True,None);
		SetBorder(fw,False,True,True,None);
	    }
	}
	else
#endif
	{
	    SetBorder(fw,True,True,True,None);
	    SetBorder(fw,False,True,True,None);
	}
    }
    SetBorder(Scr.Hilite,True,True,True,None);
}

#endif
