/****************************************************************************
 * Copyright 1997, Maciej Stachowiak
 */
/* uses some code from color.c and builtins.c from fvwm: */
/****************************************************************************
 * This module is all original code 
 * by Rob Nation 
 * Copyright 1993, Robert Nation
 *     You may use this code for any purpose, as long as the original
 *     copyright remains in the source code and all documentation
 ****************************************************************************/
/****************************************************************************
 * This module is all new
 * by Rob Nation 
 * Copyright 1993 Robert Nation. No restrictions are placed on this code,
 * as long as the copyright notice is preserved
 ****************************************************************************/

#include <guile/gh.h>
#include <libguile.h>
#include <X11/Xproto.h>
#include <X11/Xatom.h>

#include "../configure.h"
#include "scwm.h"
#include "screen.h"
#include "color.h"
#include "errors.h"
#include "../version.h"

#ifdef USEDECOR
extern ScwmDecor *last_decor, *cur_decor;
#endif

/* XXX - these should be made visible as bindings */
#define BLACK COLOR(load_color(gh_str02scm("black")))
#define WHITE COLOR(load_color(gh_str02scm("white")))

long scm_tc16_scwm_color;

int print_color (SCM obj, SCM port, scm_print_state *pstate)
{
  scm_gen_puts(scm_regular_port, "#<color ", port);
  scm_write(gh_int2scm(COLOR(obj)), port);
  scm_gen_putc('>', port);
  return 1;
}

/* XXX - should be memoized - store linked list of colors somewhere w/ ref
   counts and cleaned by gc? */

SCM load_color (SCM cname) 
{ 
  SCM answer;
  XColor color;
  char *cn; 
  int len;
  if (!gh_string_p(cname)) {
    scm_wrong_type_arg("load-color",1,cname);
  }
  cn=gh_scm2newstr(cname,&len);
  color.pixel = 0;
  if (!XParseColor (dpy, Scr.ScwmRoot.attr.colormap, cn, &color)) {
    /*    scm_error(gh_symbol2scm("scwm-error"),"load-color","%s",
	      gh_list(gh_str02scm("Unable to parse color."),SCM_UNDEFINED),
	      gh_list(gh_int2scm(1),SCM_UNDEFINED)); */
  }
  else if(!XAllocColor (dpy, Scr.ScwmRoot.attr.colormap, &color)) {
    /*  scm_error(gh_symbol2scm("scwm-error"),"load-color","%s",
	      gh_list(gh_str02scm("Unable to allocate color."),SCM_UNDEFINED),
	      gh_list(gh_int2scm(1),SCM_UNDEFINED));
	      */
  }
  free(cn);
  SCM_NEWCELL(answer);
  SCM_SETCAR(answer, scm_tc16_scwm_color);
  SCM_SETCDR(answer, (SCM)(color.pixel)); 
  return(answer);
}

/* eventually add option to pass background relief, shadow? */
SCM set_hilight_colors (SCM fg, SCM bg) {
  XGCValues gcv;
  unsigned long gcm;
#ifdef USEDECOR
  ScwmDecor *fl = cur_decor ? cur_decor : &Scr.DefaultDecor;
#else
  ScwmDecor *fl = &Scr.DefaultDecor;
#endif  
  if(gh_string_p(fg)) {
    fg=load_color(fg);
  } else if(fg==SCM_UNDEFINED) {
    fg=SCM_BOOL_F;
  } else if(!((SCM_NIMP(fg) && COLORP(fg))||fg==SCM_BOOL_F)) {
    scm_wrong_type_arg("set-highlight-colors!",1,fg);
  }
  if(gh_string_p(bg)) {
    bg=load_color(bg);
  } else if(bg==SCM_UNDEFINED) {
    bg=SCM_BOOL_F;
  } else if(!((SCM_NIMP(bg) && COLORP(bg))||bg==SCM_BOOL_F)) {
    scm_wrong_type_arg("set-highlight-colors!",2,bg);
  }
  
  if(Scr.d_depth > 2) {
    if (fg != SCM_BOOL_F) {
      fl->HiColors.fore = COLOR(fg);
    }
    if (bg != SCM_BOOL_F) {
      fl->HiColors.back = COLOR(bg);
    }
    fl->HiRelief.back  = GetShadow(fl->HiColors.back);
    fl->HiRelief.fore  = GetHilite(fl->HiColors.back);
  } else {
    fl->HiColors.back  = WHITE;
    fl->HiColors.fore  = BLACK; 
    fl->HiRelief.back  = BLACK;
    fl->HiRelief.fore  = WHITE;
  }
  gcm = GCFunction|GCPlaneMask|GCGraphicsExposures|GCLineWidth|GCForeground|
    GCBackground;
  gcv.foreground = fl->HiRelief.fore;
  gcv.background = fl->HiRelief.back;
  gcv.fill_style = FillSolid;
  gcv.plane_mask = AllPlanes;
  gcv.function = GXcopy;
  gcv.graphics_exposures = False;
  gcv.line_width = 0;
  if(fl->HiReliefGC != NULL)
    {
      XFreeGC(dpy,fl->HiReliefGC);
    }
  fl->HiReliefGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);  
  gcv.foreground = fl->HiRelief.back;
  gcv.background = fl->HiRelief.fore;
  if(fl->HiShadowGC != NULL)
    {
      XFreeGC(dpy,fl->HiShadowGC);
    }
  fl->HiShadowGC = XCreateGC(dpy, Scr.Root, gcm, &gcv);  
  
  if((Scr.flags & WindowsCaptured)&&(Scr.Hilite != NULL))
    {
      SetBorder(Scr.Hilite,False,True,True,None);
      SetBorder(Scr.Hilite,True,True,True,None);
    }
  return(SCM_UNSPECIFIED);
}

SCM color_p(SCM obj) {
  return  ((SCM_NIMP(obj) && COLORP(obj)) ? SCM_BOOL_T : SCM_BOOL_F);
}

SCM set_menu_colors(SCM fg, SCM bg, SCM stipple) {
  XGCValues gcv;
  unsigned long gcm;
  int wid,hei;

  if(gh_string_p(fg)) {
    fg=load_color(fg);
  } else if(fg==SCM_UNDEFINED) {
    fg=SCM_BOOL_F;
  } else if(!((SCM_NIMP(fg) && COLORP(fg))||fg==SCM_BOOL_F)) {
    scm_wrong_type_arg("set-highlight-colors!",1,fg);
  }
  if(gh_string_p(bg)) {
    bg=load_color(bg);
  } else if(bg==SCM_UNDEFINED) {
    bg=SCM_BOOL_F;
  } else if(!((SCM_NIMP(bg) && COLORP(bg))||bg==SCM_BOOL_F)) {
    scm_wrong_type_arg("set-highlight-colors!",2,bg);
  }
  if(gh_string_p(stipple)) {
    stipple=load_color(stipple);
  } else if(stipple==SCM_UNDEFINED) {
    stipple=SCM_BOOL_F;
  } else if(!((SCM_NIMP(stipple) && COLORP(stipple))||stipple==SCM_BOOL_F)) {
    scm_wrong_type_arg("set-highlight-colors!",3,stipple);
  }
  if(Scr.d_depth > 2) {
    if(fg != SCM_BOOL_F) {
      Scr.MenuColors.fore = COLOR(fg);
    }
    if(bg != SCM_BOOL_F) {
      Scr.MenuColors.back = COLOR(bg);
    }
    Scr.MenuRelief.back  = GetShadow(Scr.MenuColors.back);
    Scr.MenuRelief.fore  = GetHilite(Scr.MenuColors.back);
    Scr.MenuStippleColors.back = Scr.MenuColors.back;
    if(stipple != SCM_BOOL_F) {
      Scr.MenuStippleColors.fore = COLOR(stipple);
    }
  } else {
    Scr.MenuColors.back  = WHITE;
    Scr.MenuColors.fore  = BLACK;
    Scr.MenuRelief.back  = BLACK;
    Scr.MenuRelief.fore  = WHITE;
    Scr.MenuStippleColors.back = WHITE;
    Scr.MenuStippleColors.fore = BLACK;
  }

  gcm = GCForeground|GCBackground;
  gcv.foreground = Scr.MenuRelief.fore;
  gcv.background = Scr.MenuRelief.back;
  XChangeGC(dpy, Scr.MenuReliefGC, gcm, &gcv);  
  gcv.foreground = Scr.MenuRelief.back;
  gcv.background = Scr.MenuRelief.fore;
  XChangeGC(dpy, Scr.MenuShadowGC, gcm, &gcv);  
  gcv.foreground = Scr.MenuColors.fore;
  gcv.background = Scr.MenuColors.back;
  XChangeGC(dpy, Scr.MenuGC, gcm, &gcv);
  if(Scr.d_depth < 2)
  {
    gcv.fill_style = FillStippled;
    gcv.stipple = Scr.gray_bitmap;
    gcm=GCForeground|GCBackground|GCStipple|GCFillStyle;
  } else {
    gcv.foreground = Scr.MenuStippleColors.fore;
    gcv.background = Scr.MenuStippleColors.back;
  }
  XChangeGC(dpy, Scr.MenuStippleGC, gcm, &gcv);  
  MakeMenus();

  return(SCM_UNSPECIFIED);
}



#if 0

void ProcessNewStyle(XEvent *eventp,
                     Window w,
                     ScwmWindow *tmp_win,
                     unsigned long context,
                     char *text,
                     int *Module)
{
  char *name, *line;
  char *restofline,*tmp;
  char *icon_name = NULL;
#ifdef MINI_ICONS
  char *miniicon_name = NULL;
#endif
#ifdef USEDECOR
  char *decor = NULL;
#endif
  char *forecolor = NULL;
  char *backcolor = NULL;
  unsigned long off_buttons=0;
  unsigned long on_buttons=0;
  name_list *nptr;
  int butt;
  int BoxFillMethod = 0;
  int IconBox[4];
  int num,i;

  int len,desknumber = 0,bw=0, nobw = 0;
  unsigned long off_flags = 0;
  unsigned long on_flags = 0;
  
  IconBox[0] = -1;
  IconBox[1] = -1;
  IconBox[2] = Scr.MyDisplayWidth;
  IconBox[3] = Scr.MyDisplayHeight;

  restofline = GetNextToken(text,&name);
  /* in case there was no argument! */
  if((name == NULL)||(restofline == NULL))
    return;

  while(isspace(*restofline)&&(*restofline != 0))restofline++;
  line = restofline;

  if(restofline == NULL)return;
  while((*restofline != 0)&&(*restofline != '\n'))
  {
    while(isspace(*restofline)) restofline++;
    switch (tolower(restofline[0]))
    {
      case 'a':
        if(mystrncasecmp(restofline,"ACTIVEPLACEMENT",15)==0)
        {
          restofline +=15;
          on_flags |= RANDOM_PLACE_FLAG;
        }
        break;
      case 'b':
        if(mystrncasecmp(restofline,"BACKCOLOR",9)==0)
        {
          restofline +=9;
          while(isspace(*restofline))restofline++;
          tmp = restofline;
          len = 0;
          while((tmp != NULL)&&(*tmp != 0)&&(*tmp != ',')&&
                (*tmp != '\n')&&(!isspace(*tmp)))
          {
            tmp++;
            len++;
          }
          if(len > 0)
          {
            backcolor = safemalloc(len+1);
            strncpy(backcolor,restofline,len);
            backcolor[len] = 0;
            off_flags |= BACK_COLOR_FLAG;
          }
          restofline = tmp;
        }
        else if (mystrncasecmp(restofline,"BUTTON",6)==0)
        {
          restofline +=6;
	  
          sscanf(restofline,"%d",&butt);
          while(isspace(*restofline))restofline++;
          while((!isspace(*restofline))&&(*restofline!= 0)&&
                (*restofline != ',')&&(*restofline != '\n'))
            restofline++;
          while(isspace(*restofline))restofline++;
	  
          on_buttons |= (1<<(butt-1));        
        }
        else if(mystrncasecmp(restofline,"BorderWidth",11)==0)
        {
          restofline +=11;
          off_flags |= BW_FLAG;
          sscanf(restofline,"%d",&bw);
          while(isspace(*restofline))restofline++;
          while((!isspace(*restofline))&&(*restofline!= 0)&&
                (*restofline != ',')&&(*restofline != '\n'))
            restofline++;
          while(isspace(*restofline))restofline++;
        }
        break;
      case 'c':
        if(mystrncasecmp(restofline,"COLOR",5)==0)
        {
          restofline +=5;
          while(isspace(*restofline))restofline++;
          tmp = restofline;
          len = 0;
          while((tmp != NULL)&&(*tmp != 0)&&(*tmp != ',')&&
                (*tmp != '\n')&&(*tmp != '/')&&(!isspace(*tmp)))
          {
            tmp++;
            len++;
          }
          if(len > 0)
          {
            forecolor = safemalloc(len+1);
            strncpy(forecolor,restofline,len);
            forecolor[len] = 0;
            off_flags |= FORE_COLOR_FLAG;
          }
          
          while(isspace(*tmp))tmp++;
          if(*tmp == '/')
          {
            tmp++;
            while(isspace(*tmp))tmp++;
            restofline = tmp;
            len = 0;
            while((tmp != NULL)&&(*tmp != 0)&&(*tmp != ',')&&
                  (*tmp != '\n')&&(*tmp != '/')&&(!isspace(*tmp)))
            {
              tmp++;
              len++;
            }
            if(len > 0)
            {
              backcolor = safemalloc(len+1);
              strncpy(backcolor,restofline,len);
              backcolor[len] = 0;
              off_flags |= BACK_COLOR_FLAG;
            }
          }
          restofline = tmp;
        }
        else if(mystrncasecmp(restofline,"CirculateSkipIcon",17)==0)
        {
          restofline +=17;
          off_flags |= CIRCULATE_SKIP_ICON_FLAG;
        }
        else if(mystrncasecmp(restofline,"CirculateHitIcon",16)==0)
        {
          restofline +=16;
          on_flags |= CIRCULATE_SKIP_ICON_FLAG;
        }
        else if(mystrncasecmp(restofline,"CLICKTOFOCUS",12)==0)
        {
          restofline +=12;
          off_flags |= CLICK_FOCUS_FLAG;
          on_flags |= SLOPPY_FOCUS_FLAG;
        }
        else if(mystrncasecmp(restofline,"CirculateSkip",13)==0)
        {
          restofline +=13;
          off_flags |= CIRCULATESKIP_FLAG;
        }
        else if(mystrncasecmp(restofline,"CirculateHit",12)==0)
        {
          restofline +=12;
          on_flags |= CIRCULATESKIP_FLAG;
        }
        break;
      case 'd':
        if(mystrncasecmp(restofline,"DecorateTransient",17)==0)
        {
          restofline +=17;
          off_flags |= DECORATE_TRANSIENT_FLAG;
        }
        else if(mystrncasecmp(restofline,"DUMBPLACEMENT",13)==0)
        {
          restofline +=13;
          on_flags |= SMART_PLACE_FLAG;
        }
        break;
      case 'e':
        break;
      case 'f':
        if(mystrncasecmp(restofline,"FORECOLOR",9)==0)
        {
          restofline +=9;
          while(isspace(*restofline))restofline++;
          tmp = restofline;
          len = 0;
          while((tmp != NULL)&&(*tmp != 0)&&(*tmp != ',')&&
                (*tmp != '\n')&&(!isspace(*tmp)))
          {
            tmp++;
            len++;
          }
          if(len > 0)
          {
            forecolor = safemalloc(len+1);
            strncpy(forecolor,restofline,len);
            forecolor[len] = 0;
            off_flags |= FORE_COLOR_FLAG;
          }
          restofline = tmp;
        }
        else if(mystrncasecmp(restofline,"SCWMBUTTONS",11)==0)
        {
          restofline +=11;
          on_flags |= MWM_BUTTON_FLAG;
        }
        else if(mystrncasecmp(restofline,"SCWMBORDER",10)==0)
        {
          restofline +=10;
          on_flags |= MWM_BORDER_FLAG;
        }
        else if(mystrncasecmp(restofline,"FocusFollowsMouse",17)==0)
        {
          restofline +=17;
          on_flags |= CLICK_FOCUS_FLAG;
          on_flags |= SLOPPY_FOCUS_FLAG;
        }
        break;
      case 'g':
        break;
      case 'h':
        if(mystrncasecmp(restofline,"HINTOVERRIDE",12)==0)
        {
          restofline +=12;
          off_flags |= MWM_OVERRIDE_FLAG;
        }
        else if(mystrncasecmp(restofline,"HANDLES",7)==0)
        {
          restofline +=7;
          on_flags |= NOBORDER_FLAG;
        }
        else if(mystrncasecmp(restofline,"HandleWidth",11)==0)
        {
          restofline +=11;
          off_flags |= NOBW_FLAG;
          sscanf(restofline,"%d",&nobw);
          while(isspace(*restofline))restofline++;
          while((!isspace(*restofline))&&(*restofline!= 0)&&
                (*restofline != ',')&&(*restofline != '\n'))
            restofline++;
          while(isspace(*restofline))restofline++;
        }
        break;
      case 'i':
        if(mystrncasecmp(restofline,"IconTitle",9)==0)
        {
          on_flags |= NOICON_TITLE_FLAG;
          restofline +=9;
        }
        else if(mystrncasecmp(restofline,"IconBox",7) == 0)
        {
          restofline +=7;
          /* Standard X11 geometry string */
          num = sscanf(restofline,"%d%d%d%d",&IconBox[0], &IconBox[1],
                       &IconBox[2],&IconBox[3]);
          for(i=0;i<num;i++)
          {
            while(isspace(*restofline))restofline++;
            while((!isspace(*restofline))&&(*restofline!= 0)&&
                  (*restofline != ',')&&(*restofline != '\n'))
              restofline++;
          }
          if(num !=4)
            scwm_msg(ERR,"ProcessNewStyle",
                     "IconBox style requires 4 arguments!");
          else
          {
            /* check for negative locations */
            if(IconBox[0] < 0)
              IconBox[0] += Scr.MyDisplayWidth;
            if(IconBox[1] < 0)
              IconBox[1] += Scr.MyDisplayHeight;
            if(IconBox[2] < 0)
              IconBox[2] += Scr.MyDisplayWidth;
            if(IconBox[3] < 0)
              IconBox[3] += Scr.MyDisplayHeight;
          }
        }
        else if(mystrncasecmp(restofline,"ICON",4)==0)
        {
          restofline +=4;
          while(isspace(*restofline))restofline++;
          tmp = restofline;
          len = 0;
          while((tmp != NULL)&&(*tmp != 0)&&(*tmp != ',')&&(*tmp != '\n'))
          {
            tmp++;
            len++;
          }
          if(len > 0)
          {
            icon_name = safemalloc(len+1);
            strncpy(icon_name,restofline,len);
            icon_name[len] = 0;
            off_flags |= ICON_FLAG;
            on_flags |= SUPPRESSICON_FLAG;
          }
          else
            on_flags |= SUPPRESSICON_FLAG;	    
          restofline = tmp;
        }
        break;
      case 'j':
        break;
      case 'k':
        break;
      case 'l':
        if(mystrncasecmp(restofline,"LENIENCE",8)==0)
        {
          restofline += 8;
          off_flags |= LENIENCE_FLAG;
        }
        break;
      case 'm':
        if(mystrncasecmp(restofline,"MWMBUTTONS",10)==0)
        {
          restofline +=10;
          off_flags |= MWM_BUTTON_FLAG;
        }
#ifdef MINI_ICONS
	else if (mystrncasecmp(restofline,"MINIICON", 8) == 0)
	{
	  restofline += 8;
	  while (isspace (*restofline)) restofline++;
	  tmp = restofline;
	  len = 0;
	  while((tmp != NULL)&&(*tmp != 0)&&(*tmp != ',')&&(*tmp != '\n'))
          {
            tmp++;
            len++;
          }
          if(len > 0)
          {
            miniicon_name = safemalloc(len+1);
            strncpy(miniicon_name,restofline,len);
            miniicon_name[len] = 0;
            off_flags |= MINIICON_FLAG;
          }
          restofline = tmp;
	}
#endif
        else if(mystrncasecmp(restofline,"MWMBORDER",9)==0)
        {
          restofline +=9;
          off_flags |= MWM_BORDER_FLAG;
        }
        else if(mystrncasecmp(restofline,"MWMDECOR",8)==0)
        {
          restofline +=8;
          off_flags |= MWM_DECOR_FLAG;
        }
        else if(mystrncasecmp(restofline,"MWMFUNCTIONS",12)==0)
        {
          restofline +=12;
          off_flags |= MWM_FUNCTIONS_FLAG;
        }
        else if(mystrncasecmp(restofline,"MOUSEFOCUS",10)==0)
        {
          restofline +=10;
          on_flags |= CLICK_FOCUS_FLAG;
          on_flags |= SLOPPY_FOCUS_FLAG;
        }
        break;
      case 'n':
        if(mystrncasecmp(restofline,"NoIconTitle",11)==0)
        {
          off_flags |= NOICON_TITLE_FLAG;
          restofline +=11;
        }
        else if(mystrncasecmp(restofline,"NOICON",6)==0)
        {
          restofline +=6;
          off_flags |= SUPPRESSICON_FLAG;
        }
        else if(mystrncasecmp(restofline,"NOTITLE",7)==0)
        {
          restofline +=7;
          off_flags |= NOTITLE_FLAG;
        }
        else if(mystrncasecmp(restofline,"NoPPosition",11)==0)
        {
          restofline +=11;
          off_flags |= NO_PPOSITION_FLAG;
        }
        else if(mystrncasecmp(restofline,"NakedTransient",14)==0)
        {
          restofline +=14;
          on_flags |= DECORATE_TRANSIENT_FLAG;
        }
        else if(mystrncasecmp(restofline,"NODECORHINT",11)==0)
        {
          restofline +=11;
          on_flags |= MWM_DECOR_FLAG;
        }
        else if(mystrncasecmp(restofline,"NOFUNCHINT",10)==0)
        {
          restofline +=10;
          on_flags |= MWM_FUNCTIONS_FLAG;
        }
        else if(mystrncasecmp(restofline,"NOOVERRIDE",10)==0)
        {
          restofline +=10;
          on_flags |= MWM_OVERRIDE_FLAG;
        }
        else if(mystrncasecmp(restofline,"NOHANDLES",9)==0)
        {
          restofline +=9;
          off_flags |= NOBORDER_FLAG;
        }
        else if(mystrncasecmp(restofline,"NOLENIENCE",10)==0)
        {
          restofline += 10;
          on_flags |= LENIENCE_FLAG;
        }
        else if (mystrncasecmp(restofline,"NOBUTTON",8)==0)
        {
          restofline +=8;
	  
          sscanf(restofline,"%d",&butt);
          while(isspace(*restofline))restofline++;
          while((!isspace(*restofline))&&(*restofline!= 0)&&
                (*restofline != ',')&&(*restofline != '\n'))
            restofline++;
          while(isspace(*restofline))restofline++;
	  
          off_buttons |= (1<<(butt-1));
        }
        else if(mystrncasecmp(restofline,"NOOLDECOR",9)==0)
        {
          restofline += 9;
          on_flags |= OL_DECOR_FLAG;
        }
        break;
      case 'o':
        if(mystrncasecmp(restofline,"OLDECOR",7)==0)
        {
          restofline += 7;
          off_flags |= OL_DECOR_FLAG;
        }
        break;
      case 'p':
        break;
      case 'q':
        break;
      case 'r':
        if(mystrncasecmp(restofline,"RANDOMPLACEMENT",15)==0)
        {
          restofline +=15;
          off_flags |= RANDOM_PLACE_FLAG;
        }
        break;
      case 's':
        if(mystrncasecmp(restofline,"SMARTPLACEMENT",14)==0)
        {
          restofline +=14;
          off_flags |= SMART_PLACE_FLAG;
        }
        else if(mystrncasecmp(restofline,"SkipMapping",11)==0)
        {
          restofline +=11;
          off_flags |= SHOW_MAPPING;
        }
        else if(mystrncasecmp(restofline,"ShowMapping",11)==0)
        {
          restofline +=12;
          on_flags |= SHOW_MAPPING;
        }
        else if(mystrncasecmp(restofline,"StickyIcon",10)==0)
        {
          restofline +=10;
          off_flags |= STICKY_ICON_FLAG;
        }
        else if(mystrncasecmp(restofline,"SlipperyIcon",12)==0)
        {
          restofline +=12;
          on_flags |= STICKY_ICON_FLAG;
        }
        else if(mystrncasecmp(restofline,"SLOPPYFOCUS",11)==0)
        {
          restofline +=11;
          on_flags |= CLICK_FOCUS_FLAG;
          off_flags |= SLOPPY_FOCUS_FLAG;
        }
        else if(mystrncasecmp(restofline,"StartIconic",11)==0)
        {
          restofline +=11;
          off_flags |= START_ICONIC_FLAG;
        }
        else if(mystrncasecmp(restofline,"StartNormal",11)==0)
        {
          restofline +=11;
          on_flags |= START_ICONIC_FLAG;
        }
        else if(mystrncasecmp(restofline,"StaysOnTop",10)==0)
        {
          restofline +=10;
          off_flags |= STAYSONTOP_FLAG;	  
        }
        else if(mystrncasecmp(restofline,"StaysPut",8)==0)
        {
          restofline +=8;
          on_flags |= STAYSONTOP_FLAG;	  
        }
        else if(mystrncasecmp(restofline,"Sticky",6)==0)
        {
          off_flags |= STICKY_FLAG;	  
          restofline +=6;
        }
        else if(mystrncasecmp(restofline,"Slippery",8)==0)
        {
          on_flags |= STICKY_FLAG;	  
          restofline +=8;
        }
        else if(mystrncasecmp(restofline,"STARTSONDESK",12)==0)
        {
          restofline +=12;
          off_flags |= STARTSONDESK_FLAG;
          sscanf(restofline,"%d",&desknumber);
          while(isspace(*restofline))restofline++;
          while((!isspace(*restofline))&&(*restofline!= 0)&&
                (*restofline != ',')&&(*restofline != '\n'))
            restofline++;
          while(isspace(*restofline))restofline++;
        }
        else if(mystrncasecmp(restofline,"STARTSANYWHERE",14)==0)
        {
          restofline +=14;
          on_flags |= STARTSONDESK_FLAG;
        }
        break;
      case 't':
        if(mystrncasecmp(restofline,"TITLE",5)==0)
        {
          restofline +=5;
          on_flags |= NOTITLE_FLAG;
        }
        break;
      case 'u':
        if(mystrncasecmp(restofline,"UsePPosition",12)==0)
        {
          restofline +=12;
          on_flags |= NO_PPOSITION_FLAG;
        }
#ifdef USEDECOR
        if(mystrncasecmp(restofline,"UseDecor",8)==0)
        {
          int is_quoted = 0;
          restofline += 8;
          while(isspace(*restofline))restofline++;
          if (*restofline == '"') {
              is_quoted = 1;
              ++restofline;
          }
          tmp = restofline;
          len = 0;
          while (tmp && *tmp &&
                 ((!is_quoted&&(*tmp != ',')&&(*tmp != '\n')&&(!isspace(*tmp)))
                  || (is_quoted&&(*tmp != '\n')&&(*tmp != '"'))))
          {
            tmp++;
            len++;
          }
          if (tmp && (*tmp == '"')) ++tmp;
          if (len > 0)
          {
            decor = safemalloc(len+1);
            strncpy(decor,restofline,len);
            decor[len] = 0;
          }
          restofline = tmp;
        }
#endif
        else if(mystrncasecmp(restofline,"UseStyle",8)==0)
        {
          int is_quoted = 0;
          restofline +=8;
          while(isspace(*restofline))restofline++;
          if (*restofline == '"') {
              is_quoted = 1;
              ++restofline;
          }
          tmp = restofline;
          len = 0;
          while (tmp && *tmp &&
                 ((!is_quoted&&(*tmp != ',')&&(*tmp != '\n')&&(!isspace(*tmp)))
                  || (is_quoted&&(*tmp != '\n')&&(*tmp != '"'))))
	  {
            tmp++;
	    len++;
	  }
          if (tmp && (*tmp == '"')) ++tmp;
          if (len > 0)
          {
	    int hit = 0;            
	    /* changed to accumulate multiple Style definitions (veliaa@rpi.edu) */
            for ( nptr = Scr.TheList; nptr; nptr = nptr->next ) {
		if (!mystrncasecmp(restofline,nptr->name,len))
		{
		    if (!hit) {
			on_flags      = nptr->on_flags;
			off_flags     = nptr->off_flags;
			icon_name     = nptr->value;
#ifdef MINI_ICONS
			miniicon_name = nptr->mini_value;
#endif
#ifdef USEDECOR
			decor	    = nptr->Decor;
#endif
			desknumber    = nptr->Desk;
			bw            = nptr->border_width;
			nobw          = nptr->resize_width;
			forecolor     = nptr->ForeColor;
			backcolor     = nptr->BackColor;
			BoxFillMethod = nptr->BoxFillMethod;
			IconBox[0]    = nptr->IconBox[0];
			IconBox[1]    = nptr->IconBox[1];
			IconBox[2]    = nptr->IconBox[2];
			IconBox[3]    = nptr->IconBox[3];
			off_buttons   = nptr->off_buttons;
			on_buttons    = nptr->on_buttons;
			hit = 1;
		    } else {
			off_flags     |= nptr->off_flags;
			on_flags      &= ~(nptr->on_flags);
			off_buttons   |= nptr->off_buttons;
			on_buttons    &= ~(nptr->on_buttons);
			if(nptr->value) icon_name = nptr->value;
#ifdef MINI_ICONS
			if(nptr->mini_value) miniicon_name = nptr->mini_value;
#endif
#ifdef USEDECOR
			if(nptr->Decor) decor = nptr->Decor;
#endif
			if(nptr->off_flags & STARTSONDESK_FLAG)
			    desknumber = nptr->Desk;
			if(nptr->off_flags & BW_FLAG)
			    bw = nptr->border_width;
			if(nptr->off_flags & NOBW_FLAG)
			    nobw = nptr->resize_width;
			if(nptr->off_flags & FORE_COLOR_FLAG)
			    forecolor = nptr->ForeColor;
			if(nptr->off_flags & BACK_COLOR_FLAG)
			    backcolor = nptr->BackColor;

			if(nptr->BoxFillMethod != 0)
			    BoxFillMethod = nptr->BoxFillMethod;
			if(nptr->IconBox[0] >= 0)
			{
			    IconBox[0] = nptr->IconBox[0];
			    IconBox[1] = nptr->IconBox[1];
			    IconBox[2] = nptr->IconBox[2];
			    IconBox[3] = nptr->IconBox[3];
			}
		    }
		}
	    }
	    restofline = tmp;
	    if (!hit)
            {
              tmp=safemalloc(500);
              strcat(tmp,"UseStyle: ");
              strncat(tmp,restofline-len,len);
              strcat(tmp," style not found!");
              scwm_msg(ERR,"ProcessNewStyle",tmp);
              free(tmp);
            }
          }
          while(isspace(*restofline)) restofline++;
        }
        break;
      case 'v':
        break;
      case 'w':
        if(mystrncasecmp(restofline,"WindowListSkip",14)==0)
        {
          restofline +=14;
          off_flags |= LISTSKIP_FLAG;
        }
        else if(mystrncasecmp(restofline,"WindowListHit",13)==0)
        {
          restofline +=13;
          on_flags |= LISTSKIP_FLAG;
        }
        break;
      case 'x':
        break;
      case 'y':
        break;
      case 'z':
        break;
      default:
        break;
    }

    while(isspace(*restofline))restofline++;
    if(*restofline == ',')
      restofline++;
    else if((*restofline != 0)&&(*restofline != '\n'))
    {
      scwm_msg(ERR,"ProcessNewStyle",
               "bad style command: %s", restofline);
      return;
    }
  }

  /* capture default icons */
  if(strcmp(name,"*") == 0)
  {
    if(off_flags & ICON_FLAG)
      Scr.DefaultIcon = icon_name;
    off_flags &= ~ICON_FLAG;
    icon_name = NULL;
  }

  AddToList(name,icon_name,
#ifdef MINI_ICONS
            miniicon_name,
#endif
#ifdef USEDECOR
	    decor,
#endif
            off_flags,on_flags,desknumber,bw,nobw,
	    forecolor,backcolor,off_buttons,on_buttons,IconBox,BoxFillMethod);
}


void AddToList(char *name,
               char *icon_name,
#ifdef MINI_ICONS
               char *miniicon_name,
#endif
#ifdef USEDECOR
	       char *decor,
#endif
               unsigned long off_flags, 
	       unsigned long on_flags,
               int desk,
               int bw,
               int nobw,
	       char *forecolor,
               char *backcolor,
               unsigned long off_buttons,
               unsigned long on_buttons,
	       int *IconBox,
               int BoxFillMethod)
{
  name_list *nptr,*lastptr = NULL;

  if((name == NULL)||((off_flags == 0)&&(on_flags == 0)&&(on_buttons == 0)&&
 		      (off_buttons == 0)&&(IconBox[0] < 0)
#ifdef MINI_ICONS
		      &&(miniicon_name == NULL)
#endif
#ifdef USEDECOR
		      &&(decor == NULL)
#endif
      ))
  {
    if(name)
      free(name);
    if(icon_name)
      free(icon_name);
    return;
  }

  /* used to merge duplicate entries, but that is no longer
   * appropriate since conficting styles are possible, and the
   * last match should win! */
  for (nptr = Scr.TheList; nptr != NULL; nptr = nptr->next)
  {
    lastptr=nptr;
  }

  nptr = (name_list *)safemalloc(sizeof(name_list));
  nptr->next = NULL;
  nptr->name = name;
  nptr->on_flags = on_flags;
  nptr->off_flags = off_flags;
  nptr->value = icon_name;
#ifdef MINI_ICONS
  nptr->mini_value = miniicon_name;
#endif
#ifdef USEDECOR
  nptr->Decor = decor;
#endif
  nptr->Desk = desk;
  nptr->border_width = bw;
  nptr->resize_width = nobw;
  nptr->ForeColor = forecolor;
  nptr->BackColor = backcolor;
  nptr->BoxFillMethod = BoxFillMethod;
  nptr->IconBox[0] = IconBox[0];
  nptr->IconBox[1] = IconBox[1];
  nptr->IconBox[2] = IconBox[2];
  nptr->IconBox[3] = IconBox[3];
  nptr->off_buttons = off_buttons;
  nptr->on_buttons = on_buttons;

  if(lastptr != NULL)
    lastptr->next = nptr;
  else
    Scr.TheList = nptr;
}

#endif
