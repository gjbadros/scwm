
#include "../configure.h"
#include "scwm.h"
#include "screen.h"

void redraw_titlebars(ScwmDecor *fl, int extra_height) {
  int x,w,y,h;
  ScwmWindow *tmp,*hi;
  tmp = Scr.ScwmRoot.next;
  hi = Scr.Hilite;
  while(tmp != NULL)
    {
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
      h = tmp->frame_height-extra_height;
      tmp->frame_x = 0;
      tmp->frame_y = 0;
      tmp->frame_height = 0;
      tmp->frame_width = 0;
      SetupFrame(tmp,x,y,w,h,True);
      SetTitleBar(tmp,True,True);
      SetTitleBar(tmp,False,True);
      tmp = tmp->next;
    }
  SetTitleBar(hi,True,True);
}



void refresh_common(Window win_or_root)
{
  XSetWindowAttributes attributes;
  unsigned long valuemask;
  Window w;
  valuemask = CWOverrideRedirect | CWBackingStore | CWSaveUnder | CWBackPixmap;
  attributes.override_redirect = True;
  attributes.save_under = False;
  attributes.background_pixmap = None;
  attributes.backing_store = NotUseful;
  w = XCreateWindow (dpy,
		     win_or_root,
                     0, 0,
		     (unsigned int) Scr.MyDisplayWidth,
		     (unsigned int) Scr.MyDisplayHeight,
		     (unsigned int) 0,
		     CopyFromParent, (unsigned int) CopyFromParent,
		     (Visual *) CopyFromParent, valuemask,
		     &attributes);
  XMapWindow (dpy, w);
  XDestroyWindow (dpy, w);
  XFlush (dpy);

}

