/* $Id$
 * pager.c
 */

#include "pager.h"

ScreenInfo Scr;
PagerWindow *Start = NULL;
PagerWindow *FocusWin = NULL;

Display *dpy;			/* which display are we talking to */

char *PagerFore = "black";
char *PagerBack = "white";
char *font_string = "fixed";
char *smallFont = NULL;
char *HilightC = "black";

int window_w = 0, window_h = 0, window_x = 0, window_y = 0;
int icon_x = -10000, icon_y = -10000, icon_w = 0, icon_h = 0;
int usposition = 0, uselabel = 1;
int xneg = 0, yneg = 0;
extern DeskInfo *Desks;
int StartIconic = 0;
int Rows = -1, Columns = -1;
int desk1 = 0, desk2 = 0;
int ndesks = 0;


/* ParseOptions does a lot to get the following options:
   Geometery of the window
   Rows of desks
   Columns of desks
   Icon geometry, start iconified flag
   Desktop scale (divisor; wierd interaction w/ geometry?)
   Font, smallFont
   FG, BG, Hilight colors
   Pager labels for each desktop
   Colors for each desktop
 */

main()
{
  /* parse args */
  desk1 = atoi(argv[6]);
  desk2 = atoi(argv[7]);

  if (desk2 < desk1) {
    itemp = desk1;
    desk1 = desk2;
    desk2 = itemp;
  }
  ndesks = desk2 - desk1 + 1;

  Desks = (DeskInfo *) malloc(ndesks * sizeof(DeskInfo));
  for (i = 0; i < ndesks; i++) {
    sprintf(line, "Desk %d", i + desk1);
    strcpy(&Desks[i].label, line);
    strcpy(&Desks[i].Dcolor, PagerBack);
#ifdef DEBUG
    fprintf(stderr, "[main]: Desks[%d].Dcolor == %s\n", i, Desks[i].Dcolor);
#endif /* DEBUG */
  }

  /* open a pager window */
  initialize_pager();

}


/***********************************************************************
 *
 *  Procedure:
 *	Process message - examines packet types, and takes appropriate action
 *
 ***********************************************************************/
void 
process_message(unsigned long type, unsigned long *body)
{
  switch (type) {
  case M_ADD_WINDOW:
    list_configure(body);
    break;
  case M_CONFIGURE_WINDOW:
    list_configure(body);
    break;
  case M_DESTROY_WINDOW:
    list_destroy(body);
    break;
  case M_FOCUS_CHANGE:
    list_focus(body);
    break;
  case M_NEW_PAGE:
    list_new_page(body);
    break;
  case M_NEW_DESK:
    list_new_desk(body);
    break;
  case M_RAISE_WINDOW:
    list_raise(body);
    break;
  case M_LOWER_WINDOW:
    list_lower(body);
    break;
  case M_ICONIFY:
  case M_ICON_LOCATION:
    list_iconify(body);
    break;
  case M_DEICONIFY:
    list_deiconify(body);
    break;
  case M_ICON_NAME:
    list_icon_name(body);
    break;
  case M_END_WINDOWLIST:
    list_end();
    break;
  default:
    list_unknown(body);
    break;
  }
}

/***********************************************************************
 *
 *  Procedure:
 *	list_add - displays packet contents to stderr
 *
 ***********************************************************************/
void 
list_add(unsigned long *body)
{
  PagerWindow *t, **prev;
  int i = 0;

  t = Start;
  prev = &Start;
  while (t != NULL) {
    prev = &(t->next);
    t = t->next;
    i++;
  }
  *prev = (PagerWindow *) safemalloc(sizeof(PagerWindow));
  (*prev)->w = body[0];
  (*prev)->t = (char *) body[2];
  (*prev)->frame = body[1];
  (*prev)->x = body[3];
  (*prev)->y = body[4];
  (*prev)->width = body[5];
  (*prev)->height = body[6];
  (*prev)->desk = body[7];
  (*prev)->next = NULL;
  (*prev)->flags = body[8];
  (*prev)->icon_name = NULL;
  (*prev)->title_height = body[9];
  (*prev)->border_width = body[10];
  (*prev)->icon_w = body[19];
  (*prev)->icon_pixmap_w = body[20];
  (*prev)->text = body[22];
  (*prev)->back = body[23];
  AddNewWindow(*prev);
}

/***********************************************************************
 *
 *  Procedure:
 *	list_configure - displays packet contents to stderr
 *
 ***********************************************************************/
void 
list_configure(unsigned long *body)
{
  PagerWindow *t;
  Window target_w;

  target_w = body[0];
  t = Start;
  while ((t != NULL) && (t->w != target_w)) {
    t = t->next;
  }
  if (t == NULL) {
    list_add(body);

  } else {
    t->t = (char *) body[2];
    t->frame = body[1];
    t->frame_x = body[3];
    t->frame_y = body[4];
    t->frame_width = body[5];
    t->frame_height = body[6];
    t->title_height = body[9];
    t->border_width = body[10];
    t->flags = body[8];
    t->icon_w = body[19];
    t->icon_pixmap_w = body[20];
    t->text = body[22];
    t->back = body[23];
    if (t->flags & ICONIFIED) {
      t->x = t->icon_x;
      t->y = t->icon_y;
      t->width = t->icon_width;
      t->height = t->icon_height;
      if (t->flags & SUPPRESSICON) {
	t->x = -10000;
	t->y = -10000;
      }
    } else {
      t->x = t->frame_x;
      t->y = t->frame_y;
      t->width = t->frame_width;
      t->height = t->frame_height;
    }
    if (t->desk != body[7]) {
      ChangeDeskForWindow(t, body[7]);
    } else
      MoveResizePagerView(t);
    if (FocusWin == t)
      Hilight(t, True);
    else
      Hilight(t, False);
  }
}

/***********************************************************************
 *
 *  Procedure:
 *	list_destroy - displays packet contents to stderr
 *
 ***********************************************************************/
void 
list_destroy(unsigned long *body)
{
  PagerWindow *t, **prev;
  Window target_w;

  target_w = body[0];
  t = Start;
  prev = &Start;
  while ((t != NULL) && (t->w != target_w)) {
    prev = &(t->next);
    t = t->next;
  }
  if (t != NULL) {
    if (prev != NULL)
      *prev = t->next;
    /* remove window from the chain */
    if (t->PagerView != None)
      XDestroyWindow(dpy, t->PagerView);
    XDestroyWindow(dpy, t->IconView);
    if (FocusWin == t)
      FocusWin = NULL;

    free(t);
  }
}

/***********************************************************************
 *
 *  Procedure:
 *	list_focus - displays packet contents to stderr
 *
 ***********************************************************************/
void 
list_focus(unsigned long *body)
{
  PagerWindow *t, *temp;
  Window target_w;
  extern Pixel focus_pix, focus_fore_pix;

  target_w = body[0];

  focus_pix = body[4];
  focus_fore_pix = body[3];
  t = Start;
  while ((t != NULL) && (t->w != target_w)) {
    t = t->next;
  }
  if (t != FocusWin) {
    temp = FocusWin;
    FocusWin = t;

    if (temp != NULL)
      Hilight(temp, OFF);
    if (FocusWin != NULL)
      Hilight(FocusWin, ON);
  }
}

/***********************************************************************
 *
 *  Procedure:
 *	list_new_page - displays packet contents to stderr
 *
 ***********************************************************************/
void 
list_new_page(unsigned long *body)
{
  Scr.Vx = (long) body[0];
  Scr.Vy = (long) body[1];
  Scr.CurrentDesk = (long) body[2];
  if ((Scr.VxMax != body[3]) || (Scr.VyMax != body[4])) {
    Scr.VxMax = body[3];
    Scr.VyMax = body[4];
    ReConfigure();
  }
  MovePage();
  MoveStickyWindows();
  Hilight(FocusWin, OFF);
  Hilight(FocusWin, ON);
}

/***********************************************************************
 *
 *  Procedure:
 *	list_new_desk - displays packet contents to stderr
 *
 ***********************************************************************/
void 
list_new_desk(unsigned long *body)
{
  int oldDesk;

  oldDesk = Scr.CurrentDesk;
  Scr.CurrentDesk = (long) body[0];

  MovePage();

  DrawGrid(oldDesk - desk1, 1);
  DrawGrid(Scr.CurrentDesk - desk1, 1);
  MoveStickyWindows();
  Hilight(FocusWin, OFF);
  Hilight(FocusWin, ON);
}

/***********************************************************************
 *
 *  Procedure:
 *	list_raise - displays packet contents to stderr
 *
 ***********************************************************************/
void 
list_raise(unsigned long *body)
{
  PagerWindow *t;
  Window target_w;

  target_w = body[0];
  t = Start;
  while ((t != NULL) && (t->w != target_w)) {
    t = t->next;
  }
  if (t != NULL) {
    if (t->PagerView != None)
      XRaiseWindow(dpy, t->PagerView);
    XRaiseWindow(dpy, t->IconView);
  }
}


/***********************************************************************
 *
 *  Procedure:
 *	list_lower - displays packet contents to stderr
 *
 ***********************************************************************/
void 
list_lower(unsigned long *body)
{
  PagerWindow *t;
  Window target_w;

  target_w = body[0];
  t = Start;
  while ((t != NULL) && (t->w != target_w)) {
    t = t->next;
  }
  if (t != NULL) {
    if (t->PagerView != None)
      XLowerWindow(dpy, t->PagerView);
    if ((t->desk - desk1 >= 0) && (t->desk - desk1 < ndesks))
      XLowerWindow(dpy, Desks[t->desk - desk1].CPagerWin);
    XLowerWindow(dpy, t->IconView);
  }
}


/***********************************************************************
 *
 *  Procedure:
 *	list_unknow - handles an unrecognized packet.
 *
 ***********************************************************************/
void 
list_unknown(unsigned long *body)
{
  /*  fprintf(stderr,"Unknown packet type\n"); */
}

/***********************************************************************
 *
 *  Procedure:
 *	list_iconify - displays packet contents to stderr
 *
 ***********************************************************************/
void 
list_iconify(unsigned long *body)
{
  PagerWindow *t;
  Window target_w;

  target_w = body[0];
  t = Start;
  while ((t != NULL) && (t->w != target_w)) {
    t = t->next;
  }
  if (t == NULL) {
    return;
  } else {
    t->t = (char *) body[2];
    t->frame = body[1];
    t->icon_x = body[3];
    t->icon_y = body[4];
    t->icon_width = body[5];
    t->icon_height = body[6];
    t->flags |= ICONIFIED;
    t->x = t->icon_x;
    t->y = t->icon_y;
    if (t->flags & SUPPRESSICON) {
      t->x = -10000;
      t->y = -10000;
    }
    t->width = t->icon_width;
    t->height = t->icon_height;
    MoveResizePagerView(t);
  }
}


/***********************************************************************
 *
 *  Procedure:
 *	list_deiconify - displays packet contents to stderr
 *
 ***********************************************************************/

void 
list_deiconify(unsigned long *body)
{
  PagerWindow *t;
  Window target_w;

  target_w = body[0];
  t = Start;
  while ((t != NULL) && (t->w != target_w)) {
    t = t->next;
  }
  if (t == NULL) {
    return;
  } else {
    t->flags &= ~ICONIFIED;
    t->x = t->frame_x;
    t->y = t->frame_y;
    t->width = t->frame_width;
    t->height = t->frame_height;
    MoveResizePagerView(t);
    if (FocusWin == t)
      Hilight(t, ON);
    else
      Hilight(t, OFF);
  }
}


/***********************************************************************
 *
 *  Procedure:
 *	list_icon_name - displays packet contents to stderr
 *
 ***********************************************************************/
void 
list_icon_name(unsigned long *body)
{
  PagerWindow *t;
  Window target_w;

  target_w = body[0];
  t = Start;
  while ((t != NULL) && (t->w != target_w)) {
    t = t->next;
  }
  if (t != NULL) {
    if (t->icon_name != NULL)
      free(t->icon_name);
    CopyString(&t->icon_name, (char *) (&body[3]));
    LabelWindow(t);
    LabelIconWindow(t);
  }
}




/***********************************************************************
 *
 *  Procedure:
 *	list_end - displays packet contents to stderr
 *
 ***********************************************************************/
void 
list_end(void)
{
  unsigned int nchildren, i;
  Window root, parent, *children;
  PagerWindow *ptr;

  if (!XQueryTree(dpy, Scr.Root, &root, &parent, &children, &nchildren))
    return;

  for (i = 0; i < nchildren; i++) {
    ptr = Start;
    while (ptr != NULL) {
      if ((ptr->frame == children[i]) || (ptr->icon_w == children[i]) ||
	  (ptr->icon_pixmap_w == children[i])) {
	if (ptr->PagerView != None)
	  XRaiseWindow(dpy, ptr->PagerView);
	XRaiseWindow(dpy, ptr->IconView);
      }
      ptr = ptr->next;
    }
  }


  if (nchildren > 0)
    XFree((char *) children);
}
