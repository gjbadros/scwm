/* $Id$
 * (C) 1999 Toby Sargeant and Greg J. Badros
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>
#include "guile-compat.h"

#define CURSOR_IMPLEMENTATION
#include "cursor.h"
#include "scwm.h"
#include "window.h"
#include "image.h"
#include "screen.h"
#include "xmisc.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif


static
SCM
mark_cursor(SCM ARG_IGNORE(obj)) 
{
  return SCM_BOOL_F;
}

static
size_t 
free_cursor(SCM obj) 
{
  scwm_cursor *xp=CURSOR(obj);
  XFreeCursor(dpy,xp->cursor);
  /* we only free strings from non x cursors */
  if (xp->szName && xp->is_x_cursor)
    FREE((char *)xp->szName);
  FREE(xp);
  return 0;
}

static
int 
print_cursor(SCM obj, SCM port, scm_print_state *ARG_IGNORE(pstate)) 
{
  scwm_cursor *xp=CURSOR(obj);
  scm_puts("#<cursor ", port);
  if (xp->szName)
    scm_puts((char *) xp->szName, port); /* GJB:FIXME:CONST */
  else
    scm_write(gh_ulong2scm((unsigned long)xp->cursor), port);
  scm_putc('>', port);
  return 1;
}

typedef struct {
  const char *name;
  unsigned int shape;
} CursorNameMap;

#define CURS(X)	{ #X, XC_ ## X }

#include <X11/cursorfont.h>
static CursorNameMap map[] = {
  CURS(X_cursor),
  CURS(arrow),
  CURS(based_arrow_down),
  CURS(based_arrow_up),
  CURS(boat),
  CURS(bogosity),
  CURS(bottom_left_corner),
  CURS(bottom_right_corner),
  CURS(bottom_side),
  CURS(bottom_tee),
  CURS(box_spiral),
  CURS(center_ptr),
  CURS(circle),
  CURS(clock),
  CURS(coffee_mug),
  CURS(cross),
  CURS(cross_reverse),
  CURS(crosshair),
  CURS(diamond_cross),
  CURS(dot),
  CURS(dotbox),
  CURS(double_arrow),
  CURS(draft_large),
  CURS(draft_small),
  CURS(draped_box),
  CURS(exchange),
  CURS(fleur),
  CURS(gobbler),
  CURS(gumby),
  CURS(hand1),
  CURS(hand2),
  CURS(heart),
  CURS(icon),
  CURS(iron_cross),
  CURS(left_ptr),
  CURS(left_side),
  CURS(left_tee),
  CURS(leftbutton),
  CURS(ll_angle),
  CURS(lr_angle),
  CURS(man),
  CURS(middlebutton),
  CURS(mouse),
  CURS(pencil),
  CURS(pirate),
  CURS(plus),
  CURS(question_arrow),
  CURS(right_ptr),
  CURS(right_side),
  CURS(right_tee),
  CURS(rightbutton),
  CURS(rtl_logo),
  CURS(sailboat),
  CURS(sb_down_arrow),
  CURS(sb_h_double_arrow),
  CURS(sb_left_arrow),
  CURS(sb_right_arrow),
  CURS(sb_up_arrow),
  CURS(sb_v_double_arrow),
  CURS(shuttle),
  CURS(sizing),
  CURS(spider),
  CURS(spraycan),
  CURS(star),
  CURS(target),
  CURS(tcross),
  CURS(top_left_arrow),
  CURS(top_left_corner),
  CURS(top_right_corner),
  CURS(top_side),
  CURS(top_tee),
  CURS(trek),
  CURS(ul_angle),
  CURS(umbrella),
  CURS(ur_angle),
  CURS(watch),
  CURS(xterm)
};

#undef CURS

/* GJB:FIXME:TS: Why only even XC_* values--
   can we halve the size of the array?  Is it worth it? Maybe not */
static SCM preloaded_x_cursors[XC_num_glyphs];

static
SCM
x_cursor_to_scm(Cursor xc,Bool is_x_cursor, const char *szName) 
{
  scwm_cursor *sxp;
  SCM result;

  sxp=NEW(scwm_cursor);
  sxp->cursor=xc;
  sxp->is_x_cursor = is_x_cursor;
  if (szName)
    sxp->szName = szName;
  else
    sxp->szName = NULL;

  scwm_defer_ints();
  SCWM_NEWCELL_SMOB(result,scm_tc16_scwm_cursor,sxp);
  scm_permanent_object(result);
  scwm_allow_ints();

  return result;
}

SCM
get_scm_cursor_by_number(int cursor_num) 
{
  if (cursor_num>=0 && cursor_num<XC_num_glyphs) {
    SCM sc;
    Cursor c;

    if (preloaded_x_cursors[cursor_num]!=SCM_UNDEFINED) {
      return preloaded_x_cursors[cursor_num];
    }
    c = XCursorByNumber(cursor_num);
    sc = x_cursor_to_scm(c,True,map[cursor_num].name);
    return sc;
  }
  return SCM_BOOL_F;
}

/* Return SCM_BOOL_F if failed to find the name */
SCM
get_scm_cursor_by_name(const char *sz) 
{
  int i;
  int cursor_num = -1;

  cursor_num=XC_num_glyphs;
  for (i=0; i < ARRAY_SIZE(map); ++i) {
    if (!strcmp(sz,map[i].name)) {
      cursor_num=map[i].shape;
    }
  }

  if (cursor_num == -1)
    return SCM_BOOL_F;

  return get_scm_cursor_by_number(cursor_num);
}

Cursor 
XCursorByNumber(int cursor_num) 
{
  Cursor c = XCreateFontCursor(dpy,cursor_num);
  DBUG(("XCursorByNumber","Cursor num is: %d, XCursor is: %p\n",cursor_num,c));
  return c;
}


SCWM_PROC(set_window_cursor_x,"set-window-cursor!",2,0,0,
          (SCM win, SCM cursor),
"Set the default cursor for WIN to CURSOR. 
If CURSOR is #f, this undefines the cursor for WIN and
makes that window use its parent window's cursor.
See `get-x-cursor', and `create-pixmap-cursor' for ways
to create cursor objects.")
#define FUNC_NAME s_set_window_cursor_x
{
  Window w;
  VALIDATE_ARG_WIN_ROOTSYM_OR_NUM_COPY(1,win,w);
  if (cursor == SCM_BOOL_F) {
    XUndefineCursor(dpy, w);
  } else {
    VALIDATE_ARG_CURSOR(2,cursor);
    XDefineCursor(dpy, w, XCURSOR(cursor));
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCWM_PROC(get_x_cursor,"get-x-cursor",1,0,0,
	  (SCM name_or_number),
"Return the cursor object corresponding to NAME-OR-NUMBER.
NAME-OR-NUMBER can be either a string naming an X11 cursor (e.g.,
\"trek\") or a number specifying the cursor number.  See 
<file>X11/cursorfont.h</file> for the standard cursors.  Note
that the \"XC_\" macro prefix should be omitted when used with
this procedure..")
#define FUNC_NAME s_get_x_cursor
{
  SCM sc;
  if (gh_string_p(name_or_number)) {
    char *c=gh_scm2newstr(name_or_number,NULL);
    sc=get_scm_cursor_by_name(c);
    FREE(c);
    return sc;
  } else if (gh_number_p(name_or_number)) {
    int i;
    VALIDATE_ARG_INT_RANGE_COPY(1,name_or_number,0,XC_num_glyphs-1,i);
    return get_scm_cursor_by_number(i);
  }
  scm_wrong_type_arg(FUNC_NAME,1,name_or_number);
}
#undef FUNC_NAME

SCWM_PROC(create_pixmap_cursor,"create-pixmap-cursor",1,4,0,
          (SCM image, SCM fg_color, SCM bg_color, SCM x_hotspot, SCM y_hotspot),
"Create and return a new cursor object from the pixmap image.
IMAGE specifies the look of the cursor that will be returned.
FG-COLOR and BG-COLOR specify the foreground and background colors
respectively.  X-HOTSPOT, Y-HOTSPOT give the x and y offset for the
cursor's hot spot (from the top-left of the cursor).")
#define FUNC_NAME s_create_pixmap_cursor
{
  int dpixX, dpixY;
  VALIDATE_ARG_IMAGE(1,image);
  VALIDATE_ARG_COLOR_OR_SYM_USE_WHITE(2,fg_color);
  VALIDATE_ARG_COLOR_OR_SYM_USE_BLACK(3,bg_color);
  VALIDATE_ARG_INT_COPY_USE_DEF(4,x_hotspot,dpixX,1);
  VALIDATE_ARG_INT_COPY_USE_DEF(5,y_hotspot,dpixY,1);
  { /* scope */
    scwm_image *pimg = IMAGE(image);
    Pixel fg = XCOLOR(fg_color);
    Pixel bg = XCOLOR(bg_color);
    XColor c_fg = XColorFromPixel(fg);
    XColor c_bg = XColorFromPixel(bg);
    Pixmap source = Pixmap1DeepFromPixmap(pimg->image,fg,bg);
    Pixmap mask = Pixmap1DeepFromPixmap(pimg->mask,fg,bg);
    Cursor crsr = XCreatePixmapCursor(dpy, source, mask, &c_fg, &c_bg, dpixX, dpixY);

    XFreePixmap(dpy,source);
    XFreePixmap(dpy,mask);

    return x_cursor_to_scm(crsr,False,SzNewImageShortName(pimg));
  }
}
#undef FUNC_NAME


void
CreateScmGlobalCursors()
{
  SCWM_VAR_INIT(cursor_set_focus,"cursor-set-focus", get_scm_cursor_by_number(XC_hand2));
  /** The cursor to use for set focus actions, defaults to hand2. */

  SCWM_VAR_INIT(cursor_move,"cursor-move", get_scm_cursor_by_number(XC_fleur));
  /** The cursor to use for move actions, defaults to fleur. */

  SCWM_VAR_INIT(cursor_icon,"cursor-icon", get_scm_cursor_by_number(XC_top_left_arrow));
  /** The cursor to use for icon windows, defaults to top_left_arrow. */

  SCWM_VAR_INIT(cursor_kill,"cursor-kill", get_scm_cursor_by_number(XC_pirate));
  /** The cursor to use for selecting a window to kill, defaults to pirate. */

  SCWM_VAR_INIT(cursor_select,"cursor-select", get_scm_cursor_by_number(XC_dot));
  /** The cursor to use for selecting a window, defaults to dot. */

  SCWM_VAR_INIT(cursor_menu,"cursor-menu", get_scm_cursor_by_number(XC_right_ptr));
  /** The cursor to use when in a menu, defaults to sb_left_arrow. */
}

MAKE_SMOBFUNS(cursor);

void
init_cursor() 
{
  int i;
  REGISTER_SCWMSMOBFUNS(cursor);

  for (i=0;i<XC_num_glyphs;i++) {
    preloaded_x_cursors[i]=SCM_UNDEFINED;
  }

#ifndef SCM_MAGIC_SNARFER
#include "cursor.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */
