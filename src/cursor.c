/* $Id$
 * (C) 1999 Toby Sargeant and Greg J. Badros
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>

#define CURSOR_IMPLEMENTATION
#include "cursor.h"
#include "scwm.h"
#include "window.h"

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
  FREE(xp);
  return sizeof(*xp);
}

static
int 
print_cursor(SCM obj, SCM port, scm_print_state *ARG_IGNORE(pstate)) 
{
  scwm_cursor *xp=CURSOR(obj);
  scm_puts("#<cursor ", port);
  scm_write(gh_ulong2scm((unsigned long)xp->cursor), port);
  scm_putc('>', port);
  return 1;
}

/* ========================================================================== */
typedef struct {
  const char *name;
  unsigned int shape;
} CursorNameMap;

/* ========================================================================== */
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

static SCM preloaded_x_cursors[XC_num_glyphs];

/* ========================================================================== */
static
SCM
x_cursor_to_scm(Cursor xc,int is_x_cursor) 
{
  scwm_cursor *sxp;
  SCM result;

  sxp=NEW(scwm_cursor);
  sxp->cursor=xc;
  sxp->is_x_cursor=is_x_cursor?1:0;
  gh_defer_ints();
  SCWM_NEWCELL_SMOB(result,scm_tc16_scwm_cursor,sxp);
  scm_permanent_object(result);
  gh_allow_ints();
  return result;
}

/* ========================================================================== */
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
    sc=x_cursor_to_scm(c,1);
    return sc;
  }
  return SCM_UNDEFINED;
}

/* Return SCM_BOOL_F if failed to find the name */
SCM
get_scm_cursor_by_name(const char *sz) 
{
  int i;
  unsigned int cursor_num = -1;

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


/* ========================================================================== */
SCWM_PROC(get_x_cursor,"get-x-cursor",1,0,0,
	  (SCM name_or_number))
     /** Return the cursor object corresponding to NAME-OR-NUMBER.
NAME-OR-NUMBER can be either a string naming an X11 cursor (e.g.,
"trek") or a number specifying the cursor number.  See 
<file>X11/cursorfont.h</file> for the standard cursors.  Note
that the "XC_" macro prefix should be omitted when used with
this procedure.. */
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

/* ========================================================================== */
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
/* vim:ts=8:sw=2:sta */


