/* $Id$
 * icons.h
 */

#ifndef ICONS_H
#define ICONS_H

#include "image.h"

void CreateIconWindow(ScwmWindow * tmp_win, int def_x, int def_y);
void DrawIconWindow(ScwmWindow * Tmp_win);
void RedoIconName(ScwmWindow * Tmp_win);
void AutoPlace(ScwmWindow * t);
void DeIconify(ScwmWindow * tmp_win);
void Iconify(ScwmWindow * tmp_win, int def_x, int def_y);
void SetMapStateProp(ScwmWindow * tmp_win, int state);
void redraw_icon_titles();

#define ICON_P_WIDTH(sw) (sw->icon_image != SCM_BOOL_F? \
			  IMAGE(sw->icon_image)->width : 0)
#define ICON_P_HEIGHT(sw) (sw->icon_image != SCM_BOOL_F? \
			   IMAGE(sw->icon_image)->height : 0)

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
