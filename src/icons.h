/* $Id$
 * icons.h
 */

#ifndef ICONS_H
#define ICONS_H

#include "image.h"

void CreateIconWindow(ScwmWindow *psw, int def_x, int def_y);
void DrawIconWindow(ScwmWindow *psw);
void RedoIconName(ScwmWindow *psw);
void AutoPlace(ScwmWindow *psw);
void DeIconify(ScwmWindow *psw);
void Iconify(ScwmWindow *psw, int def_x, int def_y);
void SetMapStateProp(ScwmWindow *psw, int state);
void redraw_icon_titles();

#define ICON_P_WIDTH(psw) (psw->icon_image != SCM_BOOL_F? \
			  IMAGE(psw->icon_image)->width : 0)
#define ICON_P_HEIGHT(psw) (psw->icon_image != SCM_BOOL_F? \
			   IMAGE(psw->icon_image)->height : 0)

#define ICON_HEIGHT (FONTHEIGHT(Scr.icon_font)+6)

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
