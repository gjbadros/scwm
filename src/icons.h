/* $Id$
 * icons.h
 */

#ifndef ICONS_H
#define ICONS_H

void CreateIconWindow(ScwmWindow * tmp_win, int def_x, int def_y);
void DrawIconWindow(ScwmWindow * Tmp_win);
void RedoIconName(ScwmWindow * Tmp_win);
void AutoPlace(ScwmWindow * t);
void DeIconify(ScwmWindow * tmp_win);
void Iconify(ScwmWindow * tmp_win, int def_x, int def_y);
void SetMapStateProp(ScwmWindow * tmp_win, int state);

#define ICON_P_WIDTH(sw) (sw->picIcon?sw->picIcon->width:0)
#define ICON_P_HEIGHT(sw) (sw->picIcon?sw->picIcon->height:0)

#endif
