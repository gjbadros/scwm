/* $Id$
 * drawmenu.h
 * By Greg J. Badros -- Nov 22, 1997
 */

#ifndef DRAWMENU_H
#define DRAWMENU_H

#include "scwmmenu.h"

void PaintMenuItem(Window w, DynamicMenu *pmd, MenuItemInMenu *pmiim);
void ConstructDynamicMenu(DynamicMenu *pmd);
void PaintDynamicMenu(DynamicMenu *pmd, XEvent *pxe);
void menu_font_update();

#endif
