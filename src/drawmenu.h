/* $Id$
 * drawmenu.h
 * By Greg J. Badros -- Nov 22, 1997
 */

#ifndef DRAWMENU_H
#define DRAWMENU_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "scwmmenu.h"

/* This is the function any menu drawing code needs to implement */

void ConstructDynamicMenu(DynamicMenu *pmd);

#endif
