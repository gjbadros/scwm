/* $Id$
 * placement.h
 * (C) 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef PLACEMENT_H
#define PLACEMENT_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "window.h"

Bool PlaceWindow(ScwmWindow *psw, int Desk);

void init_placement();

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
