/* $Id$
 * util.h
 * Copyright (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef UTIL_H
#define UTIL_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "screen.h"

void redraw_titlebars(ScwmDecor * fl, int extra_height);
void redraw_borders(ScwmDecor *fl) ;
void refresh_common(Window win_or_root);
void ms_sleep(unsigned long ms);

#endif	/* UTIL_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta */

