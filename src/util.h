/* $Id$
 * util.h
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
 */

#ifndef UTIL_H
#define UTIL_H

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include "screen.h"

void redraw_titlebars(ScwmDecor * fl, int extra_height);
void redraw_border(ScwmWindow *psw) ;
void redraw_borders(ScwmDecor *fl) ;
void refresh_common(Window win_or_root);
void ms_sleep(unsigned long ms);

#endif	/* UTIL_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

