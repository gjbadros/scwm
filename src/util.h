/* $Id$
 * util.h
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef UTIL_H
#define UTIL_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

void redraw_titlebars(ScwmDecor * fl, int extra_height);
void redraw_borders(ScwmDecor *fl) ;
void refresh_common(Window win_or_root);
SCM call_thunk_with_message_handler(SCM thunk);

#endif	/* UTIL_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
