/* $Id$
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
 *
 * #include "window_fwd.h" when you just need to
 * be able to talk about struct ScwmWindow *'s, and not
 * see their insides.
 */

#ifndef WINDOW_H__
#ifndef WINDOW_FWD_H__
#define WINDOW_FWD_H__

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#define Bool int /* from Xlib.h */

struct ScwmWindow;
/* Bit tricky because we need to avoid a redef error if window.h
   (the real thing) actually gets included after this file */
typedef struct ScwmWindow ScwmWindow;

#endif /* WINDOW_FWD_H__ */
#endif /* WINDOW_H__ */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

