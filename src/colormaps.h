/* $Id$
 * colormaps.h
 * Copyright (C) 1998, 1999, 2000  Greg J. Badros and Maciej Stachowiak 
 */

#ifndef COLORMAPS_H__
#define COLORMAPS_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "window_fwd.h"

void HandleColormapNotify(void);
void ReInstallActiveColormap(void);
void InstallWindowColormaps(ScwmWindow *psw);
void InstallRootColormap(void);
void UninstallRootColormap(void);
void FetchWmColormapWindows(ScwmWindow *psw);

#endif /* COLORMAPS_H__ */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

