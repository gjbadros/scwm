/* $Id$
 * colormaps.h
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef COLORMAPS_H__
#define COLORMAPS_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "window_fwd.h"

#define ScwmWindow    struct ScwmWindow

void HandleColormapNotify(void);
void ReInstallActiveColormap(void);
void InstallWindowColormaps(ScwmWindow *psw);
void InstallRootColormap(void);
void UninstallRootColormap(void);
void FetchWmColormapWindows(ScwmWindow *psw);

#undef ScwmWindow

#endif /* COLORMAPS_H__ */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
