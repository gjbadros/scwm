/* $Id$
 * colormaps.h
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef COLORMAPS_H
#define COLORMAPS_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "window.h"

void HandleColormapNotify(void);
void ReInstallActiveColormap(void);
void InstallWindowColormaps(ScwmWindow * tmp);
void InstallRootColormap(void);
void UninstallRootColormap(void);
void FetchWmColormapWindows(ScwmWindow * tmp);

#endif COLORMAPS_H
