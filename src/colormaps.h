/* $Id$
 * colormaps.h
 */

#ifndef COLORMAPS_H
#define COLORMAPS_H

void HandleColormapNotify(void);
void ReInstallActiveColormap(void);
void InstallWindowColormaps(ScwmWindow * tmp);
void InstallRootColormap(void);
void UninstallRootColormap(void);
void FetchWmColormapWindows(ScwmWindow * tmp);

#endif COLORMAPS_H
