/* $Id$
 *
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef SHUTDOWN_H
#define SHUTDOWN_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

void Done(int restart, char *command);
void SaveDesktopState(void);

SCM restart(SCM command);
SCM scwm_quit(SCM args);

void init_shutdown();

#endif /* SHUTDOWN_H */
