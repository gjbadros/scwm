/* $Id$
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
 */

#ifndef SHUTDOWN_H
#define SHUTDOWN_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

void Done(int restart, char *command);
void SaveDesktopState(void);
void run_startup_hook();

#endif /* SHUTDOWN_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

