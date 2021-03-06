/* $Id$
 * mwmcom.h
 * Copyright (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef MWMCOM_H
#define MWMCOM_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/***************************************************************************
 * Please translate the strings into the language which you use for 
 * your pop-up menus.
 *
 * Some decisions about where a function is prohibited (based on 
 * mwm-function-hints) is based on a string comparison between the 
 * menu item and the strings below.
 ***************************************************************************/
#define MOVE_STRING "move"
#define RESIZE_STRING1 "size"
#define RESIZE_STRING2 "resize"
#define MINIMIZE_STRING "minimize"
#define MINIMIZE_STRING2 "iconify"
#define MAXIMIZE_STRING "maximize"
#define CLOSE_STRING1 "close"
#define CLOSE_STRING2 "delete"
#define CLOSE_STRING3 "destroy"
#define CLOSE_STRING4 "quit"

#endif /* MWMCOM_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
