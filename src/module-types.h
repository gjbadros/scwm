/* $Id$
 * module-types.h
 * Copyright (C) 1997, 1998, 1999, 2000 Greg J. Badros and Maciej Stachowiak
 */

/* Used this perl initially script to create ../scheme/module-types.scm
perl -ne 'BEGIN { print ";;;Auto-generated, do not edit\n(define-module (app scwm module-types))\n"; }
if (/^\#define (M_.*)\s+\((.*)\)/) {
my ($name,$val) = ($1, $2);
print "(define-public $name ", eval($val), ")\n";
}' < module-types.h >../scheme/module-types.scm

Later, hand edited the script to get the alist mapping numbers to names
   --Early 1998 gjb
*/

#ifndef MODULE_TYPES_H
#define MODULE_TYPES_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define M_NEW_PAGE           (1)
#define M_NEW_DESK           (1<<1)
#define M_ADD_WINDOW         (1<<2)
#define M_RAISE_WINDOW       (1<<3)
#define M_LOWER_WINDOW       (1<<4)
#define M_CONFIGURE_WINDOW   (1<<5)
#define M_FOCUS_CHANGE       (1<<6)
#define M_DESTROY_WINDOW     (1<<7)
#define M_ICONIFY            (1<<8)
#define M_DEICONIFY          (1<<9)
#define M_WINDOW_NAME        (1<<10)
#define M_ICON_NAME          (1<<11)
#define M_RES_CLASS          (1<<12)
#define M_RES_NAME           (1<<13)
#define M_END_WINDOWLIST     (1<<14)
#define M_ICON_LOCATION      (1<<15)
#define M_MAP                (1<<16)
#define M_FVWM_ERROR         (1<<17)
#define M_CONFIG_INFO        (1<<18)
#define M_END_CONFIG_INFO    (1<<19)
#define M_ICON_FILE          (1<<20)
#define M_DEFAULTICON        (1<<21)
#define M_STRING             (1<<22)
#define M_MINI_ICON          (1<<23)
#define M_WINDOWSHADE        (1<<24)
#define M_DEWINDOWSHADE      (1<<25)
#define MAX_MESSAGES         26
#define MAX_MASK             ((1<<MAX_MESSAGES)-1)

#endif
