/* $Id$
 * drawing.h 
 * (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
 */

#ifndef DRAWING_H
#define DRAWING_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/X.h>
#include <X11/Intrinsic.h>
/*
SCM set_drawing_mask_x( SCM value );

SCM xlib_draw_rectangle_x( SCM x, SCM y, SCM width, SCM height );
SCM xlib_draw_line_x( SCM x1, SCM y1, SCM x2, SCM y2 );
SCM xlib_draw_arc_x( SCM x, SCM y, SCM width, SCM height, SCM angle1, SCM angle2 );
SCM xlib_set_line_width_x( SCM width );
SCM xlib_set_fill_style_x( SCM style );
*/
void init_drawing_gcs();
void init_drawing();

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
