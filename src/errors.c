/* $Id$ */
/*
 *      Copyright (C) 1997, Maciej Stachowiak and Greg J. Badros
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.GPL.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 */

#include <config.h>

#include <guile/gh.h>
#include "errors.h"


/* FIXGJB: this mechanism is *terrible*; I'm all for localizable
 code, trying to keep indexes in sync with this array is
 a maintenance headache, and debugging w/ broken error messages
 is an utterly depressing notion */

/* MS: the eventual intention is to use preprocessor symbols instead
   of explicit constants to represent different errors. I've just been
   too lazy to put them in the header. Also, it is possible to give
   scheme objects as parameters to erros, I just haven't examined the
   guile error system in sufficient depth to figure out the best way
   to do it yet. Localization should be doable by initialzing this
   array in a slightly smarter way, */

static char *scwm_errors[] =
{
  "",
  "Unable to load 'fixed' font.",
  "Unable to parse color.",
  "Unable to allocate color.",
  "Bad binding specifier.",
  "Justification must be \'left, \'right or \'center.",
  "Window no longer valid.",
  "Bad height argument; must be from 5 to 256.",
  "No binding contexts specified.",
  "Invalid binding context.",
  "Colormap focus must be \'focus or \'mouse.",
  "Bad menu entry specifier.",
  "Bad event for binding.",
  "Window focus must be \'click, \'mouse, \'sloppy or \'none.",
  "Unable to load image."
};


void 
scwm_error(char *subr, int err)
{
  scm_error(gh_symbol2scm("scwm-error"), subr, "%s",
	    gh_list(gh_str02scm(scwm_errors[err]), SCM_UNDEFINED),
	    gh_list(gh_int2scm(err), SCM_UNDEFINED));
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
