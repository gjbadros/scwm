/* $Id$
 *
 * Copyright (C) 1997, 1998 Maciej Stachowiak and Greg J. Badros
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
 * As a special exception, this file may alternatively be distributed under 
 * the fvwm license (see COPYING.FVWM).
 *
 */

#include <config.h>

#include <ctype.h>
#include <string.h>
#include "string_token.h"


/* IchIgnoreCaseInSz(sz,ch)
 * Return the offset of ch in sz, ignoring case
 */
int
IchIgnoreCaseInSz(const char *sz, char ch)
{
  int ich = 0;
  ch = tolower(ch);
  while (*sz && tolower(*sz++) != ch) ich++;
  if (*sz == '\0') ich = -1;
  return ich;
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
