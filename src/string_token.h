/* $Id$ 
 * string_token.h
 */

#ifndef STRING_TOKEN_H
#define STRING_TOKEN_H

#ifndef HAVE_STRCASECMP
int strcasecmp(char *s1, char *s2);
#endif /* !HAVE_STRCASECMP */

#ifndef HAVE_STRNCASECMP
int strncasecmp(char *s1, char *s2, int n);
#endif /* !HAVE_STRNCASECMP */

int IchIgnoreCaseInSz(const char *sz, char ch);

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
