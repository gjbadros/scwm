/* $Id$ */
#ifndef STRING_TOKEN_H
#define STRING_TOKEN_H
char *stripcpy(char *source);

#ifdef MISSING_STRCASECMP
int strcasecmp(char *s1, char *s2);
int strncasecmp(char *s1, char *s2, int n);
#endif

#endif
