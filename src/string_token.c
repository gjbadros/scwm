#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "system.h"

/* FIXGJB: strcasecmp and strncasecmp should be autoconf tested for,
   and versions should be linked in if they don't exist */
#ifdef MISSING_STRCASECMP
int 
strcasecmp(char *s1, char *s2)
{
  int c1, c2;
  int n, n2;

  n = strlen(s1);
  n2 = strlen(s2);
  if (n != n2)
    return 1;

  for (;;) {
    c1 = *s1;
    c2 = *s2;
    if (!c1 || !c2)
      return (c1 - c2);
    if (isupper(c1))
      c1 = 'a' - 1 + (c1 & 31);
    if (isupper(c2))
      c2 = 'a' - 1 + (c2 & 31);
    if (c1 != c2)
      return (c1 - c2);
    n--, s1++, s2++;
  }
}

int 
strncasecmp(char *s1, char *s2, int n)
{
  register int c1, c2;

  for (;;) {
    if (!n)
      return (0);
    c1 = *s1, c2 = *s2;
    if (!c1 || !c2)
      return (c1 - c2);
    if (isupper(c1))
      c1 = 'a' - 1 + (c1 & 31);
    if (isupper(c2))
      c2 = 'a' - 1 + (c2 & 31);
    if (c1 != c2)
      return (c1 - c2);
    n--, s1++, s2++;
  }
}
#endif

/****************************************************************************
 * 
 * Copies a string into a new, malloc'ed string
 * Strips leading spaces and trailing spaces and new lines
 *
 ****************************************************************************/
char *
stripcpy(char *source)
{
  char *tmp, *ptr;
  int len;

  if (source == NULL)
    return NULL;

  while (isspace(*source))
    source++;
  len = strlen(source);
  tmp = source + len - 1;
  while (((isspace(*tmp)) || (*tmp == '\n')) && (tmp >= source)) {
    tmp--;
    len--;
  }
  ptr = safemalloc(len + 1);
  strncpy(ptr, source, len);
  ptr[len] = 0;
  return ptr;
}


/* FIXGJB: strip this out */
/****************************************************************************
 *
 * Gets the next "word" of input from char string indata.
 * "word" is a string with no spaces, or a qouted string.
 * Return value is ptr to indata,updated to point to text after the word
 * which is extracted.
 * token is the extracted word, which is copied into a malloced
 * space, and must be freed after use. 
 *
 **************************************************************************/
char *
GetNextToken(char *indata, char **token)
{
  char *t, *start, *end, *text;

  t = indata;
  if (t == NULL) {
    *token = NULL;
    return NULL;
  }
  while (isspace(*t) && (*t != 0))
    t++;
  start = t;
  while (!isspace(*t) && (*t != 0)) {
    /* Check for qouted text */
    if (*t == '"') {
      t++;
      while ((*t != '"') && (*t != 0)) {
	/* Skip over escaped text, ie \" or \space */
	if ((*t == '\\') && (*(t + 1) != 0))
	  t++;
	t++;
      }
      if (*t == '"')
	t++;
    } else {
      /* Skip over escaped text, ie \" or \space */
      if ((*t == '\\') && (*(t + 1) != 0))
	t++;
      t++;
    }
  }
  end = t;

  text = malloc(end - start + 1);
  *token = text;

  while (start < end) {
    /* Check for qouted text */
    if (*start == '"') {
      start++;
      while ((*start != '"') && (*start != 0)) {
	/* Skip over escaped text, ie \" or \space */
	if ((*start == '\\') && (*(start + 1) != 0))
	  start++;
	*text++ = *start++;
      }
      if (*start == '"')
	start++;
    } else {
      /* Skip over escaped text, ie \" or \space */
      if ((*start == '\\') && (*(start + 1) != 0))
	start++;
      *text++ = *start++;
    }
  }
  *text = 0;
  if (*end != 0)
    end++;

  return end;
}
