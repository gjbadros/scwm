/* $Id$ */
#ifndef SYSTEM_H
#define SYSTEM_H

#include <config.h>

void *safemalloc(int length);

#ifndef HAVE_GETHOSTNAME
int gethostname(char *client, size_t length);
#endif

char *envDupExpand(const char *s, int extra);
void sleep_ms(int n);
int GetFdWidth(void);
char *findFile(char *name, char *pathlist, int type);

#endif

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
