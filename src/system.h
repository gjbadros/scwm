/* $Id$ */
#ifndef SYSTEM_H
#define SYSTEM_H

void *safemalloc(int length);
int gethostname(char *client, size_t length);
char *envDupExpand(const char *s, int extra);
void sleep_ms(int n);
int GetFdWidth(void);

#endif
