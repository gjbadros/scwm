#ifndef SYSCOMPAT_H
#define SYSCOMPAT_H

#include <config.h>

#include <sys/wait.h>
#ifdef HAVE_WAITPID
#define ReapChildren()  while ((waitpid(-1, NULL, WNOHANG)) > 0)
#else /* !HAVE_WAITPID */
#define ReapChildren()  while ((wait3(NULL, WNOHANG, NULL)) > 0)
#endif /* !HAVE_WAITPID */

#ifndef HAVE_GETHOSTNAME
int gethostname(char *client, size_t length);
#endif

#ifndef HAVE_USLEEP
void usleep(unsigned long n);
#endif

#ifndef HAVE_STRCASECMP
int  strcasecmp(char *s1, char *s2);
#endif /* !HAVE_STRCASECMP */

#ifndef HAVE_STRNCASECMP
int  strncasecmp(char *s1, char *s2, int n);
#endif /* !HAVE_STRNCASECMP */

#endif /* SYSCOMPAT_H */
