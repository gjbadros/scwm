



/**************************************************************************/
/* If I do ALL this, I can compile OK with -Wall -Wstrict-prototypes on the
 * alpha 
 */
#include <sys/types.h>
#include <sys/time.h>


extern int select(int, fd_set *, fd_set *, fd_set *, struct timeval *);

/* string manipulation */
#ifdef __GNUC__
extern size_t strlen(char *);

#endif

extern void bzero(char *, int);
extern int gethostname(char *, int);

/**************************************************************************/

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
