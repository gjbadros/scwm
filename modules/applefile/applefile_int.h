#include "applefile.h"
#include "scwm.h" /* for NEW and friends */

struct AppleFileVTable 
{
    int (*close)(AppleFile *);
    int (*resourceFork)(AppleFile *, void **ppStart, size_t * pLen);
};
typedef struct AppleFileVTable AppleFileVTable;

struct AppleFile 
{
    AppleFileVTable * pafvt;
    void * pData;
};

