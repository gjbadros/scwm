/* $Id$
 */

#include "applefile_int.h"

#include "applesingledouble.h"
#include "resource.h"

struct AppleFile *
open_apple_file(char * filename)
{
    struct AppleFile * paf;

    paf = NEW(struct AppleFile);

    do {
	/* FIXJTL: if there's any more of these, there should be a general
	   way of registering them instead of hardcoding them here */
	if (open_macbinary_file(filename, paf) == 0)
	    break;
	if (open_applesingle_file(filename, paf) == 0)
	    break;
	if (open_appledouble_file(filename, paf) == 0)
	    break;
	
	FREE(paf);
    } while (0);

    if (paf->pafvt == NULL) {
	FREE(paf);
	return NULL;
    }

    paf->pafvt->resourceFork(paf, &pResource, &cResourse);
    paf->resource_file = load_resource_file(pResource);
    return paf;
}

void
close_apple_file(struct AppleFile * paf)
{
    free_resource_file(paf->resource_file);
    paf->pafvt->close(paf);
    FREE(paf);
}

resource_data *
get_resource_by_id(AppleFile * paf, char type[4], int id)
{
    return rf_get_resource_by_id(paf->resource_file, type, id);
}

