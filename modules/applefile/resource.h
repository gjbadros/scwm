#include "applefile.h"

typedef struct resource_file resource_file;

/* Load a resource file from a buffer

   FIXJTL: Right now, little data validation is done.  Don't call this
   on a buffer that you aren't sure is a resource file.  Don't be
   surprised if corrupt resource files cause crashes. */
resource_file * load_resource_file(char * pFileBuffer);

void free_resource_file(resource_file * prf);

/* Get a resource by a type and ID; the returned structure should NOT
   be modified.  The structure and the data it points to will be valid
   until free_resource_file() is called on this file; if the data is
   needed longer than that, it should be copied by the caller.  No
   interpretation of the resource data is done, so any necessary byte
   swapping will have to be done by the caller.  The read_u32 and
   read_u16 macros may be helpful for this. Returns NULL if no such
   resource is found. */
resource_data * rf_get_resource_by_id(resource_file * prf, char *type, int id);

/*
 * Resource data format found via trial and error, helped with Resourcer
 * template files (decoded via trial and error).
 * Errors likely abound
 */

/* HBYT: halfbyte?  uses a whole byte, info in top half
   HLNG: long, 4 bytes
   WBxx: xx bits
   DBYT: byte?  uses 1 byte
   RGNC: region code.  2 bytes?
   PSTR: P-string
   ZCNT: count for list. number of elements - 1
   LSTC: list begin...count?  ZCNT precedes; LSTE ends
   LSTE: list end
   DWRD: 2 bytes
   COLR: 6 bytes - rgb triple, 2 bytes each
*/

