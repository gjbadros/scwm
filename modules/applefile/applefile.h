#ifndef _APPLEFILE_H_
#define _APPLEFILE_H_
struct AppleFile;
typedef struct AppleFile AppleFile;

typedef struct 
{
    int id;
    int cName;
    char * psName;
    int cData;
    unsigned char * pData;
} resource_data;

AppleFile * open_apple_file(char * filename);
void close_apple_file(struct AppleFile * paf);
resource_data * get_resource_by_id(AppleFile * paf, char type[4], int id);

/* These are all unsafe to call with args with side effects.

   Whether they're horrible inefficient or more efficient than real
   functions is an open issue; it depends on how smart the compiler is,
   and I haven't checked */

#define read_u32(rc) ((rc)[0] <<24) + ((rc)[1] << 16) + ((rc)[2] << 8) + (rc)[3]
#define read_u24(rc) ((rc)[0] <<16) + ((rc)[1] << 8)  + (rc)[2]
#define read_u16(rc) ((rc)[0] << 8) + (rc)[1]
#define read_s16(rc) (read_u16(rc) > 32768? read_u16(rc) - 65536: read_u16(rc))
#endif
