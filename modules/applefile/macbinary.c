#include <assert.h>
#include <fcntl.h>
#include <stddef.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "macbinary.h"
#include "crc.h"

/* The MacBinary/MacBinary II/MacBinary III file format

   By JTL, based on specifications found at
   http://www.lazerware.com/formats/macbinary.html
*/
struct MBHeader
{
    unsigned char u8_old_version[1];	   /* 0 for I; kept 0 for II&III */
    unsigned char u8_fname_length[1];	   /* filename length */
    unsigned char u8_fname[63];		   /* filename; only fname_length used*/

    /* begin finder info */
    unsigned char ost_type[4];		   /* file type */
    unsigned char ost_creator[4];	   /* file creator */
    unsigned char u8_flags[1];		   /* finder flags */
    unsigned char u8_unused1[1];	   /* 0 */
    unsigned char u16_ypos[2];		   /* vertical position of icon */
    unsigned char u16_xpos[2];		   /* horizontal position of icon */
    unsigned char u16_window[2];	   /* window or folder ID */
    /* end finder info */

    unsigned char u8_protected[1];	   /* low order big means "protected" */
    unsigned char u8_unused2[1];	   /* 0 */
    unsigned char u32_data_length[4];	   /* data fork length */
    unsigned char u32_resource_length[4];  /* resource fork length */
    unsigned char u32_create_date[4];	   /* creation date */
    unsigned char u32_modified_date[4];	   /* "last modified" date */
    unsigned char u16_info_length[2];	   /* II - length of 'info' segment */
    unsigned char u8_flags_low[1];	   /* II - low-order finder flags */
    unsigned char ost_magic[4];		   /* III - 'mBIN' to ID format; too
					      bad it took 14 years for them
					      to add this. */
    unsigned char u8_script[1];		   /* III - script of filename(?) */
    unsigned char u8_flags_extended[1];	   /* III - extended finder flags */
    unsigned char u8_unused3[1][8];	   /* 0 */
    unsigned char u32_unpacked_len[4];	   /* II - if packed, length when
					      unpacked */
    unsigned char u16_expansion_len[2];	   /* II - length of expansion header
					      following this one - never used */
    unsigned char u8_format_version[1];	   /* II - format version: II = 129
					      III = 130*/
    unsigned char u8_read_version[1];	   /* II - min version that can read
					      this format; II = 129; still 129
					      for III, since it's backwards
					      compatible */
    unsigned char u16_crc[2];		   /* CRC of the header up to here */
    unsigned char u16_comptype[2];	   /* reserved - 0 */
}; /* Size should be 128 */
typedef struct MBHeader MBHeader;

/* Finder flags */
/* sheesh - these are taken from the MacBinary I spec, and II and III are
   supposed to be backwards compatible, but the III spec matches the
   AS_F definitions in applefile.h; I suggest using those, and keeping
   in mind that MB splits the finder flags into two fields; the flag values
   need to be shifted accordingly */
#define MB_F_LOCKED	(1<<7)
#define MB_F_INVISIBLE	(1<<6)
#define MB_F_BUNDLE	(1<<5)
#define MB_F_SYSTEM	(1<<4)
#define MB_F_BOZO	(1<<3)
#define MB_F_BUSY	(1<<2)
#define MB_F_CHANGED	(1<<1)
#define MB_F_INITED	(1<<0)

struct MBFile
{
    MBHeader header;
/*  uchar8 expansion_header[(expansion_len+127)%128]; */ /* II - expansion
							    header */
/*  uchar8 data[data_size rounded up to 128]; */
/*  uchar8 resource[resource_size rounded up to 128]; */
/*  uchar8 info[info_len rounded up to 128]; */    /* II - 'get info' data */
};

#define ROUND_UP(x) ((((x)+127)>>7)<<7)
typedef struct MBFile MBFile;

/* The official way to see if a file is a MacBinary file:

   if ost_magic is 'mBIN', this is a MacBinary III file;
   else
     if u8_old_version == 0 && u8_unused1 == 0
       if u16_crc matches, it's a MacBinary II file
       else if u8_unused2 == 0
       if u8_flags_low, ost_magic, u8_script, u8_flags_extended,
         u8_unused3, u32_unpacked_len, u16_expansion_len, u8_format_version,
         u8_read_version, and u16_crc are all 0,
	 u8_fname_length in range 1..63,
	 u32_data_length and u32_resource_length in range 0-0x007fffff
	   it may be a MacBinary I file
   if it's MacBinary II or MacBinary III, and u8_read_version > 129, give up

   Aren't ad-hoc file formats fun? */

/* The information about an open MacBinary file needed for the various
   AppleFile operations */
struct MBData 
{
    int fd;
    int iType;
    size_t cFile;
    size_t cData;
    size_t cResource;
    void * pBase;
    void * pData;
    void * pResource;
};

static int close_macbinary_file(AppleFile *);
static int macbinary_resource_fork(AppleFile *, void **, size_t *);

AppleFileVTable afvtMacBinary = {
    close_macbinary_file,
    macbinary_resource_fork
};

int
open_macbinary_file(char * filename, AppleFile * paf)
{
    int fd;
    int type = 0;
    size_t cFile;
    struct MBData * pmbd;
    struct MBHeader * pmbh;
    struct stat stat_buf;
    unsigned char * p;
    void * pBase = (void *)-1;
    
    do {
	fd = open(filename, O_RDONLY);
	if (fd < 0)
	    break;

	if (fstat(fd, &stat_buf) < 0)
	    break;

	cFile = stat_buf.st_size;
	if (cFile < 128)
	    break;

	assert(sizeof *pmbh == 128);

	pBase = mmap(NULL, cFile, PROT_READ, MAP_SHARED, fd, 0);
	if (pBase == (void *)-1)
	    break;

	pmbh = pBase;
	
	if (strncmp(pmbh->ost_magic, "mBIN", 4) == 0) {
	    type = 3;
	    break;
	}

	if (pmbh->u8_old_version[0] != 0 || pmbh->u8_unused1[0] != 0)
	    break;
	
	if (ComputeCrc(0, pBase, offsetof(MBHeader, u16_crc)) ==
	    read_u16(pmbh->u16_crc)) {
	    type = 2;
	    break;
	}

	for (p = pmbh->u8_flags_low; p < pmbh->u16_comptype; p++)
	    if (*p != 0)
		break;

	if (pmbh->u8_fname_length[0] < 1 || pmbh->u8_fname_length[0] > 63)
	    break;

	if (read_u32(pmbh->u32_data_length) > 0x007fffff ||
	    read_u32(pmbh->u32_resource_length) > 0x007fffff)
	    break;

	/* Now, it *may* be a MacBinary file.  *joy* */
	type = 1;
    } while (0);

    if (type == 0 || (type > 1 && pmbh->u8_read_version[0] != 129)) {
	if (pBase != (void *)-1)
	    munmap(pBase, cFile);
	close(fd);
	return -1;
    }

#if DEBUG_TYPE
    printf("Type %d\n", type);
#endif

    pmbd = NEW(struct MBData);
    pmbd->fd = fd;
    pmbd->iType = type;
    pmbd->cFile = cFile;
    pmbd->cData = read_u32(pmbh->u32_data_length);
    pmbd->cResource = read_u32(pmbh->u32_resource_length);
    pmbd->pBase = pBase;
    pmbd->pData = (char *)pBase + sizeof(*pmbh) +
	ROUND_UP(read_u16(pmbh->u16_expansion_len));
    pmbd->pResource = (char *)pmbd->pData + ROUND_UP(pmbd->cData);

    paf->pafvt = &afvtMacBinary;
    paf->pData = pmbd;

    return 0;
}

static
int
close_macbinary_file(AppleFile *paf)
{
    struct MBData * pmbd;

    pmbd = paf->pData;

    close(pmbd->fd);
    munmap(pmbd->pBase, pmbd->cFile);
    FREE(pmbd);

    paf->pafvt = NULL;
    paf->pData = NULL;

    return 0;
}

static
int
macbinary_resource_fork(AppleFile *paf, void **ppBase, size_t *pcSize)
{
    struct MBData * pmbd;

    pmbd = paf->pData;
    
    *ppBase = pmbd->pResource;
    *pcSize = pmbd->cResource;

    return 0;
}


#if DEBUG_TYPE
int
main(int argc, char ** argv)
{
    open_macbinary_file(argv[1]);
}
#endif
