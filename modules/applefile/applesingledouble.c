#include <assert.h>
#include <fcntl.h>
#include <stddef.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "applesingledouble.h"

/* Taken from rfc 1740.  Thank god for the IETF - Apple certainly
   can't get or keep their documentation accessible without help. */

/* Modified by Todd Larason <jtl@molehill.org> to aid portability; all
   of the size & byteorder assumptions are gone, with the penalty that
   all conversion must be done manually no matter the architecture.
   All the variables have been renamed to aid in this.

   All variables and macros now begin with "AS".  Most already did;
   ASPoint, ASFInfo ans ASFXInfo didn't; they're not actually
   AppleSingle/AppleDouble specific.
*/

/* I have also found a PDF of the full specification at
   http://www.lazerware.com/formats/Specs/AppleSingle_AppleDouble.pdf
   It looks Apple-official, but I've never been able to find anything
   from Apple on the subject in anything other than AppleTalk format,
   which I doubt even most real Mac programmers can read any more.
   Silly Apple */
   
/*    This is an example of a header file for the language C which can be */
/*    used when parsing the data in either an AppleSingle file or */
/*    AppleDouble header. */

/*    The file is written by Lee Jones.  Distribution is unlimited. */

   /* applefile.h - Data structures used by AppleSingle/AppleDouble
    * file format
    *
    * Written by Lee Jones, 22-Oct-1993
    *
    * For definitive information, see "AppleSingle/AppleDouble
    * Formats for Foreign Files Developer's Note"; Apple Computer
    * Inc.; (c) 1990.
    *
    * Other details were added from:
    *   Inside Macintosh [old version], volumes II to VI,
    *   Apple include files supplied with Think C 5.0.1,
    *   Microsoft MS-DOS Programmer's Reference, version 5, and
    *   Microsoft C 6.00a's dos.h include file.
    *
    * I don't have ProDOS or AFP Server documentation so related
    * entries may be a bit skimpy.
    *
    * Edit history:
    *
    * when       who  why
    * ---------  ---  ------------------------------------------
    * 22-Oct-93  LMJ  Pull together from Inside Macintosh,
    *                 Developer's Note, etc
    * 26-Oct-93  LMJ  Finish writing first version and list
    *                 references
    * 06-Feb-94  EEF  Very minor cleanup
    */

   /* Following items define machine specific size (for porting). */
#if 0
   typedef char            xchar8;         /* 8-bit field */
   typedef char            schar8;	   /* signed 8-bit field */
   typedef unsigned char   uchar8;	   /* unsigned 8-bit field */
   typedef short           xint16;	   /* 16-bit field */
   typedef unsigned short  uint16;	   /* unsigned 16-bit field */
   typedef long            xint32;	   /* 32-bit field */
   typedef long            sint32;	   /* signed 32-bit field */
   typedef unsigned long   uint32;	   /* unsigned 32-bit field */

/* REMINDER: the Motorola 680x0 is a big-endian architecture! */

   typedef uint32 OSType;		   /* 32 bit field */

#endif
   /* In the QuickDraw coordinate plane, each coordinate is
    * -32767..32767. Each point is at the intersection of a
    * horizontal grid line and a vertical grid line.  Horizontal
    * coordinates increase from left to right. Vertical
    * coordinates increase from top to bottom. This is the way
    * both a TV screen and page of English text are scanned:
    * from top left to bottom right.
    */

   struct ASPoint			   /* spot in QuickDraw 2-D grid */
   {
       unsigned char x16_v[2];		   /* vertical coordinate */
       unsigned char x16_h[2];		   /* horizontal coordinate */
   }; /* ASPoint */

   typedef struct ASPoint ASPoint;

   /* See older Inside Macintosh, Volume II page 84 or Volume IV
    * page 104. [ yeah, if you can find one.  all hail the Apple
    anti-documentation campaign! -- jtl ]
    */

   struct ASFInfo			   /* Finder information */
   {
       unsigned char ost_fdType[4];	   /* File type, 4 ASCII chars */
       unsigned char ost_fdCreator[4];	   /* File's creator, 4 ASCII chars */
       unsigned char u16_fdFlags[2];	   /* Finder flag bits */
       ASPoint fdLocation;		   /* file's location in folder */
       unsigned char x16_fdFldr[2];	   /* file 's folder (aka window) */
   }; /* FInfo */

   typedef struct ASFInfo ASFInfo;

   /*
    * Masks for finder flag bits (field fdFlags in struct
    * ASFInfo).
    */

   #define AS_F_fOnDesk       0x0001	   /* file is on desktop (HFS only) */
   #define AS_F_maskColor     0x000E	   /* color coding (3 bits) */
/*                            0x0010*/     /* reserved (System 7) */
   #define AS_F_fSwitchLaunch 0x0020	   /* reserved (System 7) */
   #define AS_F_fShared       0x0040	   /* app available to multiple users */
   #define AS_F_fNoINITs      0x0080	   /* file contains no INIT resources */
   #define AS_F_fBeenInited   0x0100	   /* Finder has loaded bundle res. */
/*                            0x0200*/     /* reserved (System 7) */
   #define AS_F_fCustomIcom   0x0400	   /* file contains custom icon */
   #define AS_F_fStationary   0x0800	   /* file is a stationary pad */
   #define AS_F_fNameLocked   0x1000	   /* file can't be renamed by Finder */
   #define AS_F_fHasBundle    0x2000	   /* file has a bundle */
   #define AS_F_fInvisible    0x4000	   /* file's icon is invisible */
   #define AS_F_fAlias        0x8000	   /* file is an alias file (System 7)*/

   /* See older Inside Macintosh, Volume IV, page 105. */

   struct ASFXInfo			   /* Extended finder information */
   {
       unsigned char x16_fdIconID[2];	   /* icon ID number */
       unsigned char x16_fdUnused[2][3];   /* spare */
       unsigned char s8_fdScript[1];	   /* scrip flag and code */
       unsigned char s8_fdXFlags[1];	   /* reserved */
       unsigned char x16_fdComment[2];	   /* comment ID number */
       unsigned char x32_fdPutAway[4];	   /* home directory ID */
   }; /* FXInfo */

   typedef struct ASFXInfo ASFXInfo;

   /* Pieces used by AppleSingle & AppleDouble (defined later). */

   struct ASHeader			   /* header portion of AppleSingle */
   {
       /* AppleSingle = 0x00051600; AppleDouble = 0x00051607 */
       unsigned char u32_magicNum[4];	   /* internal file type tag */
       unsigned char u32_versionNum[4];	   /* format version: 2 = 0x00020000 */
       unsigned char u8_filler[1][16];	   /* filler, currently all bits 0 */
       unsigned char u16_numEntries[2];	   /* number of entries which follow */
   }; /* ASHeader */

   typedef struct ASHeader ASHeader;

   struct ASEntry			   /* AppleSingle entry descriptor */
   {
       unsigned char u32_entryID[4];	   /* entry type: see list, 0 invalid */
       unsigned char u32_entryOffset[4];   /* offset, in octets, from
					      beginning of file to this
					      entry's data */
       unsigned char u32_entryLength[4];   /* length of data in octets */
   }; /* ASEntry */

   typedef struct ASEntry ASEntry;

   /* Apple reserves the range of entry IDs from 1 to 0x7FFFFFFF.
    * Entry ID 0 is invalid.  The rest of the range is available
    * for applications to define their own entry types.  "Apple does
    * not arbitrate the use of the rest of the range."
    */

   #define AS_DATA         1		   /* data fork */
   #define AS_RESOURCE     2		   /* resource fork */
   #define AS_REALNAME     3		   /* File's name on home file system */
   #define AS_COMMENT      4		   /* standard Mac comment */
   #define AS_ICONBW       5		   /* Mac black & white icon */
   #define AS_ICONCOLOR    6		   /* Mac color icon */
/* I think AS_FILEDATES should be 7, and 8 may be unused -- jtl */
/*                         7 */            /* not used */
/* The PDF version of the spec agrees that AS_FILEDATES is 8; my sample files
   all have it as 7 though; it may be a netatalk bug */
   #define AS_FILEDATES    8		   /* file dates; create, modify, etc */
   #define AS_FINDERINFO   9		   /* Mac Finder info & extended info */
   #define AS_MACINFO      10		   /* Mac file info, attributes, etc */
   #define AS_PRODOSINFO   11		   /* Pro-DOS file info, attrib. */
   #define AS_MSDOSINFO    12		   /* MS-DOS file info, attributes */
   #define AS_AFPNAME      13		   /* Short name on AFP server */
   #define AS_AFPINFO      14		   /* AFP file info, attrib., etc */
   #define AS_AFPDIRID     15		   /* AFP directory ID */

   /* matrix of entry types and their usage:
    *
    *                   Macintosh    Pro-DOS    MS-DOS    AFP server
    *                   ---------    -------    ------    ----------
    *  1   AS_DATA         xxx         xxx       xxx         xxx
    *  2   AS_RESOURCE     xxx         xxx
    *  3   AS_REALNAME     xxx         xxx       xxx         xxx
    *  4   AS_COMMENT      xxx
    *  5   AS_ICONBW       xxx
    *  6   AS_ICONCOLOR    xxx
    *
    *  8   AS_FILEDATES    xxx         xxx       xxx         xxx
    *  9   AS_FINDERINFO   xxx
    * 10   AS_MACINFO      xxx
    *
    * 11   AS_PRODOSINFO               xxx
    * 12   AS_MSDOSINFO                          xxx
    *
    * 13   AS_AFPNAME                                        xxx
    * 14   AS_AFPINFO                                        xxx
    * 15   AS_AFPDIRID                                       xxx
    */

   /* entry ID 1, data fork of file - arbitrary length octet string */

   /* entry ID 2, resource fork - arbitrary length opaque octet
    *              string; as created and managed by Mac
    *              O.S. resoure[sic -- jtl] manager
    */

   /* entry ID 3, file's name as created on home file system - arbitrary
    *              length octet string; usually short, printable ASCII
    */

   /* entry ID 4, standard Macintosh comment - arbitrary length octet
    *              string; printable ASCII, claimed 200 chars or less
    */

   /* This is probably a simple duplicate of the 128 octet bitmap
    * stored as the 'ICON' resource or the icon element from an 'ICN#'
    * resource.
    */

   struct ASIconBW /* entry ID 5, standard Mac black and white icon */
   {
       unsigned char u32_bitrow[4][32];	   /* 32 rows of 32 1-bit pixels */
   }; /* ASIconBW */

   typedef struct ASIconBW ASIconBW;

   /* entry ID 6, "standard" Macintosh color icon - several competing
    *              color icons are defined.  Given the copyright dates
    * of the Inside Macintosh volumes, the 'cicn' resource predominated
    * when the AppleSingle Developer's Note was written (most probable
    * candidate).  See Inside Macintosh, Volume V, pages 64 & 80-81 for
    * a description of 'cicn' resources.
    *
    * With System 7, Apple introduced icon families.  They consist of:
    *      large (32x32) B&W icon, 1-bit/pixel,    type 'ICN#',
    *      small (16x16) B&W icon, 1-bit/pixel,    type 'ics#',
    *      large (32x32) color icon, 4-bits/pixel, type 'icl4',
    *      small (16x16) color icon, 4-bits/pixel, type 'ics4',
    *      large (32x32) color icon, 8-bits/pixel, type 'icl8', and
    *      small (16x16) color icon, 8-bits/pixel, type 'ics8'.
    * If entry ID 6 is one of these, take your pick.  See Inside
    * Macintosh, Volume VI, pages 2-18 to 2-22 and 9-9 to 9-13, for
    * descriptions.
    */

   /* entry ID 7, not used */

   /* Times are stored as a "signed number of seconds before of after
    * 12:00 a.m. (midnight), January 1, 2000 Greenwich Mean Time (GMT).
    * Applications must convert to their native date and time
    * conventions." Any unknown entries are set to 0x80000000
    * (earliest reasonable time).
    */

   struct ASFileDates			   /* entry ID 8, file dates info */
   {
       unsigned char s32_create[4];	   /* file creation date/time */
       unsigned char s32_modify[4];	   /* last modification date/time */
       unsigned char s32_backup[4];	   /* last backup date/time */
       unsigned char s32_access[4];	   /* last access date/time */
   }; /* ASFileDates */

   typedef struct ASFileDates ASFileDates;

   /* See older Inside Macintosh, Volume II, page 115 for
    * PBGetFileInfo(), and Volume IV, page 155, for PBGetCatInfo().
    */

   /* entry ID 9, Macintosh Finder info & extended info */
   struct ASFinderInfo
   {
       ASFInfo ioFlFndrInfo;		   /* PBGetFileInfo or PBGetCatInfo */
       ASFXInfo ioFlXFndrInfo;		   /* PBGetCatInfo (HFS only) */
   }; /* ASFinderInfo */

   typedef struct ASFinderInfo ASFinderInfo;

   struct ASMacInfo			   /* ID 10, Macintosh file info */
   {
       unsigned char u8_filler[1][3];	   /* filler, currently all bits 0 */
       unsigned char u8_ioFlAttrib[1];	   /* PBGetFileInfo or PBGetCatInfo */
   }; /* ASMacInfo */

   typedef struct ASMacInfo ASMacInfo;

   #define AS_PROTECTED    0x0002	   /* protected bit */
   #define AS_LOCKED       0x0001	   /* locked bit */

   /* NOTE: ProDOS-16 and GS/OS use entire fields.  ProDOS-8 uses low
    * order half of each item (low byte in access & filetype, low word
    * in auxtype); remainder of each field should be zero filled.
    */

   struct ASProdosInfo			   /* ID 11, ProDOS file information */
   {
       unsigned char u16_access[2];	   /* access word */
       unsigned char u16_filetype[2];	   /* file type of original file */
       unsigned char u32_auxtype[4];	   /* auxiliary type of the orig file */
   }; /* ASProDosInfo */

   typedef struct ASProdosInfo ASProdosInfo;

   /* MS-DOS file attributes occupy 1 octet; since the Developer Note
    * is unspecific, I've placed them in the low order portion of the
    * field (based on example of other ASMacInfo & ASProdosInfo).
    */

   struct ASMsdosInfo			   /* ID 12, MS-DOS file information */
   {
       unsigned char u8_filler[1];	   /* filler, currently all bits 0 */
       unsigned char u8_attr[1];	   /* _dos_getfileattr(), MS-DOS */
					   /* interrupt 21h function 4300h */
   }; /* ASMsdosInfo */

   typedef struct ASMsdosInfo ASMsdosInfo;

   #define AS_DOS_NORMAL   0x00		   /* normal file (all bits clear) */
   #define AS_DOS_READONLY 0x01		   /* file is read-only */
   #define AS_DOS_HIDDEN   0x02		   /* hidden file (not shown by DIR) */
   #define AS_DOS_SYSTEM   0x04		   /* system file (not shown by DIR) */
   #define AS_DOS_VOLID    0x08		   /* volume label (only in root dir) */
   #define AS_DOS_SUBDIR   0x10		   /* file is a subdirectory */
   #define AS_DOS_ARCHIVE  0x20		   /* new or modified (needs backup) */

   /* entry ID 13, short file name on AFP server - arbitrary length
    *              octet string; usualy printable ASCII starting with
    *              '!' (0x21)
    */

   struct ASAfpInfo			   /* entry ID 12 [sic; should be
					      14 -- jtl], AFP server file
					      information */
   {
       unsigned char u8_filler[1][3];	   /* filler, currently all bits 0 */
       unsigned char u8_attr[1];	   /* file attributes */
   }; /* ASAfpInfo */

   typedef struct ASAfpInfo ASAfpInfo;

   #define AS_AFP_Invisible    0x01	   /* file is invisible */
   #define AS_AFP_MultiUser    0x02	   /* simultaneous access allowed */
   #define AS_AFP_System       0x04	   /* system file */
   #define AS_AFP_BackupNeeded 0x40	   /* new or modified (needs backup) */

   struct ASAfpDirId			   /* entry ID 15, AFP server
					      directory ID */
   {
       unsigned char u32_dirid[4];	   /* file's directory ID on server */
   }; /* ASAfpDirId */

   typedef struct ASAfpDirId ASAfpDirId;

   /*
    * The format of an AppleSingle/AppleDouble header
    */
   struct ASFile			   /* format of disk file */
   {
       ASHeader header;			   /* AppleSingle header part */
/*     ASEntry  entry[cNumEntries]; */	   /* array of entry descriptors */
/*      uchar8  filedata[]; */             /* followed by rest of file */
   }; /* AppleSingle */

   typedef struct ASFile ASFile;

   /*
    * FINAL REMINDER: the Motorola 680x0 is a big-endian architecture!
    */

   /* End of applefile.h */

struct ASDData
{
    int iType;			/* 1 = AppleSingle; 2 = AppleDouble */

    /* These are valid for either AppleSingle or AppleDouble */
    int fdHeader;
    size_t cHeaderFile;
    size_t cData;
    size_t cResource;
    void * pHeaderFile;
    void * pData;
    void * pResource;

    /* These are valid only for AppleDouble */
    int fdData;
    size_t cDataFile;
    void * pDataFile;
};
typedef struct ASDData ASDData;


static int close_applesingledouble_file(AppleFile *);
static int applesingledouble_resource_fork(AppleFile *, void **, size_t *);

AppleFileVTable afvtAppleSingleDouble = {
    close_applesingledouble_file,
    applesingledouble_resource_fork
};

static
int
open_appleheader_file(char * filename, AppleFile * paf, int iTypeRequired)
{
    int fd, iEntry;
    size_t cData, cEntries, cFile, cResource;
    struct ASDData * pasdd;
    struct ASEntry * prase;
    struct ASHeader * pash;
    struct stat stat_buf;
    unsigned long id, type, magicRequired;
    void *pBase = (void *)-1, *pData, *pResource;

    if (iTypeRequired == 1)
	magicRequired = 0x00051600;
    else
	magicRequired = 0x00051607;
    
    do {
	fd = open(filename, O_RDONLY);
	if (fd < 0)
	    break;

	if (fstat(fd, &stat_buf) < 0)
	    break;

	cFile = stat_buf.st_size;
	if (cFile < sizeof(ASHeader))
	    break;

	pBase = mmap(NULL, cFile, PROT_READ, MAP_SHARED, fd, 0);
	if (pBase == (void *)-1)
	    break;

	pash = (ASHeader *)pBase;

	if (read_u32(pash->u32_magicNum) != magicRequired)
	    break;

	if (read_u32(pash->u32_versionNum) != 0x00020000)
	    break;
	
	cEntries = read_u16(pash->u16_numEntries);
	if (cFile < sizeof(ASHeader) + cEntries * sizeof(ASEntry))
	    break;

	prase = (ASEntry *)((char *)pBase + sizeof(ASHeader));
	for (iEntry = 0; iEntry < cEntries; iEntry++) {
	    id = read_u32(prase[iEntry].u32_entryID);

	    switch (id) {
		case AS_DATA:
		    pData = (char *)pBase +
			read_u32(prase[iEntry].u32_entryOffset);
		    cData = read_u32(prase[iEntry].u32_entryOffset);
		    break;
		case AS_RESOURCE:
		    pResource = (char *)pBase +
			read_u32(prase[iEntry].u32_entryOffset);
		    cResource = read_u32(prase[iEntry].u32_entryOffset);
		    break;
	    }
	}

	type = iTypeRequired;
    } while (0);

    if (type == 0) {
	if (pBase != (void *)-1)
	    munmap(pBase, cFile);
	close(fd);
	return -1;
    }
    
    pasdd = NEW(ASDData);
    pasdd->iType = 1;
    pasdd->fdHeader = fd;
    pasdd->cHeaderFile = cFile;
    pasdd->cData = cData;
    pasdd->cResource = cResource;
    pasdd->pHeaderFile = pBase;
    pasdd->pData = pData;
    pasdd->pResource = pResource;

    paf->pafvt = &afvtAppleSingleDouble;
    paf->pData = pasdd;

    return 0;
}

int
open_applesingle_file(char * filename, AppleFile * paf)
{
    return open_appleheader_file(filename, paf, 1);
}

int
open_appledouble_file(char * filename, AppleFile * paf)
{
    char * pLastSlash;
    char * szHeaderFilename;
    int fd = -1;
    int r = -1;
    size_t cFile;
    struct stat stat_buf;
    void * pFile = (void *)-1;
    
    do {
	fd = open(filename, O_RDONLY);
	if (fd < 0)
	    break;

	if (fstat(fd, &stat_buf) < 0)
	    break;

	cFile = stat_buf.st_size;
	pFile = mmap(NULL, cFile, PROT_READ, MAP_SHARED, fd, 0);
	if (pFile == (void *)-1)
	    break;
	
	pLastSlash = strrchr(filename, '/');
	szHeaderFilename = NEWC(strlen(filename)+strlen("/.AppleDouble") + 1, char);

	if (pLastSlash) {
	    sprintf(szHeaderFilename, "%.*s/.AppleDouble%s",
		    pLastSlash - filename, filename, pLastSlash);
	} else {
	    sprintf(szHeaderFilename, ".AppleDouble/%s", filename);
	}

	r = open_appleheader_file(filename, paf, 2);
    } while (0);
    
    if (r == 0) {
	struct ASDData * pasdd;

	pasdd = paf->pData;

	pasdd->fdData = fd;
	pasdd->cDataFile = cFile;
	pasdd->pDataFile = pFile;

	return 0;
    }

    if (pFile != (void *)-1) {
	munmap(pFile, cFile);
    }
    close(fd);
    
    return r;
}

static
int
close_applesingledouble_file(AppleFile * paf)
{
    struct ASDData * pasdd;

    pasdd = paf->pData;
    close(pasdd->fdHeader);
    munmap(pasdd->pHeaderFile, pasdd->cHeaderFile);

    if (pasdd->iType == 2) {
	close(pasdd->fdData);
	munmap(pasdd->pDataFile, pasdd->cDataFile);
    }

    FREE(pasdd);

    paf->pafvt = NULL;
    paf->pData = NULL;

    return 0;
}

static
int
applesingledouble_resource_fork(AppleFile *paf, void **ppBase, size_t *pcSize)
{
    struct ASDData * pasdd;

    pasdd = paf->pData;

    *ppBase = pasdd->pResource;
    *pcSize = pasdd->cResource;

    return 0;
}

