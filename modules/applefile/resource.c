#include <stdlib.h>

#include "applefile_int.h"
#include "resource.h"


/* Structure of Mac resource files.  Beautiful! */
/*
 * Information for the format of a resource file (or resource fork,
 * 'Resource Fork' section of an AppleSingle or AppleDouble file)
 * from Inside Macintosh, Volume I (old series, (C) 1985)
 * pages I-128 - I-130
 *
 * A resource file consists of
 *                                 --,
 *	Resource Header (16 bytes)    \
 *	Reserved Area (112 bytes)      } 256 bytes
 *	Application Data (128 bytes)  /
 *                                 --`
 *      Resource Data
 *      Resource Map
 */

/* Resource Header:
 * Gives offsets from the beginning of the Resource File for the
 * Resource Data and Resource Map, and lengths, in bytes
 * */
struct MRHeader 
{
    unsigned char u32_data_off[4];	   /* offset of resource data */
    unsigned char u32_map_off[4];	   /* offset of map */
    unsigned char u32_data_len[4];	   /* data length */
    unsigned char u32_map_len[4];	   /* map length */
};

/* Reserved Area:
 * The reserved area is reserved for system use */

/* Application Data:
 * 128 bytes that may be used by the application as it wishes */

/* Resource Data:
 * the actual data for the resources; 4 byte lengths followed by the
 * data.  There may be holes in here, with invalid lengths, so the only
 * way to find these is to get an offset from a resource list
 */
struct MRResourceData
{
    unsigned char u32_data_length[4];	   /* data length */
/*  unsigned char u8_data[data_length]; */ /* data */
};


/* Resource Map:
 * Includes a Resource Map Header, a type list, a reference list,
 * and a resource name list. */

/*     Resource Map Header:
 *     Gives offsets from the beginning of the Resource Map for the type
 *     and name lists, and some attributes */
struct MRMapHeader
{
    unsigned char reserved[22];
    unsigned char u16_attributes[2];	   /* attributes for the file */
    unsigned char u16_type_off[2];	   /* type list offset from map */
    unsigned char u16_name_off[2];	   /* name list offset from map */
};

/*          Type List:
 *          Contains 2 bytes giving the number of types in the list, minus 1,
 *          and a vector of Type Records */
struct MRTypeList
{
    unsigned char u16_num_type_records[2]; /* number of type records-1 */
/*  MRType types[u16_num_type_records+1]; */
};

/*              Type Record:
 *              Gives a resource type tag, the number of resources of this
 *              type (minus 1), and an offset from the beginning of the type
 *              list to the reference list for this type
 */
struct MRType
{
    unsigned char ost_resource_type[4]; /* resource type tag */
    unsigned char u16_num_resources[2]; /* number of resources-1 */
    unsigned char u16_reference_off[2]; /* reference offset from type list */
};

/*          Reference List:
 *          A vector of type references */
/*
struct MRReferenceList
{
    MRReference[];
};
*/

/*              Reference:
 *              Gives resource ID, offset from beginning of Name List to the
 *              resource's name, attributes, offset from beginning of resource
 *              data to Data for this resource, and a reserved area */
struct MRReference 
{
    unsigned char s16_id[2];		   /* Resource ID */
    unsigned char s16_name_off[2];	   /* Name offset from name list */
    unsigned char u8_attributes[1];	   /* attributes */
    unsigned char u24_data_off[3];	   /* Data offset from start of data */
    unsigned char reserved[4];
};

/*
 *          Name List:
 *          p-string names for resources; there may be holes in here,
 *          only look at offsets given by name_off in MRReference;
 */
/*
struct MRNameList
{
MRName[];
};
*/

/*              Name Entry:
 *              pstring
 */
struct MRName
{
    unsigned char u8_len[1];		   /* name length */
/*  unsigned char[u8_len]; */
};

#include "macresource.h"

typedef unsigned char byte;

/* Use the 'reference' name internally to more closely
   model the Apple data types, but the structure is
   identical to the external-interface resource_data
   struct */

typedef resource_data reference;

typedef struct 
{
    char type[4];
    int cNumResources;
    reference * prrReference;
} resource_type;

struct resource_file
{
    byte * pbData;
    
    int cNumTypes;
    resource_type * prtType;
};

static
void
copyReferences(byte * pbData, byte * pbNameList, reference * prrDest, struct MRReference * prmrSrc, int c)
{
    int i;
    int id;
    int cName;
    char * psName;
    int cData;
    unsigned char * pData;
    int name_off;
    
    for (i = 0; i < c; i++) {
	id = read_s16(prmrSrc[i].s16_id);

	name_off = read_s16(prmrSrc[i].s16_name_off);
	if (name_off == -1) {
	    psName = NULL;
	    cName = 0;
	} else {
	    psName = pbNameList + name_off;
	    cName = *psName++;
	}
	pData = pbData + read_u24(prmrSrc[i].u24_data_off);
	cData = read_u32(pData);
	pData += 4;
	
	prrDest[i].id = id;
	prrDest[i].cName = cName;
	prrDest[i].psName = psName;
	prrDest[i].cData = cData;
	prrDest[i].pData = pData;
    }
}

static
void
copyTypes(byte * pbData, byte * pbTypeList, byte * pbNameList, resource_type * prtDest, struct MRType * prmrtSrc, int c)
{
    int i;
    int cNumResources;
    struct MRReference * prmrRawReference;
    reference * prrReference;

    for (i = 0; i < c; i++) {
	cNumResources = read_u16(prmrtSrc[i].u16_num_resources) + 1;
	prmrRawReference = (struct MRReference *)
	    (pbTypeList + read_u16(prmrtSrc[i].u16_reference_off));

	prrReference = NEWC(cNumResources, reference);
	copyReferences(pbData, pbNameList, prrReference, prmrRawReference, cNumResources);

	memcpy(prtDest[i].type, prmrtSrc[i].ost_resource_type, 4);
	prtDest[i].cNumResources = cNumResources;
	prtDest[i].prrReference = prrReference;
    }
}

resource_file *
load_resource_file(char * pFileBuffer)
{
    int cNumTypes;
    resource_file * prfFile;
    resource_type * prtType;
    byte * pbData;
    byte * pbMap;
    byte * pbTypeList;
    byte * pbNameList;
    struct MRHeader * pmrh;
    struct MRMapHeader * pmrmh;
    struct MRType * prmrt;

    pmrh = (struct MRHeader *) pFileBuffer;
    pbData = pFileBuffer + read_u32(pmrh->u32_data_off);
    pbMap = pFileBuffer + read_u32(pmrh->u32_map_off);

    pmrmh = (struct MRMapHeader *)(pbMap);
    pbTypeList = pbMap + read_u16(pmrmh->u16_type_off);
    pbNameList = pbMap + read_u16(pmrmh->u16_name_off);
    
    cNumTypes = read_u16(pbTypeList) + 1;
    prmrt = (struct MRType *)(pbTypeList + 2);
    prtType = NEWC(cNumTypes, resource_type);
    copyTypes(pbData, pbTypeList, pbNameList, prtType, prmrt, cNumTypes);
    
    prfFile = NEW(resource_file);

    prfFile->pbData = pbData;

    prfFile->cNumTypes = cNumTypes;
    prfFile->prtType = prtType;
    
    return prfFile;
}

void
free_resource_file(resource_file * prf)
{
    int iType;
    
    for (iType = 0; iType < prf->cNumTypes; iType++) {
	FREEC(prf->prtType[iType].prrReference);
    }
    FREEC(prf->prtType);
    FREE(prf);
}

resource_data *
rf_get_resource_by_id(resource_file * prf, char type[4], int id)
{
    resource_type * prtType;
    
    {
	int iType;
	for (prtType = prf->prtType, iType = 0;
	     iType < prf->cNumTypes;
	     prtType++, iType++) {
	    if (memcmp(type, prtType->type, 4) == 0)
		break;
	}
	if (iType == prf->cNumTypes)
	    return NULL;
    }
    
    {
	reference * prReference;
	int iReference;

	for (prReference = prtType->prrReference, iReference = 0;
	     iReference < prtType->cNumResources;
	     prReference++, iReference++) {
	    if (prReference->id == id)
		return prReference;
	}
    }

    return NULL;
}


