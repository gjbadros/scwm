/* $Id$
 * scwmproplist.c
 * (C) 1999, 2000 Toby Sargeant and Greg J. Badros
 */

#ifdef HAVE_CONFIG_H
#include "scwmconfig.h"
#endif

#include <guile/gh.h>
#include "guile-compat.h"

#include <proplist.h>
#include "proplistP.h" /* from the proplist source distribution */

#include "scwm.h"
#include "validate.h"
#include "callbacks.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

typedef struct {
  proplist_t value;
} scwm_proplist_t;

long scm_tc16_scwm_proplist_t;

#define PROPLIST_T(X)  (((scwm_proplist_t *)(gh_cdr(X)))->value)
#define INTERNAL_PROPLIST_T(X)  ((plptr_t)((scwm_proplist_t *)(gh_cdr(X)))->value)
#define SCWMPROPLIST_T(X)  ((scwm_proplist_t *)(gh_cdr(X)))
#define IS_PROPLIST_T(X) (SCM_NIMP(X) && gh_car(X) == (SCM)scm_tc16_scwm_proplist_t)

#define IS_PROPLIST_DICT(X) (IS_PROPLIST_T((X)) && \
                             INTERNAL_PROPLIST_T((X))->type == PLDICTIONARY)

#define VALIDATE_ARG_PROPLIST_DICT(iarg,pl) do { \
    if (!IS_PROPLIST_DICT((pl))) SCWM_WRONG_TYPE_ARG(iarg,(pl)); \
  } while (0)

/* takes a proplist or a string */
#define VALIDATE_ARG_PROPLIST(iarg,pl) do { \
    if (gh_string_p(pl)) { char *sz = gh_scm2newstr(pl,NULL); \
                           pl = ScmProplist(PLMakeString(sz)); \
                           free(sz); } \
    if (!IS_PROPLIST_T((pl))) SCWM_WRONG_TYPE_ARG(iarg,(pl)); \
  } while (0)

#define VALIDATE_ARG_PROPLIST_COPY(iarg,pl,cvar) do { \
    if (gh_string_p(pl)) { char *sz = gh_scm2newstr(pl,NULL); \
                           pl = ScmProplist(PLMakeString(sz)); \
                           free(sz); } \
    if (!IS_PROPLIST_T((pl))) SCWM_WRONG_TYPE_ARG(iarg,(pl)); \
    else cvar = PROPLIST_T(pl); \
  } while (0)

#define ASSERT_PROPLIST(pl) do { \
    if (!IS_PROPLIST_T((pl))) SCWM_WRONG_TYPE_ARG(0,(pl)); \
  } while (0)


/* also permit strings */
#define ASSERT_CONVERT_PROPLIST(pl) do { \
    if (gh_string_p(pl)) { char *sz = gh_scm2newstr(pl,NULL); \
                           pl = ScmProplist(PLMakeString(sz)); \
                           free(sz); } \
    if (!IS_PROPLIST_T((pl))) SCWM_WRONG_TYPE_ARG(0,(pl)); \
  } while (0)


static
SCM
mark_proplist_t(SCM ARG_IGNORE(obj)) {
  return SCM_BOOL_F;
}

static
size_t 
free_proplist_t(SCM obj) {
#if 0 /* GJB:FIXME:: LEAK! */
  scwm_proplist_t *spt=PROPLIST_T(obj);
  PLRelease(spt->value);
  FREE(spt);
#endif
  return 0;
}



static
int 
print_proplist_t(SCM obj, SCM port, scm_print_state *ARG_IGNORE(pstate)) {
  plptr_t internal;
  scwm_proplist_t *spt=PROPLIST_T(obj);
  scm_puts("#<proplist_t ", port);
  scm_write(gh_ulong2scm((unsigned long)spt), port);   

  internal = (plptr_t)spt;
  switch (internal->type) {
  case PLSTRING:
    scm_puts(" string \"",port);
    scm_puts(internal->t.str.string,port);
    scm_puts("\"",port);
    break;
  case PLDATA:
    scm_puts(" data",port);
    break;
  case PLARRAY:
    scm_puts(" array ",port);
    scm_write(gh_long2scm(internal->t.array.number),port);
    break;
  case PLDICTIONARY:
    scm_puts(" dictionary ",port);
    scm_write(gh_long2scm(internal->t.dict.number),port);
    break;
  default:
    scm_puts("[unknown type]",port);
    break;
  }

  scm_putc('>', port);
  return 1;
}

/* ScmProplist always returns a proplist object,
   even if pl is a string pl. */
static
SCM
ScmProplist(proplist_t pl) {
  scwm_proplist_t *spt;
  SCM result;
  spt=NEW(scwm_proplist_t);
  spt->value=pl;
  SCWM_NEWCELL_SMOB(result,scm_tc16_scwm_proplist_t,spt);
  return result;
}

/* proplist_to_scm auto converts string pl-s into scheme strings */
static
SCM
proplist_to_scm(proplist_t pl,int retain_count) 
{
  if (!pl) {
    return SCM_BOOL_F;
  } 
  
  while (retain_count--) {
    PLRetain(pl);
  }

  if (PLIsString(pl)) {
    plptr_t internal = (plptr_t) pl;
    return gh_str02scm(internal->t.str.string);
  } else {
    return ScmProplist(pl);
  }
}

static SCM scm_callback=SCM_BOOL_F;

static
void
c_callback(void) {
  if (scm_callback!=SCM_BOOL_F) {
    scwm_safe_call0(scm_callback);
  }
}

static SCM scm_cmp_callback=SCM_BOOL_F;

static
BOOL
c_cmp_callback(proplist_t a,proplist_t b) {
  SCM r;
  if (scm_cmp_callback!=SCM_BOOL_F) {
    r=scwm_safe_call2(scm_cmp_callback,
		      proplist_to_scm(a,1),
		      proplist_to_scm(b,1));
    return gh_scm2bool(r);
  }
  return NO;
}
/* proplist_t PLMakeArrayFromElements(proplist_t pl, ...); */
SCWM_PROC(proplist_make_array_from_elements,"proplist-make-array-from-elements",0,0,1,
	  (SCM items),
"Return a newly created array object populated with elements ITEMS.")
#define FUNC_NAME s_proplist_make_array_from_elements
{
  proplist_t array=NULL;
  SCM value;
  while (1) {
    if (items==SCM_EOL) break;
    value=gh_car(items),items=gh_cdr(items);
    ASSERT_PROPLIST(value);

    if (array==NULL) {
      array=PLMakeArrayFromElements(PROPLIST_T(value),NULL);
      if (!array) return SCM_BOOL_F;
    } else {
      proplist_t n;
      n=PLAppendArrayElement(array,PROPLIST_T(value));
      if (!n) {
        PLRelease(array);
        return SCM_BOOL_F;
      }
      array=n;
    }
  }
  return proplist_to_scm(array,0);
}
#undef FUNC_NAME

/* proplist_t PLMakeDictionaryFromEntries(proplist_t key, proplist_t value,
   ...); */
SCWM_PROC(proplist_make_dictionary_from_entries,"proplist-make-dictionary-from-entries",0,0,1,
	  (SCM items),
"Return a newly created dictionary object with elements ITEMS.
The elements in ITEMS should be a flat list (key1 value1 key2 value2 ...).")
#define FUNC_NAME s_proplist_make_dictionary_from_entries
{
  proplist_t dict=NULL;
  SCM key,value;
  VALIDATE_ARG_LIST(1,items);
  while (1) {
    if (items==SCM_EOL) break;
    key=gh_car(items),items=gh_cdr(items);
    if (items==SCM_EOL) {
      scm_misc_error(FUNC_NAME,"items must have an even number of elements",SCM_EOL);
    }
    value=gh_car(items),items=gh_cdr(items);
    ASSERT_CONVERT_PROPLIST(key);
    ASSERT_CONVERT_PROPLIST(value);

    if (dict==NULL) {
      dict=PLMakeDictionaryFromEntries(PROPLIST_T(key),
                                       PROPLIST_T(value),NULL);
      if (!dict) return SCM_BOOL_F;
    } else {
      proplist_t n;
      n=PLInsertDictionaryEntry(dict,
                                PROPLIST_T(key),
                                PROPLIST_T(value));
      if (!n) {
        PLRelease(dict);
        return SCM_BOOL_F;
      }
      dict=n;
    }
  }
  return proplist_to_scm(dict,0);
}
#undef FUNC_NAME

/* proplist_t PLRemoveArrayElement(proplist_t array,
   unsigned int index); */
SCWM_PROC(proplist_remove_array_element,"proplist-remove-array-element",2,0,0,
	  (SCM pl_array,SCM index),
"Return PL-ARRAY after deleting the element at position INDEX.")
#define FUNC_NAME s_proplist_remove_array_element
{
  unsigned int i;
  VALIDATE_ARG_PROPLIST(1,pl_array);
  VALIDATE_ARG_INT_COPY(2,index,i);
  /* PLRemoveArrayElement returns Array. must inc ref */
  return proplist_to_scm(PLRemoveArrayElement(PROPLIST_T(pl_array),i),1);
}
#undef FUNC_NAME

/* proplist_t PLInsertArrayElement(proplist_t array, proplist_t pl,
   unsigned int index); */
SCWM_PROC(proplist_insert_array_element,"proplist-insert-array-element",3,0,0,
	  (SCM pl_array,SCM item,SCM index),
"Return the PL-ARRAY after inserting ITEM at offset INDEX.")
#define FUNC_NAME s_proplist_insert_array_element
{
  unsigned int i;
  VALIDATE_ARG_PROPLIST(1,pl_array);
  VALIDATE_ARG_PROPLIST(2,item);
  VALIDATE_ARG_INT_COPY(3,index,i);
  /* PLInsertArrayElement returns Array. must inc ref */
  return proplist_to_scm(PLInsertArrayElement(PROPLIST_T(pl_array),
                                              PROPLIST_T(item),
                                              i),1);
}
#undef FUNC_NAME

/* proplist_t PLAppendArrayElement(proplist_t array, proplist_t pl); */
SCWM_PROC(proplist_append_array_element,"proplist-append-array-element",2,0,0,
	  (SCM pl_array,SCM item),
"Return the PL-ARRAY after appending ITEM to the end.")
#define FUNC_NAME s_proplist_append_array_element
{
  VALIDATE_ARG_PROPLIST(1,pl_array);
  VALIDATE_ARG_PROPLIST(2,item);
  /* PLAppendArrayElement returns pl_array. must inc ref */
  return proplist_to_scm(PLAppendArrayElement(PROPLIST_T(pl_array),
                                              PROPLIST_T(item)),1);
}
#undef FUNC_NAME

/* proplist_t PLInsertDictionaryEntry(proplist_t dict, proplist_t key,
   proplist_t value); */
SCWM_PROC(proplist_insert_dictionary_entry,"proplist-insert-dictionary-entry",3,0,0,
	  (SCM pl_dict,SCM key,SCM Value),
"Return PL-DICT after inserting (KEY,VALUE) into it.")
#define FUNC_NAME s_proplist_insert_dictionary_entry
{
  VALIDATE_ARG_PROPLIST_DICT(1,pl_dict);
  VALIDATE_ARG_PROPLIST(2,key);
  VALIDATE_ARG_PROPLIST(3,Value);
  /* PLInsertDictionaryEntry returns pl_dict. must inc ref */
  return proplist_to_scm(PLInsertDictionaryEntry(PROPLIST_T(pl_dict),
                                                 PROPLIST_T(key),
                                                 PROPLIST_T(Value)),1);
}
#undef FUNC_NAME

/* proplist_t PLRemoveDictionaryEntry(proplist_t dict, proplist_t key); */
SCWM_PROC(proplist_remove_dictionary_entry,"proplist-remove-dictionary-entry",2,0,0,
	  (SCM pl_dict,SCM key),
"Return PL-DICT after removing KEY and its corresponding value from it.")
#define FUNC_NAME s_proplist_remove_dictionary_entry
{
  VALIDATE_ARG_PROPLIST_DICT(1,pl_dict);
  VALIDATE_ARG_PROPLIST(2,key);
  /* PLRemoveDictionaryEntry returns pl_dict. must inc ref */
  return proplist_to_scm(PLRemoveDictionaryEntry(PROPLIST_T(pl_dict),
                                                 PROPLIST_T(key)),1);
}
#undef FUNC_NAME

/* proplist_t PLMergeDictionaries(proplist_t dest, proplist_t source); */
SCWM_PROC(proplist_merge_dictionaries,"proplist-merge-dictionaries",2,0,0,
	  (SCM pl_dict_dest,SCM pl_dict_source),
"Return PL-DICT-DEST after merging in all entries from PL-DICT-SOURCE.")
#define FUNC_NAME s_proplist_merge_dictionaries
{
  VALIDATE_ARG_PROPLIST_DICT(1,pl_dict_dest);
  VALIDATE_ARG_PROPLIST_DICT(2,pl_dict_source);
  /* PLMergeDictionaries returns pl_dict_dest. must inc ref */
  return proplist_to_scm(PLMergeDictionaries(PROPLIST_T(pl_dict_dest),
                                             PROPLIST_T(pl_dict_source)),1);
}
#undef FUNC_NAME

/* proplist_t PLShallowCopy(proplist_t pl); */
SCWM_PROC(proplist_shallow_copy,"proplist-shallow-copy",1,0,0,
	  (SCM pl),
"Return a shallow copy of property list PL.")
#define FUNC_NAME s_proplist_shallow_copy
{
  VALIDATE_ARG_PROPLIST(1,pl);
  /* PLShallowCopy returns a new object. must not inc ref */
  return proplist_to_scm(PLShallowCopy(PROPLIST_T(pl)),0);
}
#undef FUNC_NAME

/* proplist_t PLDeepCopy(proplist_t pl); */
SCWM_PROC(proplist_deep_copy,"proplist-deep-copy",1,0,0,
	  (SCM pl),
"Return a deep copy of property list PL.")
#define FUNC_NAME s_proplist_deep_copy
{
  VALIDATE_ARG_PROPLIST(1,pl);
  /* PLDeepCopy returns a new object. must not inc ref */
  return proplist_to_scm(PLDeepCopy(PROPLIST_T(pl)),0);
}
#undef FUNC_NAME

/* BOOL PLIsEqual(proplist_t pl1, proplist_t pl2); */
SCWM_PROC(proplist_is_equal,"proplist-is-equal",2,0,0,
	  (SCM plA,SCM plB),
"Return #t iff PLA is equal to PLB.
This does a case sensitive comparison by default.
See `proplist-set-string-cmp-hook' for changing the behaviour.")
#define FUNC_NAME s_proplist_is_equal
{
  VALIDATE_ARG_PROPLIST(1,plA);
  VALIDATE_ARG_PROPLIST(2,plB);
  return gh_bool2scm(PLIsEqual(PROPLIST_T(plA),
                               PROPLIST_T(plB)));
}
#undef FUNC_NAME

/* void PLSetStringCmpHook(BOOL(*fn)(proplist_t, proplist_t)); */
SCWM_PROC(proplist_set_string_cmp_hook,"proplist-set-string-cmp-hook",1,0,0,
	  (SCM pred),
"Use PRED as a predicate for doing proplist string comparisons.
PRED should take two proplist objects and return #t or #f to answer
whether they are equal in whatever sense it chooses.")
#define FUNC_NAME s_proplist_set_string_cmp_hook
{
  BOOL (*cb)(proplist_t,proplist_t);
  if (pred==SCM_BOOL_F) {
    cb=NULL;
  } else {
    cb=c_cmp_callback;
  }
  PLSetStringCmpHook(cb);
  return SCM_BOOL_T;
}
#undef FUNC_NAME

/* proplist_t PLMakeString(char *bytes); */
SCWM_PROC(string_to_proplist,"string->proplist",1,0,0,
	  (SCM string),
"Return a proplist object containing STRING.
This is often unnecessary as the proplist procedures will
permit Guile strings to be used anywhere a string proplist object is expected. ")
#define FUNC_NAME s_string_to_proplist
{
  char *s;
  SCM result;
  VALIDATE_ARG_STR_NEWCOPY(1,string,s);
  result=ScmProplist(PLMakeString(s));
  FREE(s);

  return result;
}
#undef FUNC_NAME

/* proplist_t PLMakeData(unsigned char *data, unsigned int length); */
SCWM_PROC(proplist_make_data,"proplist-make-data",1,0,0,
	  (SCM data),
"Return a proplist argument containing arbitrary data from DATA.
This is often unnecessary as the proplist procedures will
permit Guile strings to be used anywhere a string proplist object is expected. ")
#define FUNC_NAME s_proplist_make_data
{
  int len;
  char *s = gh_scm2newstr(data,&len);
  SCM result;

  /* PLMakeData returns new object. must not inc refcount */
  result=proplist_to_scm(PLMakeData(s,(unsigned int)len),0);
  FREE(s);

  return result;
}
#undef FUNC_NAME

/* proplist_t PLGetContainer(proplist_t pl); */
SCWM_PROC(proplist_get_container,"proplist-get-container",1,0,0,
	  (SCM pl),
"Return the array or dictionary of which PL is an element.")
#define FUNC_NAME s_proplist_get_container
{
  VALIDATE_ARG_PROPLIST(1,pl);
  /* PLGetContainer returns parent of pl. must inc refcount */
  return proplist_to_scm(PLGetContainer(PROPLIST_T(pl)),1);
}
#undef FUNC_NAME

/* proplist_t PLGetArrayElement(proplist_t pl, unsigned int index); */
SCWM_PROC(proplist_get_array_element,"proplist-get-array-element",2,0,0,
	  (SCM pl,SCM index),
"Return the element at INDEX offset of PL.")
#define FUNC_NAME s_proplist_get_array_element
{
  unsigned int i;
  VALIDATE_ARG_PROPLIST(1,pl);
  VALIDATE_ARG_INT_COPY(2,index,i);

  /* PLGetArrayElement returns an array element. must inc refcount */
  return proplist_to_scm(PLGetArrayElement(PROPLIST_T(pl),i),1);
}
#undef FUNC_NAME

/* unsigned int PLGetNumberOfElements(proplist_t pl); */
SCWM_PROC(proplist_get_number_of_elements,"proplist-get-number-of-elements",1,0,0,
	  (SCM pl),
"Return the number of elements in PL.
If PL is a string or data objects, this returns 0.  If it 
is an array or dictionary, it is the number of elements
or pairs.")
#define FUNC_NAME s_proplist_get_number_of_elements
{
  VALIDATE_ARG_PROPLIST(1,pl);
  return gh_ulong2scm(PLGetNumberOfElements(PROPLIST_T(pl)));
}
#undef FUNC_NAME

/* proplist_t PLGetAllDictionaryKeys(proplist_t pl); */
SCWM_PROC(proplist_get_all_dictionary_keys,"proplist-get-all-dictionary-keys",1,0,0,
	  (SCM pl_dict),
"Return a proplist array object that contains all the keys of PL-DICT.")
#define FUNC_NAME s_proplist_get_all_dictionary_keys
{
  VALIDATE_ARG_PROPLIST_DICT(1,pl_dict);
  /* PLGetAllDictionaryKeys returns a new array proplist. must not inc
   * refcount */
  return proplist_to_scm(PLGetAllDictionaryKeys(PROPLIST_T(pl_dict)),0);
}
#undef FUNC_NAME

/* proplist_t PLGetDictionaryEntry(proplist_t pl, proplist_t key); */
SCWM_PROC(proplist_get_dictionary_entry,"proplist-get-dictionary-entry",2,0,0,
	  (SCM pl_dict,SCM key),
"Return the proplist dictionary entry associated with entry KEY in PL-DICT. 
Returns #f if KEY is not in PL-DICT.")
#define FUNC_NAME s_proplist_get_dictionary_entry
{
  VALIDATE_ARG_PROPLIST_DICT(1,pl_dict);
  VALIDATE_ARG_PROPLIST(2,key);
  /* PLGetDictionaryEntry returns a dict entry from Dict. must inc refcount */
  return proplist_to_scm(PLGetDictionaryEntry(PROPLIST_T(pl_dict),
                                              PROPLIST_T(key)),1);
}
#undef FUNC_NAME

/* char *PLGetStringDescription(proplist_t pl); */
SCWM_PROC(proplist_get_string_description,"proplist-get-string-description",1,0,0,
	  (SCM pl),
"Retuns a description of PL (a string proplist object) in GNUstep format.
If the string contains whitespace or special characters, the string returned will
be enclosed in quotes.")
#define FUNC_NAME s_proplist_get_string_description
{
  char *s;
  SCM result;

  VALIDATE_ARG_PROPLIST(1,pl);
  s=PLGetStringDescription(PROPLIST_T(pl));
  if (s) {
    result=gh_str02scm(s);
    free(s);
  } else {
    result=SCM_BOOL_F;
  }
  return result;
}
#undef FUNC_NAME

/* char *PLGetDataDescription(proplist_t pl); */
SCWM_PROC(proplist_get_data_description,"proplist-get-data-description",1,0,0,
	  (SCM pl),
"Returns a description of PL (a data proplist object) in GNUstep format.")
#define FUNC_NAME s_proplist_get_data_description
{
  char *s;
  SCM result;

  VALIDATE_ARG_PROPLIST(1,pl);
  s=PLGetDataDescription(PROPLIST_T(pl));
  if (s) {
    result=gh_str02scm(s);
    free(s);
  } else {
    result=SCM_BOOL_F;
  }
  return result;
}
#undef FUNC_NAME

/* unsigned int PLGetDataLength(proplist_t pl); */
/* unsigned char *PLGetDataBytes(proplist_t pl); */
SCWM_PROC(proplist_get_data,"proplist-get-data",1,0,0,
	  (SCM pl),
"Returns the raw data from PL (a data proplist object) as a string.")
#define FUNC_NAME s_proplist_get_data
{
  unsigned char *s;
  unsigned int sl;
  SCM result;
  proplist_t p;

  VALIDATE_ARG_PROPLIST(1,pl);
  p=PROPLIST_T(pl);
  s=PLGetDataBytes(p);
  if (s) {
    sl=PLGetDataLength(p);
    result=gh_str2scm((char *)s,(int)sl);
    free(s);
  } else {
    result=SCM_BOOL_F;
  }
  return result;
}
#undef FUNC_NAME

/* char *PLGetString(proplist_t pl); */
SCWM_PROC(proplist_get_string,"proplist-get-string",1,0,0,
	  (SCM pl),
"Returns the string from PL (a string proplist object).
Note that numerous procedures automatically convert string 
proplist objects into Guile strings, so this may not often be necessary.")
#define FUNC_NAME s_proplist_get_string
{
  char *s;
  SCM result;
  VALIDATE_ARG_PROPLIST(1,pl);
  s=PLGetString(PROPLIST_T(pl));
  if (s) {
    result=gh_str02scm(s);
  } else {
    result=SCM_BOOL_F;
  }
  return result;
}
#undef FUNC_NAME

/* BOOL PLIsString(proplist_t pl); */
SCWM_PROC(proplist_is_string,"proplist-is-string",1,0,0,
	  (SCM pl),
"Return #t iff PL is a string proplist object, #f otherwise.")
#define FUNC_NAME s_proplist_is_string
{
  VALIDATE_ARG_PROPLIST(1,pl);
  return gh_bool2scm(PLIsString(PROPLIST_T(pl)));
}
#undef FUNC_NAME

/* BOOL PLIsData(proplist_t pl); */
SCWM_PROC(proplist_is_data,"proplist-is-data",1,0,0,
	  (SCM pl),
"Return #t iff PL is a data proplist object, #f otherwise.")
#define FUNC_NAME s_proplist_is_data
{
  VALIDATE_ARG_PROPLIST(1,pl);
  return gh_bool2scm(PLIsData(PROPLIST_T(pl)));
}
#undef FUNC_NAME

/* BOOL PLIsArray(proplist_t pl); */
SCWM_PROC(proplist_is_array,"proplist-is-array",1,0,0,
	  (SCM pl),
"Return #t iff PL is an array proplist object, #f otherwise.")
#define FUNC_NAME s_proplist_is_array
{
  VALIDATE_ARG_PROPLIST(1,pl);
  return gh_bool2scm(PLIsArray(PROPLIST_T(pl)));
}
#undef FUNC_NAME

/* BOOL PLIsDictionary(proplist_t pl); */
SCWM_PROC(proplist_is_dictionary,"proplist-is-dictionary",1,0,0,
	  (SCM pl),
"Return #t iff PL is a dictionary proplist object, #f otherwise.")
#define FUNC_NAME s_proplist_is_dictionary
{
  VALIDATE_ARG_PROPLIST(1,pl);
  return gh_bool2scm(PLIsDictionary(PROPLIST_T(pl)));
}
#undef FUNC_NAME

/* BOOL PLIsSimple(proplist_t pl); */
SCWM_PROC(proplist_is_simple,"proplist-is-simple",1,0,0,
	  (SCM pl),
"Return #t iff PL is a string or data proplist object, #f otherwise.")
#define FUNC_NAME s_proplist_is_simple
{
  VALIDATE_ARG_PROPLIST(1,pl);
  return gh_bool2scm(PLIsSimple(PROPLIST_T(pl)));
}
#undef FUNC_NAME

/* BOOL PLIsCompound(proplist_t pl); */
SCWM_PROC(proplist_is_compound,"proplist-is-compound",1,0,0,
	  (SCM pl),
"Return #t iff PL is a dictionary or array proplist object, #f otherwise.")
#define FUNC_NAME s_proplist_is_compound
{
  VALIDATE_ARG_PROPLIST(1,pl);
  return gh_bool2scm(PLIsCompound(PROPLIST_T(pl)));
}
#undef FUNC_NAME

/* proplist_t PLUnregister(proplist_t name); */
SCWM_PROC(proplist_unregister,"proplist-unregister",1,0,0,
	  (SCM pl_domain_name),
"No longer invoke callback when domain associated with PL-DOMAIN-NAME is changed.")
#define FUNC_NAME s_proplist_unregister
{
  VALIDATE_ARG_PROPLIST(1,pl_domain_name);
  /* PLUnregister returns the input parameter - must inc refcount */
  return proplist_to_scm(PLUnregister(PROPLIST_T(pl_domain_name)),1);
}
#undef FUNC_NAME

/* proplist_t PLRegister(proplist_t name, plcallback_t callback); */
SCWM_PROC(proplist_register,"proplist-register",2,0,0,
	  (SCM pl_domain_name,SCM callback),
"Register thunk CALLBACK to be called when PL-DOMAIN-NAME changes.")
#define FUNC_NAME s_proplist_register
{
  void (*cb)(void);
  VALIDATE_ARG_PROPLIST(1,pl_domain_name);
  if (callback==SCM_BOOL_F) {
    cb=NULL;
  } else {
    cb=c_callback;
    scm_callback=callback;
  }
  /* PLRegister returns the input parameter - must inc refcount */
  return proplist_to_scm(PLRegister(PROPLIST_T(pl_domain_name),cb),1);
}
#undef FUNC_NAME

/* proplist_t PLSetDomain(proplist_t name, proplist_t value, BOOL kickme); */
SCWM_PROC(proplist_set_domain,"proplist-set-domain",3,0,0,
	  (SCM pl_domain_name,SCM value,SCM kick_me_p),
"Set PL-DOMAIN-NAME to have VALUE.
If KICK-ME? is #f, any callback registered for the domain will not be called.")
#define FUNC_NAME s_proplist_set_domain
{
  Bool kick_me;
  VALIDATE_ARG_PROPLIST(1,pl_domain_name);
  VALIDATE_ARG_PROPLIST(2,value);
  VALIDATE_ARG_BOOL_COPY(3,kick_me_p,kick_me);
  /* PLSetDomain returns the input value - must inc refcount */
  return proplist_to_scm(PLSetDomain(PROPLIST_T(pl_domain_name),
                                     PROPLIST_T(value),
                                     kick_me),1);
}
#undef FUNC_NAME

/* proplist_t PLDeleteDomain(proplist_t name, BOOL kickme); */
SCWM_PROC(proplist_delete_domain,"proplist-delete-domain",2,0,0,
	  (SCM pl_domain_name,SCM kick_me_p),
"Delete domain PL-DOMAIN-NAME.
If KICK-ME? is #f, any callback registered for the domain will not be called.")
#define FUNC_NAME s_proplist_delete_domain
{
  Bool kick_me;
  VALIDATE_ARG_PROPLIST(1,pl_domain_name);
  kick_me=gh_scm2bool(kick_me_p);
  /* PLDeleteDomain returns the input name - must inc refcount */
  return proplist_to_scm(PLDeleteDomain(PROPLIST_T(pl_domain_name),
                                        kick_me),1);
}
#undef FUNC_NAME

/* proplist_t PLGetDomain(proplist_t name); */
SCWM_PROC(proplist_get_domain,"proplist-get-domain",1,0,0,
	  (SCM pl_domain_name),
"Return a property list represeting the domain PL-DOMAIN-NAME.")
#define FUNC_NAME s_proplist_get_domain
{
  VALIDATE_ARG_PROPLIST(1,pl_domain_name);
  /* PLGetDomain returns the domain - don't inc refcount */
  return proplist_to_scm(PLGetDomain(PROPLIST_T(pl_domain_name)),0);
}
#undef FUNC_NAME

/* proplist_t PLGetDomainNames(); */
SCWM_PROC(proplist_get_domainnames,"proplist-get-domainnames",0,0,0,
	  (),
"Return a array proplist containing all registered domain names.")
#define FUNC_NAME s_proplist_get_domainnames
{
  /* PLGetDomainNames returns an array of domain names - don't inc refcount */
  return proplist_to_scm(PLGetDomainNames(),0);
}
#undef FUNC_NAME

/* proplist_t PLGetFilename(proplist_t pl); */
SCWM_PROC(proplist_get_filename,"proplist-get-filename",1,0,0,
	  (SCM pl),
"Return the filename of PL. ")
#define FUNC_NAME s_proplist_get_filename
{
  VALIDATE_ARG_PROPLIST(1,pl);
  /* PLGetFilename returns an attribute of pl - inc refcount */
  return proplist_to_scm(PLGetFilename(PROPLIST_T(pl)),1);
}
#undef FUNC_NAME

/* proplist_t PLSetfilename(proplist_t pl, proplist_t filename); */
SCWM_PROC(proplist_set_filename_x,"proplist-set-filename!",2,0,0,
	  (SCM pl,SCM filename),
"Set the filename for PL to be FILENAME.")
#define FUNC_NAME s_proplist_set_filename_x
{
  proplist_t fname;
  int must_release;
  SCM result;

  VALIDATE_ARG_PROPLIST(1,pl);
  if (gh_string_p(filename)) {
    char *sz = gh_scm2newstr(filename,NULL);
    fname = PLMakeString(sz);
    FREE(sz);
    must_release=1;
  } else {
    VALIDATE_ARG_PROPLIST(2,filename);
    fname = PROPLIST_T(filename);
    must_release=0;
  }
  /* PLSetFilename returns Filename - don't inc refcount */
  result=proplist_to_scm(PLSetFilename(PROPLIST_T(pl),fname),0);
  if (must_release) {
    PLRelease(fname);
  }
  return result;
}
#undef FUNC_NAME

/* BOOL PLSave(proplist_t pl, BOOL atomically); */
SCWM_PROC(proplist_save,"proplist-save",2,0,0,
	  (SCM pl,SCM atomically_p),
"Save PL, atomically if ATOMICALLY? is #t.")
#define FUNC_NAME s_proplist_save
{
  scwm_proplist_t *proplist;
  Bool atomically;
  VALIDATE_ARG_PROPLIST_COPY(1,pl,proplist);
  VALIDATE_ARG_BOOL_COPY(2,atomically_p,atomically);
  return gh_bool2scm(PLSave(proplist,atomically));
}
#undef FUNC_NAME

/* BOOL PLSynchronize(proplist_t pl); */
SCWM_PROC(proplist_synchronize,"proplist-synchronize",1,0,0,
	  (SCM pl),
"Synchronize the in-memory proplist PL with the disk contents.")
#define FUNC_NAME s_proplist_synchronize
{
  scwm_proplist_t *proplist;
  VALIDATE_ARG_PROPLIST_COPY(1,pl,proplist);
  return gh_bool2scm(PLSynchronize(proplist));
}
#undef FUNC_NAME

/* proplist_t PLGetProplistWithPath(const char *filename); */
SCWM_PROC(get_proplist_with_path,"get-proplist-with-path",1,0,0,
	  (SCM filename),
"Return the proplist from path FILENAME.")
#define FUNC_NAME s_get_proplist_with_path
{
  char *sz;
  proplist_t pl;

  VALIDATE_ARG_STR_NEWCOPY(1,filename,sz);
  pl = PLGetProplistWithPath(sz);
  FREE(sz);
  /* PLGetProplistWithPath returns a new object - don't inc refcount */
  return proplist_to_scm(pl,0);
}
#undef FUNC_NAME

/* proplist_t PLGetProplistWithDescription(const char *description); */
SCWM_PROC(get_proplist_with_description,"get-proplist-with-description",1,0,0,
	  (SCM desc),
"Return a property list by parsing DESC in GNUstep proplist format.")
#define FUNC_NAME s_get_proplist_with_description
{
  char *sz;
  proplist_t pl;

  VALIDATE_ARG_STR_NEWCOPY(1,desc,sz);
  pl=PLGetProplistWithDescription(sz);
  FREE(sz);
  /* PLGetProplistWithDescription returns a new object - don't inc refcount */
  return proplist_to_scm(pl,0);
}
#undef FUNC_NAME

MAKE_SMOBFUNS(proplist_t);

static
void
init_proplist_wrapper() {
  REGISTER_SCWMSMOBFUNS(proplist_t);
#ifndef SCM_MAGIC_SNARFER
#include "scwmproplist.x"
#endif
}


void scm_init_app_scwm_scwmproplist_module() {
  scm_register_module_xxx("app scwm scwmproplist", init_proplist_wrapper);
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
