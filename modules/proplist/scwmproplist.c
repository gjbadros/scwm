/* $Id$
 * scwmproplist.c
 * (C) 1999 Toby Sargeant and Greg J. Badros
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>

#include <proplist.h>
#include "proplistP.h" /* from the proplist source distribution */

#include "scwm.h"
#include "validate.h"
#include "callbacks.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

/* ========================================================================== */
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


/* ========================================================================== */
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
  gh_defer_ints();
  SCWM_NEWCELL_SMOB(result,scm_tc16_scwm_proplist_t,spt);
  gh_allow_ints();
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
/* ========================================================================== */
/* proplist_t PLMakeArrayFromElements(proplist_t pl, ...); */
SCWM_PROC(proplist_make_array_from_elements,"proplist-make-array-from-elements",0,0,1,
	  (SCM List))
     /**  */
#define FUNC_NAME s_proplist_make_array_from_elements
{
  proplist_t array=NULL;
  SCM value;
  while (1) {
    if (List==SCM_EOL) break;
    value=gh_car(List),List=gh_cdr(List);
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

/* ========================================================================== */
/* proplist_t PLMakeDictionaryFromEntries(proplist_t key, proplist_t value,
   ...); */
SCWM_PROC(proplist_make_dictionary_from_entries,"proplist-make-dictionary-from-entries",0,0,1,
	  (SCM List))
     /**  */
#define FUNC_NAME s_proplist_make_dictionary_from_entries
{
  proplist_t dict=NULL;
  SCM key,value;
  if (!gh_list_p(List)) {
    SCWM_WRONG_TYPE_ARG(1,List);
  }
  while (1) {
    if (List==SCM_EOL) break;
    key=gh_car(List),List=gh_cdr(List);
    if (List==SCM_EOL) {
      scm_misc_error(FUNC_NAME,"List must have an even number of elements",SCM_EOL);
    }
    value=gh_car(List),List=gh_cdr(List);
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

/* ========================================================================== */
/* proplist_t PLRemoveArrayElement(proplist_t array,
   unsigned int index); */
SCWM_PROC(proplist_remove_array_element,"proplist-remove-array-element",2,0,0,
	  (SCM Array,SCM Index))
     /**  */
#define FUNC_NAME s_proplist_remove_array_element
{
  unsigned int index;
  VALIDATE_ARG_PROPLIST(1,Array);
  index=gh_scm2ulong(Index);
  /* PLRemoveArrayElement returns Array. must inc ref */
  return proplist_to_scm(PLRemoveArrayElement(PROPLIST_T(Array),
                                              index),1);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLInsertArrayElement(proplist_t array, proplist_t pl,
   unsigned int index); */
SCWM_PROC(proplist_insert_array_element,"proplist-insert-array-element",3,0,0,
	  (SCM Array,SCM Pl,SCM Index))
     /**  */
#define FUNC_NAME s_proplist_insert_array_element
{
  unsigned int index;
  VALIDATE_ARG_PROPLIST(1,Array);
  VALIDATE_ARG_PROPLIST(2,Pl);
  index=gh_scm2ulong(Index);
  /* PLInsertArrayElement returns Array. must inc ref */
  return proplist_to_scm(PLInsertArrayElement(PROPLIST_T(Array),
                                              PROPLIST_T(Pl),
                                              index),1);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLAppendArrayElement(proplist_t array, proplist_t pl); */
SCWM_PROC(proplist_append_array_element,"proplist-append-array-element",2,0,0,
	  (SCM Array,SCM Pl))
     /**  */
#define FUNC_NAME s_proplist_append_array_element
{
  VALIDATE_ARG_PROPLIST(1,Array);
  VALIDATE_ARG_PROPLIST(2,Pl);
  /* PLAppendArrayElement returns Array. must inc ref */
  return proplist_to_scm(PLAppendArrayElement(PROPLIST_T(Array),
                                              PROPLIST_T(Pl)),1);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLInsertDictionaryEntry(proplist_t dict, proplist_t key,
   proplist_t value); */
SCWM_PROC(proplist_insert_dictionary_entry,"proplist-insert-dictionary-entry",3,0,0,
	  (SCM Dict,SCM Key,SCM Value))
     /**  */
#define FUNC_NAME s_proplist_insert_dictionary_entry
{
  VALIDATE_ARG_PROPLIST_DICT(1,Dict);
  VALIDATE_ARG_PROPLIST(2,Key);
  VALIDATE_ARG_PROPLIST(3,Value);
  /* PLInsertDictionaryEntry returns Dict. must inc ref */
  return proplist_to_scm(PLInsertDictionaryEntry(PROPLIST_T(Dict),
                                                 PROPLIST_T(Key),
                                                 PROPLIST_T(Value)),1);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLRemoveDictionaryEntry(proplist_t dict, proplist_t key); */
SCWM_PROC(proplist_remove_dictionary_entry,"proplist-remove-dictionary-entry",2,0,0,
	  (SCM Dict,SCM Key))
     /**  */
#define FUNC_NAME s_proplist_remove_dictionary_entry
{
  VALIDATE_ARG_PROPLIST_DICT(1,Dict);
  VALIDATE_ARG_PROPLIST(2,Key);
  /* PLRemoveDictionaryEntry returns Dict. must inc ref */
  return proplist_to_scm(PLRemoveDictionaryEntry(PROPLIST_T(Dict),
                                                 PROPLIST_T(Key)),1);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLMergeDictionaries(proplist_t dest, proplist_t source); */
SCWM_PROC(proplist_merge_dictionaries,"proplist-merge-dictionaries",2,0,0,
	  (SCM Dest,SCM Source))
     /**  */
#define FUNC_NAME s_proplist_merge_dictionaries
{
  VALIDATE_ARG_PROPLIST_DICT(1,Dest);
  VALIDATE_ARG_PROPLIST_DICT(2,Source);
  /* PLMergeDictionaries returns Dest. must inc ref */
  return proplist_to_scm(PLMergeDictionaries(PROPLIST_T(Dest),
                                             PROPLIST_T(Source)),1);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLShallowCopy(proplist_t pl); */
SCWM_PROC(proplist_shallow_copy,"proplist-shallow-copy",1,0,0,
	  (SCM Pl))
     /**  */
#define FUNC_NAME s_proplist_shallow_copy
{
  VALIDATE_ARG_PROPLIST(1,Pl);
  /* PLShallowCopy returns a new object. must not inc ref */
  return proplist_to_scm(PLShallowCopy(PROPLIST_T(Pl)),0);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLDeepCopy(proplist_t pl); */
SCWM_PROC(proplist_deep_copy,"proplist-deep-copy",1,0,0,
	  (SCM Pl))
     /**  */
#define FUNC_NAME s_proplist_deep_copy
{
  VALIDATE_ARG_PROPLIST(1,Pl);
  /* PLDeepCopy returns a new object. must not inc ref */
  return proplist_to_scm(PLDeepCopy(PROPLIST_T(Pl)),0);
}
#undef FUNC_NAME

/* ========================================================================== */
/* BOOL PLIsEqual(proplist_t pl1, proplist_t pl2); */
SCWM_PROC(proplist_is_equal,"proplist-is-equal",2,0,0,
	  (SCM PlA,SCM PlB))
     /**  */
#define FUNC_NAME s_proplist_is_equal
{
  VALIDATE_ARG_PROPLIST(1,PlA);
  VALIDATE_ARG_PROPLIST(2,PlB);
  return gh_bool2scm(PLIsEqual(PROPLIST_T(PlA),
                               PROPLIST_T(PlB)));
}
#undef FUNC_NAME

/* ========================================================================== */
/* void PLSetStringCmpHook(BOOL(*fn)(proplist_t, proplist_t)); */
SCWM_PROC(proplist_set_string_cmp_hook,"proplist-set-string-cmp-hook",1,0,0,
	  (SCM Hook))
     /**  */
#define FUNC_NAME s_proplist_set_string_cmp_hook
{
  BOOL (*cb)(proplist_t,proplist_t);
  if (Hook==SCM_BOOL_F) {
    cb=NULL;
  } else {
    cb=c_cmp_callback;
  }
  PLSetStringCmpHook(cb);
  return SCM_BOOL_T;
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLMakeString(char *bytes); */
SCWM_PROC(string_to_proplist,"string->proplist",1,0,0,
	  (SCM Data))
     /**  */
#define FUNC_NAME s_string_to_proplist
{
  char *s;
  SCM result;
  VALIDATE_ARG_STR_NEWCOPY(1,Data,s);
  result=ScmProplist(PLMakeString(s));
  FREE(s);

  return result;
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLMakeData(unsigned char *data, unsigned int length); */
SCWM_PROC(proplist_make_data,"proplist-make-data",1,0,0,
	  (SCM Data))
     /**  */
#define FUNC_NAME s_proplist_make_data
{
  int len;
  char *s;
  SCM result;

  s=gh_scm2newstr(Data,&len);

  /* PLMakeData returns new object. must not inc refcount */
  result=proplist_to_scm(PLMakeData(s,(unsigned int)len),0);
  FREE(s);

  return result;
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLGetContainer(proplist_t pl); */
SCWM_PROC(proplist_get_container,"proplist-get-container",1,0,0,
	  (SCM Pl))
     /**  */
#define FUNC_NAME s_proplist_get_container
{
  VALIDATE_ARG_PROPLIST(1,Pl);
  /* PLGetContainer returns parent of Pl. must inc refcount */
  return proplist_to_scm(PLGetContainer(PROPLIST_T(Pl)),1);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLGetArrayElement(proplist_t pl, unsigned int index); */
SCWM_PROC(proplist_get_array_element,"proplist-get-array-element",2,0,0,
	  (SCM Pl,SCM Index))
     /**  */
#define FUNC_NAME s_proplist_get_array_element
{
  unsigned int index;
  VALIDATE_ARG_PROPLIST(1,Pl);
  index=gh_scm2ulong(Index);

  /* PLGetArrayElement returns an array element. must inc refcount */
  return proplist_to_scm(PLGetArrayElement(PROPLIST_T(Pl),index),1);
}
#undef FUNC_NAME

/* ========================================================================== */
/* unsigned int PLGetNumberOfElements(proplist_t pl); */
SCWM_PROC(proplist_get_number_of_elements,"proplist-get-number-of-elements",1,0,0,
	  (SCM Pl))
     /**  */
#define FUNC_NAME s_proplist_get_number_of_elements
{
  VALIDATE_ARG_PROPLIST(1,Pl);
  return gh_ulong2scm(PLGetNumberOfElements(PROPLIST_T(Pl)));
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLGetAllDictionaryKeys(proplist_t pl); */
SCWM_PROC(proplist_get_all_dictionary_keys,"proplist-get-all-dictionary-keys",1,0,0,
	  (SCM Dict))
     /**  */
#define FUNC_NAME s_proplist_get_all_dictionary_keys
{
  VALIDATE_ARG_PROPLIST_DICT(1,Dict);
  /* PLGetAllDictionaryKeys returns a new array proplist. must not inc
   * refcount */
  return proplist_to_scm(PLGetAllDictionaryKeys(PROPLIST_T(Dict)),0);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLGetDictionaryEntry(proplist_t pl, proplist_t key); */
SCWM_PROC(proplist_get_dictionary_entry,"proplist-get-dictionary-entry",2,0,0,
	  (SCM Dict,SCM Key))
     /**  */
#define FUNC_NAME s_proplist_get_dictionary_entry
{
  VALIDATE_ARG_PROPLIST_DICT(1,Dict);
  VALIDATE_ARG_PROPLIST(2,Key);
  /* PLGetDictionaryEntry returns a dict entry from Dict. must inc refcount */
  return proplist_to_scm(PLGetDictionaryEntry(PROPLIST_T(Dict),
                                              PROPLIST_T(Key)),1);
}
#undef FUNC_NAME

/* ========================================================================== */
/* char *PLGetStringDescription(proplist_t pl); */
SCWM_PROC(proplist_get_string_description,"proplist-get-string-description",1,0,0,
	  (SCM Pl))
     /**  */
#define FUNC_NAME s_proplist_get_string_description
{
  char *s;
  SCM result;

  VALIDATE_ARG_PROPLIST(1,Pl);
  s=PLGetStringDescription(PROPLIST_T(Pl));
  if (s) {
    result=gh_str02scm(s);
    free(s);
  } else {
    result=SCM_BOOL_F;
  }
  return result;
}
#undef FUNC_NAME

/* ========================================================================== */
/* char *PLGetDataDescription(proplist_t pl); */
SCWM_PROC(proplist_get_data_description,"proplist-get-data-description",1,0,0,
	  (SCM Pl))
     /**  */
#define FUNC_NAME s_proplist_get_data_description
{
  char *s;
  SCM result;

  VALIDATE_ARG_PROPLIST(1,Pl);
  s=PLGetDataDescription(PROPLIST_T(Pl));
  if (s) {
    result=gh_str02scm(s);
    free(s);
  } else {
    result=SCM_BOOL_F;
  }
  return result;
}
#undef FUNC_NAME

/* ========================================================================== */
/* unsigned int PLGetDataLength(proplist_t pl); */
/* unsigned char *PLGetDataBytes(proplist_t pl); */
SCWM_PROC(proplist_get_data,"proplist-get-data",1,0,0,
	  (SCM Pl))
     /**  */
#define FUNC_NAME s_proplist_get_data
{
  unsigned char *s;
  unsigned int sl;
  SCM result;
  proplist_t pl;

  VALIDATE_ARG_PROPLIST(1,Pl);
  pl=PROPLIST_T(Pl);
  s=PLGetDataBytes(pl);
  if (s) {
    sl=PLGetDataLength(pl);
    result=gh_str2scm((char *)s,(int)sl);
    free(s);
  } else {
    result=SCM_BOOL_F;
  }
  return result;
}
#undef FUNC_NAME

/* ========================================================================== */
/* char *PLGetString(proplist_t pl); */
SCWM_PROC(proplist_get_string,"proplist-get-string",1,0,0,
	  (SCM Pl))
     /**  */
#define FUNC_NAME s_proplist_get_string
{
  char *s;
  SCM result;
  VALIDATE_ARG_PROPLIST(1,Pl);
  s=PLGetString(PROPLIST_T(Pl));
  if (s) {
    result=gh_str02scm(s);
  } else {
    result=SCM_BOOL_F;
  }
  return result;
}
#undef FUNC_NAME

/* ========================================================================== */
/* BOOL PLIsString(proplist_t pl); */
SCWM_PROC(proplist_is_string,"proplist-is-string",1,0,0,
	  (SCM Pl))
     /**  */
#define FUNC_NAME s_proplist_is_string
{
  VALIDATE_ARG_PROPLIST(1,Pl);
  return gh_bool2scm(PLIsString(PROPLIST_T(Pl)));
}
#undef FUNC_NAME

/* ========================================================================== */
/* BOOL PLIsData(proplist_t pl); */
SCWM_PROC(proplist_is_data,"proplist-is-data",1,0,0,
	  (SCM Pl))
     /**  */
#define FUNC_NAME s_proplist_is_data
{
  VALIDATE_ARG_PROPLIST(1,Pl);
  return gh_bool2scm(PLIsData(PROPLIST_T(Pl)));
}
#undef FUNC_NAME

/* ========================================================================== */
/* BOOL PLIsArray(proplist_t pl); */
SCWM_PROC(proplist_is_array,"proplist-is-array",1,0,0,
	  (SCM Pl))
     /**  */
#define FUNC_NAME s_proplist_is_array
{
  VALIDATE_ARG_PROPLIST(1,Pl);
  return gh_bool2scm(PLIsArray(PROPLIST_T(Pl)));
}
#undef FUNC_NAME

/* ========================================================================== */
/* BOOL PLIsDictionary(proplist_t pl); */
SCWM_PROC(proplist_is_dictionary,"proplist-is-dictionary",1,0,0,
	  (SCM Pl))
     /**  */
#define FUNC_NAME s_proplist_is_dictionary
{
  VALIDATE_ARG_PROPLIST(1,Pl);
  return gh_bool2scm(PLIsDictionary(PROPLIST_T(Pl)));
}
#undef FUNC_NAME

/* ========================================================================== */
/* BOOL PLIsSimple(proplist_t pl); */
SCWM_PROC(proplist_is_simple,"proplist-is-simple",1,0,0,
	  (SCM Pl))
     /**  */
#define FUNC_NAME s_proplist_is_simple
{
  VALIDATE_ARG_PROPLIST(1,Pl);
  return gh_bool2scm(PLIsSimple(PROPLIST_T(Pl)));
}
#undef FUNC_NAME

/* ========================================================================== */
/* BOOL PLIsCompound(proplist_t pl); */
SCWM_PROC(proplist_is_compound,"proplist-is-compound",1,0,0,
	  (SCM Pl))
     /**  */
#define FUNC_NAME s_proplist_is_compound
{
  VALIDATE_ARG_PROPLIST(1,Pl);
  return gh_bool2scm(PLIsCompound(PROPLIST_T(Pl)));
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLUnregister(proplist_t name); */
SCWM_PROC(proplist_unregister,"proplist-unregister",1,0,0,
	  (SCM Name))
     /**  */
#define FUNC_NAME s_proplist_unregister
{
  VALIDATE_ARG_PROPLIST(1,Name);
  /* PLUnregister returns the input parameter - must inc refcount */
  return proplist_to_scm(PLUnregister(PROPLIST_T(Name)),1);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLRegister(proplist_t name, plcallback_t callback); */
SCWM_PROC(proplist_register,"proplist-register",2,0,0,
	  (SCM Name,SCM Callback))
     /**  */
#define FUNC_NAME s_proplist_register
{
  void (*cb)(void);
  VALIDATE_ARG_PROPLIST(1,Name);
  if (Callback==SCM_BOOL_F) {
    cb=NULL;
  } else {
    cb=c_callback;
    scm_callback=Callback;
  }
  /* PLRegister returns the input parameter - must inc refcount */
  return proplist_to_scm(PLRegister(PROPLIST_T(Name),cb),1);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLSetDomain(proplist_t name, proplist_t value, BOOL kickme); */
SCWM_PROC(proplist_set_domain,"proplist-set-domain",3,0,0,
	  (SCM Name,SCM Value,SCM Kick_me))
     /**  */
#define FUNC_NAME s_proplist_set_domain
{
  Bool kick_me;
  VALIDATE_ARG_PROPLIST(1,Name);
  VALIDATE_ARG_PROPLIST(2,Value);
  VALIDATE_ARG_BOOL_COPY(3,Kick_me,kick_me);
  /* PLSetDomain returns the input value - must inc refcount */
  return proplist_to_scm(PLSetDomain(PROPLIST_T(Name),
                                     PROPLIST_T(Value),
                                     kick_me),1);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLDeleteDomain(proplist_t name, BOOL kickme); */
SCWM_PROC(proplist_delete_domain,"proplist-delete-domain",2,0,0,
	  (SCM Name,SCM Kick_me))
     /**  */
#define FUNC_NAME s_proplist_delete_domain
{
  Bool kick_me;
  VALIDATE_ARG_PROPLIST(1,Name);
  kick_me=gh_scm2bool(Kick_me);
  /* PLDeleteDomain returns the input name - must inc refcount */
  return proplist_to_scm(PLDeleteDomain(PROPLIST_T(Name),
                                        kick_me),1);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLGetDomain(proplist_t name); */
SCWM_PROC(proplist_get_domain,"proplist-get-domain",1,0,0,
	  (SCM Name))
     /**  */
#define FUNC_NAME s_proplist_get_domain
{
  VALIDATE_ARG_PROPLIST(1,Name);
  /* PLGetDomain returns the domain - don't inc refcount */
  return proplist_to_scm(PLGetDomain(PROPLIST_T(Name)),0);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLGetDomainNames(); */
SCWM_PROC(proplist_get_domainnames,"proplist-get-domainnames",0,0,0,
	  ())
     /**  */
#define FUNC_NAME s_proplist_get_domainnames
{
  /* PLGetDomainNames returns an array of domain names - don't inc refcount */
  return proplist_to_scm(PLGetDomainNames(),0);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLGetFilename(proplist_t pl); */
SCWM_PROC(proplist_get_filename,"proplist-get-filename",1,0,0,
	  (SCM Proplist))
     /**  */
#define FUNC_NAME s_proplist_get_filename
{
  VALIDATE_ARG_PROPLIST(1,Proplist);
  /* PLGetFilename returns an attribute of Proplist - inc refcount */
  return proplist_to_scm(PLGetFilename(PROPLIST_T(Proplist)),1);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLSetFilename(proplist_t pl, proplist_t filename); */
SCWM_PROC(proplist_set_filename,"proplist-set-filename",2,0,0,
	  (SCM Proplist,SCM Filename))
     /**  */
#define FUNC_NAME s_proplist_set_filename
{
  proplist_t filename;
  int must_release;
  SCM result;

  VALIDATE_ARG_PROPLIST(1,Proplist);
  if (gh_string_p(Filename)) {
    char *f=gh_scm2newstr(Filename,NULL);
    filename=PLMakeString(f);
    FREE(f);
    must_release=1;
  } else {
    VALIDATE_ARG_PROPLIST(2,Filename);
    filename=PROPLIST_T(Filename);
    must_release=0;
  }
  /* PLSetFilename returns Filename - don't inc refcount */
  result=proplist_to_scm(PLSetFilename(PROPLIST_T(Proplist),filename),0);
  if (must_release) {
    PLRelease(filename);
  }
  return result;
}
#undef FUNC_NAME

/* ========================================================================== */
/* BOOL PLSave(proplist_t pl, BOOL atomically); */
SCWM_PROC(proplist_save,"proplist-save",2,0,0,
	  (SCM Proplist,SCM Atomically))
     /**  */
#define FUNC_NAME s_proplist_save
{
  scwm_proplist_t *proplist;
  Bool atomically;
  VALIDATE_ARG_PROPLIST_COPY(1,Proplist,proplist);
  VALIDATE_ARG_BOOL_COPY(2,Atomically,atomically);
  return gh_bool2scm(PLSave(proplist,atomically));
}
#undef FUNC_NAME

/* ========================================================================== */
/* BOOL PLSynchronize(proplist_t pl); */
SCWM_PROC(proplist_synchronize,"proplist-synchronize",1,0,0,
	  (SCM Proplist))
     /**  */
#define FUNC_NAME s_proplist_synchronize
{
  scwm_proplist_t *proplist;
  VALIDATE_ARG_PROPLIST_COPY(1,Proplist,proplist);
  return gh_bool2scm(PLSynchronize(proplist));
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLGetProplistWithPath(const char *filename); */
SCWM_PROC(get_proplist_with_path,"get-proplist-with-path",1,0,0,
	  (SCM Filename))
     /**  */
#define FUNC_NAME s_get_proplist_with_path
{
  char *filename;
  proplist_t pl;

  VALIDATE_ARG_STR_NEWCOPY(1,Filename,filename);
  pl = PLGetProplistWithPath(filename);
  FREE(filename);
  /* PLGetProplistWithPath returns a new object - don't inc refcount */
  return proplist_to_scm(pl,0);
}
#undef FUNC_NAME

/* ========================================================================== */
/* proplist_t PLGetProplistWithDescription(const char *description); */
SCWM_PROC(get_proplist_with_description,"get-proplist-with-description",1,0,0,
	  (SCM Desc))
     /**  */
#define FUNC_NAME s_get_proplist_with_description
{
  char *desc;
  proplist_t pl;

  VALIDATE_ARG_STR_NEWCOPY(1,Desc,desc);
  pl=PLGetProplistWithDescription(desc);
  FREE(desc);
  /* PLGetProplistWithDescription returns a new object - don't inc refcount */
  return proplist_to_scm(pl,0);
}
#undef FUNC_NAME

/* ========================================================================== */
MAKE_SMOBFUNS(proplist_t);

static
void
init_proplist_wrapper() {
  REGISTER_SCWMSMOBFUNS(proplist_t);
#ifndef SCM_MAGIC_SNARFER
#include "scwmproplist.x"
#endif
}


/* ========================================================================== */
void scm_init_app_scwm_scwmproplist_module() {
  scm_register_module_xxx("app scwm scwmproplist", init_proplist_wrapper);
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
