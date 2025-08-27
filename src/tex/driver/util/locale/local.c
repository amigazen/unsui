
/*************  L O K A L E    ***************/

#include <stdio.h>

#include <exec/types.h>
#include <exec/libraries.h>
#include <libraries/locale.h>
#include <intuition/intuition.h>

#include <clib/exec_protos.h>
#include <clib/dos_protos.h>
#include <clib/locale_protos.h>
#include <clib/intuition_protos.h>

#include <pragmas/exec_pragmas.h>
#include <pragmas/dos_pragmas.h>
#include <pragmas/locale_pragmas.h>
#include <pragmas/intuition_pragmas.h>

#include "defines.h"
#include "globals.h"
#include "amscreen.h"

#include "globals.i"
#include "amscreen.i"

#include "local.h"
#include "local.i"

#define STRINGARRAY
#include "localstr.h"



static struct LocaleInfo   li = { NULL, NULL };
static struct Locale     * locale = NULL;

static struct EasyStruct WrongCat = {
    sizeof (struct EasyStruct),
    0,
    "Catalog Problem",
    NULL,
    "OK",
};


void   _STIInitTeXLocale(void)		// auto-init function
{
  if (li.li_LocaleBase = OpenLibrary("locale.library",38)) {
    catalog = OpenCatalog(NULL, "PasTeX.catalog", NULL, OC_BuiltInLanguage,"english", TAG_DONE);
    locale  = OpenLocale(NULL);
    
    if (catalog) {
      char * intern_version, * extern_version;
      
      UWORD  i, last;

      intern_version = "\0";
      extern_version = " \0";

      last = sizeof(CatCompArray) / sizeof(CatCompArray[0]);

      for (i = 0; i < last; i++) {
        if(CatCompArray[i].cca_ID == MSG_CATALOG_VERSION)
        {
            intern_version = CatCompArray[i].cca_Str;
            break;
        }
      }

      if (li.li_LocaleBase)
        extern_version = GetCatalogStr(li.li_Catalog, MSG_CATALOG_VERSION, extern_version);
        
      if (!intern_version || !extern_version || strcmp(intern_version, extern_version)) {
        // die beiden Cataloge sind unterschiedlicher Version
        
        WrongCat.es_TextFormat = GetTeXString(MSG_WRONG_CATALOG_VERSION);
        
        IntuitionBase = (struct IntuitionBase *) OpenLibrary("intuition.library", 37);	// ist hier noch nicht geoeffnet
        if (IntuitionBase) {
          EasyRequest(NULL, &WrongCat, NULL, 0);
          CloseLibrary((struct Library *)IntuitionBase);	// naja, ich setze IntuitionBase nicht auf NULL zurueck...
        }
        
        CloseCatalog(catalog);
        catalog = NULL;
      }
      
    }
  }
}


void   _STDCloseTeXLocale(void)		// auto-termination function
{
  if (li.li_LocaleBase) {
    if (catalog) CloseCatalog(catalog);
    catalog = NULL;
    if (locale) CloseLocale(locale);
    locale = NULL;
    CloseLibrary(li.li_LocaleBase);
    li.li_LocaleBase = NULL;
  }
}



char * GetLocString(int stringNum, char * defstr)
{
  if (li.li_LocaleBase && locale) return GetLocaleStr(locale, stringNum);
  else return defstr;
}


#if defined(CATCOMP_BLOCK)

STRPTR GetTeXString(LONG stringNum)
{
    LONG   *l;
    UWORD  *w;
    STRPTR  builtIn;

    l = (LONG *)CatCompBlock;

    while (*l != stringNum)
    {
        w = (UWORD *)((ULONG)l + 4);
        l = (LONG *)((ULONG)l + (ULONG)*w + 6);
    }
    builtIn = (STRPTR)((ULONG)l + 6);

    if (li.li_LocaleBase) return(GetCatalogStr(li.li_Catalog,stringNum,builtIn));

    return(builtIn);
}

#endif	// CATCOMP_BLOCK


#if defined(CATCOMP_ARRAY)

STRPTR GetTeXString (LONG stringNum)
{
    STRPTR local = "### Locale String not found! ###";
    UWORD  i, last;

    last = sizeof(CatCompArray) / sizeof(CatCompArray[0]);

    for (i = 0; i < last; i++)
    {
        if(CatCompArray[i].cca_ID == stringNum)
        {
            local = CatCompArray[i].cca_Str;
            break;
        }
    }

    if (li.li_LocaleBase && catalog && *local)
        local = GetCatalogStr(li.li_Catalog, stringNum, local);

    return(local);
}

#endif

