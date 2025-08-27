
/*************  L O K A L E    ***************/


/**
struct LocaleInfo {
    struct Library * li_LocaleBase;
    struct Catalog * li_Catalog;
};
**/


/**
VOID CloseCatalog(struct Catalog *);
VOID CloseLocale(struct Locale *);
ULONG ConvToLower(struct Locale *,ULONG);
ULONG ConvToUpper(struct Locale *,ULONG);
VOID FormatDate(struct Locale *,STRPTR,struct DateStamp *, struct Hook *);
APTR FormatString(struct LocaleBase *,STRPTR,APTR,struct Hook *);
STRPTR GetCatalogStr(struct Catalog *,LONG,STRPTR);
STRPTR GetLocaleStr(struct Locale *,ULONG);
struct Catalog *OpenCatalogA(struct Locale *,STRPTR,struct TagItem *);
struct Catalog *OpenCatalog(struct Locale *,STRPTR,Tag, ...);
struct Locale *OpenLocale(STRPTR);
BOOL ParseDate(struct Locale *,struct DateStamp *,STRPTR,struct Hook *);
ULONG StrConvert(struct Locale *,STRPTR,APTR,ULONG,ULONG);
LONG StrnCmp(struct Locale *,STRPTR,STRPTR,LONG,ULONG);
**/


#define LocaleBase li.li_LocaleBase
#define catalog    li.li_Catalog
