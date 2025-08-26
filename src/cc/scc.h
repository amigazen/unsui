/*
   scc.h

   bf 11-22-96
*/

/* Configuration */
#define SUPPORT_ADE_ENVIRONMENT
#undef PPONLY_WITH_CPP

#if defined (__GNUC__)
extern char *stpcpy (char *str, char *add);
#endif

typedef struct Node Node;
typedef struct List List;

#define LBUFSIZE   1024
#define MAXPATHLEN 1024

extern struct Process *MyProc;
