@x
#ifdef AMIGA
#include "amigapaths.h"
#endif
@y
#ifdef AMIGA
#include "amigapaths.h"
#include "evpaths.h"
#include <signal.h>
void Free_EnvVars(void);
void amiga_safexit(int);
#endif
@z

@x
char *configpath = CONFIGPATH ; /* where to find config files */
char *infont ;                /* is the file we are downloading a font? */
@y
char *configpath = CONFIGPATH ; /* where to find config files */
char *infont ;                /* is the file we are downloading a font? */

struct EnvVarPath 	*tfm_var    = NULL,	/* for tfm files */
			*vf_var     = NULL,	/* for vf files */
			*fig_var    = NULL,	/* for figure files */
			*pict_var   = NULL,	/* for IFF/etc. files */
			*header_var = NULL,	/* for header files */
			*config_var = NULL;	/* for config files */

#ifdef FONTLIB
struct EnvVarPath	*fli_var    = NULL;	/* for font libraries */
#endif

@z

@x
   if (*s=='!') {
      if (bitfile != NULL) {
         cleanprinter() ;
      }
@y
   if (*s=='!') {
      if (bitfile != NULL) {
         cleanprinter() ;
      }
      Free_EnvVars();
@z

@x
   initialize() ;
   checkenv(0) ;
@y
   config_var = Alloc_EnvVarPath("TEXCONFIG"   , 4096L);
   tfm_var    = Alloc_EnvVarPath("TEXFONTS"    , 4096L);
   vf_var     = Alloc_EnvVarPath("VFFONTS"     , 4096L);
   pict_var   = Alloc_EnvVarPath("TEXPICTS"    , 4096L);
   fig_var    = Alloc_EnvVarPath("TEXINPUTS"   , 4096L);
   header_var = Alloc_EnvVarPath("DVIPSHEADERS", 4096L);
#ifdef FONTLIB
   fli_var    = Alloc_EnvVarPath("DVIPSFLIB", 2048L);
#endif

   if (signal(SIGINT, &amiga_safexit) == SIG_ERR)
      exit(EXIT_FAILURE);

   initialize() ;
   checkenv(0) ;
@z

@x
   if (dd(D_PATHS)) {
#ifdef SHORTINT
        (void)fprintf(stderr,"input file %s output file %s swmem %ld\n",
#else /* ~SHORTINT */
           (void)fprintf(stderr,"input file %s output file %s swmem %d\n",
#endif /* ~SHORTINT */
           iname, oname, swmem) ;
   (void)fprintf(stderr,"tfm path %s\npk path %s\n", tfmpath, pkpath) ;
   (void)fprintf(stderr,"fig path %s\nvf path %s\n", figpath, vfpath) ;
   (void)fprintf(stderr,"config path %s\nheader path %s\n",
                  configpath, headerpath) ;
#ifdef AMIGA
   (void)fprintf(stderr,"pict path %s\n", pictpath);
#endif

#ifdef FONTLIB
   (void)fprintf(stderr,"fli path %s\nfli names %s\n", flipath, fliname) ;
#endif
@y
   if (dd(D_PATHS)) {
	int i;

        (void)fprintf(stderr,"input file %s output file %s swmem %d\n", iname, oname, swmem);

        i = 0;
        (void)fprintf(stderr,"tfm path ");
	while (tfm_var->storage.strings[i]) {
           fprintf(stderr,"%s%s", (i==0?"":","),(tfm_var->storage.strings[i][0] == '\0' ? (unsigned char *)"." : tfm_var->storage.strings[i]));
           i++;
        }

        (void)fprintf(stderr,"\npk path %s", pkpath);

        i = 0;
        (void)fprintf(stderr,"\nfig path ");
        while (fig_var->storage.strings[i]) {
           fprintf(stderr,"%s%s", (i==0?"":","),(fig_var->storage.strings[i][0] == '\0' ? (unsigned char *)"." : fig_var->storage.strings[i]));
           i++;
        }

        i = 0;
        (void)fprintf(stderr,"\nvf path ");
        while (vf_var->storage.strings[i]) {
           fprintf(stderr,"%s%s", (i==0?"":","),(vf_var->storage.strings[i][0] == '\0' ? (unsigned char *)"." : vf_var->storage.strings[i]));
           i++;
        }

        i = 0;
        (void)fprintf(stderr,"\nconfig path ");
        while (config_var->storage.strings[i]) {
           fprintf(stderr,"%s%s", (i==0?"":","),(config_var->storage.strings[i][0] == '\0' ? (unsigned char *)"." : config_var->storage.strings[i]));
           i++;
        }

        i = 0;
        (void)fprintf(stderr,"\nheader path ");
        while (header_var->storage.strings[i]) {
           fprintf(stderr,"%s%s", (i==0?"":","),(header_var->storage.strings[i][0] == '\0' ? (unsigned char *)"." : header_var->storage.strings[i]));
           i++;
        }

        i = 0;
        (void)fprintf(stderr,"\npict path ");
        while (pict_var->storage.strings[i]) {
           fprintf(stderr,"%s%s", (i==0?"":","),(pict_var->storage.strings[i][0] == '\0' ? (unsigned char *)"." : pict_var->storage.strings[i]));
           i++;
        }

#ifdef FONTLIB
        i = 0;
        (void)fprintf(stderr,"\nfli path ");
        while (fli_var->storage.strings[i]) {
           fprintf(stderr,"%s%s", (i==0?"":","),(fli_var->storage.strings[i][0] == '\0' ? (unsigned char *)"." : fli_var->storage.strings[i]));
           i++;
        }

        (void)fprintf(stderr,"\nfli names %s", fliname);
#endif
        (void)fprintf(stderr,"\n");
@z

@x
      dvifile = stdin;
   else {
      help() ;
      exit(0) ;
@y
      dvifile = stdin;
   else {
      help() ;
      Free_EnvVars();
      exit(0) ;
@z

@x
   exit(0) ;
   /*NOTREACHED*/
@y
   Free_EnvVars();
   exit(0) ;
   /*NOTREACHED*/
@z

@x
#ifdef MVSXA  /* IBM: MVS/XA */
#include "dvipsmvs.h"
#endif
@y

void Free_EnvVars(void)
{
	Free_EnvVarPath(config_var);
	Free_EnvVarPath(tfm_var);
	Free_EnvVarPath(vf_var);
	Free_EnvVarPath(pict_var);
	Free_EnvVarPath(fig_var);
	Free_EnvVarPath(header_var);
#ifdef FONTLIB
	Free_EnvVarPath(fli_var);
#endif
}

void amiga_safexit(int dummy)
{
	extern char *_ProgramName;

	Free_EnvVars();
	fprintf(stderr,"*** Break: %s\n",_ProgramName);
	fflush(stderr);
	exit(EXIT_FAILURE);
}
@z
