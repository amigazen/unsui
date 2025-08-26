/*************************************************************************
 ***                              funcs.h                  John Bickers ***
 *** Date begun: 13 Jun 1992.                                          ***
 *** Last modified: 26 Jun 1992.                                       ***
 *************************************************************************/
/* Function prototypes for move command                                   *
 *************************************************************************/

#ifndef FUNCS_H
#define FUNCS_H

/* Function prototypes from move.c */

/* Print formatted output to console */
void mprintf(char *cp, ...);

/* Get volume information for a path */
LONG getvol(char *name);

/* Extract base path from source */
void makebase(char *from);

/* Ensure directory path exists, creating if necessary */
void checkpath(char *name);

/* Construct destination name from source */
void maketoname(char *from);

/* Process FROM argument (pattern or direct name) */
void addfrom(char *from);

/* Add non-pattern name to list */
void addit(char *from);

/* Remove trailing slash from path */
char *noslash(char *sp);

/* Add trailing slash to path if needed */
char *addslash(char *sp);

/* Recursively add subdirectory contents */
void addsub(char *from);

/* Add pattern-based names to list */
void addpat(char *from);

/* Process the move list */
void movelist(void);

/* Move a single file */
void moveit(char *basep, char *dirp, char *nodep);

/* Force creation of directory */
void forcedir(char *basep, char *nodep);

/* Rename file on same volume */
void renameit(char *fname, char *tname);

/* Copy file to different volume */
void copyit(char *fname, char *tname);

/* Main entry point */
void _main(void);

/* Handle Ctrl-C */
int CXBRK(void);

/* Cleanup and exit */
void cleanup(char *cp, ...);

/* Function prototypes from list.c */

/* Add string to list */
int l_add(char *sp);

/* Free all list memory */
void l_close(void);

/* Get first string from list */
char *l_first(void);

/* Get next string from list */
char *l_next(void);

/* Get number of items in list */
int l_num(void);

#endif /* FUNCS_H */
