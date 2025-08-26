/*************************************************************************
 ***                              list.c                  John Bickers ***
 *** Date begun: 14 Jun 1992.                                          ***
 *** Last modified: 15 Jun 1992.                                       ***
 *************************************************************************/
/* Module to handle a list of strings. We allocate 10KB blocks and use   *
 * up the space as we go. No deletion, sorting, or crap like that. :)    *
 *************************************************************************/

#include <exec/types.h>
#include <exec/memory.h>

#include <proto/exec.h>

#include <string.h>
#include "funcs.h"

#define BLOCKSIZE   10240

typedef struct BLOCK {
    struct BLOCK *next;
    int     used;
    char    dat[BLOCKSIZE];
} BLOCK;

static BLOCK *head;
static BLOCK *curblock;
static BLOCK *getblock;
static int  getoff,lcount;
static int  initialized = 0;  /* Track if list has been initialized */

/* Internal cleanup function - not exposed externally */
static void internal_cleanup(void)
{
BLOCK   *next;

    curblock = head;
    head = NULL;
    initialized = 0;  /* Mark as not initialized */
    
    while (curblock) {
        next = curblock->next;
        FreeMem(curblock,sizeof(BLOCK));
        curblock = next;
    }
    
    /* Reset all static variables */
    getblock = NULL;
    getoff = 0;
    lcount = 0;
}

int     l_add(char *sp) /*=====================================================*/
{
BLOCK   *new;
int     len;

    if (!sp) return 0;  /* NULL pointer check */

    if (!curblock) {
        head = AllocMem(sizeof(BLOCK),MEMF_CLEAR);
        if (!head) return(0);

        curblock = head;
        initialized = 1;  /* Mark as initialized */
    }

    len = strlen(sp) + 1;
    if (len > BLOCKSIZE) return(0);
    if (curblock->used + len > BLOCKSIZE) {
        new = AllocMem(sizeof(BLOCK),MEMF_CLEAR);
        if (!new) {
            /* Memory allocation failed - clean up and return error */
            internal_cleanup();  /* Use internal cleanup, not recursive l_close */
            return(0);
        }

        curblock->next = new;
        curblock = new;
    }

    memcpy(curblock->dat + curblock->used,sp,len);
    curblock->used += len;

    lcount++;
    return(1);
}

void    l_close(void) /*=====================================================*/
{
    /* Protect against multiple calls */
    if (!initialized) return;

    internal_cleanup();  /* Use internal cleanup function */
}

char    *l_first(void) /*====================================================*/
{
    if (!initialized) return NULL;  /* Check if initialized */
    
    getblock = head;
    getoff = 0;

    if (!getblock || !getblock->used) return(NULL);
    return(getblock->dat);
}

char    *l_next(void) /*====================================================*/
{
char    *cp;

    if (!initialized) return NULL;  /* Check if initialized */
    if (!getblock || !getblock->used) return(NULL);
    
    for (cp = getblock->dat + getoff; *cp; cp++) ;
    cp++;
    getoff = cp - getblock->dat;
    if (getoff >= getblock->used) {
        getblock = getblock->next;
        getoff = 0;
        if (!getblock || !getblock->used) return(NULL);
        cp = getblock->dat;
    }
    return(cp);
}

int     l_num(void) /*=======================================================*/
{
    if (!initialized) return 0;  /* Check if initialized */
    return(lcount);
}
