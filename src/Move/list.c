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


int     l_add(sp) /*=====================================================*/
char    *sp;
{
BLOCK   *new;
int     len;

    if (!curblock) {
        head = AllocMem(sizeof(BLOCK),MEMF_CLEAR);
        if (!head) return(0);

        curblock = head;
    }

    len = strlen(sp) + 1;
    if (len > BLOCKSIZE) return(0);
    if (curblock->used + len > BLOCKSIZE) {
        new = AllocMem(sizeof(BLOCK),MEMF_CLEAR);
        if (!new) return(0);

        curblock->next = new;
        curblock = new;
    }

    memcpy(curblock->dat + curblock->used,sp,len);
    curblock->used += len;

    lcount++;
    return(1);
}

void    l_close() /*=====================================================*/
{
BLOCK   *next;

    curblock = head;
    head = NULL;
    while (curblock) {
        next = curblock->next;
        FreeMem(curblock,sizeof(BLOCK));
        curblock = next;
    }
}

char    *l_first() /*====================================================*/
{
    getblock = head;
    getoff = 0;

    if (!getblock || !getblock->used) return(NULL);
    return(getblock->dat);
}

char    *l_next() /*=====================================================*/
{
char    *cp;

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

int     l_num() /*=======================================================*/
{
    return(lcount);
}
