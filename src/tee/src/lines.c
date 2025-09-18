/* lines - extract lines from a text file (head & tail in Unix)
 *
 * Copyright (C) 1995 by Ingo Wilken (Ingo.Wilken@informatik.uni-oldenburg.de)
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.  This software is provided "as is" without express or
 * implied warranty.
 *
 * V1.0: 16/Feb/95  first version
 * V1.1: 19/Feb/95  minor bugfix
 * V2.0: 06/Aug/95  major write speed-up
 */
#define THIS_PROGRAM    "lines"
#define THIS_VERSION    "1.2"

#include <exec/types.h>
#include <exec/libraries.h>
#include <exec/memory.h>
#include <dos/dos.h>
#include <dos/rdargs.h>
#include <dos/dosasl.h>
/*#include <clib/alib_protos.h>*/
#include <stdlib.h>
#include <string.h>

extern __stkargs void * LibCreatePool(ULONG, ULONG, ULONG);
extern __stkargs void   LibDeletePool(void *);
extern __stkargs void * LibAllocPooled(void *, ULONG);
extern __stkargs void   LibFreePooled(void *, void *, ULONG);
extern void NewList(struct List *);

#define SysBase_DECLARED
#include <proto/exec.h>
#include <proto/dos.h>

extern struct Library *SysBase;


#define BUFFER_BLOCK_SIZE   2*1024
#define POOLSIZE            32*1024
#define BREAKSIGS           SIGBREAKF_CTRL_C

static const char amiga_version[] = "\0$VER: " THIS_PROGRAM " " THIS_VERSION " (" __COMMODORE_DATE__ ")";

const char Template[] = "FIRST/A/N,LAST/A/N,FROM,TO";
__aligned struct {
    LONG    *first;
    LONG    *last;
    STRPTR  from;
    STRPTR  to;
} Args;

typedef struct {
    enum {
        FINISH = 0,
        SKIPUNTIL,      /* skip lines until line # <num> */
        COPYTO,         /* copy lines until (including) line # <num> */
        FIFOCOPY,       /* store upto <num> lines in FIFO, copy excess lines to output */
        FIFOSKIP,       /* store upto <num> lines in FIFO, throw excess lines away */
    } state;
    LONG num;
} State;


typedef struct {
    LONG            refs;   /* how many LineSegments point to this block */
    LONG            off;    /* offset of next unused byte in data */
    LONG            len;    /* number of valid bytes in data */
    UBYTE           data[BUFFER_BLOCK_SIZE];
} Block;

typedef struct {
    struct MinNode  node;
    Block *         block;  /* block data belongs to */
    UBYTE *         data;   /* points into 'data' part of block */
    LONG            len;    /* length of segment */
} LineSegment;

typedef struct {
    Block *         block;  /* block data belongs to */
    UBYTE *         data;   /* points into 'data' part of block */
    LONG            len;    /* length of segment */
} BlockSegment;

typedef struct {
    struct MinNode  node;
    struct MinList  segs;   /* list of LineSegments */
} Line;

static LONG err = 0;        /* error code */
static BOOL eof = FALSE;    /* end of file in input */
static int lastout = -1;    /* last character written to output */


/* prototypes */
static void     FreeBlock(Block *);     /* decrement reference count, free if 0 */
static Block *  GetBlock(BPTR);         /* get current block (reads from file) */
static void     FreeLine(Line *);       /* free a line & all segments */
static Line *   ReadLine(BPTR);         /* read a line */
static void     WriteLine(Line *, BPTR);/* write a line */
static void     WriteBlock(BlockSegment *, BPTR);
static void     ReduceFifo(LONG, BPTR); /* reduce FIFO to <num> lines, write excess to fh (unless fh = 0) */
static void     StoreLine(Line *);      /* store line in FIFO */
static void *   palloc(ULONG);
static void     pfree(void *);


static APTR mempool;

static void *
palloc(bytes)
    ULONG bytes;
{
    ULONG *p;
    if( p = (ULONG *)LibAllocPooled(mempool, bytes+sizeof(ULONG)) )
        *p++ = bytes;
    return (void *)p;
}


static void
pfree(p)
    void *p;
{
    p = (ULONG *)p-1;
    LibFreePooled(mempool, p, *(ULONG *)p);
}


static void
FreeBlock(block)
    Block *block;
{
    if( --block->refs == 0 )
        pfree(block);
}


static Block *
GetBlock(fh)
    BPTR fh;
{
    static Block *block = NULL;
    LONG r;

    if( !block )
        block = (Block *)palloc(sizeof(Block));
    else
    if( block->off >= block->len ) {
        /* all data of block has been used, free & get new */
        FreeBlock(block);
        block = (Block *)palloc(sizeof(Block));
    }
    else
        return block;   /* block has unused data */

    if( block ) {
        r = Read(fh, block->data, BUFFER_BLOCK_SIZE);
        if( r > 0 ) {
            block->len = r;
            block->off = 0;
            block->refs = 1;        /* static block ptr */
        }
        else {
            if( r == 0 )
                eof = TRUE;
            else
                err = IoErr();
            pfree(block);
            block = NULL;
        }
    }
    else
        err = ERROR_NO_FREE_STORE;
    return block;
}


static void
FreeLine(line)
    Line *line;
{
    LineSegment *seg;

    if( line ) {
        while( seg = (LineSegment *)RemHead((struct List *)&(line->segs)) ) {
            FreeBlock(seg->block);
            pfree(seg);
        }
        pfree(line);
    }
}


static Line *
ReadLine(fh)
    BPTR fh;
{
    Block *block;
    LineSegment *seg;
    Line *line;
    LONG r, total;
    BOOL found = FALSE;

    if( line = (Line *)palloc(sizeof(Line)) ) {
        NewList((struct List *)&(line->segs));
        total = 0;
        while( !found && !err && !eof ) {
            if( block = GetBlock(fh) ) {
                if( seg = (LineSegment *)palloc(sizeof(LineSegment)) ) {
                    AddTail((struct List *)&(line->segs), (struct Node *)seg);
                    seg->block = block;
                    block->refs++;
                    seg->data = &(block->data[block->off]);

                    /* find newline */
                    for( r = block->off; !found && r < block->len; r++ ) {
                        if( block->data[r] == '\n' )
                            found = TRUE;
                    }
                    seg->len = r - block->off;
                    total += seg->len;
                    block->off = r;
                }
                else
                    err = ERROR_NO_FREE_STORE;
            }
        }

        if( total == 0 ) {
            FreeLine(line);
            line = NULL;
        }
    }
    else
        err = ERROR_NO_FREE_STORE;

    return line;
}


static void
WriteBlock(seg, fh)
    BlockSegment *seg;
    BPTR fh;
{
    LONG w;

    if( seg->block ) {
        w = Write(fh, seg->data, seg->len);
        if( w > 0 )
            lastout = seg->data[w-1];
        if( w != seg->len )
            err = IoErr();
        FreeBlock(seg->block);
        seg->block = NULL;
        seg->data = NULL;
        seg->len = 0;
    }
}


static void
WriteLine(line, fh)
    Line *line;
    BPTR fh;
{
    static BlockSegment bseg;
    LineSegment *seg;

    if( line ) {
        for( seg = (LineSegment *)(line->segs.mlh_Head);
             !err && seg->node.mln_Succ;
             seg = (LineSegment *)(seg->node.mln_Succ) ) {

            if( seg->block != bseg.block ) {
                WriteBlock(&bseg, fh);
                bseg.data  = seg->data;
                bseg.block = seg->block;
                seg->block->refs++;
            }
            bseg.len += seg->len;
        }
    }
    else
        WriteBlock(&bseg, fh);
}


static struct MinList fifo;
static LONG fifosize = 0;       /* number of entries in FIFO */

static void
ReduceFifo(num, fh)
    LONG num;
    BPTR fh;
{
    Line *line;

    while( !err && fifosize > num ) {
        line = (Line *)RemHead((struct List *)&fifo);
        --fifosize;
        if( fh )
            WriteLine(line, fh);
        FreeLine(line);
    }
}


static void
StoreLine(line)
    Line *line;
{
    if( line ) {
        AddTail((struct List *)&fifo, (struct Node *)line);
        ++fifosize;
    }
}



void
_main()
{
    struct RDArgs *rdargs;
    BPTR infh, outfh;
    BOOL closeinfh, closeoutfh;
    LONG nextline;
    State statelist[3], *state = &statelist[0];

    if( SysBase->lib_Version < 37 ) {
        #define MESSAGE  THIS_PROGRAM " requires AmigaOS 2.04 or higher\n"
        Write(Output(), MESSAGE, sizeof(MESSAGE)-1);
        _exit(RETURN_FAIL);
        #undef MESSAGE
    }

    if( mempool = LibCreatePool(MEMF_ANY, POOLSIZE, POOLSIZE) ) {
        if( rdargs = ReadArgs(Template, (LONG *)&Args, NULL) ) {

            NewList((struct List *)&fifo);

            if( Args.from ) {
                infh = Open(Args.from, MODE_OLDFILE);
                closeinfh = TRUE;
            }
            else {
                infh = Input();
                closeinfh = FALSE;
            }
            if( Args.to ) {
                outfh = Open(Args.to, MODE_NEWFILE);
                closeoutfh = TRUE;
            }
            else {
                outfh = Output();
                closeoutfh = FALSE;
            }
            if( !infh || !outfh ) {
                err = IoErr();
                goto fail;
            }

            statelist[2].state = FINISH;
            if( *Args.first > 0 ) {
                statelist[0].state = SKIPUNTIL; statelist[0].num = *Args.first;
                if( *Args.last < 0 ) {
                    statelist[1].state = FIFOCOPY; statelist[1].num = -(*Args.last+1);
                }
                else
                if( *Args.last >= *Args.first ) {
                    statelist[1].state = COPYTO; statelist[1].num = *Args.last;
                }
                else
                    err = ERROR_BAD_NUMBER;
            }
            else
            if( *Args.first < 0 ) {
                statelist[0].state = FIFOSKIP; statelist[0].num = -(*Args.first);
                if( *Args.last >= 0 )
                    err = ERROR_BAD_NUMBER;
                else
                if( *Args.last >= *Args.first ) {
                    statelist[1].state = FIFOCOPY; statelist[1].num = -(*Args.last+1);
                }
                else
                    err = ERROR_BAD_NUMBER;
            }
            else    /* *Args.first == 0 */
                err = ERROR_BAD_NUMBER;

            if( err )
                goto fail;

            nextline = 1;

            while( !err && state->state != FINISH ) {
                Line *line;

                if( CheckSignal(BREAKSIGS) != 0 )
                    err = ERROR_BREAK;
                else
                switch( state->state ) {
                    case SKIPUNTIL:
                        if( eof || (nextline >= state->num) )
                            ++state;
                        else {
                            line = ReadLine(infh); ++nextline;
                            FreeLine(line);
                        }
                    break;
                    case COPYTO:
                        if( eof || (nextline > state->num) )
                            ++state;
                        else {
                            line = ReadLine(infh); ++nextline;
                            WriteLine(line, outfh);
                            FreeLine(line);
                        }
                    break;
                    case FIFOCOPY:
                        ReduceFifo(state->num, outfh);
                        if( eof )
                            ++state;
                        else {
                            line =  ReadLine(infh); ++nextline;
                            StoreLine(line);
                        }
                    break;
                    case FIFOSKIP:
                        if( eof )
                            ++state;
                        else {
                            line = ReadLine(infh); ++nextline;
                            StoreLine(line);
                            ReduceFifo(state->num, (BPTR)0);
                        }
                    break;
                }
            }

            WriteLine(NULL, outfh);     /* flush pending writes */
            /* ReduceFifo(0, (BPTR)0); */

        fail:
            if( infh && closeinfh )
                Close(infh);
            if( outfh ) {
                if( IsInteractive(outfh) ) {
                    if( lastout > 0  && lastout != '\n' )
                        FPutC(outfh, '\n');
                }
                if( closeoutfh )
                    Close(outfh);
            }

            FreeArgs(rdargs);
        }
        else    /* error in arguments */
            err = IoErr();

        LibDeletePool(mempool);
    }
    else    /* could not create mempool */
        err = ERROR_NO_FREE_STORE;

    if( err ) {
        char smallbuf[256];

        /* can't use PrintFault() because it prints to stdout */
        if( Fault(err, THIS_PROGRAM, smallbuf, 256) ) {
            if( outfh = Open("CONSOLE:", MODE_READWRITE) ) {
                FPuts(outfh, smallbuf);
                FPutC(outfh, '\n');
                Close(outfh);
            }
        }
        SetIoErr(err);  /* restore for 'why' command */
    }
    _exit(err ? RETURN_ERROR : RETURN_OK);
}

