#ifndef POSTLIB_H
#define POSTLIB_H
/*------------------------------------------------------------------------*/
/*                                                                        *
 *  $Id: postlib.h,v 2.17 1994/09/09 16:42:31 heinz Exp $
 *                                                                        */
/*------------------------------------------------------------------------*/

/*------------------------------------------------------------------------*/

/* PostScript interpreter file "postlib.h" - library interface header (Amiga)
 *
 * Based on postlib.h 1.7, (C) Adrian Aylward 1989, 1991.
 *
 * This file defines the library interface, so that other programs can
 * use the PostScript drawing machinery.  It is totally Amiga specific.
 *
 * All versions >=2.0 ©1993,1994 HWG, For Joan Thuesen.
 *
 * HWGPOST leans heavily towards PostScript Level 2
 *
 * All rights reserved. Commercial use of HWGPOST only with my permission!
 * Read the docs for further information on legal issues.
 * Don't use HWGPOST without reading up on the legal issues!
 *
 *
 *  Heinz Wrobel
 *  Karlstr. 16
 *  82131 Gauting
 *  Germany
 *  FAX +49 89 850 51 25 (I don't like phone calls)
 *  <heinz@hwg.muc.de>
 *
 */


#include <exec/types.h>
#include <dos/dos.h>

/*------------------------------------------------------------------------*/
/* Default and minimum memory sizes */

#define defmemflen  60000       /* Font cache */
#define defmemhlen  20000       /* Halftone screen cache */
#define defmemvlen  50000       /* VM segment length */
#define defmemllen  15000       /* Path element memory */
#define defmemplen  60000       /* Pattern cache */

#define minmemflen   5000
#define minmemhlen   1000
#define minmemvlen   5000
#define minmemllen   1000
#define minmemplen   5000

/*------------------------------------------------------------------------*/
/* The device page and parameter block */

/*
 **************************************************************************
 *** IMPORTANT NOTE: For anything below, the General Amiga Development  ***
 ***                 Guidelines as described in the Addison Wesley "ROM ***
 ***                 Kernel Reference Manual Libraries" are valid.      ***
 ***                                                                    ***
 ***                 Note especially that "Fields that are not defined  ***
 ***                 to contain particular initial values must be       ***
 ***                 initialized to zero. This includes pointer         ***
 ***                 fields." and "All reserved or unused fields must   ***
 ***                 be initialized to zero for future compatibility."  ***
 **************************************************************************
*/

/* A possible extension to the device structure described below */
struct PSextdevice
{
    unsigned long       flags;

    unsigned char       *maskbuf;

    unsigned char       *Kbuf[8];

    unsigned char       *alphabuf[8];

    long                reserved[16];
    /* The length of this structure is subject to change. */
};

/* The standard device structure */
struct PSdevice
{
    /*---------------------------------------------------------------------
       "buf"            These are 24 bitplane pointers which can be set or
                        cleared. HWGPOST will just ignore NULL pointers to
                        make masking possible.

       "len"            This value gives the total length in bytes of the
                        memory for one bitplane.

       "depth"          How many bitplanes should be used at all.

       "flags"          Various. See below.

       "bitspercolor"   To specify how many bitplanes are used per color,
                        you set "bitspercolor" to the appropriate value. If
                        this value is not set, one bitplane per color is
                        assumed.

                        Example: RGB 3:3:2 => depth=8, bitspercolor=3

       "PSextdevice"    Set it for the special features or leave it NULL.

       "xoff", "yoff"   To specify rendering offsets into the bitplanes.

       "xbytes"         The size of one row in a bitplane in bytes.

       "xsize", "ysize" The size of the bitplane in pixels.

       "ybase"          The first row to draw into for band rendering.

       "yheight"        The number of rows to use for band rendering.

       "xden", "yden"   The dpi values to use.

       "ydir"           Direction of the y axis for rendering.


       Fields in the extended device structure:

       "flags"          See below.

       "maskbuf"        If set, must be a plane as in "buf". For any
                        rendering you'll get bits set in this mask plane.
                        This gives you a mask of the actually used places
                        in the buffer. The mask might be useful for
                        BltMaskBitMapRastPort() or some such. erasepage
                        will clear this mask plane.

        "Kbuf"          The missing eight plane pointers to be able to do
                        CMYK rendering with 8 bits per color.

        "alphabuf"      Plane pointers for an alpha channel. Do not use
                        or set until further notice.

      ---------------------------------------------------------------------
    */

    unsigned char       *buf[24];
    int                 len;
    short               depth;
    unsigned char       flags;
    unsigned char       bitspercolor;
    struct PSextdevice  *PSextdevice;
    short               xoff, yoff;
    short               xbytes, xsize, ysize;
    short               ybase, yheight;
    short               xden, yden;
    short               ydir;
};

/* Definitions for the flags field in the standard device structure */
#define HWGPOST_DEVB_NOSHADE           0    /* Suppress any halftoning */
#define HWGPOST_DEVF_NOSHADE        0x01
#define HWGPOST_DEVB_CMYK              1    /* CMYK instead of RGBW */
#define HWGPOST_DEVF_CMYK           0x02
#define HWGPOST_DEVB_INVERTOUTPUT      2    /* Inverted rendering */
#define HWGPOST_DEVF_INVERTOUTPUT   0x04

/* Flags defined for the extended device structure */
#define HWGPOST_EXTDEVB_USEALPHACHANNEL     0   /* Not implemented */
#define HWGPOST_EXTDEVF_USEALPHACHANNEL  0x01

/*------------------------------------------------------------------------*/
/* The parameter block */

struct PSparm
{
    /*---------------------------------------------------------------------
        "page"          The page description to set on startup.

        "memvlen",      Memory size in bytes to use for VM segments.
        "memflen",                                      the font cache.
        "memllen",                                      path elements.
        "memhlen"                                       halftone screens.

        "userdata"      Data passed to the "flushfunc" and "copyfunc".

        "flushfunc",    Callback functions to implement page flushing and
        "copyfunc"      copying to the display.

        "infh",         AmigaDOS (BPTR) handles to implement %stdin,
        "outfh",        %stdout and %stderr.
        "errfh"

        "funcmax"       Number of external functions.

        "functab"       Pointer to external function table, or zero.

        "reserved"      DO NOT TOUCH AND SET TO ZERO!

      ---------------------------------------------------------------------
    */
    struct PSdevice page;
    int             memvlen, memflen, memllen, memhlen;
    APTR            userdata, flushfunc, copyfunc;
    BPTR            infh, outfh, errfh;
    int             funcmax;
    APTR            *functab;
    long            reserved[2];
};

/*------------------------------------------------------------------------*/
/* Errors */

#define errdictfull             1
#define errdictstackoverflow    2
#define errdictstackunderflow   3
#define errexecstackoverflow    4
#define errinterrupt            5
#define errinvalidaccess        6
#define errinvalidexit          7
#define errinvalidfileaccess    8
#define errinvalidfont          9
#define errinvalidrestore      10
#define errinvalidstop         11
#define errioerror             12
#define errlimitcheck          13
#define errnocurrentpoint      14
#define errrangecheck          15
#define errstackoverflow       16
#define errstackunderflow      17
#define errsyntaxerror         18
#define errtimeout             19
#define errtypecheck           20
#define errundefined           21
#define errundefinedfilename   22
#define errundefinedresult     23
#define errunmatchedmark       24
#define errunregistered        25
#define errVMerror             26
#define errmemoryallocation    27
#define errkill                28
#define errconfiguration       29
#define errundefinedresource   30
#define errmax                 31

/*------------------------------------------------------------------------*/
/* Flags to control interpretation behaviour

    DO NOT USE THESE WITHOUT READING THE DOCS FOR THEM!
*/

#define PSFLAGSTRING            0x00000001 /* Interpret string */
#define PSFLAGFILE              0x00000002 /* Interpret file */
#define PSFLAGINTER             0x00000004 /* Interactive, issue prompts */
#define PSFLAGCLEAR             0x00000008 /* Clear stacks afterwards */
#define PSFLAGSAVE              0x00000010 /* Save and restore */
#define PSFLAGERASE             0x00000020 /* Erase page afterwards */
#define PSFLAGSTARTJOBSERVER    0x00000040 /* Save VM and init before run */
#define PSFLAGENDJOBSERVER      0x00000080 /* Do the final cleanup */

/*------------------------------------------------------------------------*/
/* Flags for PSsignalint() */

/* Bit numbers */
#define PSINTSIGB_ABORT              0
#define PSINTSIGB_KILL               1
#define PSINTSIGB_VMRECLAIMLOCAL     2
#define PSINTSIGB_VMRECLAIMALL       3

/* Masks to use */
#define PSINTSIGF_ABORT              0x00000001
#define PSINTSIGF_KILL               0x00000002
#define PSINTSIGF_VMRECLAIMLOCAL     0x00000004
#define PSINTSIGF_VMRECLAIMALL       0x00000008

/*------------------------------------------------------------------------*/
/* Entry points */

extern int      PScreateact(struct PSparm *parm);
extern void     PSdeleteact(int arec);
extern int      PSintstring(int arec, char *string, int length, int flags);
extern void     PSsignalint(int arec, int flag);
extern void     PSsignalfpe(int arec);
extern void     PSerror(int arec, int errnum);
extern void     PSsetdevice(int arec, struct PSdevice *page);
extern char     *PSerrstr(int arec, int errnum);

/*------------------------------------------------------------------------*/
#ifdef __SASC
#pragma libcall PSbase PScreateact  1e    901 /* d0 = (a1)*/
#pragma libcall PSbase PSdeleteact  24    801 /*      (a0)*/
#pragma libcall PSbase PSintstring  2A 109804 /* d0 = (a0, a1, d0, d1)*/
#pragma libcall PSbase PSsignalint  30   0802 /*      (a0, d0)*/
#pragma libcall PSbase PSsignalfpe  36    801 /*      (a0)    */
#pragma libcall PSbase PSerror      3C   0802 /*      (a0, d0)*/
#pragma libcall PSbase PSsetdevice  4E   9802 /*      (a0, a1)*/
#pragma libcall PSbase PSerrstr     54   0802 /* d0 = (a0, d0)*/
#endif /* __SASC */

/*------------------------------------------------------------------------*/

#endif /* POSTLIB_H */

