/* (c)  Copyright 1993 Commodore-Amiga, Inc.   All rights reserved. */
/* The information contained herein is subject to change without    */
/* notice, and is provided "as is" without warranty of any kind,    */
/* either expressed or implied.  The entire risk as to the use of   */
/* this information is assumed by the user.                         */

#ifndef ASYNCIO_H
#define ASYNCIO_H


/*****************************************************************************/


#ifndef EXEC_TYPES_H
#include <exec/types.h>
#endif

#ifndef EXEC_PORTS_H
#include <exec/ports.h>
#endif

#ifndef DOS_DOSEXTENS_H
#include <dos/dosextens.h>
#endif


/*****************************************************************************/


/* This structure is public only by necessity, don't muck with it yourself, or
 * you're looking for trouble
 */
struct AsyncFile
{
    BPTR                  af_File;
    ULONG                 af_BlockSize;
    struct MsgPort       *af_Handler;
    APTR                  af_Offset;
    LONG                  af_BytesLeft;
    ULONG                 af_BufferSize;
    APTR                  af_Buffers[2];
    struct StandardPacket af_Packet;
    struct MsgPort        af_PacketPort;
    ULONG                 af_CurrentBuf;
    ULONG                 af_SeekOffset;
    UBYTE                 af_PacketPending;
    UBYTE                 af_ReadMode;

    UBYTE                 af_Resync;
};


/*****************************************************************************/


#define MODE_READ   0  /* read an existing file                             */
#define MODE_WRITE  1  /* create a new file, delete existing file if needed */
#define MODE_APPEND 2  /* append to end of existing file, or create new     */

#define MODE_START   -1   /* relative to start of file         */
#define MODE_CURRENT  0   /* relative to current file position */
#define MODE_END      1   /* relative to end of file         */


/*****************************************************************************/


struct AsyncFile * __regargs OpenAsync(const STRPTR fileName, UBYTE accessMode, LONG bufferSize);
LONG __regargs CloseAsync(struct AsyncFile *file);
LONG __regargs ReadAsync(struct AsyncFile *file, APTR buffer, LONG numBytes);
LONG __regargs ReadCharAsync(struct AsyncFile *file);
LONG __regargs WriteAsync(struct AsyncFile *file, APTR buffer, LONG numBytes);
LONG __regargs WriteCharAsync(struct AsyncFile *file, UBYTE ch);
LONG __regargs SeekAsync(struct AsyncFile *file, LONG position, BYTE mode);


/*****************************************************************************/


#endif /* ASYNCIO_H */
