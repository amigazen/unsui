/* Copyright 2008-2010 Fredrik Wikstrom. All rights reserved.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions
** are met:
**
** 1. Redistributions of source code must retain the above copyright
**    notice, this list of conditions and the following disclaimer.
**
** 2. Redistributions in binary form must reproduce the above copyright
**    notice, this list of conditions and the following disclaimer in the
**    documentation and/or other materials provided with the distribution.
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS `AS IS'
** AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
** IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
** ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
** LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
** CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
** SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
** INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
** CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
** ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
** POSSIBILITY OF SUCH DAMAGE.
*/

#include "types.h"
#include "newdosfuncs.h"
#include <proto/dos.h>
#ifndef __AROS__
#include "dos64.h"
#endif

#ifdef __AROS__

int64 GetFileSize64 (BPTR file) {
	SetIoErr(ERROR_ACTION_NOT_KNOWN);
	return -1LL;
}

int64 GetFilePosition64 (BPTR file) {
	SetIoErr(ERROR_ACTION_NOT_KNOWN);
	return -1LL;
}

LONG ChangeFilePosition64 (BPTR file, int64 offset, LONG mode) {
	SetIoErr(ERROR_ACTION_NOT_KNOWN);
	return FALSE;
}

#endif

int64 GetFileSize (BPTR file) {
	int64 size = GetFileSize64(file);
	if (size == -1LL &&
		IoErr() == ERROR_ACTION_NOT_KNOWN)
	{
		struct FileInfoBlock *fib;
		fib = AllocDosObject(DOS_FIB, NULL);
		if (fib) {
			if (ExamineFH(file, fib)) {
				size = (ULONG)fib->fib_Size;
			}
			FreeDosObject(DOS_FIB, fib);
		}
	}
	return size;
}

int64 GetFilePosition (BPTR file) {
	int64 position = GetFilePosition64(file);
	if (position == -1LL &&
		IoErr() == ERROR_ACTION_NOT_KNOWN)
	{
		position = Seek(file, 0, OFFSET_CURRENT);
	}
	return position;
}

LONG ChangeFileSize (BPTR file, int64 position, LONG mode) {
	LONG res;
	res = ChangeFileSize64(file, position, mode);
	if (!res &&
		IoErr() == ERROR_ACTION_NOT_KNOWN)
	{
		LONG size;
		size = SetFileSize(file, (LONG)position, mode);
		if (size != -1 || !IoErr()) {
			res = TRUE;
		}
	}
	return res;
}

LONG ChangeFilePosition (BPTR file, int64 position, LONG mode) {
	LONG res;
	res = ChangeFilePosition64(file, position, mode);
	if (!res &&
		IoErr() == ERROR_ACTION_NOT_KNOWN)
	{
		LONG pos;
		pos = Seek(file, (LONG)position, mode);
		if (pos != -1 || !IoErr()) {
			res = TRUE;
		}
	}
	return res;
}
