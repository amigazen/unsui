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
#include <dos/dos.h>
#include <proto/dos.h>
#include <ctype.h>
#include "mkfile_rev.h"

#ifndef __amigaos4__
#include "newdosfuncs.h"
#define USED
#define ZERO MKBADDR(NULL)
#endif

#define PROGNAME "mkfile"
#define TEMPLATE "FILE/A,SIZE/M,SIUNITS/S"
const char USED verstag[] = VERSTAG;

int64 parse_size (const char *str, uint32 siunits) {
	uint64 size = 0;
	while (*str >= '0' && *str <= '9') {
		size = (size * 10) + (*str++ - '0');
	}
	switch (toupper(*str++)) {
		case 'T':
			if (!siunits)
				size <<= 40;
			else
				size *= 1000000000000ULL;
			break;
		case 'G':
			if (!siunits)
				size <<= 30;
			else
				size *= 1000000000ULL;
			break;
		case 'M':
			if (!siunits)
				size <<= 20;
			else
				size *= 1000000ULL;
			break;
		case 'K':
			if (!siunits)
				size <<= 10;
			else
				size *= 1000ULL;
			break;
		default:
			str--;
			break;
	}
	if (toupper(*str) == 'B') {
		str++;
	}
	if (*str != 0) return -1;
	return size;
}

int main () {
	struct {
		const char *filename;
		const char **sizes;
		uint32 siunits;
	} args = {0};
	void *rd_args;
	int rc = RETURN_FAIL;
	BPTR file = ZERO;
	int64 filesize, size;
	const char **sizes;
	
	rd_args = ReadArgs(TEMPLATE, (LONG *)&args, NULL);
	if (!rd_args) {
		PrintFault(IoErr(), PROGNAME);
		return rc;
	}
	
	if (!args.sizes || !args.sizes[0]) {
		PrintFault(ERROR_REQUIRED_ARG_MISSING, PROGNAME);
		goto out;
	}
	
	filesize = 0;
	for (sizes = args.sizes; *sizes; sizes++) {
		size = parse_size(*sizes, args.siunits);
		if (size < 0) {
			PrintFault(ERROR_BAD_NUMBER, PROGNAME);
			goto out;
		}
		filesize += size;
	}
	
	file = Open(args.filename, MODE_OLDFILE);
	if (!file) {
		file = Open(args.filename, MODE_NEWFILE);
	}
	if (file) {
		if (ChangeFileSize(file, filesize, OFFSET_BEGINNING)) {
			rc = RETURN_OK;
		} else {
			PrintFault(IoErr(), args.filename);
		}
		Close(file);
	} else {
		PrintFault(IoErr(), args.filename);
	}
	
out:
	FreeArgs(rd_args);
	return rc;
}
