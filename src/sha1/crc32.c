/*
 * Copyright (c) 2005-2006, Shane Lahey
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, 
 * are permitted provided that the following conditions are met:
 *
 *    * Redistributions of source code must retain the above copyright notice, 
 *      this list of conditions and the following disclaimer.
 *
 *    * Redistributions in binary form must reproduce the above copyright notice, 
 *      this list of conditions and the following disclaimer in the documentation 
 *      and/or other materials provided with the distribution.
 *
 *    * Neither the name of the OpenSFV nor the names of its contributors 
 *      may be used to endorse or promote products derived from this software 
 *      without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR 
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// $Id: crc32.c,v 1.1.1.1 2005/09/18 13:28:21 craz1 Exp $

#include <stdio.h>
#include <unistd.h>
#include "crc32.h"

static unsigned long crc_table[256];

void gen_table_crc32(void)
{
    unsigned long crc, poly;
    int i, j;
    
    poly = 0xEDB88320L;
    
    for (i = 0; i < 256 ; i++)
    {
	crc = i;
	for (j = 8; j > 0 ; j--)
	{
	    if (crc & 1)
		crc = (crc >> 1) ^ poly;
	    else
		crc >>= 1;
	}
	crc_table[i] = crc;
    }
}

unsigned long get_crc32(int fd)
{
    register unsigned long crc;
    register int i;
    char buf[4096];
    size_t bytes;
    
    crc = 0xFFFFFFFF;

    while ((bytes = read(fd, buf, sizeof(buf))) > 0)
    {
	for (i = 0; i < bytes ; i++)
	    crc = (crc >> 8) ^ crc_table[(crc ^ buf[i]) & 0xFF];
    }
    
    return crc ^ 0xFFFFFFFF;
}
