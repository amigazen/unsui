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

// $Id: md5file.c,v 1.1.1.1 2005/09/18 13:28:21 craz1 Exp $

#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>

#include "md5.h"
#include "md5file.h"

char *md5file(int fd)
{
    MD5_CTX context;
    unsigned char buf[4096];
    unsigned char digest[16];
    static char ret[32];
    size_t bytes;
    int i;
    
    MD5Init(&context);
    while ((bytes = read(fd, buf, sizeof(buf))) > 0)
	MD5Update(&context, buf, bytes);
    MD5Final(digest, &context);
    
    strncpy(ret, "", sizeof(ret));
    // convert to base16
    for (i = 0; i < 16; i++)
    {
	char tmp[3];
	snprintf(tmp, sizeof(tmp), "%02x", digest[i]);
	tmp[2] = 0x0;
	strncat(ret, tmp, sizeof(ret));
    }
    ret[sizeof(ret)] = 0x0;
    return ret;
}
