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

// $Id: functions.c,v 1.1.1.1 2005/09/18 13:28:21 craz1 Exp $

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

/**
 * @fn void getinput(char *dest, int sizeofdest)
 * @brief Read a line of input from stdin.
 * @param dest The memory area to copy to.
 * @param sizeofdest The size (in bytes) of dest.
 */
void getinput(char *dest, int sizeofdest)
{
    char ch;
    int bytes = 0;
    
    memset(dest, 0x0, sizeofdest);
    while ((ch = getchar()) != '\n')
    {
	if (bytes++ >= (sizeofdest-1))
	    break;
	
	*(dest)++ = ch;
    }
}

/**
 * @fn int isfile(char *path)
 * @brief Checks to see if a file exists, and that it is a regular file.
 * @param path The full path to a file.
 * @return Returns 1 if the file exists, and is a regular file, otherwise it will return 0.
 * @retval 0 File does not exist, or is not a normal file.
 * @retval 1 File Exists.
 */
int isfile(char *path)
{
    struct stat st;
    
    if (stat(path, &st) != 0)
	return 0x0;
    
    return S_ISREG(st.st_mode);
}

/**
 * @fn int isdir(char *path)
 * @brief Checks to see if the provided path is a directory.
 * @param path The full path to a directory.
 * @return Returns 1 if the directory exists, 0 if it does not exist.
 * @retval 0 Directory does not exist.
 * @retval 1 Directory exists.
 */
int isdir(char *path)
{
    struct stat st;
    if (stat(path, &st) != 0)
	return 0x0;
    return S_ISDIR(st.st_mode);
}

/**
 * @fn char *striproot(char *path, char *str)
 * @brief strips off the leading path 'str' from the 'path' and returns the resulting string.
 * @param path The full path to a file/directory.
 * @param str The leading string to strip from the path.
 * @return Returns the resulting path without the leading str in it.
 * @retval char* path without str/ in it.
 */
char *striproot(char *path, char *str)
{
    char *p = path + strlen(str);
    
    while (*p == '/')
	p = p+1;
    return p;
}
