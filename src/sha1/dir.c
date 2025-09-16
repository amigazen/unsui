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

// $Id: dir.c,v 1.2 2005/09/18 20:51:50 craz1 Exp $

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <dirent.h>

#include <sys/stat.h>
#include <unistd.h>

#include "functions.h"
#include "dir.h"
#include "opensfv.h"

struct fileinfo *filetree = NULL;

struct fileinfo *newfileinfo(char *path)
{
    struct fileinfo *p;
    int memlen;
    
    memlen = sizeof(struct fileinfo) + 1;
    if ((p = (struct fileinfo *)malloc(memlen)) == NULL)
    {
	fprintf(stderr, "MEMORY ERROR ALLOCATING %d BYTES OF MEMORY%s", memlen, EOLSTR);
	return NULL;
    }
    p->next = NULL;
    
    memlen = strlen(path) + 1;
    p->filename = (char *)malloc(memlen);
    if (p->filename == NULL) {
	fprintf(stderr, "MEMORY ERROR ALLOCATING %d BYTES OF MEMORY%s", memlen, EOLSTR);
	free(p);
	return NULL;
    }
    strncpy(p->filename, path, (memlen - 1));
    p->filename[memlen-1] = 0x0;
    stat(path, &p->st);
    
    return p;
}

void addfile(char *path)
{
    struct fileinfo *fi;
    struct fileinfo *current = filetree;

    // fill in a new fileinfo struct with the file info
    fi = newfileinfo(path);
    if (fi == NULL)
	return; // failed to allocate memory =(
    
    // now add it to the current file tree
    if (filetree == NULL)
	filetree = fi;
    else {
	while (current->next != NULL)
	    current = current->next;
	current->next = fi;
    }

    // all done
    return;
}

void getdircontents(char *path, int recursive)
{
    DIR *dir;
    struct dirent *file;
    char *fullpath;
    int memlen;
    
    if ((dir = opendir(path)) == NULL)
	return;
    
    while ((file = readdir(dir)) != NULL)
    {
	if (strcasecmp(file->d_name, ".") == 0 || strcasecmp(file->d_name, "..") == 0)
	    continue;
	
	memlen = strlen(path) + strlen(file->d_name) + 3;
	fullpath = (char *)malloc(memlen);
	if (fullpath == NULL) {
	    printf("ERROR: FAILED TO ALLOCATED %d BYTES OF MEMORY%s", memlen, EOLSTR);
	    continue; //try to continue anyway *shrug*
	}
	memset(fullpath, 0x0, memlen);
	snprintf(fullpath, memlen - 1, "%s/%s", path, file->d_name);
	
	if (isdir(fullpath) && recursive == 1) {
	    getdircontents(fullpath, recursive);
	} else {
	    addfile(fullpath);
	}
	free(fullpath);
    }
    
    closedir(dir);
}
