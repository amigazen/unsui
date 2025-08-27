/*
 *		Cross Development System for Atari ST 
 *     Copyright (c) 1988, Memorial University of Newfoundland
 *
 * $Header: access.c,v 1.1 88/01/29 17:30:49 m68k Exp $
 *
 * $Log:	access.c,v $
 * Revision 1.1  88/01/29  17:30:49  m68k
 * Initial revision
 * 
 */
#include	<ext.h>
#include	<errno.h>
#include    <stdio.h>

int
access(path, mode)
	char	*path;
	int	mode;
{
 struct stat	statb;
 int i;
 
 if (stat(path, &statb) < 0)
  {
   return -1;
  }
 if ((statb.st_mode & mode) == mode)
  {
   return 0;
  }
 errno = EACCES;
 return -1;
}
