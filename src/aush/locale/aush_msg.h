/*
* This file is part of AUSH.
* Copyright (C) 1994 Denis Gounelle
* 
* AUSH is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* AUSH is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with AUSH.  If not, see <http://www.gnu.org/licenses/>.
*
*/
#ifndef AUSH_MSG_H
#define AUSH_MSG_H


/****************************************************************************/


/* This file was created automatically by CatComp.
 * Do NOT edit by hand!
 */


#ifndef EXEC_TYPES_H
#include <exec/types.h>
#endif


/****************************************************************************/


#define MSG_TOOMUCHARGS 0
#define MSG_TOOMUCHARGS_STR "too much arguments"

#define MSG_TOOMUCHREDIR 1
#define MSG_TOOMUCHREDIR_STR "too much redirections"

#define MSG_CANTREDIRECT 2
#define MSG_CANTREDIRECT_STR "can't open redirection"

#define MSG_CANTEXECUTE 3
#define MSG_CANTEXECUTE_STR "can't execute"

#define MSG_CANTSEEK 4
#define MSG_CANTSEEK_STR "can't seek to end of stdout"

#define MSG_BADARGS 5
#define MSG_BADARGS_STR "bad arguments"

#define MSG_OVERFLOW 6
#define MSG_OVERFLOW_STR "internal overflow"

#define MSG_NOMSGPORT 7
#define MSG_NOMSGPORT_STR "no message port"

#define MSG_SYNTAXERR 8
#define MSG_SYNTAXERR_STR "syntax error"

#define MSG_IFERROR 9
#define MSG_IFERROR_STR "'if...else...endif' error"

#define MSG_NOSUCHDIR 10
#define MSG_NOSUCHDIR_STR "not that many directories"

#define MSG_ZERODIV 11
#define MSG_ZERODIV_STR "zero divide"

#define MSG_STACKFULL 12
#define MSG_STACKFULL_STR "not enough stack space"

#define MSG_READONLY 13
#define MSG_READONLY_STR "variable is read only"

#define MSG_NOMATCH 14
#define MSG_NOMATCH_STR "no match"

#define MSG_BADFUNC 15
#define MSG_BADFUNC_STR "unkown function"

#define MSG_WAITINGBG 16
#define MSG_WAITINGBG_STR "Waiting for %ld background process(es) to terminate\n"

#define MSG_STARTEDBG 17
#define MSG_STARTEDBG_STR "[%ld] %s launched\n"

#define MSG_TIMECMD 18
#define MSG_TIMECMD_STR "time: %ld seconds\n"

#define MSG_FINISHBG 19
#define MSG_FINISHBG_STR "[%ld] %s terminated (status %ld)\n"

#define MSG_STACKSIZE 20
#define MSG_STACKSIZE_STR "Current stack size: %ld bytes\n"

#define MSG_FAILLEVEL 21
#define MSG_FAILLEVEL_STR "Current fail level: %ld\n"

#define MSG_UNKNOWNERR 22
#define MSG_UNKNOWNERR_STR "%s: error code %ld\n"

#define MSG_ENDSHELL 23
#define MSG_ENDSHELL_STR "Shell %ld ending\n"


/****************************************************************************/


#ifdef STRINGARRAY

struct AppString
{
    LONG   as_ID;
    STRPTR as_Str;
};

struct AppString AppStrings[] =
{
    {MSG_TOOMUCHARGS,MSG_TOOMUCHARGS_STR},
    {MSG_TOOMUCHREDIR,MSG_TOOMUCHREDIR_STR},
    {MSG_CANTREDIRECT,MSG_CANTREDIRECT_STR},
    {MSG_CANTEXECUTE,MSG_CANTEXECUTE_STR},
    {MSG_CANTSEEK,MSG_CANTSEEK_STR},
    {MSG_BADARGS,MSG_BADARGS_STR},
    {MSG_OVERFLOW,MSG_OVERFLOW_STR},
    {MSG_NOMSGPORT,MSG_NOMSGPORT_STR},
    {MSG_SYNTAXERR,MSG_SYNTAXERR_STR},
    {MSG_IFERROR,MSG_IFERROR_STR},
    {MSG_NOSUCHDIR,MSG_NOSUCHDIR_STR},
    {MSG_ZERODIV,MSG_ZERODIV_STR},
    {MSG_STACKFULL,MSG_STACKFULL_STR},
    {MSG_READONLY,MSG_READONLY_STR},
    {MSG_NOMATCH,MSG_NOMATCH_STR},
    {MSG_BADFUNC,MSG_BADFUNC_STR},
    {MSG_WAITINGBG,MSG_WAITINGBG_STR},
    {MSG_STARTEDBG,MSG_STARTEDBG_STR},
    {MSG_TIMECMD,MSG_TIMECMD_STR},
    {MSG_FINISHBG,MSG_FINISHBG_STR},
    {MSG_STACKSIZE,MSG_STACKSIZE_STR},
    {MSG_FAILLEVEL,MSG_FAILLEVEL_STR},
    {MSG_UNKNOWNERR,MSG_UNKNOWNERR_STR},
    {MSG_ENDSHELL,MSG_ENDSHELL_STR},
};


#endif /* STRINGARRAY */


/****************************************************************************/


#endif /* AUSH_MSG_H */
