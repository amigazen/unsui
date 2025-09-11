/*
**
** ESTACK.C     Manipulates a stack of tokens.
**
** Originally written 6/89 in ANSI C
**
** Eval is a floating point expression evaluator.
** This file last updated in version 1.10
** For the version number, see eval.h
** Copyright (C) 1993  Will Menninger
**
*/

#include   "eval.h"

static  TOKEN   tstack[MAXINPUT+1];
static  int     tsp;


void clear_stack(void)

    {
    tsp=0;
    }


BOOLEAN push_token(TOKENPTR t)

    {
    if (tsp>MAXINPUT)
        {
        printf("Out of expression stack space.\n");
        return(0);
        }
    tokcpy(&tstack[tsp],t);
    tsp++;
    return(1);
    }


BOOLEAN pop_token(TOKENPTR t)

    {
    if (!tsp)
        return(0);
    tokcpy(t,&tstack[--tsp]);
    return(1);
    }



BOOLEAN top_of_stack(TOKENPTR t)

    {
    if (!tsp)
        return(0);
    tokcpy(t,&tstack[tsp-1]);
    return(1);
    }
