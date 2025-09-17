/*
 * expr - unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on expr by Peter S. Housel.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

/* Amiga-specific includes */
#include <dos/dos.h>
#include <dos/rdargs.h>
#include <libraries/dosextens.h>
#include <exec/memory.h>

#include <proto/dos.h>
#include <proto/utility.h>

#include "expr.h"
#include "common.h"
#include "getopt.h"

/* Define snprintf if not available */
#ifndef snprintf
int snprintf(char *str, size_t size, const char *format, ...) {
    va_list args;
    int result;
    va_start(args, format);
    result = VSNPrintf(str, size, format, args);
    va_end(args);
    return result;
}
#endif

extern struct DosLibrary *DOSBase;

/* Global variables */
char *progname;
char **argp;
char NUMARG[] = "numeric argument required";

/* Check for arithmetic overflow */
static int check_overflow(intmax_t a, intmax_t b, int op)
{
  intmax_t result;
  
  switch (op) {
    case '+':
      result = a + b;
      if ((b > 0 && result < a) || (b < 0 && result > a))
        return 1;
      break;
    case '-':
      result = a - b;
      if ((b > 0 && result > a) || (b < 0 && result < a))
        return 1;
      break;
    case '*':
      if (a != 0 && b != 0) {
        result = a * b;
        if (result / b != a)
          return 1;
      }
      break;
    case '/':
      if (b == 0)
        return 1; /* division by zero */
      break;
    case '%':
      if (b == 0)
        return 1; /* division by zero */
      break;
  }
  return 0;
}

int main(int argc, char *argv[])
{
    int result = SUCCESS;
    
    /* Initialize DOSBase */
    if ((DOSBase = (struct DosLibrary *)OpenLibrary("dos.library", 0)) == NULL) {
        fprintf(stderr, "expr: Cannot open dos.library\n");
        return FAILURE;
    }
    
    /* Set program name */
    progname = my_basename(argv[0]);
    
    /* Check for version request */
    if (argc > 1 && strcmp(argv[1], "--version") == 0) {
        show_version();
        CloseLibrary((struct Library *)DOSBase);
        return SUCCESS;
    }
    
    /* Check for help request */
    if (argc > 1 && (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0)) {
        usage(progname);
        CloseLibrary((struct Library *)DOSBase);
        return SUCCESS;
    }
    
    /* Run the main expr logic */
    result = run_expr_logic(argc, argv);
    
    CloseLibrary((struct Library *)DOSBase);
    return result;
}

/* Yet Another recursive descent parser. */
void expr1(struct value *valp)
/* [<][>][^][v][top][bottom][index][help] */
{
  struct value val1;

  expr2(valp);

  while (*argp != NULL) {
        if (strcmp(*argp, "|") == 0) {
                ++argp;
                expr2(&val1);
                if (nullz(valp))
                        *valp = val1;
                else;           /* return the first arg (already in *valp) */
        } else
                break;
  }
}

void expr2(struct value *valp)
/* [<][>][^][v][top][bottom][index][help] */
{
  struct value val1;

  expr3(valp);

  while (*argp != NULL) {
        if (strcmp(*argp, "&") == 0) {
                ++argp;
                expr3(&val1);
                if (nullz(valp) && nullz(&val1))
                        numresult(valp, 0);
                else;           /* return the first arg (already in *valp) */
        } else
                break;
  }
}

/* Save source code lines but not object code, unfortunately. */

#define RELOP(OP) ++argp;expr4(&val1);if(numvalue(valp) && numvalue(&val1)) numresult(valp, valp->numval OP val1.numval); else numresult(valp, strcmp(strvalue(valp), strvalue(&val1)) OP 0);

void expr3(struct value *valp)
{
  struct value val1;

  expr4(valp);

  while (*argp != NULL) {
        if (strcmp(*argp, "<") == 0) {
                RELOP(<)
        } else if (strcmp(*argp, "<=") == 0) {
                RELOP(<=)
        } else if (strcmp(*argp, "=") == 0) {
                RELOP(==)
        } else if (strcmp(*argp, "!=") == 0) {
                RELOP(!=)
        } else if (strcmp(*argp, ">=") == 0) {
                RELOP(>=)
        } else if (strcmp(*argp, ">") == 0) {
                RELOP(>)
        } else
                break;
  }
}

/* Helper function for binary operations with overflow checking */
static void do_binop(struct value *valp, struct value *val1, int op)
{
  if (!numvalue(valp) || !numvalue(val1)) 
    invalid(NUMARG);
  else {
    if (check_overflow(valp->numval, val1->numval, op))
      invalid("arithmetic overflow");
    switch (op) {
      case '+': numresult(valp, valp->numval + val1->numval); break;
      case '-': numresult(valp, valp->numval - val1->numval); break;
      case '*': numresult(valp, valp->numval * val1->numval); break;
    }
  }
}


void expr4(struct value *valp)
{
  struct value val1;

  expr5(valp);

  while (*argp != NULL) {
        if (strcmp(*argp, "+") == 0) {
                ++argp;
                expr5(&val1);
                do_binop(valp, &val1, '+');
        } else if (strcmp(*argp, "-") == 0) {
                ++argp;
                expr5(&val1);
                do_binop(valp, &val1, '-');
        } else
                break;
  }
}

void expr5(struct value *valp)
/* [<][>][^][v][top][bottom][index][help] */
{
  struct value val1;

  expr6(valp);

  while (*argp != NULL) {
        if (strcmp(*argp, "*") == 0) {
                ++argp;
                expr6(&val1);
                do_binop(valp, &val1, '*');
        } else if (strcmp(*argp, "/") == 0) {
                ++argp;
                expr6(&val1);
                if (!numvalue(valp) || !numvalue(&val1))
                        invalid(NUMARG);
                else {
                        if (check_overflow(valp->numval, val1.numval, '/'))
                                invalid("division by zero");
                        numresult(valp, valp->numval / val1.numval);
                }
        } else if (strcmp(*argp, "%") == 0) {
                ++argp;
                expr6(&val1);
                if (!numvalue(valp) || !numvalue(&val1))
                        invalid(NUMARG);
                else {
                        if (check_overflow(valp->numval, val1.numval, '%'))
                                invalid("division by zero");
                        numresult(valp, valp->numval % val1.numval);
                }
        } else
                break;
  }
}

void expr6(struct value *valp)
/* [<][>][^][v][top][bottom][index][help] */
{
  struct value val1;

  expr7(valp);

  while (*argp != NULL) {
        if (strcmp(*argp, ":") == 0) {
                ++argp;
                expr7(&val1);
#ifndef NOCOLON
                docolon(valp, &val1);
#else
                valp->nf_valid = 0;
                valp->strval = NULL;
#endif
        } else
                break;
  }
}

void expr7(struct value *valp)
/* [<][>][^][v][top][bottom][index][help] */
{
  if (*argp == NULL)
        invalid("missing argument(s)");
  else if (strcmp(*argp, "(") == 0) {
        ++argp;
        expr1(valp);
        if (strcmp(*argp++, ")") != 0) invalid("unbalanced parentheses");
  } else {
        valp->nf_valid = 0;
        valp->strval = *argp++;
  }
}

/* Return 1 if the argument is zero (numeric) or null (string */
int nullz(struct value *valp)
/* [<][>][^][v][top][bottom][index][help] */
{
  if (numvalue(valp)) return(valp->numval == 0);

  return(strlen(strvalue(valp)) == 0);
}

/* Return 1 if the argument is a valid number, insuring that the nf_valid
 * and numval fields are set properly. Does the string-to-number
 * conversion if nf_valid is false.
 */
int numvalue(struct value *valp)
/* [<][>][^][v][top][bottom][index][help] */
{
  char *p;
  int sign = 0, digits = 0;
  uintmax_t num = 0;

  if (valp->nf_valid) return 1;

  if ((p = valp->strval) == NULL) return 0;

  if (*p == '-') {
        ++p;
        sign = 1;
  }
  while (isdigit(*p)) {
        num = num * 10 + (*p++ - '0');
        digits = 1;
  }

  if (!digits || *p != '\0') return 0;

  valp->numval = sign ? -num : num;
  valp->nf_valid = 1;
  return 1;
}

/* Return the string value of the given argument. If there is only a
 * numeric value, convert it to a string
 */
char *strvalue(struct value *valp)
/* [<][>][^][v][top][bottom][index][help] */
{
  char numbuf[30];
  char *p;
  uintmax_t num;
  int sign = 0;

  if (!valp->nf_valid) return(valp->strval != NULL) ? valp->strval : "";

  p = numbuf + sizeof numbuf;
  *--p = '\0';

  if (valp->numval < 0) {
        num = -(valp->numval);
        sign = 1;
  } else
        num = valp->numval;

  do {
        *--p = '0' + (num % 10);
        num /= 10;
  } while (num);

  if (sign) *--p = '-';

  return(valp->strval = strsave(p));
}

/* Save the given string in its own allocated memory and return a pointer
 * to that memory.
 */
char *strsave(char *string)
/* [<][>][^][v][top][bottom][index][help] */
{
  char *p;

  if ((p = (char *)malloc(strlen(string) + 1)) == NULL) invalid("out of memory");

  (void) strcpy(p, string);

  return p;
}

/* Print error message and exit. */
void invalid(char *err)
/* [<][>][^][v][top][bottom][index][help] */
{
  (void) fputs(progname, stderr);
  (void) fputs(": ", stderr);
  (void) fputs(err, stderr);
  (void) putc('\n', stderr);
  exit(2);
}

#ifndef NOCOLON

#define RMIN            (UCHAR_MAX-8)   /* >= reserved as opcodes */
#define RESC            (UCHAR_MAX-8)   /* for escaping opcodes */
#define RDOT            (UCHAR_MAX-7)   /* match any character */
#define ROPEN           (UCHAR_MAX-6)   /* opening \( */
#define RCLOSE          (UCHAR_MAX-5)   /* closing \) */
#define RSTAR           (UCHAR_MAX-4)   /* Kleene closure */
#define RCLASS          (UCHAR_MAX-3)   /* character class */
#define RBACK           (UCHAR_MAX-2)   /* \digit reference */
#define REND            (UCHAR_MAX-1)   /* end of program */

#define RABEGIN         0x01    /* match anchored at BOL (^) */
#define RAEND           0x02    /* match anchored at EOL ($) */
#define RSELECT         0x04    /* \(...\) selection op used */

#define PROGLENGTH      1024    /* bytes reserved for r-programs */

#define CLASS_BYTES     ((CHAR_MAX - CHAR_MIN + 1 + CHAR_BIT-1) / CHAR_BIT)

unsigned char rprogram[PROGLENGTH];     /* regexp program storage */
unsigned int rflags = RABEGIN;  /* regexp program context */

char *rbegins[10];              /* pointers to \( beginnings */
char *rends[10];                /* pointers to \) endings */
int rlevel;                     /* \(...\) level */

/* Compile the regexp, match it against the string, and return the
 * proper result (a string if \(...\) used, and the match length otherwise.
 */
void docolon(struct value *match, struct value *pattern)
/* [<][>][^][v][top][bottom][index][help] */
{
  rcomp(strvalue(pattern));

  rmatch(strvalue(match));

  if (rflags & RSELECT) {
        match->nf_valid = 0;
        if (rends[0] == rbegins[0] || rends[1] == NULL) {
                match->strval = NULL;
        } else {
                *(rends[1]) = '\0';     /* semi-nasty */
                match->strval = rbegins[1];
        }
  } else {
        numresult(match, rends[0] - rbegins[0]);
  }
}

/* Compile an ed(1)-syntax regular-expression into the rprogram[] array. */
void rcomp(char *regexp)
/* [<][>][^][v][top][bottom][index][help] */
{
  char c;                       /* current regexp character */
  char first, last;             /* limits of character class */
  unsigned char *starable;      /* last "starable" variable */
  unsigned char *rpc;           /* pointer to next program storage byte */
  int negate;                   /* character class negated */
  int i;                        /* loop counter and such */
  int pstack[9];                /* \(...\) nesting stack */
  int pstackp = 0;              /* stack pointer for nesting stack */
  int pclosed[10];              /* flags indicating \(...\) closed */

  rpc = &rprogram[0];
  starable = NULL;

  for (i = 1; i < 10; ++i) pclosed[i] = 0;

  if (*regexp == '^') {
        rflags |= RABEGIN;      /* not needed, as it turns out */
        ++regexp;
  }
  while ((c = *regexp++)) {
        if ((rpc - rprogram) >= PROGLENGTH - 2 - CLASS_BYTES)
                invalid("regular expression program too long");

        switch (c) {
            case '.':
                starable = rpc;
                *rpc++ = RDOT;
                break;
            case '\\':
                if (isdigit(*regexp)) {
                        if (!pclosed[*regexp - '0'])
                                invalid("reference to unclosed/nonexistent \\(...\\) pair");
                        starable = NULL;
                        *rpc++ = RBACK;
                        *rpc++ = *regexp++ - '0';
                } else if (*regexp == '(') {
                        starable = NULL;
                        ++regexp;
                        rflags |= RSELECT;
                        if ((i = ++rlevel) > 9)
                                invalid("too many \\(...\\) levels");
                        pstack[pstackp++] = i;
                        *rpc++ = ROPEN;
                        *rpc++ = i;
                        break;
                } else if (*regexp == ')') {
                        starable = NULL;
                        ++regexp;
                        if (pstackp == 0)
                                invalid("\\(...\\) pairs don't balance");
                        i = pstack[--pstackp];
                        *rpc++ = RCLOSE;
                        *rpc++ = i;
                        pclosed[i] = 1;
                        break;
                } else if ((unsigned char) *regexp >= RMIN) {
                        starable = rpc;
                        *rpc++ = RESC;
                        *rpc++ = *regexp++;
                        break;
                } else {
                        starable = rpc;
                        *rpc++ = *regexp++;
                        break;
                }
            case '$':
                if (*regexp == '\0') {
                        rflags |= RAEND;
                        break;
                } else {
                        starable = rpc;
                        *rpc++ = '$';
                        break;
                }
            case '*':
                if (starable == NULL) {
                        starable = rpc;
                        *rpc++ = '*';
                        break;
                } else {
                        /* Manual memmove for ANSI C compatibility */
                        {
                                unsigned char *src, *dst;
                                int count = rpc - starable;
                                src = starable;
                                dst = starable + 1;
                                while (count-- > 0) {
                                        *dst++ = *src++;
                                }
                        }
                        *starable = RSTAR;
                        starable = NULL;
                        ++rpc;
                        break;
                }
            case '[':
                negate = 0;
                starable = rpc;
                *rpc++ = RCLASS;
                if (*regexp == '^') {
                        ++regexp;
                        negate = 1;
                }
                for (i = 0; i < CLASS_BYTES; ++i) rpc[i] = 0;
                do {
                        first = *regexp++;
                        if (*regexp == '-' && regexp[1] != ']'
                            && regexp[1] > first) {
                                ++regexp;
                                last = *regexp++;
                                for (i = first; i <= last; ++i) {
                                        rpc[(i - CHAR_MIN) / CHAR_BIT] |=
                                                1 << ((i - CHAR_MIN) % CHAR_BIT);
                                }
                        } else {
                                rpc[(first - CHAR_MIN) / CHAR_BIT] |=
                                        1 << ((first - CHAR_MIN) % CHAR_BIT);
                        }
                } while (*regexp && *regexp != ']');
                if (*regexp != ']') invalid("unterminated character class");
                ++regexp;
                if (negate) for (i = 0; i < CLASS_BYTES; ++i, ++rpc)
                                *rpc = ~*rpc;
                else
                        rpc += CLASS_BYTES;
                break;
            default:
                if ((unsigned char) c >= RMIN) {
                        starable = rpc;
                        *rpc++ = RESC;
                        *rpc++ = c;
                        break;
                } else {
                        starable = rpc;
                        *rpc++ = c;
                        break;
                }
        }
  }
  if (pstackp != 0) invalid("\\(...\\) pairs don't balance");
  *rpc = REND;
}

/* It turns out that expr regular expressions have an implicit
 * '^' prepended, and therefore RABEGIN is always on. It seemed
 * a waste to delete the code after discovering this, however.
 */
void rmatch(char *str)
/* [<][>][^][v][top][bottom][index][help] */
{
  char *end;
  unsigned char *rpc;

  rends[0] = rbegins[0] = str;

  if (rflags & RABEGIN) {
        rpc = &rprogram[0];
        if ((end = rtry(str, &rpc)) != NULL) rends[0] = end;
  } else {
        while (*str) {
                rpc = &rprogram[0];
                end = rtry(str, &rpc);
                if (end != NULL && (end - str) > (rends[0] - rbegins[0])) {
                        rbegins[0] = str;       /* longest match wins */
                        rends[0] = end;
                }
                ++str;
        }
  }

}

/* Try to match str to program from *pcp on */
char *rtry(char *str, unsigned char **pcp)
/* [<][>][^][v][top][bottom][index][help] */
{
  char *nstr;

  while (*str && **pcp != REND) {
        if ((nstr = tryone(str, pcp)) == NULL) return NULL;
        str = nstr;
  }

  while (**pcp == RCLOSE) {
        rends[*(*pcp + 1)] = str;
        *pcp += 2;
  }

  if (**pcp != REND) return NULL;

  if ((rflags & RAEND) && *str != '\0') return NULL;

  return str;
}

/* Try to match one regular expression operator */
char *tryone(char *str, unsigned char **pcp)
/* [<][>][^][v][top][bottom][index][help] */
{
  char *ret = NULL;
  unsigned char *npc;
  char *p, *q;

again:
  switch (**pcp) {
      case RESC:
        if (*str == *(*pcp + 1)) ret = str + 1;
        *pcp += 2;
        break;
      default:
        if (*str == **pcp) ret = str + 1;
        *pcp += 1;
        break;
      case RDOT:
        if (*str != '\0') ret = str + 1;
        *pcp += 1;
        break;
      case RCLASS:
        if (*str != '\0'
            && ((*pcp + 1)[(*str - CHAR_MIN) / CHAR_BIT]
                & (1 << ((*str - CHAR_MIN) % CHAR_BIT)))) {
                ret = str + 1;
        }
        *pcp += CLASS_BYTES + 1;
        break;
      case ROPEN:
        rbegins[*(*pcp + 1)] = str;
        *pcp += 2;
        goto again;
      case RCLOSE:
        rends[*(*pcp + 1)] = str;
        *pcp += 2;
        goto again;
      case RBACK:
        p = rbegins[*(*pcp + 1)];
        q = rends[*(*pcp + 1)];
        *pcp += 2;
        while (p < q)
                if (*p++ != *str++) return NULL;
        ret = str;
        break;
      case RSTAR:
        *pcp += 1;
        p = str;
        while (npc = *pcp, tryone(p, &npc) != NULL) ++p;
        *pcp = npc;
        while (p >= str
               && (npc = *pcp, (ret = rtry(p, &npc)) == NULL))
                --p;
        *pcp = npc;
        break;
      case REND:        ret = str;
}

  return ret;
}

#endif                          /* !NOCOLON */

/* Main expr logic function */
int run_expr_logic(int argc, char **argv)
{
    struct value val0;
    
    /* Check for missing arguments */
    if (argc < 2) {
        fprintf(stderr, "expr: missing argument(s)\n");
        return SYNTAX_ERROR;
    }
    
    /* Set up global argument pointer */
    argp = &argv[1];
    
    /* Parse and evaluate expression */
    expr1(&val0);
    
    /* Check for syntax errors */
    if (*argp != NULL) {
        fprintf(stderr, "expr: syntax error\n");
        return SYNTAX_ERROR;
    }
    
    /* Output result */
    puts(strvalue(&val0));
    
    /* Exit with appropriate status */
    return (nullz(&val0) ? 1 : 0);
}

/* Usage function */
void usage(const char *program)
{
    printf("Usage: %s EXPRESSION\n", program);
    printf("\n");
    printf("Evaluate expressions.\n");
    printf("\n");
    printf("Arguments:\n");
    printf("  EXPRESSION    Expression to evaluate\n");
    printf("\n");
    printf("Options:\n");
    printf("  --help        Show this help message\n");
    printf("  --version     Show version information\n");
    printf("\n");
    printf("Operators (in order of precedence):\n");
    printf("  |             OR\n");
    printf("  &             AND\n");
    printf("  = > >= < <= != Comparison\n");
    printf("  + -           Addition, subtraction\n");
    printf("  * / %%         Multiplication, division, modulo\n");
    printf("  :             Regular expression matching\n");
    printf("  ( )           Grouping\n");
    printf("\n");
    printf("Exit status:\n");
    printf("  0             Expression is neither empty string nor 0\n");
    printf("  1             Expression is empty string or 0\n");
    printf("  2             Expression is invalid\n");
}

/* Version function */
void show_version(void)
{
    printf("%s", VERSION_TAG);
}
