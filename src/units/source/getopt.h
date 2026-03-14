#ifndef GETOPT_H
#define GETOPT_H
#include "proto.h"				/* define PROTO */

int getopt PROTO((int, char **, char *));
extern char *optarg;				/* current argv string */
extern int optind;				/* current argv index */
extern int optopt;				/* option character */
extern int opterr;				/* getopt prints errors if 1 */
#endif /* GETOPT_H */
