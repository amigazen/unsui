;/*
SC LINK NOSTKCHK DEF SASC=1 sh.c
QUIT
*/
/*
 * original version: David Gay
 *
 * 05/18/93 ch added SAS support, external verbose flag and primitive_parse
 */
#include <exec/types.h>
#include <dos/dostags.h>
#include <stdio.h>

#ifdef SASC
#include <string.h>
#include <stdlib.h>
#endif

#include <proto/dos.h>

/*
  translates:
 
  echo "string1;string2" ; cd xx:c ; copy xx yy
 
  	into
 
  echo "string1;string2" \n cd xx:c \n copy xx yy

  note:
	this is a really primitive function ;-) , it may be 
	changed if necessary

*/

#define QUOTE	'"'

void primitive_parse(char *s)
{
    int c;

    while(c = *s++)
    {
	if(c == QUOTE)
	{
	    while((c = *s++) && (c != QUOTE))
		;
	    if(!c)
		break;
	}
	else if(c == ';')
	    *(s-1) = '\n';
    }
}

int execute(char *cmd, int debug)
{
    long rc;
    char *s;

    while (*cmd == ' ') cmd++;
    if (strncmp(cmd, "exec", 4) == 0 && (cmd[4] == ' ' || cmd[4] == '\t')) 
	cmd += 4;
    
    s = cmd;
    primitive_parse(s);

    if(debug)
	fprintf(stderr,"/etc/sh: preparsed line:\n%s\n", cmd);

    if ((rc = SystemTags(cmd, SYS_UserShell, TRUE, TAG_END)) == -1)
    {
	fprintf(stderr, "Failed to execute command %s\n", cmd);
	return 20;
    }
    return rc;
}

void main(int argc, char **argv)
{
  int command;
  char *command_string;
  char *program_name = argv[0];
  struct RDArgs *args;
  long opts[1];
  static char options[] = "";
  long debug = 0;
  char *shenv;

  /* Throw out AmigaDOS args so that Input() is clean */
  if (args = ReadArgs(options, opts, NULL)) FreeArgs(args);

  shenv = getenv("EMACS_SH_DEBUG");

  if(shenv)
      if(strstr(shenv, "-v")) /* external verbose flag */
	  debug = 1;
  
  command = 0;
  /* Simplistic argument parsing */
  argv++;
  argc--;
  while (argc > 0 && argv[0][0] == '-')
    {
      switch (argv[0][1])
	{
	case 'c':
	  if (argc == 1) goto usage;
	  command = 1;
	  command_string = argv[1];
	  argv++;
	  argc--;
	  break;
	case 'v':
	  debug = 1;
	  break;
	case 'i':
	  /* ignored for now */
	  break;
	default: goto usage;
	}
      argc--;
      argv++;
    }
  if (argc != 0) goto usage;

  if (command) 
  {
      if(debug)
	  fprintf(stderr,"%s: command_string = %s\n", argv[0], command_string);
      exit(execute(command_string, debug));
  }
  else exit(Execute("", Input(), NULL) ? 0 : 1);

 usage:
  fprintf(stderr, "%s [-i] [-v] [-c command]\n", program_name);
  exit(1);
}


/*
 * Local variables:
 * compile-command: "sc link nostkchk def SASC=1 sh.c"
 * end:
 */
