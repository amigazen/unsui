#include <dos.h>
#include <exec/types.h>
#include <exec/memory.h>
#include <exec/execbase.h>
#include <utility/tagitem.h>
#include <dos/dostags.h>
#include <dos/var.h>
#include <dos/dosextens.h>
#include <proto/exec.h>
#include <proto/dos.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "AXsh.h"

/*
		TODO: handle the processing in two (three ?) stages

		1) Replace escape-sequences with the real counterparts and
		   mark them escaped

		2) Expand variables (and `` -outputs) while separating the
		   arguments

*/



long parsecommand(	BPTR console,
					char *linein,
					char *lineout,
					char *args[],
					int argmax,
					char *buffer,
					int buffersize,
					char **inFile,
					char **outFile,
					char *inpipefile)
{
	char	*temp, *arg, linemode, search, *lop = lineout;
	int		argn=0, error=0, i, j, k, l, piped=0, escaped=0;

	*inFile = NULL;
	*outFile = NULL;

	temp = buffer;
	arg  = temp+ 3*ARGLEN;
	*args= arg + 2*ARGLEN;
	args[0][0] = '\0';

	linemode = j = k = l = 0;
	while(linein[l] && (linein[l]==' ' || linein[l]=='\t'))	/* skip whitespaces */
		l++;

	*lop = '\0';

	i = strlen(linein);
	if(i && linein[i-1]!='\n')
		strcat(linein, "\n");
	i = 0;

	while(linein[l])
	{
		switch(linein[l])
		{
		case '`':
			++l;
			if(linemode)
			{
				arg[i++] = '`';
			}
			else
			{
				j = 0;
				while(linein[l])
				{
					if(linein[l]=='`' && !linemode)
					{
						BPTR inhandle, outhandle;

						temp[j++] = '\n';
						temp[j++] = '\0';
						l++;

						if(inhandle = Open("NIL:", MODE_OLDFILE))
						{
							if(outhandle = Open("T:temppi", MODE_NEWFILE))
							{
								SystemTags(temp, SYS_CustomShell, "AXsh",
												SYS_Input, inhandle,
												SYS_Output, outhandle,
												TAG_DONE, 0);
								Close(inhandle);
								Close(outhandle);
								if(outhandle = Open("T:temppi", MODE_OLDFILE))
								{
									int z;

									FGets(outhandle, temp, ARGLEN);
									/* Calculate the length AFTER the string has been read :-) */
									z = strlen(temp);

									if(temp[z-1]=='\n')
										temp[z-1] = '\0';
									Close(outhandle);
									DeleteFile("T:temppi");

									arg[i] = '\0';
									if(i+z < 2*ARGLEN-2)	/* be sure we don't exceed maxlenght */
									{
										strcat(arg, temp);
										i += z;
									}
									else
									{
										PutStr(MSG_TOO_LONG_ARG_STR);
										error = 28;
										goto complete;
									}
								}
							}
							else
							{
								Close(inhandle);
								PutStr("Could not open\n");
							}
						}
						break;
					}
					else
					{
						switch(linein[l])
						{
						case '\"':
							linemode = 1-linemode;
							temp[j++] = linein[l++];
							break;
						case '\\':
							temp[j++] = linein[l++];
							/* Fall through! */
						default:
							temp[j++] = linein[l++];
						}
					}
				}
			}
			break;

		case '$':
			search = 0;
			switch(linein[l+1])
			{
			case '$':	/* $$ --> $ */
				arg[i++] = '$';
				l++;
				break;
			case '<':	/* stdin replacement "$<" */
				if(console)
				{
					if(getline(console, "", temp, ARGLEN, TRUE))
						goto complete;	/* no carrier ? */
				}
				arg[i] = '\0';
				strcat(arg, temp);
				i += strlen(temp);
				l++;
				break;
			case '(':		/* if '(', search for ')' */
				search = ')';
				break;
			case '[':		/* if '[', search for ']' */
				search = ']';
				break;
			case '{':		/* if '{', search for '}' */
				search = '}';
				break;
			case 0x00:		/* EOS -> no search ..*/
				search = 0;
				break;
			default:		/* else search for space */
				search = ' ';
				break;
			}
			if(search)
			{
				int a,b;

				b = l+1;
				while(linein[b] && linein[b]!='\n' && linein[b]!=search && (linein[b]!='[' || search!=' '))
					b++;
				if(search == ' ')
				{
					strncpy(temp, linein+l+1, b-l-1);
					temp[b-l-1] = '\0';
				}
				else
				{
					strncpy(temp, linein+l+2, b-l-2);
					temp[b-l-2] = '\0';
				}

				if(search == ' ')
					l = b;
				else
					l = b+1;

				a = -1;
				if(linein[l] == '[')
				{
					a = atoi(linein+l+1);
					while(linein[l] && linein[l]!='\n' && linein[l]!=']')
						l++;
					++l;
				}

				{	int b=0,c=0,d=0;

					if(GetVar(temp, temp, ARGLEN, GVF_LOCAL_ONLY)!=-1)
					{
						if(a != -1)
						{
							while(1)
							{
								if(temp[c] == ' ' || !temp[c])
								{
									b++;
									if(b > a)
									{
										break;
									}
									d = c+1;
								}
								if(!temp[c])	/* no such word */
								{
									d = c;
									break;
								}
								c++;
							}
							temp[c] = '\0';
							if(d)
								strcpy(temp, temp+d);
						}
					}
					else
					{
						/* variable not found, preserve the dollar-sign */
						arg[i++] = '$';
					}
				}
				arg[i] = '\0';

				if(i+strlen(temp) < 2*ARGLEN-2)	/* be sure we don't exceed maxlenght */
				{
					strcat(arg, temp);
					i += strlen(temp);
				}
				else
				{
					PutStr(MSG_TOO_LONG_ARG_STR);
					error = 28;
					goto complete;
				}
				error = 0;
			}
			else
				l++;
			break;

		case '\"':
			/*arg[i++] = '\"';*/
			linemode = 1-linemode;
			++l;
			escaped = 1;			/* Remember that the user used quotes! */
			break;

		case '*':
			/* star-escape only works inside quotes */
			if(linemode)
			{
				if(!linein[l+2])
				{
					arg[i++] = '*';
					++l;
					break;
				}
				arg[i++] = linein[l+1];
				l += 2;
				break;
			}
			arg[i++] = linein[l++];
			break;

		case '\\':
			if(!linein[l+2])
			{
				arg[i++] = '\\';
				++l;
				break;
			}
			arg[i++] = linein[l+1];
			l += 2;
			break;

		case ';':
			if(linemode)
			{
				arg[i++] = linein[l++];
				break;
			}
			/* fall through */
		case '\n':
		case ' ':
			if(!linemode)
			{
				arg[i] = '\0';

				if(argn >= argmax)
				{
					PutStr(MSG_TOO_MANY_ARGS_STR);
					error = 28;
					goto complete;
				}

				if(argn == 0)	/* first argument == command */
				{
					if(!i)
					{
						l++;
						break;	/* no null-commands */
					}
					lowercase(arg, arg);	/* commands are case-insensitive */
				}

				if(inpipefile && !stricmp(arg, "in:"))
				{
					/* Replace "IN:" with the filename */
					strcpy(arg, inpipefile);
					piped |= REDIR_PIPE;
				}

				/*i = strlen(arg);*/
				if(k+i+3 > buffersize-5*ARGLEN)
				{
					PutStr(MSG_TOO_BIG_ARG_MESS_STR);
					error = 28;
					goto complete;
				}

				if(!escaped && arg[0] == '>')	/* Check output redirection */
				{
					j = 0;
					while(arg[j] == '>')
						j++;
					if(*outFile)
					{
						PutStr(MSG_NO_MULTIPLE_REDIRECTIONS_STR);
						error = 22;
						goto complete;
					}
					*outFile = args[argn];
					strcpy(args[argn], arg+j);
					args[argn] += i+1-j;
					k += i+1-j;

					if(j>1)
						piped |= REDIR_APPEND;
				}
				else if(!escaped && arg[0] == '<')	/* Check input redirection */
				{
					j = 0;
					while(arg[j] == '<')
						j++;
					if(*inFile || (*outFile && arg[j]=='>'))
					{
						PutStr(MSG_NO_MULTIPLE_REDIRECTIONS_STR);
						error = 22;
						goto complete;
					}
					if(arg[j]=='>')
					{
						*outFile = *inFile = args[argn];
						strcpy(args[argn], arg+j+1);
						args[argn] += i+1-j-1;
						k += i+1-j-1;

						piped |= REDIR_INOUT;
					}
					else
					{
						*inFile = args[argn];
						strcpy(args[argn], arg+j);
						args[argn] += i+1-j;
						k += i+1-j;
					}
				}
				else if(argn > 0)
				{
					strcpy(args[argn++], arg);
					args[argn] = args[argn-1]+i+1;
					k += i+1;

					i = 0;
					while(arg[i])
					{
						if(arg[i] == ' ')
						{
							escaped = 1;
							break;
						}
						i++;
					}
					if(!i)
					{
						escaped = 1;
					}

					if(argn > 2)	/* argn was already incremented */
					{
						*lop++ = ' ';
					}

					if(escaped)
					{
						i = 0;
						*lop++ = '\"';
						while(arg[i])
						{
							if(arg[i] == '\"')
							{
								*lop++ = '*';
							}
							if(arg[i] == '*')
							{
								*lop++ = '*';
							}
							*lop++ = arg[i];
							i++;
						}
						*lop++ = '\"';
					}
					else
					{
						strcpy(lop, arg);
						lop += strlen(arg);
					}
				}
				else	/* Command */
				{
					strcpy(args[argn++], arg);
					args[argn] = args[argn-1]+i+1;
					k += i+1;
				}
				escaped = 0;

				i=0;
				while(linein[l]==' ' || linein[l]=='\t' || linein[l]=='\n')
					l++;
				if(linein[l] == ';' || linein[l] == '|' || linein[l] == '&')
				{
					piped |= PARSE_MULTI;
					if(linein[l] == '|')
					{
						piped |= PARSE_PIPED;
					}
					if(linein[l] == '&')
					{
						/* run command */
						if(argn+1 >= argmax)
						{
							PutStr(MSG_TOO_MANY_ARGS_STR);
							error = 28;
							goto complete;
						}
						argn++;
						/* TODO: currently doesn't handle fuzzy-named commands */
						/* (this means commands that have chars that must be escaped) */
						i = strlen(args[0]);
						*lop = '\0';
						memmove(lineout+i+1, lineout, strlen(lineout));
						memcpy(lineout, args[0], i);
						lineout[i] = ' ';
						lop += i+1;

						for(i=argn;i>0;i--)
						{
							args[i] = args[i-1];
						}
						args[0] = "run";
					}
					strcpy(linein, linein+l+1);
					goto complete;
				}
				break;
			}
			/* Fall through */
		default:
			if(i < 2*ARGLEN-2)
			{
				arg[i++] = linein[l++];
			}
			else
			{
				PutStr(MSG_TOO_LONG_ARG_LINE_STR);
				error = 28;
				goto complete;
			}
		}
	}
	linein[0] = '\0';

complete:
	if(error)
		linein[0] = '\0';

	*lop++ = '\n';
	*lop = '\0';

	if(error)
	{
		return PARSE_ERROR | piped;
	}
	else
	{
		return argn | piped;
	}
}
