#include "amiga.h"
#include "processes.h"
#include <amiga/ioctl.h>
#include <exec/memory.h>
#include <dos/dosextens.h>
#include <dos/dostags.h>
#include <string.h>

int eexec(char *program, char **argv, int input, int output, int error,
	  char *dir, int stacksize)
/* input = -1 -> inherit Input()
   output = -1 -> inherit Output()
   error = -1 -> inherit pr_CES
   error = -2 -> stderr = stdout */
{
  int index, comsize, close_input, close_output, close_error;
  char *combuf, *bp;
  BPTR in, out, err, dirlock;
  int _pseudo_close(int fd);

  comsize = 256;
  combuf = malloc(comsize);

  if (input == -1)
    {
      in = Input();
      close_input = FALSE;
    }
  else
    {
      if (ioctl(input, _AMIGA_GET_FH, &in) == -1) in = 0;
      close_input = TRUE;
      _pseudo_close(input);
    }

  if (output == -1)
    {
      out = Output();
      close_output = FALSE;
    }
  else
    {
      if (ioctl(output, _AMIGA_GET_FH, &out) == -1) out = 0;
      close_output = out != in;
      _pseudo_close(output);
    }

  if (error == -1)
    {
      err = _us->pr_CES;
      close_error = FALSE;
    }
  else if (error == -2)
    {
      err = out;
      close_error = FALSE;
    }
  else
    {
      if (ioctl(error, _AMIGA_GET_FH, &err) == -1) err = 0;
      close_error = err != out && err != in;
      _pseudo_close(error);
    }

  /* pr_CES is not always defined */
  if (in && out && (err || error == -1))
    if (combuf)
      {
	bp = combuf;
	for (index = 0; argv[index] != 0; index++)
	  {
	    /* Use program as argv[0]. This loses some information, but ... */
	    char *arg = index == 0 ? program : argv[index];
	    char *s;
	    int len;

	    len = 3;
	    s = arg;
	    while (*s)
	      {
		len++;
		if (*s == '*' || *s == '"' || *s == '\n') len++;
		s++;
	      }
	    if (bp + len + 1 >= combuf + comsize)
	      {
		char *newbuf;

		comsize += comsize + len;
		newbuf = realloc(combuf, comsize);
		if (!newbuf) { errno = ENOMEM; goto error; }
		bp = newbuf + (bp - combuf);
		combuf = newbuf;
	      }
	    *bp++ = ' ';
	    *bp++ = '"';
	    s = arg;
	    while (*s)
	      {
		if (*s == '"' || *s == '*') *bp++ = '*';
		else if (*s == '\n') *bp++ = '+';
		*bp++ = *s++;
	      }
	    *bp++ = '"';
	  }
	*bp = '\0';
	if (dir) dirlock = Lock(dir, SHARED_LOCK);
	else dirlock = 0;

	if (dirlock || !dir)
	  {
	    int pid = _start_process(combuf, in, close_input, out, close_output,
				     err, close_error, dirlock, stacksize);

	    if (pid != -1)
	      {
		free(combuf);
		return pid;
	      }
	  }
	else errno = convert_oserr(IoErr());
	if (dirlock) UnLock(dirlock);
      }
    else errno = ENOMEM;

 error:
  if (in && close_input) Close(in);
  if (out && close_output) Close(out);
  if (err && close_error) Close(err);
  if (combuf) free(combuf);
  return -1;
}

int exec(char *program, char **argv, int input, int output, 
	  char *dir, int stacksize)
{
  return eexec(program, argv, input, output, -1, dir, stacksize);
}
