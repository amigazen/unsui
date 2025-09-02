/* Program to produce output at regular intervals.  */

#include <stdio.h>
#include <time.h>

struct tm *localtime ();

main (argc, argv)
     int argc;
     char **argv;
{
  int period = 60;
  long when;
  struct tm *tp;

  if (argc > 1)
    period = atoi (argv[1]);

  while (1)
    {
#ifdef AMIGA
      if (Write(Output(), "Wake up!\n", 9) != 9) exit(0);
      chkabort();
#define sleep(n) Delay(50 * (n))
#else
      /* Make sure wakeup stops when Emacs goes away.  */
      if (getppid () == 1)
	exit (0);
      printf ("Wake up!\n");
      fflush (stdout);
#endif
      /* If using a period of 60, produce the output when the minute
	 changes. */
      if (period == 60)
	{
	  time (&when);
	  tp = localtime (&when);
	  sleep (60 - tp->tm_sec);
	}
      else
	sleep (period);
    }
}
