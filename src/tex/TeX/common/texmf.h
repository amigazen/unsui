/* Main include file for TeX in C.  Originally by Tim Morgan,
   December 23, 1987.  These routines are also used by Metafont (with
   some name changes).  The file-opening routines are also used by
   BibTeX.  */

#include "fileio.h"	/* "../common/fileio.h" */
#include "extra.h"	/* "../common/extra.h"  */

#ifdef TeX
#define dump_file fmtfile
#define dump_path TEXFORMATPATH
#define write_out writedvi
#define out_file dvifile
#define out_buf dvibuf
#else /* not TeX */
#define dump_file basefile
#define dump_path MFBASEPATH
#define write_out writegf
#define out_file gffile
#define out_buf gfbuf
#endif /* not TeX */


/* File types.  */
typedef FILE *bytefile, *wordfile;


/* Read a line of input as quickly as possible.  */
#define	inputln(stream, flag)	input_line (stream)
#ifdef ANSI
extern boolean input_line (FILE *f);
#else
extern boolean input_line ();
#endif

/* We don't know how to implement `clear_terminal' or `wake_up_terminal'
   except on BSD systems, but they aren't crucial to TeX.  */
#ifdef BSD
#define clearterminal bsd_clear_terminal
#define wakeupterminal bsd_wake_up_terminal
extern void bsd_clear_terminal();
extern void bsd_wake_up_terminal();
#else
#define clearterminal()
#define wakeupterminal()
#endif

/* We need to read an integer from stdin if we're debugging.  */
#ifdef DEBUG
#define getint()  inputint (stdin)
#else
#define getint()
#endif



/* 
   `bopenin' (and out) is used only for reading (and writing) .tfm
   files; `wopenin' (and out) only for dump files.  The filenames are
   passed in as a global variable, `nameoffile'.  */
#define bopenin(f)	open_input (&(f), TFMFILEPATH)
#define wopenin(f)	open_input (&(f), dump_path)
#ifdef atarist
#define bopenout(f)	open_output (&((FILE *)f), 0)
#define wopenout(f)	open_output (&((FILE *)f), 0)
#else
#define bopenout	aopenout
#define wopenout	aopenout
#endif
#define bclose		aclose
#define wclose		aclose


/* This routine has to return four values.  */
#define	dateandtime(i, j, k, l)	get_date_and_time (&(i), &(j), &(k), &(l))



/* If we're running under Unix, use system calls instead of standard I/O
   to read and write the output files; also, be able to make a core dump. */ 
#ifndef unix
#define	dumpcore()	exit (1)

#ifdef TeX
#define	writedvi(a, b) \
  (void) fwrite((char *) &dvibuf[a], sizeof(dvibuf[a]), (size_t)(b-a+1), dvifile)
#else
#define	writegf(a, b) \
  (void) fwrite((char *) &gfbuf[a], sizeof(gfbuf[a]), (size_t)(b-a+1), gffile)
#endif

#else /* unix */
#define	dumpcore	abort
#ifndef atarist
# define lwrite write
#endif
#ifdef TeX
#define	writedvi(start, end)						\
  (void) lwrite (fileno (dvifile), (char *) &dvibuf[start],		\
                (long) (end - start + 1))
#else
#define	writegf(start, end)						\
  (void) lwrite (fileno (gffile), (char *) &gfbuf[start],		\
                (long) (end - start + 1))
#endif
#endif /* unix */


/* Reading and writing the dump files.  */
#define	dumpthings(base, len) \
  (void) fwrite ((char *) &(base), sizeof (base), (size_t) (len), dump_file)
#define	undumpthings(base,len) \
  (void) fread ((char *) &(base), sizeof (base), (size_t) (len), dump_file)

#define	generic_dump(x) \
  (void) fwrite ((char *) &(x), sizeof (x), 1, dump_file)
#define	generic_undump(x) \
  (void) fread ((char *) &(x), sizeof (x), 1, dump_file)

#define dumpwd		generic_dump
#define undumpwd	generic_undump
#define dumphh		generic_dump
#define undumphh	generic_undump
#define dumpqqqq   	generic_dump
#define	undumpqqqq	generic_undump


#ifdef SIXTEENBIT
#define	dumpint(x)	{ integer x_val = (x);				\
                          (void) fwrite ((char *) &x_val,		\
                                         sizeof (x_val), 1, dump_file); }
#define	undumpint(x)	fread ((char *) &(x), sizeof (x), 1, dump_file)
#else
#ifdef atarist
#  define	dumpint(x)	(void) putl ((int) (x), dump_file)
#  define	undumpint(x)	(x) = getl (dump_file)
#else
#  define	dumpint(x)	(void) putw ((int) (x), dump_file)
#  define	undumpint(x)	(x) = getw (dump_file)
#endif
#endif


/* Metafont wants to write bytes to the TFM file.  The idea behind the
   `do..while(0)' is to swallow a semicolon before a possible else
   (since C syntax is archaic), as suggested in the GNU C preprocessor
   manual.  The casts in these routines are important, since otherwise
   memory is clobbered in some strange way, which causes ``13 font
   metric dimensions to be decreased'' in the trap test, instead of 4.  */
#define bwritebyte(f, b)    putc ((char) b, f)

#define bwrite2bytes(f, h)						\
  do {									\
      integer v = (integer) h;						\
      putc (v >> 8, f);  putc (v & 0xff, f);				\
  } while (0)

#define bwrite4bytes(f, w)						\
  do {									\
      integer v = (integer) w;						\
      putc (v >> 24, f); putc (v >> 16, f);				\
      putc (v >> 8, f);  putc (v & 0xff, f);				\
  } while (0)



/* If we're running on an ASCII system, there is no need to use the
   `xchr' array to convert characters to the external encoding.  */
#ifdef NONASCII
#define	Xchr(x)		xchr[x]
#else
#define	Xchr(x)		((char) (x))
#endif
