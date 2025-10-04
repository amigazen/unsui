/* fsplit.f -- translated by f2c (version 19940615).
   You must link the resulting object file with the libraries:
	f2c.lib math=standard   (in that order)
*/

#include "/f2c.h"

/* Table of constant values */

static integer c__6 = 6;
static integer c__34 = 34;
static integer c__5 = 5;
static integer c__80 = 80;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__18 = 18;
static integer c__8 = 8;
static integer c__2 = 2;
static integer c__7 = 7;
static integer c__3 = 3;
static integer c__28 = 28;
static integer c__40 = 40;
static integer c__59 = 59;

/* -- Machine generated FORTRAN 77 */
/* -- code created by RATFOR-77. */
/* -- Not intended for human consumption. */
/* Main program */ MAIN__(void)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer what;
    static char fname[80];
    extern integer openf_(integer *, char *, integer *, ftnlen);
    extern /* Subroutine */ int handle_(integer *), banner_(void), closef_(
	    integer *);
    extern integer getlin_(integer *, char *, integer *, ftnlen);
    extern /* Subroutine */ int putlin_(integer *, char *, integer *, ftnlen);

    banner_();
    putlin_(&c__6, "Enter name of FORTRAN source file:", &c__34, 34L);
    what = getlin_(&c__5, fname, &c__80, 80L);
    if (what == 1 && s_cmp(fname, " ", 80L, 1L) != 0) {
	if (openf_(&c__1, fname, &c__0, 80L) != 1) {
	    putlin_(&c__6, "Couldn't open file", &c__18, 18L);
	} else {
	    handle_(&c__1);
	    closef_(&c__1);
	}
    } else {
	putlin_(&c__6, "Aborted!", &c__8, 8L);
    }
    return 0;
} /* MAIN__ */

/* Subroutine */ int handle_(integer *funit)
{
    /* System generated locals */
    alist al__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer f_rew(alist *);

    /* Local variables */
    static char name[80], line[80];
    extern /* Subroutine */ int copy_(integer *, integer *);
    extern integer openf_(integer *, char *, integer *, ftnlen);
    static logical error, wrote, inblck;
    static char blknam[80];
    extern /* Subroutine */ int closef_(integer *), putlin_(integer *, char *,
	     integer *, ftnlen);
    extern integer lintyp_(integer *, char *, char *, ftnlen, ftnlen), 
	    opnout_(integer *, char *, ftnlen);
    static integer typ;

    error = FALSE_;
    inblck = FALSE_;
    wrote = FALSE_;
    if (openf_(&c__2, "", &c__2, 0L) != 1) {
	error = TRUE_;
    }
    if (! error) {
	putlin_(&c__6, "Writing", &c__7, 7L);
L23000:
	typ = lintyp_(funit, line, blknam, 80L, 80L);
	if (typ != -1) {
	    putlin_(&c__2, line, &c__80, 80L);
	    wrote = TRUE_;
	}
	if (typ == 1) {
	    if (! inblck) {
		inblck = TRUE_;
		s_copy(name, blknam, 80L, 80L);
	    }
	} else {
	    if ((typ == 2 || typ == -1) && wrote) {
		if (! inblck) {
		    s_copy(name, "progrm", 80L, 6L);
		}
		inblck = FALSE_;
		if (opnout_(&c__3, name, 80L) == 1) {
		    al__1.aerr = 0;
		    al__1.aunit = 2;
		    f_rew(&al__1);
		    copy_(&c__2, &c__3);
		    closef_(&c__3);
		    closef_(&c__2);
		    if (openf_(&c__2, "", &c__2, 0L) != 1) {
			error = TRUE_;
			typ = -1;
		    }
		    wrote = FALSE_;
		}
	    }
	}
/* L23001: */
	if (! (typ == -1)) {
	    goto L23000;
	}
/* L23002: */
	if (! error) {
	    closef_(&c__2);
	    putlin_(&c__6, "Done.", &c__5, 5L);
	}
    }
    if (error) {
	putlin_(&c__6, "Couldn't open temporary file", &c__28, 28L);
    }
    return 0;
} /* handle_ */

integer lintyp_(integer *funit, char *line, char *blknam, ftnlen line_len, 
	ftnlen blknam_len)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Builtin functions */
    integer i_indx(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char l[80];
    static integer where;
    extern /* Subroutine */ int strip_(char *, char *, ftnlen, ftnlen);
    extern logical getnam_(char *, integer *, char *, ftnlen, ftnlen);
    extern integer getlin_(integer *, char *, integer *, ftnlen);
    extern logical iscmnt_(char *, ftnlen);
    static integer res;

    res = getlin_(funit, line, &c__80, 80L);
    if (res != 1) {
	ret_val = -1;
	return ret_val;
    }
    if (iscmnt_(line, 80L)) {
	ret_val = 0;
	return ret_val;
    }
    strip_(line, l, 80L, 80L);
    where = i_indx(l, "subroutine", 80L, 10L);
    if (where > 0) {
	i__1 = where + 10;
	if (! getnam_(l, &i__1, blknam, 80L, 80L)) {
	    s_copy(blknam, "subrou", 80L, 6L);
	}
	ret_val = 1;
	return ret_val;
    }
    where = i_indx(l, "function", 80L, 8L);
    if (where > 0) {
	i__1 = where + 8;
	if (! getnam_(l, &i__1, blknam, 80L, 80L)) {
	    s_copy(blknam, "functn", 80L, 6L);
	}
	ret_val = 1;
	return ret_val;
    }
    where = i_indx(l, "program", 80L, 7L);
    if (where > 0) {
	i__1 = where + 7;
	if (! getnam_(l, &i__1, blknam, 80L, 80L)) {
	    s_copy(blknam, "progrm", 80L, 6L);
	}
	ret_val = 1;
	return ret_val;
    }
    where = i_indx(l, "blockdata", 80L, 9L);
    if (where > 0) {
	i__1 = where + 9;
	if (! getnam_(l, &i__1, blknam, 80L, 80L)) {
	    s_copy(blknam, "blkdta", 80L, 6L);
	}
	ret_val = 1;
	return ret_val;
    }
    if (s_cmp(l, "end", 80L, 3L) == 0) {
	ret_val = 2;
	return ret_val;
    }
    ret_val = 0;
    return ret_val;
} /* lintyp_ */

logical iscmnt_(char *line, ftnlen line_len)
{
    /* System generated locals */
    logical ret_val;

    /* Local variables */
    static char ch[1];

    *ch = *line;
    ret_val = *ch == 'c' || *ch == 'C' || *ch == '*';
    return ret_val;
} /* iscmnt_ */

logical getnam_(char *line, integer *offset, char *name, ftnlen line_len, 
	ftnlen name_len)
{
    /* System generated locals */
    logical ret_val;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char c[1];
    static integer i;
    extern logical isdig_(char *, ftnlen), islow_(char *, ftnlen);

    s_copy(name, " ", 80L, 1L);
    i = 1;
L23003:
    if (! (i <= 6 && *offset <= 80)) {
	goto L23004;
    }
    *c = line[*offset - 1];
    if (! (islow_(c, 1L) || isdig_(c, 1L))) {
	goto L23004;
    }
    name[i - 1] = *c;
    ++i;
    ++(*offset);
    goto L23003;
L23004:
    ret_val = i > 1;
    return ret_val;
} /* getnam_ */

/* Subroutine */ int strip_(char *line, char *noblnk, ftnlen line_len, ftnlen 
	noblnk_len)
{
    /* System generated locals */
    char ch__1[1];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char c[1];
    static integer i, j;
    extern logical isdig_(char *, ftnlen);
    extern /* Character */ VOID tolow_(char *, ftnlen, char *, ftnlen);

    i = 1;
    j = 1;
L23005:
    if (! (i <= 80)) {
	goto L23006;
    }
    *c = line[i - 1];
    if (*c != ' ' && *c != '\t' && ! isdig_(c, 1L)) {
	goto L23006;
    }
    ++i;
    goto L23005;
L23006:
    s_copy(noblnk, " ", 80L, 1L);
L23007:
    if (! (i <= 80)) {
	goto L23008;
    }
    *c = line[i - 1];
    if (*c == '!') {
	goto L23008;
    } else {
	if (*c != ' ' && *c != '\t') {
	    tolow_(ch__1, 1L, c, 1L);
	    noblnk[j - 1] = ch__1[0];
	    ++j;
	}
    }
    ++i;
    goto L23007;
L23008:
    return 0;
} /* strip_ */

/* Subroutine */ int copy_(integer *from, integer *to)
{
    static char line[80];
    extern integer getlin_(integer *, char *, integer *, ftnlen);
    extern /* Subroutine */ int putlin_(integer *, char *, integer *, ftnlen);

L23009:
    if (! (getlin_(from, line, &c__80, 80L) == 1)) {
	goto L23010;
    }
    putlin_(to, line, &c__80, 80L);
    goto L23009;
L23010:
    return 0;
} /* copy_ */

integer opnout_(integer *funit, char *fname, ftnlen fname_len)
{
    /* System generated locals */
    integer ret_val, i__1;
    icilist ici__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfi(icilist *), do_fio(integer *, char *, ftnlen), e_wsfi(void)
	    ;

    /* Local variables */
    static char name[80];
    static integer i;
    extern integer openf_(integer *, char *, integer *, ftnlen);
    extern /* Subroutine */ int putlin_(integer *, char *, integer *, ftnlen);
    static integer num;

    s_copy(name, fname, 80L, 80L);
    i = 1;
L23011:
    if (! (i <= 6)) {
	goto L23012;
    }
    goto L23013;
L23014:
    ++i;
    goto L23011;
L23013:
    if (name[i - 1] == ' ' || name[i - 1] == '\t') {
	goto L23012;
    }
    goto L23014;
L23012:
    s_copy(name + (i - 1), ".f", 2L, 2L);
    if (openf_(funit, name, &c__1, 80L) == 1) {
	putlin_(&c__6, name, &c__80, 80L);
	ret_val = 1;
	return ret_val;
    }
    num = 1;
L23015:
    if (! (num < 100)) {
	goto L23016;
    }
    goto L23017;
L23018:
    ++num;
    goto L23015;
L23017:
    ici__1.icierr = 0;
    ici__1.icirnum = 1;
    ici__1.icirlen = 2;
    ici__1.iciunit = name + (i - 1);
    ici__1.icifmt = "(I2.2)";
    s_wsfi(&ici__1);
    do_fio(&c__1, (char *)&num, (ftnlen)sizeof(integer));
    e_wsfi();
    i__1 = i + 1;
    s_copy(name + i__1, ".f", i + 3 - i__1, 2L);
    if (openf_(funit, name, &c__1, 80L) == 1) {
	putlin_(&c__6, name, &c__80, 80L);
	ret_val = 1;
	return ret_val;
    }
    goto L23018;
L23016:
    putlin_(&c__6, "Ran out of output file names", &c__28, 28L);
    ret_val = 0;
    return ret_val;
} /* opnout_ */

/* Subroutine */ int banner_(void)
{
    extern /* Subroutine */ int putlin_(integer *, char *, integer *, ftnlen);

    putlin_(&c__6, "This is FSPLIT, Version 1.0 [25-Jul-94].", &c__40, 40L);
    putlin_(&c__6, "Copyright (C) 1994 Torsten Poulin. Email: <torsten@diku."
	    "dk>", &c__59, 59L);
    return 0;
} /* banner_ */

/* Main program alias */ int fsplit_ () { MAIN__ (); return 0; }
