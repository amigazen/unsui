/* io.f -- translated by f2c (version 19940615).
   You must link the resulting object file with the libraries:
	f2c.lib math=standard   (in that order)
*/

#include "/f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* -- Machine generated FORTRAN 77 */
/* -- code created by RATFOR-77. */
/* -- Not intended for human consumption. */
integer openf_(integer *funit, char *name, integer *mode, ftnlen name_len)
{
    /* System generated locals */
    integer ret_val;
    olist o__1;

    /* Builtin functions */
    integer f_open(olist *);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char st[7];
    static integer ios, i23000;

    if (*mode == 2) {
	o__1.oerr = 1;
	o__1.ounit = *funit;
	o__1.ofnm = 0;
	o__1.orl = 0;
	o__1.osta = "scratch";
	o__1.oacc = 0;
	o__1.ofm = "formatted";
	o__1.oblnk = 0;
	ios = f_open(&o__1);
    } else {
	i23000 = *mode;
	if (i23000 == 0) {
	    s_copy(st, "old", 7L, 3L);
	} else if (i23000 == 1) {
	    s_copy(st, "new", 7L, 3L);
	} else {
	    s_copy(st, "unknown", 7L, 7L);
	}
	o__1.oerr = 1;
	o__1.ounit = *funit;
	o__1.ofnmlen = 80;
	o__1.ofnm = name;
	o__1.orl = 0;
	o__1.osta = st;
	o__1.oacc = 0;
	o__1.ofm = "formatted";
	o__1.oblnk = 0;
	ios = f_open(&o__1);
    }
    if (ios != 0) {
	ret_val = 0;
	return ret_val;
    } else {
	ret_val = 1;
	return ret_val;
    }
    return ret_val;
} /* openf_ */

/* Subroutine */ int closef_(integer *funit)
{
    /* System generated locals */
    cllist cl__1;

    /* Builtin functions */
    integer f_clos(cllist *);

    cl__1.cerr = 0;
    cl__1.cunit = *funit;
    cl__1.csta = 0;
    f_clos(&cl__1);
    return 0;
} /* closef_ */

integer getlin_(integer *funit, char *buf, integer *length, ftnlen buf_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer s_rsfe(cilist *), do_fio(integer *, char *, ftnlen), e_rsfe(void);

    /* Local variables */
    static integer ios;

    /* Fortran I/O blocks */
    static cilist io___5 = { 1, 0, 1, "(A)", 0 };


    io___5.ciunit = *funit;
    ios = s_rsfe(&io___5);
    if (ios != 0) {
	goto L100001;
    }
    ios = do_fio(&c__1, buf, 80L);
    if (ios != 0) {
	goto L100001;
    }
    ios = e_rsfe();
L100001:
    if (ios < 0) {
	ret_val = -1;
	return ret_val;
    } else {
	if (ios > 0) {
	    ret_val = 0;
	    return ret_val;
	} else {
	    ret_val = 1;
	    return ret_val;
	}
    }
    return ret_val;
} /* getlin_ */

/* Subroutine */ int putlin_(integer *funit, char *line, integer *length, 
	ftnlen line_len)
{
    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    static integer l;

    /* Fortran I/O blocks */
    static cilist io___7 = { 0, 0, 0, "(A)", 0 };


    l = *length;
L23001:
    if (! (l > 1)) {
	goto L23002;
    }
    goto L23003;
L23004:
    --l;
    goto L23001;
L23003:
    if (line[l - 1] != ' ') {
	goto L23002;
    }
    goto L23004;
L23002:
    io___7.ciunit = *funit;
    s_wsfe(&io___7);
    do_fio(&c__1, line, l);
    e_wsfe();
    return 0;
} /* putlin_ */

