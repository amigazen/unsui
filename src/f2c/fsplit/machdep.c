/* machdep.f -- translated by f2c (version 19940615).
   You must link the resulting object file with the libraries:
	f2c.lib math=standard   (in that order)
*/

#include "/f2c.h"

/* -- Machine generated FORTRAN 77 */
/* -- code created by RATFOR-77. */
/* -- Not intended for human consumption. */
/* Character */ VOID tolow_(char *ret_val, ftnlen ret_val_len, char *c, 
	ftnlen c_len)
{
    if (*c >= 'A' && *c <= 'Z') {
	*ret_val = (char) (*c + 32);
	return ;
    } else {
	*ret_val = *c;
	return ;
    }
} /* tolow_ */

logical isdig_(char *c, ftnlen c_len)
{
    /* System generated locals */
    logical ret_val;

    ret_val = *c >= '0' && *c <= '9';
    return ret_val;
} /* isdig_ */

logical islow_(char *c, ftnlen c_len)
{
    /* System generated locals */
    logical ret_val;

    ret_val = *c >= 'a' && *c <= 'z';
    return ret_val;
} /* islow_ */

