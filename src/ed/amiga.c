/*
 *  Fake the library routines not supplied on the Amiga.  They really aren't
 *  very important anyway...
 */

signal ()
{
	return (-1);
}

fork ()
{
	return (-1);
}
