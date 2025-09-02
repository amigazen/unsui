#include <libraries/dos.h>

struct stat  {
	int	st_mode;
	int	st_dev;
	int	st_ino;
	int	st_size;
	int	st_rdev;
	int	st_gid;
	int	st_uid;
	int	st_mtime;
	int	st_nlink;
/*
 * These last fields are specific to the Amiga
 */
	struct  DateStamp st_date;
	u_long	st_prot;
	char	st_comment[80];	  /* header files says last 36 bytes unused */
};

#define S_IFREG	01000
#define S_IFDIR	02000
#define S_IFCHR 04000
#define	S_IFMT	07000
