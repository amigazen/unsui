#include <exec/types.h>

/* 8-F are superuser levels */

#define UID_ACCESSMASK	0xf000
#define UID_ACCESSSHIFT	12L

#define UID_DAEMON		0xe000
#define UID_SYSTEM		0xc000
#define UID_WIZARD		0xa000
#define UID_SUPERUSER	0x8000

/* 0-7 are normal user levels */

#define UID_ASSISTANT	0x7000
#define UID_PRIVILEGED	0x6000
#define UID_TRUSTED		0x5000
#define UID_FRIEND		0x4000
#define UID_QUALIFIED	0x3000
#define UID_NORMAL		0x2000
#define UID_GUEST		0x1000
#define UID_VISITOR		0x0000

#define UID_DAEMON_NAME		"daemon"
#define UID_SYSTEM_NAME		"system"
#define UID_WIZARD_NAME		"wizard"
#define UID_SUPERUSER_NAME	"superuser"

#define UID_ASSISTANT_NAME	"assistant"
#define UID_PRIVILEGED_NAME	"privileged"
#define UID_TRUSTED_NAME	"trusted"
#define UID_FRIEND_NAME		"friend"
#define UID_QUALIFIED_NAME	"qualified"
#define UID_NORMAL_NAME		"normal"
#define UID_GUEST_NAME		"guest"
#define UID_VISITOR_NAME	"visitor"

#ifdef UID_NAME_ARRAY

char *UID_Name[17]=
{
	UID_VISITOR_NAME,
	UID_GUEST_NAME,
	UID_NORMAL_NAME,
	UID_QUALIFIED_NAME,
	UID_FRIEND_NAME,
	UID_TRUSTED_NAME,
	UID_PRIVILEGED_NAME,
	UID_ASSISTANT_NAME,

	UID_SUPERUSER_NAME,
	"",
	UID_WIZARD_NAME,
	"",
	UID_SYSTEM_NAME,
	"",
	UID_DAEMON_NAME,
	"",
	NULL
};
#endif

struct User
{	char Loginname[17];
	char Password[17];
	char Fullname[46];
	UWORD TimeLimit;
	UWORD UID;
	long Quota;
	char Home[100];
};


int read_userdata(char *filename,struct User *,int);
int search_userdata(char *filename,struct User *,int,char *);
int adduser(char *filename,char *lname,char *pword,char *fname,char *hme,long quota,UWORD uid);
int removeuser(char *filename,int num);
void write_userdata(char *filename,struct User *,int);

