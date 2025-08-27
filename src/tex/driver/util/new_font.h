/*
 ************************************************************************
 *									*
 *	new_font.h					04. Dez. 89	*
 *									*
 *	A replacement for the old fontstructure-handling.	(hes)	*
 *									*
 ************************************************************************
 */

#define NPXLCHARS		256
#define PATH_SIZE		128
#define FONT_NAME_SIZE		60
#define FLIB_NAME_SIZE		60
#define PK_NAME_SIZE		60
/* #define FMT_STRING_SIZE		48 unused */

/** **  name of the environment-vars ** **/
#define ENV_PK_DIRS		"PKDIR"
#define ENV_FLIB_DIRS		"FLIBDIR"

/** **  default search directories ** **/
#ifdef AMIGA
#define PK_AREA			"TeX:pk"
#define FLIB_AREA		"TeX:FontLib"
#else
#define PK_AREA			"d:\\lib\\fonts\\tex\\fonts"
#define FLIB_AREA		"d:\\lib\\fonts\\tex\\libs"
#endif

/** **  default format-strings of the file-names ** **/
#ifdef AMIGA
# define FLIB_STRING		"PK%04d"
# define PK_STRING		"%d/%s.%dpk"
# define BASE_PK_STRING		"%d/%s.%dpk"
# define PKDIR_STRING		"%s.pk"
# define FLIB_FNT_STRING	"%s"
#else
# define FLIB_STRING		"PK%04d"
# define PK_STRING		"%d\\%s"
# define BASE_PK_STRING		"%d\\%s"
# define PKDIR_STRING		"%s.pk"
# define FLIB_FNT_STRING	"%s"
#endif

/** **  filenames of the configfiles for the font-definitions ** **/
#define SFONT_DEF_FILE		"showdvi.fnt"
#define DFONT_DEF_FILE		"dviprint.fnt"

/** **  begin of a pk-file ** **/
#define PK_MAGIC		(unsigned short)0xF759	/* PK_PRE | ID-Byte (89) */


/** **  size of copy-buffer for copying font to font-cache (only AMIGA) ** **/
#define CPBUFSIZE		10240	/* damit bekommt man die meisten Fonts auf einmal */


typedef unsigned short ushort;
typedef unsigned long  ulong;


/*
 ************************************************************************
 *	definition of the magsteps	0 h 1 1h 2 2h 3 4 5 6 7 8 9	*
 ************************************************************************
 */

#define NRMAGS		13



/*
 ************************************************************************
 *	definition of one character					*
 ************************************************************************
 */

struct Char {
		ushort	width;			/* moeglicherweise unnoetig	*/
		ushort	height;
		long	xOffset;
		long	yOffset;
		/* long	bytes; */			/* size in bytes of the char	*/
		/* die Bitmap des Chars kommt direckt im Anschluss nach sizeof	*/
	};

#define CHAR_BITMAP(x)	((char *)(x+1))		/* x must be (struct Char *)	*/

struct Chars {
		short	pixelwidth;		/* width of the character (h escapement)*/
		long	packed_data;		/* pointer to the pk-data of the char	*/
						/* offset in internal memory		*/
	struct	Char	*unpacked;		/* pointer to the unpacked info		*/
	};


/*
 ************************************************************************
 *	definition of a font-library					*
 ************************************************************************
 */

struct Fnt_Lib {
		long	resolution;		/* dpi			*/
		long	lru_count;		/* LRU-counter		*/
		FILE	*file_ptr;		/* file pointer		*/
	struct	dir	directory;		/* directory of the lib */
		char	*format_str;		/* format-string	*/
		char	lib_status;		/* look LIB_xxx defines	*/
		char	path[PATH_SIZE];	/* directory of the lib	*/
	};

		/* if format_str == NULL => use the default format str	*/


/*
 ************************************************************************
 *	current states of a font-library				*
 ************************************************************************
 */

#define LIB_NONE		(char)0		/* init			*/
#define LIB_PATH_DEFINED	(char)1		/* path defined by user	*/
#define LIB_READ_CLOSED		(char)2		/* lib dir read, closed	*/
#define LIB_OPEND		(char)3		/* library opend	*/
#define LIB_NOT_EXISTS		(char)4		/* library dosn't exist	*/


/*
 ************************************************************************
 *	definition of a PK-directory					*
 ************************************************************************
 */

struct PK_Dir {
		char	*format_str;		/* format-string	*/
		char	path[PATH_SIZE];	/* pk-directory		*/
	};

/*
 ************************************************************************
 *	where can a font be found					*
 ************************************************************************
 */

union Fnt_Loc {
	struct	Fnt_Lib	*fnt_library;		/* ptr to library	*/
	struct	PK_Dir	*pk_directory;		/* ptr to directory str */
	};


/*
 ************************************************************************
 *	common elements of a fontGROUP					*
 ************************************************************************
 */

struct CommonFontGroups {
		long	tfmw[NPXLCHARS];  /* tfmw of font-group	*/
		char	fnt_name[1];	  /* e.g. cmr10		*/

	};

/*
 ************************************************************************
 *	common elements of a font					*
 ************************************************************************
 */

struct CommonFontElems {
	struct CommonFontGroups	*fnt_group;		/* def. of font-group	*/
		long		where_in_lib;		/* offset in lib	*/
		long		fnt_length;		/* length of PK-File	*/
		long		resolution;		/* dpi			*/
		long		resolution5;		/* dpi * 5		*/
		long		lru_count;		/* LRU-counter		*/
		long		*ch_start_bm;		/* start char-bitmap	*/
		ulong		ch_len;			/* length char-bitmap	*/
	union	Fnt_Loc		location;		/* define font location */
	struct	Chars		ch[NPXLCHARS];		/* char definitions	*/
		short		maxchars;		/* nr of chars in the font 	*/
		char		fnt_status;		/* look FNT_xxx defines	*/
		char		copied;			/* needed during font removing	*/
		char		where_is_font;		/* is in lib		*/
		char		was_loadet;		/* set to true at the first load*/
		};

/*
 ************************************************************************
 *	defines for where_is_font					*
 ************************************************************************
 */
#define WHERE_NONE		0
#define WHERE_LIB		1
#define WHERE_PK		2
#define WHERE_PKDIR		3

/*
 ************************************************************************
 *	definition of a font						*
 ************************************************************************
 */

struct Font {
		long		fnt_number;		/* depends on DVI-File	*/
		long		orig_dpi;		/* original resolution  */
		long		space_faktor;		/* space faktor		*/
		long		design_faktor;		/* design faktor	*/
		long		chksum;			/* font checksum	*/
		long		ctfmw[NPXLCHARS];	/* this is faster       */
	struct	CommonFontElems *common;		/* common elems	for font sharing */
		char		ctfmw_valid;		/* are ctfm's valid ?   */
	};


/*
 ************************************************************************
 *	current states of a font					*
 ************************************************************************
 */

#define FNT_NONE		(char)0		/* init			*/
#define FNT_PATH_DEFINED	(char)1		/* path defined by user */
#define FNT_FOUND		(char)2		/* path correct		*/
#define FNT_DEFINED		(char)3		/* font defined 	*/
#define FNT_LOADED		(char)4		/* font in memory	*/
#define FNT_DEFINED_OLOADED	(char)5		/* defined and old lo.	*/
#define FNT_OLD_LOADED		(char)6		/* old font in memory	*/
#define FNT_NOT_EXISTS		(char)7		/* font doesn't exists	*/


#ifdef OLD_NOFONT
/* Not needed, because of  FNT_NOT_EXISTS for `fnt_status' */

/*
 ************************************************************************
 *	list of all fonts which can't be found				*
 ************************************************************************
 */

struct noFont {
	struct	noFont	*next;
		int	resolution;	/* dpi			*/
		char	fnt_name[1];	/* name of the font	*/
	};
#endif


/*
 ************************************************************************
 *	search element for the fontlibs/pk-directories	(Env-Vars)	*
 ************************************************************************
 */

struct SeaEle {
	struct	SeaEle	*next_ele;
		char	*path;
	};


/*
 ************************************************************************
 *	searchlist for the fontlibs/pk-directories  (Env-Vars)		*
 ************************************************************************
 */

struct SeaLst {
	struct	SeaEle	*first_ele;
		long	number;
	};




/*
 ************************************************************************
 *	search element for the pk-directories with dpi  (Def-File)	*
 ************************************************************************
 */

struct SeaEleDpi {
	struct	SeaEleDpi	*next_ele;
		long		hdpi;			/* dpi of the pk-dir	*/
		long		vdpi;			/* vert-dpi		*/
		char		*path;			/* path of the pk-dir	*/
		char		*format_str;		/* format-string	*/
	};


/*
 ************************************************************************
 *	searchlist for the pk-directories with dpi  (Def-File)		*
 ************************************************************************
 */

struct SeaLstDpi {
	struct	SeaEleDpi	*first_ele;
		long		number;
	};


/*
 ************************************************************************
 *	list of all defined/loaded/... font libraries			*
 ************************************************************************
 */

struct LibLst {
	struct	LibLst	*next_lib;
	struct	Fnt_Lib	flib;
	};


/*
 ************************************************************************
 *	list of all defined/loaded/... fonts				*
 ************************************************************************
 */

struct FontLst {
	struct	FontLst	*next_font;
	struct	Font	font;
	};


/*
 ************************************************************************
 *	list of all font-groups						*
 ************************************************************************
 */

struct GroupLst {
	struct	GroupLst		*next_group;
	struct	CommonFontGroups	fnt_group;
	};


/*
 ************************************************************************
 *	what driver types exists					*
 ************************************************************************
 */

#define DRIVER_TYPE_SHOWDVI	0
#define DRIVER_TYPE_DVIPRINT	1


/*
 ************************************************************************
 *	global font management						*
 ************************************************************************
 */

struct FontMan {
#ifdef OLD_NOFONT
	struct	noFont	  *noFontList;			/* fonts can't be found */
#endif
	struct	SeaLst	    s_list_flib;		/* pathes for fontlibs	*/
	struct	SeaLst	    s_list_pkdirs;		/* pathes for pk-dirs	*/
	struct	SeaLstDpi   s_list_pkdpi; 		/* pathes for pk + dpi	*/
	struct	SeaLstDpi   s_list_base_pkdpi; 		/* pathes for pk + Bdpi	*/
	struct	FontLst	   *font_list;			/* all fonts		*/
	struct	LibLst	   *flib_list;			/* all font-libraries	*/
	struct	GroupLst   *group_list;			/* all font-groups	*/
	long		    unpacked_mem;		/* mem used from chars	*/
	short		    files_left;			/* files to open	*/
	char		   *flib_fmt;			/* format-string 	*/
	char		   *pk_fmt;			/* format-string 	*/
	char		   *pkdir_fmt;			/* format-string 	*/
	char		   *basepkdir_fmt;		/* format-string 	*/
	char		   *flib_fnt_fmt;		/* format string	*/
	char		    driver_type;		/* driver-type		*/
	};

