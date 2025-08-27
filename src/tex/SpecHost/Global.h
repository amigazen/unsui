/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heﬂmann
*/

#include "Data.h"
#include "Protos.h"
#include "asyncio.h"
#include "EVPaths.h"

#include "special.h"
#include "graphics.h"
#include "Parse.h"
#include "tpic.h"
#include "Post.h"

#define CONFIG_FILE_MAGIC	(('S'<<24) | ('C'<<16) | ('N'<<8) | 'F')
#define CONFIG_FILE_VERSION	1

struct config_struct {
    long	magic;
    long	version;
    short	use_blitter;
    short	invert_bmap;
    int		base_dpi;
    int		draw_modus;
    int		icon_x_pos;
    int		icon_y_pos;
    int		win_x_pos;
    int		win_y_pos;
    int		win_width;
    int		win_height;
    short	use_pubscr;
    short	reserved1;
    long	reserved2;
    long	reserved3;
    long	reserved4;
    long	reserved5;
    long	reserved6;
    long	reserved7;
    long	reserved8;
    long	reserved9;
  };

/***  Draw Modi ***************/

#define DRAW_IN_MEM	1
#define DRAW_FILE	2
#define DRAW_IN_MEM_B	3
#define DRAW_FILE_B	4
#define DRAW_BORDER	5
#define DRAW_RECT	6

#define BMAP_MEMF_ANY (1L << 31) /* (ghi) to store bitmap in Fast mem */

#define EVP_BUFSIZE	4096L
GLOBAL struct EnvVarPath *texconfig_var;
GLOBAL struct EnvVarPath *psheaders_var;
