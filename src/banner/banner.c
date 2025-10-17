/*
 * banner - print large ASCII art banner
 * 
 * This program prints large ASCII art banners using a 5-line font.

 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Amiga-specific includes */
#include <proto/dos.h>
#include <dos/dos.h>
#include <dos/rdargs.h>

#include "common.h"
#include "getopt.h"

/* Version tag for Amiga */
static const char *verstag = "$VER: banner 3.0 (17/10/25)\n";

static const char *stack_cookie = "$STACK: 4096";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* For ReadArgs template */
enum {
    ARG_TEXT,
    ARG_COLOR,
    ARG_WIDTH,
    ARG_POSIX,
    ARG_COUNT
};

/* ANSI C compliant function prototypes */
static void print_usage(const char *progname);
static void print_banner_line(const char *text, int line, int max_width);
static void print_banner_text(const char *text, int max_width, int use_color);
static int get_char_width(int ch);
static void print_help(void);
static void print_version(void);
static void parse_getopt_args(int argc, char **argv, int *use_color, int *max_width, int *file_start, const char *program);
static int run_banner_logic(int use_color, int max_width, int file_count, char **files, const char *program);

/* Font data - 5 lines per character, up to 6 characters wide */
static char img[5*256][6]=
{	"","","","","",		"","","","","",		"","","","","",		"","","","","",
	"","","","","",		"","","","","",		"","","","","",		"","","","","",
	"","","","","",		"","","","","",		"","","","","",		"","","","","",
	"","","","","",		"","","","","",		"","","","","",		"","","","","",

	"","","","","",		"","","","","",		"","","","","",		"","","","","",
	"","","","","",		"","","","","",		"","","","","",		"","","","","",
	"","","","","",		"","","","","",		"","","","","",		"","","","","",
	"","","","","",		"","","","","",		"","","","","",		"","","","","",

	"   ",
	"   ",
	"   ",
	"   ",
	"   ",		/*   */

	"___",
	"\\ /",
	" V ",
	" o ",
	"   ",
			/* ! */
	",,",
	"  ",
	"  ",
	"  ",
	"  ",		/* " */

	"    ",
	"_||_",
	"_||_",
	" || ",
	"    ",	/* # */

	" |_ ",
	"(|_ ",
	" | |",
	"-|-'",
	"    ",	/* $ */

	"   ",
	"O /",
	" / ",
	"/ O",
	"   ",		/* % */

	"    ",
	" O  ",
	"/ \\/",
	"\\_/\\",
	"    ",/* & */

	" ",
	"/",
	" ",
	" ",
	" ",		/* ' */

	"  .",
	" / ",
	"(  ",
	" \\ ",
	"  `",		/* ( */

	".  ",
	" \\ ",
	"  )",
	" / ",
	"'  ",		/* ) */

	"  ",
	"  ",
	"* ",
	"  ",
	"  ",		/* * */

	"   ",
	" . ",
	"-|-",
	" ` ",
	"   ",	/* + */

	" ",
	" ",
	" ",
	"O",
	"'",		/* , */

	"  ",
	"  ",
	"--",
	"  ",
	"  ",		/* - */

	" ",
	" ",
	" ",
	"o",
	" ",
			/* . */
	"   ",
	"  /",
	" / ",
	"/  ",
	"   ",
			/* / */
	"  _  ",
	" / \\ ",
	"( / )",
	" \\_/ ",
	"     ",	/* 0 */

	" .",
	"/|",
	" |",
	" |",
	"  ",		/* 1 */

	" _ ",
	"' )",
	" / ",
	"/__",
	"   ",		/* 2 */

	" _ ",
	"' )",
	" < ",
	"._)",
	"   ",		/* 3 */

	"  . ",
	" /| ",
	"/_|_",
	"  | ",
	"    ",		/* 4 */

	" __ ",
	"|  `",
	"`--.",
	"(__|",
	"    ",		/* 5 */

	" __ ",
	"|  `",
	"|--.",
	"|__|",
	"    ",		/* 6 */

	"___",
	"  /",
	" / ",
	"/  ",
	"   ",		/* 7 */

	" _ ",
	"(_)",
	"/ \\",
	"\\_/",
	"   ",		/* 8 */

	" _ ",
	"(_)",
	" / ",
	"/  ",
	"   ",		/* 9 */

	" ",
	" ",
	"o",
	"o",
	" ",		/* : */

	" ",
	" ",
	"o",
	"q",
	" ",		/* ; */

	"  ",
	" /",
	"< ",
	" \\",
	"  ",		/* < */

	"  ",
	"  ",
	"--",
	"--",
	"  ",		/* = */

	"  ",
	"\\ ",
	" >",
	"/ ",
	"  ",		/* > */

	" __ ",
	"(  )",
	"  / ",
	" o  ",
	"    ",		/* ? */

	" ___ ",
	"| _ |",
	"|(_||",
	"|___ ",
	"     ",	/* @ */

	"   .",
	"  /|",
	" /-|",
	"/  |",
	"    ",		/* A */

	" __ ",
	"|__)",
	"|  \\",
	"|__/",
	"    ",		/* B */

	"  _ ",
	" / \\",
	"(   ",
	" \\_/",
	"    ",		/* C */

	" _  ",
	"| \\ ",
	"|  )",
	"|_/ ",
	"    ",		/* D */

	" __",
	"|  ",
	"|- ",
	"|__",
	"   ",		/* E */

	" __",
	"|  ",
	"|- ",
	"|  ",
	"   ",		/* F */

	"  _ ",
	" / \\",
	"( __",
	" \\_/",
	"    ",		/* G */

	".  .",
	"|  |",
	"|--|",
	"|  |",
	"    ",		/* H */

	".",
	"|",
	"|",
	"|",
	" ",		/* I */

	"  .",
	"  |",
	"  |",
	"(_)",
	"   ",		/* J */

	".  ",
	"| /",
	"|< ",
	"| \\",
	"   ",		/* K */

	".  ",
	"|  ",
	"|  ",
	"|__",
	"   ",		/* L */

	".  .",
	"|\\/|",
	"|  |",
	"|  |",
	"    ",		/* M */

	".  .",
	"|\\ |",
	"| \\|",
	"|  |",
	"    ",		/* N */

	"  _  ",
	" / \\ ",
	"(   )",
	" \\_/ ",
	"     ",	/* O */

	" __ ",
	"|  |",
	"|--'",
	"|   ",
	"    ",		/* P */

	"  _  ",
	" / \\ ",
	"(   )",
	" \\_\\ ",
	"     ",	/* Q */

	" __ ",
	"|  |",
	"|--'",
	"| \\ ",
	"    ",		/* R */

	" __",
	"(  ",
	" \\ ",
	"__)",
	"   ",		/* S */

	"___",
	" | ",
	" | ",
	" | ",
	"   ",		/* T */

	".  .",
	"|  |",
	"|  |",
	"|__|",
	"    ",		/* U */

	".  .",
	"|  |",
	"|  |",
	" \\/ ",
	"    ",		/* V */

	".  .",
	"|  |",
	"|  |",
	"|/\\|",
	"    ",		/* W */

	"   ",
	"\\ /",
	" X ",
	"/ \\",
	"   ",		/* X */

	"   ",
	"\\ /",
	" Y ",
	" | ",
	"   ",		/* Y */

	"___",
	"  /",
	" / ",
	"/__",
	"   ",		/* Z */

	" .-",
	" | ",
	" | ",
	" | ",
	" `-",		/* [ */

	"   ",
	"\\  ",
	" \\ ",
	"  \\",
	"   ",		/* \\ */

	"-. ",
	" | ",
	" | ",
	" | ",
	"-' ",		/* ] */

	"  ",
	"/\\",
	"  ",
	"  ",
	"  ",		/* ^ */

	"   ",
	"   ",
	"   ",
	"___",
	"   ",		/* _ */

	" ",
	"\\",
	" ",
	" ",
	" ",		/* ` */

	"   ",
	" _ ",
	"'_|",
	"(_|",
	"   ",		/* a */

	"   ",
	"|  ",
	"|-.",
	"|_|",
	"   ",		/* b */

	"   ",
	" _ ",
	"| `",
	"|_,",
	"   ",		/* c */

	"   ",
	"  |",
	".-|",
	"|_|",
	"   ",		/* d */

	"   ",
	" _ ",
	"|_)",
	"|_,",
	"   ",		/* e */

	"  _",
	" | ",
	"-|-",
	" | ",
	"-' ",		/* f */

	"   ",
	" _ ",
	"| |",
	"|_|",
	"._|",		/* g */

	".  ",
	"|_ ",
	"| |",
	"| |",
	"   ",		/* h */

	" ",
	".",
	"|",
	"|",
	" ",		/* i */

	"   ",
	"  .",
	"  |",
	"  |",
	"\\_|",		/* j */

	"  ",
	"| ",
	"|/",
	"|\\",
	"  ",		/* k */

	"  ",
	"| ",
	"| ",
	"|_",
	"  ",		/* l */

	"   ",
	". .",
	"|V|",
	"|||",
	"   ",		/* m */

	"   ",
	"._ ",
	"| |",
	"| |",
	"   ",		/* n */

	"   ",
	" _ ",
	"/ \\",
	"\\_/",
	"   ",		/* o */

	"   ",
	" _ ",
	"| \\",
	"|_/",
	"|  ",		/* p */

	"   ",
	" _ ",
	"| |",
	"|_|",
	"  |",		/* q */

	"   ",
	"._ ",
	"| `",
	"|  ",
	"   ",		/* r */

	"   ",
	" _ ",
	"(_`",
	"._)",
	"   ",		/* s */

	" . ",
	"-+-",
	" | ",
	" |_",
	"   ",		/* t */

	"   ",
	"   ",
	"| |",
	"|_|",
	"   ",		/* u */

	"   ",
	"   ",
	"\\ /",
	" V ",
	"   ",		/* v */

	"    ",
	"    ",
	"|  |",
	"|/\\|",
	"    ",		/* w */

	"  ",
	"  ",
	"\\/",
	"/\\",
	"  ",		/* x */

	"   ",
	"   ",
	"| |",
	"|_|",
	"._|",		/* y */

	"  ",
	"__",
	" /",
	"/_",
	"  ",		/* z */

	" .-",
	" | ",
	" < ",
	" | ",
	" `-",		/* { */

	"| ",
	"| ",
	"| ",
	"| ",
	"| ",		/* | */

	"-. ",
	" | ",
	" > ",
	" | ",
	"-'  ",		/* } */

	"   ",
	"/\\/",
	"   ",
	"   ",
	"   ",		/* ~ */

	"_-_-",
	"_-_-",
	"_-_-",
	"_-_-",
	"_-_-",		/* DEL */

	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	"+-+",
	"| |",
	"| |",
	"| |",
	"+-+",
	

	" ",
	" ",
	" ",
	" ",
	" ",
			/*  */
	" o ",
	" A ",
	"/ \\",
	"~~~",
	"   ",	/* � */

	"   ",
	" ,_",
	"(__",
	" ' ",
	"   ",	/* � */

	"  _",
	" | ",
	"-+-",
	"_|_",
	" � ",	/* � */

	"\\_/",
	"(_)",
	"/ \\",
	"   ",
	"   ",	/* � */

	"   ",
	"\\ /",
	"_Y_",
	" | ",
	"   ",	/* � */

	"  ",
	"| ",
	"  ",
	"| ",
	"  ",		/* � */

	" _ ",
	"(_`",
	"(_)",
	"._)",
	"   ",		/* � */

	"- -",
	"   ",
	"   ",
	"   ",
	"   ",		/* � */

	" ___ ",
	"| _ |",
	"|/ `|",
	"|\\_,|",
	"`---'",	/* � */

	"   ",
	" _ ",
	"(_|",
	"___",
	"   ",		/* � */

	"  ",
	"  ",
	"//",
	"\\\\",
	"  ",		/* � */

	"__ ",
	"  `",
	"   ",
	"   ",
	"   ",		/* � */

	"   ",
	"   ",
	"___",
	"   ",
	"   ",		/* - */

	" ___ ",
	"| _ |",
	"||_)|",
	"|| \\|",
	"`---'",	/* � */

	"   ",
	"---",
	"   ",
	"   ",
	"   ",		/* � */

	" _ ",
	"(_)",
	"   ",
	"   ",
	"   ",		/* � */

	"   ",
	"_|_",
	" | ",
	"---",
	"   ",		/* � */

	" _ ",
	"'_)",
	"(__",
	"   ",
	"   ",		/* � */

	" _ ",
	"'_)",
	"._)",
	"   ",
	"   ",		/* � */

	" ",
	"/",
	" ",
	" ",
	" ",		/* ' */

	"   ",
	"   ",
	"| |",
	"|_|",
	"|  ",		/* � */

	" __",
	"/ H",
	"\\_H",
	"  H",
	"   ",		/* � */

	"  ",
	"  ",
	". ",
	"  ",
	"  ",		/* � */

	"  ",
	"  ",
	"  ",
	"  ",
	", ",		/* , */

	" .",
	"/|",
	" |",
	"  ",
	"  ",		/* � */

	"   ",
	" _ ",
	"(_)",
	"___",
	"   ",		/* � */

	"  ",
	"  ",
	"\\\\",
	"//",
	"  ",		/* � */

	"   ",
	"1 /",
	" / ",
	"/ 4",
	"   ",	/* � */

	"   ",
	"1 /",
	" / ",
	"/ 2",
	"   ",	/* � */

	"   ",
	"3 /",
	" / ",
	"/ 4",
	"   ",	/* � */

	"    ",
	"  o ",
	" /  ",
	"(__)",
	"    ",	/* � */

	"   \\",
	"  /|",
	" /-|",
	"/  |",
	"    ",		/* � */

	"   /",
	"  /|",
	" /-|",
	"/  |",
	"    ",		/* � */

	"  /\\",
	"  /|",
	" /-|",
	"/  |",
	"    ",		/* � */

	" /\\/",
	"  /|",
	" /-|",
	"/  |",
	"    ",		/* � */

	"  oo",
	"  /|",
	" /-|",
	"/  |",
	"    ",		/* � */

	"   O",
	"  /|",
	" /-|",
	"/  |",
	"    ",		/* � */

	" ___",
	"| | ",
	"|-+-",
	"| |_",
	"     ",	/* � */

	"  _ ",
	" / \\",
	"(   ",
	" \\_/",
	"  / ",	/* � */

	" _\\_",
	"|   ",
	"|-- ",
	"|___",
	"    ",		/* � */

	" _/_",
	"|   ",
	"|-- ",
	"|___",
	"    ",		/* � */

	" /\\_",
	"|   ",
	"|-- ",
	"|___",
	"    ",		/* � */

	" o_o",
	"|   ",
	"|-- ",
	"|___",
	"    ",		/* � */

	"\\",
	"|",
	"|",
	"|",
	" ",		/* � */

	"/",
	"|",
	"|",
	"|",
	" ",		/* � */

	"^",
	"|",
	"|",
	"|",
	" ",		/* � */

	"-",
	"|",
	"|",
	"|",
	" ",		/* � */

	" _  ",
	"| \\ ",
	"+  )",
	"|_/ ",
	"    ",		/* � */

	"./\\/",
	"|\\ |",
	"| \\|",
	"|  |",
	"    ",		/* � */

	"  \\  ",
	" / \\ ",
	"(   )",
	" \\_/ ",
	"     ",	/* � */

	"  /  ",
	" / \\ ",
	"(   )",
	" \\_/ ",
	"     ",	/* � */

	"  ^  ",
	" / \\ ",
	"(   )",
	" \\_/ ",
	"     ",	/* � */

	" /\\/ ",
	" / \\ ",
	"(   )",
	" \\_/ ",
	"     ",	/* � */

	" o_o ",
	" / \\ ",
	"(   )",
	" \\_/ ",
	"     ",	/* � */

	"  ",
	"  ",
	"\\/",
	"/\\",
	"   ",	/* � */

	" __/",
	"| /|",
	"|/ |",
	"/--'",
	"    ",	/* � */

	" \\  ",
	"|  |",
	"|  |",
	"|__|",
	"    ",		/* � */

	"  / ",
	"|  |",
	"|  |",
	"|__|",
	"    ",		/* � */

	" /\\ ",
	"|  |",
	"|  |",
	"|__|",
	"    ",		/* � */

	" oo ",
	"|  |",
	"|  |",
	"|__|",
	"    ",		/* � */

	" / ",
	"\\ /",
	" Y ",
	" | ",
	"   ",	/* � */

	"   ",
	"|_ ",
	"|_)",
	"|  ",
	"   ",	/* � */

	"  ",
	" _",
	"|_)",
	"|_)",
	"| ",	/* � */

	" \\ ",
	" _ ",
	"'_|",
	"(_|",
	"   ",		/* � */

	" / ",
	" _ ",
	"'_|",
	"(_|",
	"   ",		/* � */

	" ^ ",
	" _ ",
	"'_|",
	"(_|",
	"   ",		/* � */

	"/\\/",
	" _ ",
	"'_|",
	"(_|",
	"   ",		/* � */

	"   ",
	"o_o",
	"'_|",
	"(_|",
	"   ",		/* � */

	" O ",
	" _ ",
	"'_|",
	"(_|",
	"   ",		/* � */

	"     ",
	" _ _ ",
	"'_|_)",
	"(_|_,",
	"     ",		/* � */

	"   ",
	" _ ",
	"| `",
	"|_,",
	" ' ",	/* � */

	" \\ ",
	" _ ",
	"|_)",
	"|_,",
	"   ",		/* � */

	" / ",
	" _ ",
	"|_)",
	"|_,",
	"   ",		/* � */

	" ^ ",
	" _ ",
	"|_)",
	"|_,",
	"   ",		/* � */

	"   ",
	"o_o",
	"|_)",
	"|_,",
	"   ",		/* � */

	"\\",
	" ",
	"|",
	"|",
	" ",		/* � */

	"/",
	" ",
	"|",
	"|",
	" ",		/* � */

	"^ ",
	" ",
	"|",
	"|",
	" ",		/* � */

	"_",
	" ",
	"|",
	"|",
	" ",		/* � */

	"\\/ ",
	"/\\ ",
	"/�\\",
	"\\_/",
	"   ",	/* � */

	"/\\/",
	"._ ",
	"| |",
	"| |",
	"   ",	/* � */

	" \\ ",
	" _ ",
	"/ \\",
	"\\_/",
	"   ",		/* � */

	" / ",
	" _ ",
	"/ \\",
	"\\_/",
	"   ",		/* � */

	" ^ ",
	" _ ",
	"/ \\",
	"\\_/",
	"   ",		/* � */

	"/\\/",
	" _ ",
	"/ \\",
	"\\_/",
	"   ",		/* � */

	"   ",
	"o_o",
	"/ \\",
	"\\_/",
	"   ",		/* � */

	"   ",
	" O ",
	"---",
	" O ",
	"   ",	/* � */

	"   ",
	" _/",
	"|/|",
	"/-'",
	"   ",	/* � */

	" \\ ",
	"   ",
	"| |",
	"|_|",
	"   ",		/* � */

	" / ",
	"   ",
	"| |",
	"|_|",
	"   ",		/* � */

	" ^ ",
	"   ",
	"| |",
	"|_|",
	"   ",		/* � */

	"   ",
	"o o",
	"| |",
	"|_|",
	"   ",		/* � */

	"  ",
	" /",
	"\\/",
	"/ ",
	"  ",	/* � */

	"  ",
	"| ",
	"|)",
	"| ",
	"  ",	/* � */

	"  ",
	"oo",
	"\\/",
	"/  ",
	"   "	/* � */
};

/* ANSI escape codes for colors */
#define COLOR_RESET   "\033[0m"
#define COLOR_RED     "\033[31m"
#define COLOR_GREEN   "\033[32m"
#define COLOR_YELLOW  "\033[33m"
#define COLOR_BLUE    "\033[34m"
#define COLOR_MAGENTA "\033[35m"
#define COLOR_CYAN    "\033[36m"
#define COLOR_WHITE   "\033[37m"
#define COLOR_BRIGHT  "\033[1m"

/* Default values */
#define DEFAULT_WIDTH 80
#define MIN_WIDTH 20
#define MAX_WIDTH 200

int main(int argc, char *argv[])
{
    int max_width = DEFAULT_WIDTH;
    int use_color = 0;
    int file_start = 1;
    char **files = NULL;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "TEXT/M,COLOR/S,WIDTH/K/N,POSIX/K/F";
    LONG arg_array[ARG_COUNT] = {0};
    struct RDArgs *rdargs = NULL;
    char *cmd_string = NULL;
    int ret_code = SUCCESS;
    BOOL interactive_help = FALSE;
    
    /* POSIX/F Path Variables */
    char *posix_str;
    int new_argc;
    char *new_argv[MAX_TEMPLATE_ITEMS];
    int i;
    char initial_args_str[256];
    char user_input_buf[256];
    char *temp_str;
    size_t combined_len;

    if (argc < 1) {
        exit(FAILURE);
    }
    
    program = my_basename(argv[0]);

    if (argc == 1) {
        /* No arguments, read from stdin with default settings */
        return run_banner_logic(use_color, max_width, 0, NULL, program);
    }

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        parse_getopt_args(argc, argv, &use_color, &max_width, &file_start, program);
        files = &argv[file_start];
        return run_banner_logic(use_color, max_width, argc - file_start, files, program);
        
    } else {
        /* --- READARGS PATH --- */
        for (i = 1; i < argc; i++) {
            if (strcmp(argv[i], "?") == 0) {
                interactive_help = TRUE;
                break;
            }
        }

        rdargs = AllocDosObject(DOS_RDARGS, NULL);
        if (!rdargs) {
            fprintf(stderr, "%s: out of memory for RDArgs\n", program);
            return FAILURE;
        }

        if (interactive_help) {
            /* Initialize buffers */
            initial_args_str[0] = '\0';
            user_input_buf[0] = '\0';

            /* Build a string from any args that are NOT '?' */
            temp_str = build_command_string(argc, argv, "?");
            if (temp_str) {
                strncpy(initial_args_str, temp_str, 255);
                free(temp_str);
            }

            /* Print template and prompt for more input */
            printf("%s: ", template);
            fflush(stdout);
            if (fgets(user_input_buf, sizeof(user_input_buf), stdin)) {
                /* Combine initial args with the new user input */
                combined_len = strlen(initial_args_str) + strlen(user_input_buf) + 2;
                cmd_string = malloc(combined_len);
                if (cmd_string) {
                    strcpy(cmd_string, initial_args_str);
                    if (initial_args_str[0] != '\0' && user_input_buf[0] != '\n') {
                        strcat(cmd_string, " ");
                    }
                    strcat(cmd_string, user_input_buf);
                }
            } else {
                cmd_string = strdup(initial_args_str);
                if (cmd_string) strcat(cmd_string, "\n");
            }
        } else {
            /* Standard case: build command string from all args */
            cmd_string = build_command_string(argc, argv, NULL);
        }

        if (!cmd_string) {
            fprintf(stderr, "%s: out of memory for command string\n", program);
            FreeDosObject(DOS_RDARGS, rdargs);
            return FAILURE;
        }

        /* Set up ReadArgs to parse from our string */
        rdargs->RDA_Source.CS_Buffer = cmd_string;
        rdargs->RDA_Source.CS_Length = strlen(cmd_string);
        rdargs->RDA_Source.CS_CurChr = 0;
        rdargs->RDA_Flags |= RDAF_NOPROMPT;

        if (!ReadArgs(template, arg_array, rdargs)) {
            PrintFault(IoErr(), program);
            ret_code = FAILURE;
        } else {
            /* Check for POSIX/F override first */
            if (arg_array[ARG_POSIX]) {
                posix_str = (char *)arg_array[ARG_POSIX];

                /* Tokenize the string and build a new argv for getopt */
                new_argv[0] = program;
                new_argc = tokenize_string(posix_str, &new_argv[1], MAX_TEMPLATE_ITEMS - 1) + 1;

                parse_getopt_args(new_argc, new_argv, &use_color, &max_width, &file_start, program);
                files = &new_argv[file_start];
                ret_code = run_banner_logic(use_color, max_width, new_argc - file_start, files, program);

            } else {
                /* Standard ReadArgs processing */
                if (arg_array[ARG_COLOR]) {
                    use_color = 1;
                }
                if (arg_array[ARG_WIDTH]) {
                    max_width = *(LONG *)arg_array[ARG_WIDTH];
                    if (max_width < MIN_WIDTH) max_width = MIN_WIDTH;
                    if (max_width > MAX_WIDTH) max_width = MAX_WIDTH;
                }
                
                if (arg_array[ARG_TEXT]) {
                    /* Count files and allocate array */
                    int file_count = 0;
                    while (((char **)arg_array[ARG_TEXT])[file_count] != NULL) {
                        file_count++;
                    }
                    
                    files = (char **)arg_array[ARG_TEXT];
                    ret_code = run_banner_logic(use_color, max_width, file_count, files, program);
                } else {
                    /* No files specified, read from stdin */
                    ret_code = run_banner_logic(use_color, max_width, 0, NULL, program);
                }
            }
        }

        /* Clean up */
        FreeDosObject(DOS_RDARGS, rdargs);
        free(cmd_string);
    }

    return ret_code;
}

/**
 * @brief Parse arguments using getopt (POSIX style)
 * @param argc Argument count
 * @param argv Argument vector
 * @param use_color Flag to enable color output
 * @param max_width Maximum line width
 * @param file_start Index where files start in argv
 * @param program Program name for error messages
 */
static void parse_getopt_args(int argc, char **argv, int *use_color, int *max_width, int *file_start, const char *program) {
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "cw:hV")) != -1) {
        switch (c) {
            case 'c':
                *use_color = 1;
                break;
            case 'w':
                *max_width = atoi(optarg);
                if (*max_width < MIN_WIDTH) *max_width = MIN_WIDTH;
                if (*max_width > MAX_WIDTH) *max_width = MAX_WIDTH;
                break;
            case 'h':
            case 'V':
                print_help();
                exit(SUCCESS);
                break;
            case '?':
                exit(FAILURE);
                break;
        }
    }
    
    *file_start = optind;
}

/**
 * @brief Core banner logic separated from argument parsing
 * @param use_color Flag to enable color output
 * @param max_width Maximum line width
 * @param file_count Number of files to process
 * @param files Array of file names
 * @param program Program name for error messages
 * @return Exit code
 */
static int run_banner_logic(int use_color, int max_width, int file_count, char **files, const char *program) {
    char input_text[1024] = "";
    int i;
    
    /* Check to see if input comes from std input */
    if (file_count == 0) {
        printf("Enter text for banner: ");
        fflush(stdout);
        if (fgets(input_text, sizeof(input_text), stdin) == NULL) {
            fprintf(stderr, "Error reading input\n");
            return FAILURE;
        }
        /* Remove trailing newline */
        input_text[strcspn(input_text, "\n")] = '\0';
        
        if (strlen(input_text) == 0) {
            fprintf(stderr, "Error: No text provided\n");
            return FAILURE;
        }
        
        print_banner_text(input_text, max_width, use_color);
        return SUCCESS;
    }

    /* There is an explicit list of files. Process each one */
    for (i = 0; i < file_count; i++) {
        if (strlen(input_text) > 0) {
            strcat(input_text, " ");
        }
        strncat(input_text, files[i], sizeof(input_text) - strlen(input_text) - 1);
    }
    
    if (strlen(input_text) == 0) {
        fprintf(stderr, "Error: No text provided\n");
        return FAILURE;
    }
    
    print_banner_text(input_text, max_width, use_color);
    return SUCCESS;
}

/* Print usage information */
static void print_usage(const char *progname)
{
    fprintf(stderr, "Version: %s\n", &verstag[6]);
    fprintf(stderr, "Usage (POSIX): %s [OPTIONS] [TEXT]\n", progname);
    fprintf(stderr, "Usage (Amiga): %s TEXT/M [COLOR/S] [WIDTH/K/N]\n", progname);
    fprintf(stderr, "               %s ? for template\n", progname);
    fprintf(stderr, "OPTIONS:\n");
    fprintf(stderr, "  -c          enable color output\n");
    fprintf(stderr, "  -w N        set maximum line width (default: 80, min: 20, max: 200)\n");
    fprintf(stderr, "  -h, -V      display this help and version\n");
    fprintf(stderr, "DESCRIPTION:\n");
    fprintf(stderr, "  Print large ASCII art banners using a 5-line font.\n");
    fprintf(stderr, "  With no TEXT, read from standard input.\n");
    fprintf(stderr, "  Supports full ASCII character set (0-255) including international characters.\n");
    exit(FAILURE);
}

/* Print detailed help information */
static void print_help(void)
{
    printf("banner - Print large ASCII art banners\n\n");
    printf("DESCRIPTION\n");
    printf("    banner prints large ASCII art banners using a 5-line font.\n");
    printf("    It supports the full ASCII character set (0-255) including\n");
    printf("    international characters and special symbols.\n\n");
    printf("FEATURES\n");
    printf("    - Full ASCII character support (0-255)\n");
    printf("    - Line wrapping at configurable width\n");
    printf("    - Color support via ANSI escape codes\n");
    printf("    - Input from command line or standard input\n");
    printf("    - International character support\n");
    printf("    - Hybrid POSIX/Amiga argument parsing\n\n");
    printf("EXAMPLES\n");
    printf("    %s Hello World\n", "banner");
    printf("    %s -c -w 60 \"Welcome to UNSUI\"\n", "banner");
    printf("    echo \"Multi-line\\ntext\" | %s\n", "banner");
    printf("    %s TEXT=\"Hello\" COLOR WIDTH=60\n", "banner");
    printf("    %s POSIX=\"-c -w 40 Hello World\"\n", "banner");
    printf("\n");
    print_usage("banner");
}

/* Print version information */
static void print_version(void)
{
    printf("banner 1.0.0\n");
    printf("amigazen project\n");
    printf("ANSI C compliant implementation with hybrid parsing\n");
}

/* Get the display width of a character */
static int get_char_width(int ch)
{
    int i;
    int max_width = 0;
    
    if (ch < 0 || ch > 255) return 0;
    
    for (i = 0; i < 5; i++) {
        int len = strlen(img[ch * 5 + i]);
        if (len > max_width) max_width = len;
    }
    
    return max_width;
}

/* Print a single line of the banner */
static void print_banner_line(const char *text, int line, int max_width)
{
    int i, j, pos, ch, char_width;
    char line_buffer[1024] = "";
    int current_width = 0;
    
    pos = 0;
    i = 0;
    
    while (text[i] != '\0' && current_width < max_width) {
        ch = (unsigned char)text[i];
        char_width = get_char_width(ch);
        
        /* Check if adding this character would exceed width */
        if (current_width + char_width > max_width) {
            break;
        }
        
        /* Add character to line */
        for (j = 0; j < char_width && pos < sizeof(line_buffer) - 1; j++) {
            if (j < strlen(img[ch * 5 + line])) {
                line_buffer[pos++] = img[ch * 5 + line][j];
            } else {
                line_buffer[pos++] = ' ';
            }
        }
        
        current_width += char_width;
        i++;
    }
    
    line_buffer[pos] = '\0';
    printf("%s\n", line_buffer);
}

/* Print the complete banner text */
static void print_banner_text(const char *text, int max_width, int use_color)
{
    int line;
    const char *color_code = "";
    
    if (use_color) {
        color_code = COLOR_CYAN;
    }
    
    for (line = 0; line < 5; line++) {
        if (use_color) {
            printf("%s", color_code);
        }
        print_banner_line(text, line, max_width);
        if (use_color) {
            printf("%s", COLOR_RESET);
        }
    }
}
