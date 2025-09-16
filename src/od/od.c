/* od - octal, decimal, hex, ASCII dump
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on od by Andy Tanenbaum.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 *
 * License: See LICENSE.md
 */

#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>


static const char *version = "$VER: od 2.0 (16/09/25)\n";

static const char *stack_cookie = "$STACK: 4096";

/* Global variables */
static int aflag, bflag, cflag, dflag, eflag, fflag, hflag, iflag, lflag, oflag, sflag, vflag, xflag;
static int Aflag, jflag, Nflag, tflag;
static char *Aarg, *jarg, *Narg, *targ;
static long skip_bytes = 0;
static long max_bytes = -1;
static int address_base = 0; /* 0=octal, 1=decimal, 2=hex, 3=none */
static int output_formats[16];
static int num_formats = 0;
static int current_offset = 0;
static int bytes_read = 0;

/* Function prototypes */
int main(int argc, char *argv[]);
void usage(void);
int parse_options(int argc, char *argv[]);
long parse_number(char *str, char **endptr);
void dump_file(FILE *fp);
void output_address(long addr);
void output_data(unsigned char *data, int len);
void output_octal_bytes(unsigned char *data, int len);
void output_octal_shorts(unsigned char *data, int len);
void output_octal_ints(unsigned char *data, int len);
void output_decimal_shorts(unsigned char *data, int len);
void output_decimal_ints(unsigned char *data, int len);
void output_decimal_longs(unsigned char *data, int len);
void output_hex_shorts(unsigned char *data, int len);
void output_hex_ints(unsigned char *data, int len);
void output_ascii_named(unsigned char *data, int len);
void output_ascii_cstyle(unsigned char *data, int len);
void output_float_single(unsigned char *data, int len);
void output_float_double(unsigned char *data, int len);
void output_unsigned_decimal_shorts(unsigned char *data, int len);
void output_unsigned_decimal_ints(unsigned char *data, int len);
void output_signed_decimal_shorts(unsigned char *data, int len);
void output_signed_decimal_ints(unsigned char *data, int len);
void output_signed_decimal_longs(unsigned char *data, int len);
char *get_control_char_name(int c);
void print_number(long num, int base, int width);

int main(int argc, char *argv[])
{
    FILE *fp;
    int i;
    
    /* Parse command line options */
    if (parse_options(argc, argv) != 0) {
        usage();
        return 1;
    }
    
    /* Set default format if none specified */
    if (num_formats == 0) {
        output_formats[0] = 'o'; /* octal shorts */
        num_formats = 1;
    }
    
    /* Open input file or use stdin */
    if (argc > 1) {
        fp = fopen(argv[argc-1], "rb");
        if (fp == NULL) {
            fprintf(stderr, "od: cannot open %s: %s\n", argv[argc-1], strerror(errno));
            return 1;
        }
    } else {
        fp = stdin;
    }
    
    /* Skip bytes if requested */
    if (skip_bytes > 0) {
        if (fseek(fp, skip_bytes, SEEK_SET) != 0) {
            fprintf(stderr, "od: cannot seek: %s\n", strerror(errno));
            if (fp != stdin) fclose(fp);
            return 1;
        }
    }
    
    /* Dump the file */
    dump_file(fp);
    
    if (fp != stdin) {
        fclose(fp);
    }
    
    return 0;
}

void usage(void)
{
    fprintf(stderr, "Usage: od [-aBbcDdeFfHhiLlOosvXx] [-A base] [-j skip] [-N length] [-t type]\n");
    fprintf(stderr, "           [[+]offset[.][Bb]] [file ...]\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  -A base    Address base: d=decimal, o=octal, x=hex, n=none\n");
    fprintf(stderr, "  -a         Named characters (ASCII)\n");
    fprintf(stderr, "  -b         Octal bytes\n");
    fprintf(stderr, "  -c         C-style escaped characters\n");
    fprintf(stderr, "  -d         Unsigned decimal shorts\n");
    fprintf(stderr, "  -e         Double-precision floating point\n");
    fprintf(stderr, "  -f         Single-precision floating point\n");
    fprintf(stderr, "  -h         Hexadecimal shorts\n");
    fprintf(stderr, "  -i         Signed decimal ints\n");
    fprintf(stderr, "  -j skip    Skip skip bytes before dumping\n");
    fprintf(stderr, "  -l         Signed decimal longs\n");
    fprintf(stderr, "  -N length  Dump at most length bytes\n");
    fprintf(stderr, "  -o         Octal shorts\n");
    fprintf(stderr, "  -s         Signed decimal shorts\n");
    fprintf(stderr, "  -t type    Output format type\n");
    fprintf(stderr, "  -v         Verbose (no duplicate suppression)\n");
    fprintf(stderr, "  -x         Hexadecimal shorts\n");
}

int parse_options(int argc, char *argv[])
{
    int i, j;
    char *p;
    
    for (i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            p = &argv[i][1];
            while (*p) {
                switch (*p) {
                    case 'A':
                        if (p[1] != '\0') {
                            Aarg = &p[1];
                        } else if (i + 1 < argc) {
                            Aarg = argv[++i];
                        } else {
                            fprintf(stderr, "od: -A requires an argument\n");
                            return -1;
                        }
                        switch (Aarg[0]) {
                            case 'd': address_base = 1; break;
                            case 'o': address_base = 0; break;
                            case 'x': address_base = 2; break;
                            case 'n': address_base = 3; break;
                            default:
                                fprintf(stderr, "od: invalid address base '%c'\n", Aarg[0]);
                                return -1;
                        }
                        p += strlen(p);
                        break;
                    case 'a':
                        aflag = 1;
                        output_formats[num_formats++] = 'a';
                        break;
                    case 'b':
                        bflag = 1;
                        output_formats[num_formats++] = 'b';
                        break;
                    case 'c':
                        cflag = 1;
                        output_formats[num_formats++] = 'c';
                        break;
                    case 'd':
                        dflag = 1;
                        output_formats[num_formats++] = 'd';
                        break;
                    case 'e':
                        eflag = 1;
                        output_formats[num_formats++] = 'e';
                        break;
                    case 'f':
                        fflag = 1;
                        output_formats[num_formats++] = 'f';
                        break;
                    case 'h':
                        hflag = 1;
                        output_formats[num_formats++] = 'h';
                        break;
                    case 'i':
                        iflag = 1;
                        output_formats[num_formats++] = 'i';
                        break;
                    case 'j':
                        if (p[1] != '\0') {
                            jarg = &p[1];
                        } else if (i + 1 < argc) {
                            jarg = argv[++i];
                        } else {
                            fprintf(stderr, "od: -j requires an argument\n");
                            return -1;
                        }
                        skip_bytes = parse_number(jarg, NULL);
                        p += strlen(p);
                        break;
                    case 'l':
                        lflag = 1;
                        output_formats[num_formats++] = 'l';
                        break;
                    case 'N':
                        if (p[1] != '\0') {
                            Narg = &p[1];
                        } else if (i + 1 < argc) {
                            Narg = argv[++i];
                        } else {
                            fprintf(stderr, "od: -N requires an argument\n");
                            return -1;
                        }
                        max_bytes = parse_number(Narg, NULL);
                        p += strlen(p);
                        break;
                    case 'o':
                        oflag = 1;
                        output_formats[num_formats++] = 'o';
                        break;
                    case 's':
                        sflag = 1;
                        output_formats[num_formats++] = 's';
                        break;
                    case 't':
                        if (p[1] != '\0') {
                            targ = &p[1];
                        } else if (i + 1 < argc) {
                            targ = argv[++i];
                        } else {
                            fprintf(stderr, "od: -t requires an argument\n");
                            return -1;
                        }
                        /* Parse type specification */
                        for (j = 0; targ[j]; j++) {
                            if (strchr("abcdeflosux", targ[j])) {
                                output_formats[num_formats++] = targ[j];
                            }
                        }
                        p += strlen(p);
                        break;
                    case 'v':
                        vflag = 1;
                        break;
                    case 'x':
                        xflag = 1;
                        output_formats[num_formats++] = 'x';
                        break;
                    default:
                        fprintf(stderr, "od: unknown option '%c'\n", *p);
                        return -1;
                }
                if (*p) p++;
            }
        } else if (argv[i][0] == '+') {
            /* Offset specification */
            skip_bytes = parse_number(&argv[i][1], NULL);
        }
    }
    
    return 0;
}

long parse_number(char *str, char **endptr)
{
    long val = 0;
    int base = 10;
    char *p = str;
    
    /* Check for octal */
    if (*p == '0' && (p[1] == 'x' || p[1] == 'X')) {
        base = 16;
        p += 2;
    } else if (*p == '0') {
        base = 8;
        p++;
    }
    
    while (*p) {
        int digit;
        if (*p >= '0' && *p <= '9') {
            digit = *p - '0';
        } else if (*p >= 'a' && *p <= 'f') {
            digit = *p - 'a' + 10;
        } else if (*p >= 'A' && *p <= 'F') {
            digit = *p - 'A' + 10;
        } else {
            break;
        }
        
        if (digit >= base) break;
        
        val = val * base + digit;
        p++;
    }
    
    if (endptr) *endptr = p;
    return val;
}

void dump_file(FILE *fp)
{
    unsigned char buffer[16];
    int bytes_read_this_time;
    int i;
    
    while ((bytes_read_this_time = fread(buffer, 1, 16, fp)) > 0) {
        if (max_bytes > 0 && bytes_read >= max_bytes) break;
        
        /* Output address */
        if (address_base != 3) {
            output_address(current_offset);
        }
        
        /* Output data in requested formats */
        for (i = 0; i < num_formats; i++) {
            if (i > 0) printf("       ");
            output_data(buffer, bytes_read_this_time);
            printf("\n");
        }
        
        current_offset += bytes_read_this_time;
        bytes_read += bytes_read_this_time;
        
        if (max_bytes > 0 && bytes_read >= max_bytes) break;
    }
}

void output_address(long addr)
{
    switch (address_base) {
        case 0: /* octal */
            printf("%07lo", addr);
            break;
        case 1: /* decimal */
            printf("%07ld", addr);
            break;
        case 2: /* hex */
            printf("%07lx", addr);
            break;
    }
    printf(" ");
}

void output_data(unsigned char *data, int len)
{
    int i;
    
    for (i = 0; i < num_formats; i++) {
        switch (output_formats[i]) {
            case 'a': output_ascii_named(data, len); break;
            case 'b': output_octal_bytes(data, len); break;
            case 'c': output_ascii_cstyle(data, len); break;
            case 'd': output_unsigned_decimal_shorts(data, len); break;
            case 'e': output_float_double(data, len); break;
            case 'f': output_float_single(data, len); break;
            case 'h': output_hex_shorts(data, len); break;
            case 'i': output_signed_decimal_ints(data, len); break;
            case 'l': output_signed_decimal_longs(data, len); break;
            case 'o': output_octal_shorts(data, len); break;
            case 's': output_signed_decimal_shorts(data, len); break;
            case 'x': output_hex_shorts(data, len); break;
        }
    }
}

void output_octal_bytes(unsigned char *data, int len)
{
    int i;
    for (i = 0; i < len; i++) {
        printf(" %03o", data[i]);
    }
}

void output_octal_shorts(unsigned char *data, int len)
{
    int i;
    for (i = 0; i < len; i += 2) {
        unsigned short val = data[i];
        if (i + 1 < len) {
            val |= (data[i + 1] << 8);
        }
        printf(" %06o", val);
    }
}

void output_octal_ints(unsigned char *data, int len)
{
    int i;
    for (i = 0; i < len; i += 4) {
        unsigned int val = data[i];
        if (i + 1 < len) val |= (data[i + 1] << 8);
        if (i + 2 < len) val |= (data[i + 2] << 16);
        if (i + 3 < len) val |= (data[i + 3] << 24);
        printf(" %011o", val);
    }
}

void output_unsigned_decimal_shorts(unsigned char *data, int len)
{
    int i;
    for (i = 0; i < len; i += 2) {
        unsigned short val = data[i];
        if (i + 1 < len) {
            val |= (data[i + 1] << 8);
        }
        printf(" %5u", val);
    }
}

void output_unsigned_decimal_ints(unsigned char *data, int len)
{
    int i;
    for (i = 0; i < len; i += 4) {
        unsigned int val = data[i];
        if (i + 1 < len) val |= (data[i + 1] << 8);
        if (i + 2 < len) val |= (data[i + 2] << 16);
        if (i + 3 < len) val |= (data[i + 3] << 24);
        printf(" %10u", val);
    }
}

void output_signed_decimal_shorts(unsigned char *data, int len)
{
    int i;
    for (i = 0; i < len; i += 2) {
        short val = data[i];
        if (i + 1 < len) {
            val |= (data[i + 1] << 8);
        }
        printf(" %5d", val);
    }
}

void output_signed_decimal_ints(unsigned char *data, int len)
{
    int i;
    for (i = 0; i < len; i += 4) {
        int val = data[i];
        if (i + 1 < len) val |= (data[i + 1] << 8);
        if (i + 2 < len) val |= (data[i + 2] << 16);
        if (i + 3 < len) val |= (data[i + 3] << 24);
        printf(" %10d", val);
    }
}

void output_signed_decimal_longs(unsigned char *data, int len)
{
    int i;
    for (i = 0; i < len; i += 4) {
        long val = data[i];
        if (i + 1 < len) val |= (data[i + 1] << 8);
        if (i + 2 < len) val |= (data[i + 2] << 16);
        if (i + 3 < len) val |= (data[i + 3] << 24);
        printf(" %10ld", val);
    }
}

void output_hex_shorts(unsigned char *data, int len)
{
    int i;
    for (i = 0; i < len; i += 2) {
        unsigned short val = data[i];
        if (i + 1 < len) {
            val |= (data[i + 1] << 8);
        }
        printf(" %04x", val);
    }
}

void output_hex_ints(unsigned char *data, int len)
{
    int i;
    for (i = 0; i < len; i += 4) {
        unsigned int val = data[i];
        if (i + 1 < len) val |= (data[i + 1] << 8);
        if (i + 2 < len) val |= (data[i + 2] << 16);
        if (i + 3 < len) val |= (data[i + 3] << 24);
        printf(" %08x", val);
    }
}

void output_ascii_named(unsigned char *data, int len)
{
    int i;
    for (i = 0; i < len; i++) {
        char *name = get_control_char_name(data[i]);
        if (name) {
            printf(" %3s", name);
        } else {
            printf(" %3c", data[i]);
        }
    }
}

void output_ascii_cstyle(unsigned char *data, int len)
{
    int i;
    for (i = 0; i < len; i++) {
        unsigned char c = data[i];
        if (c == 0) {
            printf("  \\0");
        } else if (c == '\a') {
            printf("  \\a");
        } else if (c == '\b') {
            printf("  \\b");
        } else if (c == '\f') {
            printf("  \\f");
        } else if (c == '\n') {
            printf("  \\n");
        } else if (c == '\r') {
            printf("  \\r");
        } else if (c == '\t') {
            printf("  \\t");
        } else if (c == '\v') {
            printf("  \\v");
        } else if (c >= ' ' && c < 127) {
            printf("   %c", c);
        } else {
            printf(" %03o", c);
        }
    }
}

void output_float_single(unsigned char *data, int len)
{
    int i;
    for (i = 0; i < len; i += 4) {
        if (i + 3 < len) {
            float val;
            memcpy(&val, &data[i], sizeof(float));
            printf(" %13.6e", val);
        }
    }
}

void output_float_double(unsigned char *data, int len)
{
    int i;
    for (i = 0; i < len; i += 8) {
        if (i + 7 < len) {
            double val;
            memcpy(&val, &data[i], sizeof(double));
            printf(" %22.15e", val);
        }
    }
}

char *get_control_char_name(int c)
{
    static char *names[] = {
        "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
        "BS ", "HT ", "NL ", "VT ", "FF ", "CR ", "SO ", "SI ",
        "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
        "CAN", "EM ", "SUB", "ESC", "FS ", "GS ", "RS ", "US ",
        "SP "
    };
    
    if (c >= 0 && c <= 32) {
        return names[c];
    } else if (c == 127) {
        return "DEL";
    }
    return NULL;
}

void print_number(long num, int base, int width)
{
    char buffer[32];
    int i = 0;
    int digits = 0;
    
    if (num == 0) {
        buffer[i++] = '0';
        digits = 1;
    } else {
        while (num > 0) {
            int digit = num % base;
            buffer[i++] = (digit < 10) ? ('0' + digit) : ('a' + digit - 10);
            num /= base;
            digits++;
        }
    }
    
    /* Pad with leading zeros */
    while (digits < width) {
        printf("0");
        digits++;
    }
    
    /* Print digits in reverse order */
    while (i > 0) {
        printf("%c", buffer[--i]);
    }
}