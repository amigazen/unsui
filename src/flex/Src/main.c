/* flex - tool to generate fast lexical analyzers */

/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Vern Paxson.
 *
 * The United States Government has rights in this work pursuant
 * to contract no. DE-AC03-76SF00098 between the United States
 * Department of Energy and the University of California.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

/* $Header: /home/daffy/u0/vern/flex/RCS/main.c,v 2.64 96/05/25 20:42:42 vern Exp $ */


#include "flexdef.h"
#include "version.h"

static char flex_version[] = FLEX_VERSION;


/* declare functions that have forward references */

void flexinit PROTO((int, char**));
void readin PROTO((void));
void set_up_initial_allocations PROTO((void));

#ifdef NEED_ARGV_FIXUP
extern void argv_fixup PROTO((int *, char ***));
#endif


/* these globals are all defined and commented in flexdef.h */
int printstats, syntaxerror, eofseen, ddebug, trace, nowarn, spprdflt;
int interactive, caseins, lex_compat, do_yylineno, useecs, fulltbl, usemecs;
int fullspd, gen_line_dirs, performance_report, backing_up_report;
int C_plus_plus, long_align, use_read, yytext_is_array, do_yywrap, csize;
int yymore_used, reject, real_reject, continued_action, in_rule;
int yymore_really_used, reject_really_used;
int datapos, dataline, linenum, out_linenum;
FILE *skelfile = NULL;
int skel_ind = 0;
char *action_array;
int action_size, defs1_offset, prolog_offset, action_offset, action_index;
char *infilename = NULL, *outfilename = NULL;
int did_outfilename;
char *prefix, *yyclass;
int do_stdinit, use_stdout;
int onestate[ONE_STACK_SIZE], onesym[ONE_STACK_SIZE];
int onenext[ONE_STACK_SIZE], onedef[ONE_STACK_SIZE], onesp;
int current_mns, current_max_rules;
int num_rules, num_eof_rules, default_rule, lastnfa;
int *firstst, *lastst, *finalst, *transchar, *trans1, *trans2;
int *accptnum, *assoc_rule, *state_type;
int *rule_type, *rule_linenum, *rule_useful;
int current_state_type;
int variable_trailing_context_rules;
int numtemps, numprots, protprev[MSP], protnext[MSP], prottbl[MSP];
int protcomst[MSP], firstprot, lastprot, protsave[PROT_SAVE_SIZE];
int numecs, nextecm[CSIZE + 1], ecgroup[CSIZE + 1], nummecs, tecfwd[CSIZE + 1];
int tecbck[CSIZE + 1];
int lastsc, *scset, *scbol, *scxclu, *sceof;
int current_max_scs;
char **scname;
int current_max_dfa_size, current_max_xpairs;
int current_max_template_xpairs, current_max_dfas;
int lastdfa, *nxt, *chk, *tnxt;
int *base, *def, *nultrans, NUL_ec, tblend, firstfree, **dss, *dfasiz;
union dfaacc_union *dfaacc;
int *accsiz, *dhash, numas;
int numsnpairs, jambase, jamstate;
int lastccl, *cclmap, *ccllen, *cclng, cclreuse;
int current_maxccls, current_max_ccl_tbl_size;
Char *ccltbl;
char nmstr[MAXLINE];
int sectnum, nummt, hshcol, dfaeql, numeps, eps2, num_reallocs;
int tmpuses, totnst, peakpairs, numuniq, numdup, hshsave;
int num_backing_up, bol_needed;
FILE *backing_up_file;
int end_of_buffer_state;
char **input_files;
int num_input_files;

/* Make sure program_name is initialized so we don't crash if writing
 * out an error message before getting the program name from argv[0].
 */
char *program_name = "flex";

#ifndef SHORT_FILE_NAMES
static char *outfile_template = "lex.%s.%s";
static char *backing_name = "lex.backup";
#else
static char *outfile_template = "lex%s.%s";
static char *backing_name = "lex.bck";
#endif

#ifdef THINK_C
#include <console.h>
#endif

#ifdef MS_DOS
extern unsigned _stklen = 16384;
#endif

static char outfile_path[MAXLINE];
static int outfile_created = 0;
static char *skelname = NULL;


int main( argc, argv )
int argc;
char **argv;
	{
	int i;

#ifdef THINK_C
	argc = ccommand( &argv );
#endif
#ifdef NEED_ARGV_FIXUP
	argv_fixup( &argc, &argv );
#endif

	flexinit( argc, argv );

	readin();

	ntod();

	for ( i = 1; i <= num_rules; ++i )
		if ( ! rule_useful[i] && i != default_rule )
			line_warning( _( "rule cannot be matched" ),
					rule_linenum[i] );

	if ( spprdflt && ! reject && rule_useful[default_rule] )
		line_warning(
			_( "-s option given but default rule can be matched" ),
			rule_linenum[default_rule] );

	/* Generate the C state transition tables from the DFA. */
	make_tables();

	/* Note, flexend does not return.  It exits with its argument
	 * as status.
	 */
	flexend( 0 );

	return 0;	/* keep compilers/lint happy */
	}


/* check_options - check user-specified options */

void check_options()
	{
	int i;

	if ( lex_compat )
		{
		if ( C_plus_plus )
			flexerror( _( "Can't use -+ with -l option" ) );

		if ( fulltbl || fullspd )
			flexerror( _( "Can't use -f or -F with -l option" ) );

		/* Don't rely on detecting use of yymore() and REJECT,
		 * just assume they'll be used.
		 */
		yymore_really_used = reject_really_used = true;

		yytext_is_array = true;
		do_yylineno = true;
		use_read = false;
		}

	if ( do_yylineno )
		/* This should really be "maintain_backup_tables = true" */
		reject_really_used = true;

	if ( csize == unspecified )
		{
		if ( (fulltbl || fullspd) && ! useecs )
			csize = DEFAULT_CSIZE;
		else
			csize = CSIZE;
		}

	if ( interactive == unspecified )
		{
		if ( fulltbl || fullspd )
			interactive = false;
		else
			interactive = true;
		}

	if ( fulltbl || fullspd )
		{
		if ( usemecs )
			flexerror(
			_( "-Cf/-CF and -Cm don't make sense together" ) );

		if ( interactive )
			flexerror( _( "-Cf/-CF and -I are incompatible" ) );

		if ( lex_compat )
			flexerror(
		_( "-Cf/-CF are incompatible with lex-compatibility mode" ) );

		if ( do_yylineno )
			flexerror(
			_( "-Cf/-CF and %option yylineno are incompatible" ) );

		if ( fulltbl && fullspd )
			flexerror( _( "-Cf and -CF are mutually exclusive" ) );
		}

	if ( C_plus_plus && fullspd )
		flexerror( _( "Can't use -+ with -CF option" ) );

	if ( C_plus_plus && yytext_is_array )
		{
		warn( _( "%array incompatible with -+ option" ) );
		yytext_is_array = false;
		}

	if ( useecs )
		{ /* Set up doubly-linked equivalence classes. */

		/* We loop all the way up to csize, since ecgroup[csize] is
		 * the position used for NUL characters.
		 */
		ecgroup[1] = NIL;

		for ( i = 2; i <= csize; ++i )
			{
			ecgroup[i] = i - 1;
			nextecm[i - 1] = i;
			}

		nextecm[csize] = NIL;
		}

	else
		{
		/* Put everything in its own equivalence class. */
		for ( i = 1; i <= csize; ++i )
			{
			ecgroup[i] = i;
			nextecm[i] = BAD_SUBSCRIPT;	/* to catch errors */
			}
		}

	if ( ! use_stdout )
		{
		FILE *prev_stdout;

		if ( ! did_outfilename )
			{
			char *suffix;

			if ( C_plus_plus )
				suffix = "cc";
			else
				suffix = "c";

			sprintf( outfile_path, outfile_template,
				prefix, suffix );

			outfilename = outfile_path;
			}

		prev_stdout = freopen( outfilename, "w", stdout );

		if ( prev_stdout == NULL )
			lerrsf( _( "could not create %s" ), outfilename );

		outfile_created = 1;
		}

	if ( skelname && (skelfile = fopen( skelname, "r" )) == NULL )
		lerrsf( _( "can't open skeleton file %s" ), skelname );

	if ( strcmp( prefix, "yy" ) )
		{
#define GEN_PREFIX(name) out_str3( "#define yy%s %s%s\n", name, prefix, name )
		if ( C_plus_plus )
			GEN_PREFIX( "FlexLexer" );
		else
			{
			GEN_PREFIX( "_create_buffer" );
			GEN_PREFIX( "_delete_buffer" );
			GEN_PREFIX( "_scan_buffer" );
			GEN_PREFIX( "_scan_string" );
			GEN_PREFIX( "_scan_bytes" );
			GEN_PREFIX( "_flex_debug" );
			GEN_PREFIX( "_init_buffer" );
			GEN_PREFIX( "_flush_buffer" );
			GEN_PREFIX( "_load_buffer_state" );
			GEN_PREFIX( "_switch_to_buffer" );
			GEN_PREFIX( "in" );
			GEN_PREFIX( "leng" );
			GEN_PREFIX( "lex" );
			GEN_PREFIX( "out" );
			GEN_PREFIX( "restart" );
			GEN_PREFIX( "text" );

			if ( do_yylineno )
				GEN_PREFIX( "lineno" );
			}

		if ( do_yywrap )
			GEN_PREFIX( "wrap" );

		outn( "" );
		}

	if ( did_outfilename )
		line_directive_out( stdout, 0 );

	skelout();
	}


/* flexend - terminate flex
 *
 * note
 *    This routine does not return.
 */

void flexend( exit_status )
int exit_status;

	{
	int tblsiz;
	int unlink();

	if ( skelfile != NULL )
		{
		if ( ferror( skelfile ) )
			lerrsf( _( "input error reading skeleton file %s" ),
				skelname );

		else if ( fclose( skelfile ) )
			lerrsf( _( "error closing skeleton file %s" ),
				skelname );
		}

	if ( exit_status != 0 && outfile_created )
		{
		if ( ferror( stdout ) )
			lerrsf( _( "error writing output file %s" ),
				outfilename );

		else if ( fclose( stdout ) )
			lerrsf( _( "error closing output file %s" ),
				outfilename );

		else if ( unlink( outfilename ) )
			lerrsf( _( "error deleting output file %s" ),
				outfilename );
		}

	if ( backing_up_report && backing_up_file )
		{
		if ( num_backing_up == 0 )
			fprintf( backing_up_file, _( "No backing up.\n" ) );
		else if ( fullspd || fulltbl )
			fprintf( backing_up_file,
				_( "%d backing up (non-accepting) states.\n" ),
				num_backing_up );
		else
			fprintf( backing_up_file,
				_( "Compressed tables always back up.\n" ) );

		if ( ferror( backing_up_file ) )
			lerrsf( _( "error writing backup file %s" ),
				backing_name );

		else if ( fclose( backing_up_file ) )
			lerrsf( _( "error closing backup file %s" ),
				backing_name );
		}

	if ( printstats )
		{
		fprintf( stderr, _( "%s version %s usage statistics:\n" ),
			program_name, flex_version );

		fprintf( stderr, _( "  scanner options: -" ) );

		if ( C_plus_plus )
			putc( '+', stderr );
		if ( backing_up_report )
			putc( 'b', stderr );
		if ( ddebug )
			putc( 'd', stderr );
		if ( caseins )
			putc( 'i', stderr );
		if ( lex_compat )
			putc( 'l', stderr );
		if ( performance_report > 0 )
			putc( 'p', stderr );
		if ( performance_report > 1 )
			putc( 'p', stderr );
		if ( spprdflt )
			putc( 's', stderr );
		if ( use_stdout )
			putc( 't', stderr );
		if ( printstats )
			putc( 'v', stderr );	/* always true! */
		if ( nowarn )
			putc( 'w', stderr );
		if ( interactive == false )
			putc( 'B', stderr );
		if ( interactive == true )
			putc( 'I', stderr );
		if ( ! gen_line_dirs )
			putc( 'L', stderr );
		if ( trace )
			putc( 'T', stderr );

		if ( csize == unspecified )
			/* We encountered an error fairly early on, so csize
			 * never got specified.  Define it now, to prevent
			 * bogus table sizes being written out below.
			 */
			csize = 256;

		if ( csize == 128 )
			putc( '7', stderr );
		else
			putc( '8', stderr );

		fprintf( stderr, " -C" );

		if ( long_align )
			putc( 'a', stderr );
		if ( fulltbl )
			putc( 'f', stderr );
		if ( fullspd )
			putc( 'F', stderr );
		if ( useecs )
			putc( 'e', stderr );
		if ( usemecs )
			putc( 'm', stderr );
		if ( use_read )
			putc( 'r', stderr );

		if ( did_outfilename )
			fprintf( stderr, " -o%s", outfilename );

		if ( skelname )
			fprintf( stderr, " -S%s", skelname );

		if ( strcmp( prefix, "yy" ) )
			fprintf( stderr, " -P%s", prefix );

		putc( '\n', stderr );

		fprintf( stderr, _( "  %d/%d NFA states\n" ),
			lastnfa, current_mns );
		fprintf( stderr, _( "  %d/%d DFA states (%d words)\n" ),
			lastdfa, current_max_dfas, totnst );
		fprintf( stderr, _( "  %d rules\n" ),
		num_rules + num_eof_rules - 1 /* - 1 for def. rule */ );

		if ( num_backing_up == 0 )
			fprintf( stderr, _( "  No backing up\n" ) );
		else if ( fullspd || fulltbl )
			fprintf( stderr,
			_( "  %d backing-up (non-accepting) states\n" ),
				num_backing_up );
		else
			fprintf( stderr,
				_( "  Compressed tables always back-up\n" ) );

		if ( bol_needed )
			fprintf( stderr,
				_( "  Beginning-of-line patterns used\n" ) );

		fprintf( stderr, _( "  %d/%d start conditions\n" ), lastsc,
			current_max_scs );
		fprintf( stderr,
			_( "  %d epsilon states, %d double epsilon states\n" ),
			numeps, eps2 );

		if ( lastccl == 0 )
			fprintf( stderr, _( "  no character classes\n" ) );
		else
			fprintf( stderr,
_( "  %d/%d character classes needed %d/%d words of storage, %d reused\n" ),
				lastccl, current_maxccls,
				cclmap[lastccl] + ccllen[lastccl],
				current_max_ccl_tbl_size, cclreuse );

		fprintf( stderr, _( "  %d state/nextstate pairs created\n" ),
			numsnpairs );
		fprintf( stderr, _( "  %d/%d unique/duplicate transitions\n" ),
			numuniq, numdup );

		if ( fulltbl )
			{
			tblsiz = lastdfa * numecs;
			fprintf( stderr, _( "  %d table entries\n" ), tblsiz );
			}

		else
			{
			tblsiz = 2 * (lastdfa + numtemps) + 2 * tblend;

			fprintf( stderr,
				_( "  %d/%d base-def entries created\n" ),
				lastdfa + numtemps, current_max_dfas );
			fprintf( stderr,
			_( "  %d/%d (peak %d) nxt-chk entries created\n" ),
				tblend, current_max_xpairs, peakpairs );
			fprintf( stderr,
		_( "  %d/%d (peak %d) template nxt-chk entries created\n" ),
				numtemps * nummecs,
				current_max_template_xpairs,
				numtemps * numecs );
			fprintf( stderr, _( "  %d empty table entries\n" ),
				nummt );
			fprintf( stderr, _( "  %d protos created\n" ),
				numprots );
			fprintf( stderr,
				_( "  %d templates created, %d uses\n" ),
				numtemps, tmpuses );
			}

		if ( useecs )
			{
			tblsiz = tblsiz + csize;
			fprintf( stderr,
				_( "  %d/%d equivalence classes created\n" ),
				numecs, csize );
			}

		if ( usemecs )
			{
			tblsiz = tblsiz + numecs;
			fprintf( stderr,
			_( "  %d/%d meta-equivalence classes created\n" ),
				nummecs, csize );
			}

		fprintf( stderr,
		_( "  %d (%d saved) hash collisions, %d DFAs equal\n" ),
			hshcol, hshsave, dfaeql );
		fprintf( stderr, _( "  %d sets of reallocations needed\n" ),
			num_reallocs );
		fprintf( stderr, _( "  %d total table entries needed\n" ),
			tblsiz );
		}

	exit( exit_status );
	}


/* flexinit - initialize flex */

void flexinit( argc, argv )
int argc;
char **argv;
	{
	int i, sawcmpflag;
	char *arg;

	printstats = syntaxerror = trace = spprdflt = caseins = false;
	lex_compat = C_plus_plus = backing_up_report = ddebug = fulltbl = false;
	fullspd = long_align = nowarn = yymore_used = continued_action = false;
	do_yylineno = yytext_is_array = in_rule = reject = do_stdinit = false;
	yymore_really_used = reject_really_used = unspecified;
	interactive = csize = unspecified;
	do_yywrap = gen_line_dirs = usemecs = useecs = true;
	performance_report = 0;
	did_outfilename = 0;
	prefix = "yy";
	yyclass = 0;
	use_read = use_stdout = false;

	sawcmpflag = false;

	/* Initialize dynamic array for holding the rule actions. */
	action_size = 2048;	/* default size of action array in bytes */
	action_array = allocate_character_array( action_size );
	defs1_offset = prolog_offset = action_offset = action_index = 0;
	action_array[0] = '\0';

	program_name = argv[0];

	if ( program_name[0] != '\0' &&
	     program_name[strlen( program_name ) - 1] == '+' )
		C_plus_plus = true;

	/* read flags */
	for ( --argc, ++argv; argc ; --argc, ++argv )
		{
		arg = argv[0];

		if ( arg[0] != '-' || arg[1] == '\0' )
			break;

		if ( arg[1] == '-' )
			{ /* --option */
			if ( ! strcmp( arg, "--help" ) )
				arg = "-h";

			else if ( ! strcmp( arg, "--version" ) )
				arg = "-V";

			else if ( ! strcmp( arg, "--" ) )
				{ /* end of options */
				--argc;
				++argv;
				break;
				}
			}

		for ( i = 1; arg[i] != '\0'; ++i )
			switch ( arg[i] )
				{
				case '+':
					C_plus_plus = true;
					break;

				case 'B':
					interactive = false;
					break;

				case 'b':
					backing_up_report = true;
					break;

				case 'c':
					break;

				case 'C':
					if ( i != 1 )
						flexerror(
				_( "-C flag must be given separately" ) );

					if ( ! sawcmpflag )
						{
						useecs = false;
						usemecs = false;
						fulltbl = false;
						sawcmpflag = true;
						}

					for ( ++i; arg[i] != '\0'; ++i )
						switch ( arg[i] )
							{
							case 'a':
								long_align =
									true;
								break;

							case 'e':
								useecs = true;
								break;

							case 'F':
								fullspd = true;
								break;

							case 'f':
								fulltbl = true;
								break;

							case 'm':
								usemecs = true;
								break;

							case 'r':
								use_read = true;
								break;

							default:
								lerrif(
						_( "unknown -C option '%c'" ),
								(int) arg[i] );
								break;
							}

					goto get_next_arg;

				case 'd':
					ddebug = true;
					break;

				case 'f':
					useecs = usemecs = false;
					use_read = fulltbl = true;
					break;

				case 'F':
					useecs = usemecs = false;
					use_read = fullspd = true;
					break;

				case '?':
				case 'h':
					usage();
					exit( 0 );

				case 'I':
					interactive = true;
					break;

				case 'i':
					caseins = true;
					break;

				case 'l':
					lex_compat = true;
					break;

				case 'L':
					gen_line_dirs = false;
					break;

				case 'n':
					/* Stupid do-nothing deprecated
					 * option.
					 */
					break;

				case 'o':
					if ( i != 1 )
						flexerror(
				_( "-o flag must be given separately" ) );

					outfilename = arg + i + 1;
					did_outfilename = 1;
					goto get_next_arg;

				case 'P':
					if ( i != 1 )
						flexerror(
				_( "-P flag must be given separately" ) );

					prefix = arg + i + 1;
					goto get_next_arg;

				case 'p':
					++performance_report;
					break;

				case 'S':
					if ( i != 1 )
						flexerror(
				_( "-S flag must be given separately" ) );

					skelname = arg + i + 1;
					goto get_next_arg;

				case 's':
					spprdflt = true;
					break;

				case 't':
					use_stdout = true;
					break;

				case 'T':
					trace = true;
					break;

				case 'v':
					printstats = true;
					break;

				case 'V':
					printf( _( "%s version %s\n" ),
						program_name, flex_version );
					exit( 0 );

				case 'w':
					nowarn = true;
					break;

				case '7':
					csize = 128;
					break;

				case '8':
					csize = CSIZE;
					break;

				default:
					fprintf( stderr,
		_( "%s: unknown flag '%c'.  For usage, try\n\t%s --help\n" ),
						program_name, (int) arg[i],
						program_name );
					exit( 1 );
				}

		/* Used by -C, -S, -o, and -P flags in lieu of a "continue 2"
		 * control.
		 */
		get_next_arg: ;
		}

	num_input_files = argc;
	input_files = argv;
	set_input_file( num_input_files > 0 ? input_files[0] : NULL );

	lastccl = lastsc = lastdfa = lastnfa = 0;
	num_rules = num_eof_rules = default_rule = 0;
	numas = numsnpairs = tmpuses = 0;
	numecs = numeps = eps2 = num_reallocs = hshcol = dfaeql = totnst = 0;
	numuniq = numdup = hshsave = eofseen = datapos = dataline = 0;
	num_backing_up = onesp = numprots = 0;
	variable_trailing_context_rules = bol_needed = false;

	out_linenum = linenum = sectnum = 1;
	firstprot = NIL;

	/* Used in mkprot() so that the first proto goes in slot 1
	 * of the proto queue.
	 */
	lastprot = 1;

	set_up_initial_allocations();
	}


/* readin - read in the rules section of the input file(s) */

void readin()
	{
	static char yy_stdinit[] = "FILE *yyin = stdin, *yyout = stdout;";
	static char yy_nostdinit[] =
		"FILE *yyin = (FILE *) 0, *yyout = (FILE *) 0;";

	line_directive_out( (FILE *) 0, 1 );

	if ( yyparse() )
		{
		pinpoint_message( _( "fatal parse error" ) );
		flexend( 1 );
		}

	if ( syntaxerror )
		flexend( 1 );

	if ( backing_up_report )
		{
		backing_up_file = fopen( backing_name, "w" );
		if ( backing_up_file == NULL )
			lerrsf(
			_( "could not create backing-up info file %s" ),
				backing_name );
		}

	else
		backing_up_file = NULL;

	if ( yymore_really_used == true )
		yymore_used = true;
	else if ( yymore_really_used == false )
		yymore_used = false;

	if ( reject_really_used == true )
		reject = true;
	else if ( reject_really_used == false )
		reject = false;

	if ( performance_report > 0 )
		{
		if ( lex_compat )
			{
			fprintf( stderr,
_( "-l AT&T lex compatibility option entails a large performance penalty\n" ) );
			fprintf( stderr,
_( " and may be the actual source of other reported performance penalties\n" ) );
			}

		else if ( do_yylineno )
			{
			fprintf( stderr,
	_( "%%option yylineno entails a large performance penalty\n" ) );
			}

		if ( performance_report > 1 )
			{
			if ( interactive )
				fprintf( stderr,
	_( "-I (interactive) entails a minor performance penalty\n" ) );

			if ( yymore_used )
				fprintf( stderr,
		_( "yymore() entails a minor performance penalty\n" ) );
			}

		if ( reject )
			fprintf( stderr,
			_( "REJECT entails a large performance penalty\n" ) );

		if ( variable_trailing_context_rules )
			fprintf( stderr,
_( "Variable trailing context rules entail a large performance penalty\n" ) );
		}

	if ( reject )
		real_reject = true;

	if ( variable_trailing_context_rules )
		reject = true;

	if ( (fulltbl || fullspd) && reject )
		{
		if ( real_reject )
			flexerror(
				_( "REJECT cannot be used with -f or -F" ) );
		else if ( do_yylineno )
			flexerror(
			_( "%option yylineno cannot be used with -f or -F" ) );
		else
			flexerror(
	_( "variable trailing context rules cannot be used with -f or -F" ) );
		}

	if ( reject )
		outn( "\n#define YY_USES_REJECT" );

	if ( ! do_yywrap )
		{
		outn( "\n#define yywrap() 1" );
		outn( "#define YY_SKIP_YYWRAP" );
		}

	if ( ddebug )
		outn( "\n#define FLEX_DEBUG" );

	if ( csize == 256 )
		outn( "typedef unsigned char YY_CHAR;" );
	else
		outn( "typedef char YY_CHAR;" );

	if ( C_plus_plus )
		{
		outn( "#define yytext_ptr yytext" );

		if ( interactive )
			outn( "#define YY_INTERACTIVE" );
		}

	else
		{
		if ( do_stdinit )
			{
			outn( "#ifdef VMS" );
			outn( "#ifndef __VMS_POSIX" );
			outn( yy_nostdinit );
			outn( "#else" );
			outn( yy_stdinit );
			outn( "#endif" );
			outn( "#else" );
			outn( yy_stdinit );
			outn( "#endif" );
			}

		else
			outn( yy_nostdinit );
		}

	if ( fullspd )
		outn( "typedef yyconst struct yy_trans_info *yy_state_type;" );
	else if ( ! C_plus_plus )
		outn( "typedef int yy_state_type;" );

	if ( ddebug )
		outn( "\n#define FLEX_DEBUG" );

	if ( lex_compat )
		outn( "#define YY_FLEX_LEX_COMPAT" );

	if ( do_yylineno && ! C_plus_plus )
		{
		outn( "extern int yylineno;" );
		outn( "int yylineno = 1;" );
		}

	if ( C_plus_plus )
		{
		outn( "\n#include <FlexLexer.h>" );

		if ( yyclass )
			{
			outn( "int yyFlexLexer::yylex()" );
			outn( "\t{" );
			outn(
"\tLexerError( \"yyFlexLexer::yylex invoked but %option yyclass used\" );" );
			outn( "\treturn 0;" );
			outn( "\t}" );
	
			out_str( "\n#define YY_DECL int %s::yylex()\n",
				yyclass );
			}
		}

	else
		{
		if ( yytext_is_array )
			outn( "extern char yytext[];\n" );

		else
			{
			outn( "extern char *yytext;" );
			outn( "#define yytext_ptr yytext" );
			}

		if ( yyclass )
			flexerror(
		_( "%option yyclass only meaningful for C++ scanners" ) );
		}

	if ( useecs )
		numecs = cre8ecs( nextecm, ecgroup, csize );
	else
		numecs = csize;

	/* Now map the equivalence class for NUL to its expected place. */
	ecgroup[0] = ecgroup[csize];
	NUL_ec = ABS( ecgroup[0] );

	if ( useecs )
		ccl2ecl();
	}


/* set_up_initial_allocations - allocate memory for internal tables */

void set_up_initial_allocations()
	{
	current_mns = INITIAL_MNS;
	firstst = allocate_integer_array( current_mns );
	lastst = allocate_integer_array( current_mns );
	finalst = allocate_integer_array( current_mns );
	transchar = allocate_integer_array( current_mns );
	trans1 = allocate_integer_array( current_mns );
	trans2 = allocate_integer_array( current_mns );
	accptnum = allocate_integer_array( current_mns );
	assoc_rule = allocate_integer_array( current_mns );
	state_type = allocate_integer_array( current_mns );

	current_max_rules = INITIAL_MAX_RULES;
	rule_type = allocate_integer_array( current_max_rules );
	rule_linenum = allocate_integer_array( current_max_rules );
	rule_useful = allocate_integer_array( current_max_rules );

	current_max_scs = INITIAL_MAX_SCS;
	scset = allocate_integer_array( current_max_scs );
	scbol = allocate_integer_array( current_max_scs );
	scxclu = allocate_integer_array( current_max_scs );
	sceof = allocate_integer_array( current_max_scs );
	scname = allocate_char_ptr_array( current_max_scs );

	current_maxccls = INITIAL_MAX_CCLS;
	cclmap = allocate_integer_array( current_maxccls );
	ccllen = allocate_integer_array( current_maxccls );
	cclng = allocate_integer_array( current_maxccls );

	current_max_ccl_tbl_size = INITIAL_MAX_CCL_TBL_SIZE;
	ccltbl = allocate_Character_array( current_max_ccl_tbl_size );

	current_max_dfa_size = INITIAL_MAX_DFA_SIZE;

	current_max_xpairs = INITIAL_MAX_XPAIRS;
	nxt = allocate_integer_array( current_max_xpairs );
	chk = allocate_integer_array( current_max_xpairs );

	current_max_template_xpairs = INITIAL_MAX_TEMPLATE_XPAIRS;
	tnxt = allocate_integer_array( current_max_template_xpairs );

	current_max_dfas = INITIAL_MAX_DFAS;
	base = allocate_integer_array( current_max_dfas );
	def = allocate_integer_array( current_max_dfas );
	dfasiz = allocate_integer_array( current_max_dfas );
	accsiz = allocate_integer_array( current_max_dfas );
	dhash = allocate_integer_array( current_max_dfas );
	dss = allocate_int_ptr_array( current_max_dfas );
	dfaacc = allocate_dfaacc_union( current_max_dfas );

	nultrans = (int *) 0;
	}


void usage()
	{
	FILE *f = stdout;

	fprintf( f, _( "Usage: %s [OPTIONS] [FILE...]\n\n" ), program_name );
	
	fprintf( f, _( "DESCRIPTION\n" ) );
	fprintf( f, _( "    %s generates fast lexical analyzers (scanners) from specification\n" ), program_name );
	fprintf( f, _( "    files.  It reads the given FILEs (or standard input if none are\n" ) );
	fprintf( f, _( "    given) for a description of a scanner to generate.  The scanner\n" ) );
	fprintf( f, _( "    is written to the file lex.yy.c unless -o is specified.\n\n" ) );

	fprintf( f, _( "OPTIONS\n" ) );
	
	/* Basic options */
	fprintf( f, _( "    Basic Options:\n" ) );
	fprintf( f, _( "        -h, --help     display this help and exit\n" ) );
	fprintf( f, _( "        -V, --version  display version information and exit\n" ) );
	fprintf( f, _( "        -v             write summary of scanner statistics to stderr\n" ) );
	fprintf( f, _( "        -w             suppress warnings\n" ) );
	fprintf( f, _( "        -T             run in trace mode\n" ) );
	fprintf( f, _( "        -?             display this help and exit\n\n" ) );

	/* Scanner generation options */
	fprintf( f, _( "    Scanner Generation:\n" ) );
	fprintf( f, _( "        -t             write generated scanner to stdout\n" ) );
	fprintf( f, _( "        -o FILE        specify output filename\n" ) );
	fprintf( f, _( "        -P PREFIX      specify scanner prefix other than \"yy\"\n" ) );
	fprintf( f, _( "        -S FILE        specify skeleton file\n" ) );
	fprintf( f, _( "        -+             generate C++ scanner class\n\n" ) );

	/* Scanner behavior options */
	fprintf( f, _( "    Scanner Behavior:\n" ) );
	fprintf( f, _( "        -i             generate case-insensitive scanner\n" ) );
	fprintf( f, _( "        -l             maximal compatibility with original lex\n" ) );
	fprintf( f, _( "        -s             suppress default rule to ECHO unmatched text\n" ) );
	fprintf( f, _( "        -B             generate batch scanner (opposite of -I)\n" ) );
	fprintf( f, _( "        -I             generate interactive scanner (opposite of -B)\n" ) );
	fprintf( f, _( "        -L             suppress #line directives in scanner\n" ) );
	fprintf( f, _( "        -7             generate 7-bit scanner\n" ) );
	fprintf( f, _( "        -8             generate 8-bit scanner\n\n" ) );

	/* Performance and optimization options */
	fprintf( f, _( "    Performance and Optimization:\n" ) );
	fprintf( f, _( "        -f             generate fast, large scanner\n" ) );
	fprintf( f, _( "        -F             use alternative fast scanner representation\n" ) );
	fprintf( f, _( "        -p             generate performance report to stderr\n" ) );
	fprintf( f, _( "        -b             generate backing-up information to %s\n" ), backing_name );
	fprintf( f, _( "        -d             turn on debug mode in generated scanner\n\n" ) );

	/* Table compression options */
	fprintf( f, _( "    Table Compression (default is -Cem):\n" ) );
	fprintf( f, _( "        -C[aefFmr]     specify degree of table compression:\n" ) );
	fprintf( f, _( "            -Ca        trade off larger tables for better memory alignment\n" ) );
	fprintf( f, _( "            -Ce        construct equivalence classes\n" ) );
	fprintf( f, _( "            -Cf        do not compress tables; use -f representation\n" ) );
	fprintf( f, _( "            -CF        do not compress tables; use -F representation\n" ) );
	fprintf( f, _( "            -Cm        construct meta-equivalence classes\n" ) );
	fprintf( f, _( "            -Cr        use read() instead of stdio for scanner input\n\n" ) );

	/* POSIX compatibility options */
	fprintf( f, _( "    POSIX Compatibility:\n" ) );
	fprintf( f, _( "        -c             do-nothing POSIX option (for compatibility)\n" ) );
	fprintf( f, _( "        -n             do-nothing POSIX option (for compatibility)\n\n" ) );

	if ( ! did_outfilename )
		{
		sprintf( outfile_path, outfile_template,
			prefix, C_plus_plus ? "cc" : "c" );
		outfilename = outfile_path;
		}

	fprintf( f, _( "EXAMPLES\n" ) );
	fprintf( f, _( "    %s input.l              generate scanner from input.l\n" ), program_name );
	fprintf( f, _( "    %s -o scanner.c input.l generate scanner to scanner.c\n" ), program_name );
	fprintf( f, _( "    %s -P my input.l        generate scanner with prefix 'my'\n" ), program_name );
	fprintf( f, _( "    %s -v input.l           generate scanner with statistics\n\n" ), program_name );

	fprintf( f, _( "EXIT STATUS\n" ) );
	fprintf( f, _( "    0     successful completion\n" ) );
	fprintf( f, _( "    1     error occurred\n\n" ) );
	}
