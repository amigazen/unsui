
#ifndef YYLTYPE
typedef
  struct yyltype
{
      int timestamp;
      int first_line;
      int first_column;
int last_line;
      int last_column;
      char *text;
   }
yyltype;

#define YYLTYPE yyltype
#endif

#define	YYACCEPT	return(0)
#define	YYABORT	return(1)
#define	YYERROR	goto yyerrlab
#ifndef YYSTYPE
#define YYSTYPE int
#endif
#define	array_tok	258
#define	begin_tok	259
#define	case_tok	260
#define	const_tok	261
#define	do_tok	262
#define	downto_tok	263
#define	else_tok	264
#define	end_tok	265
#define	file_tok	266
#define	for_tok	267
#define	function_tok	268
#define	goto_tok	269
#define	if_tok	270
#define	label_tok	271
#define	of_tok	272
#define	procedure_tok	273
#define	program_tok	274
#define	record_tok	275
#define	repeat_tok	276
#define	then_tok	277
#define	to_tok	278
#define	type_tok	279
#define	until_tok	280
#define	var_tok	281
#define	while_tok	282
#define	integer_tok	283
#define	real_tok	284
#define	others_tok	285
#define	r_num_tok	286
#define	i_num_tok	287
#define	string_literal_tok	288
#define	single_char_tok	289
#define	assign_tok	290
#define	two_dots_tok	291
#define	unknown_tok	292
#define	undef_id_tok	293
#define	var_id_tok	294
#define	proc_id_tok	295
#define	proc_param_tok	296
#define	fun_id_tok	297
#define	fun_param_tok	298
#define	const_id_tok	299
#define	type_id_tok	300
#define	hhb0_tok	301
#define	hhb1_tok	302
#define	field_id_tok	303
#define	define_tok	304
#define	field_tok	305
#define	break_tok	306
#define	not_eq_tok	307
#define	less_eq_tok	308
#define	great_eq_tok	309
#define	or_tok	310
#define	unary_plus_tok	311
#define	unary_minus_tok	312
#define	div_tok	313
#define	mod_tok	314
#define	and_tok	315
#define	not_tok	316

