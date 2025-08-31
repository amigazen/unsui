/*
 * INEWS.REXX
 *  
 * Copyright (c) 1992 Tapio Heiskanen
 * All rights reserved
 *
 * Only for use with GNUS.
 * 
 */

id			= pragma(id)
before	= 't:inews'id'befor'
after		= 't:inews'id'after'

address command

'perg ** >'before' 'stdin

if ~open(env, 'env:user', 'r') then
	exit 10
else do
	login=readln(env)
end
call close(env)

if ~open(passwd, 'getty:passwd', 'r') then
	exit 10
else
	do until eof(passwd)
		instr=readln(passwd)
		parse var instr user','pass','uid','gid','realname','therest
		if user=login then break
	end
call close(passwd)

if ~open(config, 'uulib:config', 'r') then
	exit 10
else
	do until eof(config)
		instr=space( translate( readln(config), ' ', '08'x), 1)
		parse var instr idstr' 'mach
		if idstr='NodeName' then break
	end
call close(config)

/*
 * This creates a command file for Edit V37.2.
 * This file is used to replace an_amiga and unknown to what they should be.
 */

call open(o, 't:'id'.ed', 'w')
	call writeln(o, 'f ?Path: ?')
	call writeln(o, 'e/an_amiga/'mach)
	call writeln(o, 'f ?From: ?')
	call writeln(o, 'e/an_amiga/'mach)
	call writeln(o, 'e/unknown/'realname)
	call writeln(o, 'f ?Message-ID: ?')
	call writeln(o, 'e/an_amiga/'mach)
	call writeln(o, 'f ?Organization: ?')
	call writeln(o, 'w')
call close(o)

'c:edit 'before' 'after' with t:'id'.ed'


/* Now add the signature */

'echo >>'after' "*n"'
'type uulib:.signature >>'after


/* Post the hole lot! */

'rnews <'after

/* Delete temp files */

'delete 'id'.ed 'before' 'after' quiet'
