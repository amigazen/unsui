/*
 *  FILE
 *	sc_emacs.rexx
 *
 *  DESCRIPTION
 *	Simple handler between GNUEmacs and SCMSG.
 *
 *	This program is used when SCMSG calls emacs. (When EMACS
 *	asks SCMSG for information it can do it directly.)
 *
 *  AUTHOR
 *	Anders Lindgren, d91ali@csd.uu.se
 */

trace ?r

if ~Show('L','rexxsupport.library') then do
        if ~AddLib('rexxsupport.library',0,-30,0) then do
                say 'Unable to open rexxsupport.library'
                exit 20
		end
	end

if ~Show('L','slashquote.library') then do
        if ~AddLib('slashquote.library',0,-30,0) then do
                say 'Unable to open slashquote.library'
                exit 20
		end
	end


port = 'SC_EMACS'
if ~OpenPort(port) then do
        say 'Error while opening port' port
        exit 20
	end

/* Start emacs if it's not already running */
if show('p', 'EMACS1') = 0 then do
	address command 'emacs'
	delay(1100)
	end

quit=0

do until quit
        call WaitPkt(port)
        pkt = GetPkt(port)
        if pkt = Null() then do
                iterate
        end

        msg = zerot2str(pkt,0)   /* getarg(pkt, 0)  */  

	parse VALUE msg WITH cmd line 'a'x errnum 'a'x,
		class 'a'x file 'a'x text

	retcode = 20

	select
		when cmd = 'QUIT' then do
			retcode = 0
			quit = 1
			end

		when cmd = 'GOTOLINE' then do
			retcode = 0
			end

		when cmd = 'GOTOFILE' then do
			
			/*
			 * Undocumented feature: SCMSG quotes filenames
			 * and texts which contains spaces. VERY STUPID!
			 * How can a call be made to emacs without
			 * extensive parsing?
			 */

			if words(file) > 1 then do
				file=substr(file,2,length(file)-2)
				end
			
			if words(text) > 1 then do
				text=substr(text,2,length(text)-2)
				end
			
			msg = '(sas-c-view-message ',
				slashquote(file) line slashquote(text),
					slashquote(class) errnum')'
			/* call writeln(stderr, msg) */
			address 'EMACS1' msg
			retcode = 0
			end

		otherwise nop
		end

	call Reply(pkt,retcode)
			
	end
	
ClosePort(port)
