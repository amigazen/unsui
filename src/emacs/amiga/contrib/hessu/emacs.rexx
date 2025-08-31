/* emacs.rexx */


parse arg params
id=pragma('id')

if ~show('P', 'EMACS1') then do
	if ~open(out, 't:emacs', 'w') then do
		say "EMACS.REXX: Can't open temporary file!"
		exit 5
	end
	call writeln(out, 'stack 49152')
	call writeln(out, 'run >nil: <nil: temacs' params)
	call writeln(out, 'stack 16384')
	call close(out)
	address command 'execute t:emacs'
	exit
end


if word(params,1)='-f' then do
	address 'EMACS1' '('subword(params, 2)')'
	exit
end
address 'EMACS1' '(find-file "'params'")'
