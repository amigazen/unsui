/*
**	ARexx script to transform `tex.lpro' into `texc.lpro'
**
**	Written by Giuseppe Ghibò <ghibo@galileo.polito.it>
**	Version 1.0 (28 Sep 1994).
*/

Signal On Break_C
Signal On Error

If ~Open('inputfile',"tex.lpro",'Read') Then Do
	Say "Can't find file 'tex.lpro'"
	Exit
End

If ~Open('outputfile',"T:texc.lpro",'Write') Then Do
	Say "Can't open file 'T:texc.lpro'"
	Exit
End

mode = 'COPY'

Do While ~Eof('inputfile')

	line = ReadLn('inputfile')

	If line = "% begin code for uncompressed fonts only" Then mode = 'ADD'

	If line = "% end code for uncompressed fonts only" Then
		Do
			mode = 'COPY'
			count = WriteCh('outputfile',"% ")
		End

	If line = "% % here's the alternate code for unpacking compressed fonts" Then mode = 'DEL'

	If line = "% % end of code for unpacking compressed fonts" Then
		Do
			line = SubStr(line, 3)
			mode = 'COPY'
		End

	Select
		when mode = 'COPY' Then count = WriteLn('outputfile', line)

		when mode = 'ADD' Then count = WriteLn('outputfile', "% " || line)

		when mode = 'DEL' Then count = WriteLn('outputfile', SubStr(line,3))
	End
End

result = Close('inputfile')
result = Close('outputfile')

Exit 0

Break_C:
	Say 'Break detected at line' SIGL
	Exit

Error:
	Say 'Error detected at line' SIGL
