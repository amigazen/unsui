{ This is a Pascal implementation of Ratfor, a rational preprocessor
  for FORTRAN. It is called RATFOR-77 because it produces code suitable
  for FORTRAN-77 (ANSI X3.9-1978).

  (A description of Ratfor can be found in B.W. Kernighan, "RATFOR - A
   Preprocessor for a Rational Fortran", Software - Practice and
   Experience, October 1975.)

  It was written with the goal of being as portable as the limitations of
  Standard Pascal permits. To make it usable some extensions had to
  be used, though. They are of course inherently system dependent, but
  I have isolated the use of them to a few (relatively) short procedures
  to facilitate easy(?) porting to different environments. With one
  exception, they are limited to the opening and closing of files. No
  non-standard types, like strings etc., have been used outside the
  two procedures that handle the opening of files.

  Neither does it explicitly allocate memory dynamically, as the standard
  doesn't provide a method for checking whether a call of NEW() succeeded.
  So the sizes of buffers, stacks, and tables are determined at
  compile time.

  The present version may be compiled without change with Turbo Pascal
  on an IBM PC, with HighSpeed Pascal on an Amiga, or with the Pascal
  to C translator, p2c, which is available on numerous platforms
  (see the comment above the procedure openfiles for details about what
  will need to be changed for other systems).

  Just for the trivia addicts: It was developed using p2c and an ANSI C
  compiler on a non-PC platform.

  Copyright (C) 1994 Torsten Poulin.

  Permission is granted to redistribute copies of this program
  as long as no profit is made.

  The author can be contacted via email: torsten@diku.dk

  History:
    Version 1.0
       21-Feb-94: Created.
    Version 1.1
       28-Jun-94: - Fixed two minor but annoying bugs in
                    the handling of else and switch statements.
    Version 1.2
       29-Jun-94: - Output file extension '.for' changed to '.f'.
                    Hopefully this version will keep better track
                    of the line numbers; multi-line defines will
                    still confuse things, though.
                  - No changes to the "code generation".
    Version 1.3
        3-Aug-94: - Adapted to work with HighSpeed Pascal for
                    for Amiga, too. Among other things, all files are
                    closed explicitly now in case of an emergency stop.
                  - The comments output at the top of the generated
                    FORTRAN file now starts with '*' instead of 'C'
                    because the latter confuses Lahey FORTRAN 3.
                  - The dates of version 1.1 and 1.2 were off by a
                    month...
}

{ No I/O checking; valid for Turbo, HighSpeed, and p2c }
{$I-}
{$IfDef Amiga}
{ Make the code as fast and small as possible }
{$D-,L-,R-,S-,V-}
{$EndIf}

program ratfor77(input, output, fortran, ratfor,
                 include1, include2, include3, include4);

  uses DOS; { Compiler specific unit }
  
  const
    poolsize = 16384;    { this many chars may be stored by define }
    lookupsize = 997;    { max number of defines; must be prime! }
    buffersize = 1000;   { pushback buffer size; limits length of defines }
    lexemesize = 64;     { this also limits the length of filenames }
    jumpstacksize = 100; { at most this many loops may be nested }

  type
    tokenkind = (unknown, iftoken, elsetoken, fortoken,
                 dotoken, repeattoken, untiltoken,
                 whiletoken, breaktoken, nexttoken,
                 switchtoken, casetoken, defaulttoken,
                 returntoken, functiontoken, labeltoken,
                 includetoken, definetoken,
                 leftbracetoken, rightbracetoken,
                 commatoken, colontoken,
                 semicolontoken, newlinetoken, eoftoken);
    tokenset = set of tokenkind;
    string10 = packed array [1..10] of char;
    lexstring = packed array [1..lexemesize] of char;
    lextype = record
                str: lexstring;
                len: 0..lexemesize
              end;
    jumpelement = record
                    next, break: integer
                  end;
    lookupelement = record
                      name, namelen, replacement, replacelen: integer
                    end;

  var
    whitespaceset: set of char;
    foldletters: boolean;
    ch: char;
    charcount: integer;
    pushbackbuffer: array [1..buffersize] of char;
    pushedback: integer;

    eofchar, eolnchar: char;
    token: tokenkind;
    lexeme, functionname: lextype;
    labelnumber: integer;

    stringpool: packed array [1..poolsize] of char;
    nextfree: 1..poolsize;
    definetable: array [0..lookupsize] of lookupelement;

    jumpstack: array [1..jumpstacksize] of jumpelement;
    jumps: integer;

    filename: lexstring;
    filenameterminator: char;
    filenumber: 0..4;
    linenumber: array [0..4] of integer;
    includeopen: array [1..4] of boolean;

    ratfor, fortran: text;
    include1, include2, include3, include4: text;
    ratforopen, fortranopen: boolean;


  procedure initializeglobals;
    var i: integer;
    begin
      filenameterminator := chr(0);

      filenumber := 0;
      nextfree := 1;
      whitespaceset := [' ', chr(9), chr(12)];
      foldletters := true;
      eofchar := chr(1); eolnchar := chr(10);
      linenumber[0] := 1;
      pushedback := 0;
      jumps := 0;
      charcount := 1;
      labelnumber := 23000;
      functionname.len := 0;

      ratforopen := false;
      fortranopen := false;
      for i := 1 to 4 do includeopen[i] := false;
     
      { initialize the define table to be empty }
      for i := 0 to lookupsize do
        begin
          definetable[i].name := 0;
          definetable[i].replacement := 0
        end
    end;


  procedure outputfilename(s: lexstring);
    var
      i: integer;
    begin
      i := 1;
      while s[i] <> filenameterminator do
        begin
          write(s[i]);
          i := i + 1
        end
    end;


  { This procedure is compiler specific due to the very serious
    shortcomings of the "ISO Standard 7185 for computer language Pascal"
    in the area of external files.

    This version of openfiles works without change when RATFOR-77 is
    compiled with Turbo Pascal on an IBM PC, with HighSpeed Pascal
    on an Amiga, or with the Pascal to C translator, p2c, with
    the -LTURBO option.

    When using a compiler without the necessary extensions to the
    standard, the body of openfiles should only consist of the
    following lines:

       reset(ratfor);
       rewrite(fortran)

    The job of assigning physical files to the identifiers is left
    to the Pascal run-time system in this case.

    Most reasonable compilers will probably require only minor
    changes to openfiles; usually something along the lines
    of replacing 'assign(f, n); reset(f);' with 'reset(f, n);' and
    changing the test of success (i.e., 'ioresult'), if applicable.
    The code for accessing commandline arguments will most likely
    need to be changed or removed altogether.
  }
  procedure openfiles;
    var
      l, i, extension: integer;
      fname: string[lexemesize];
    begin
      i := 1;
      if paramcount < 1 then  { no name given on commandline }
        begin
          fname := '';
          write('Enter RATFOR-77 (input) filename: ');
          while (not eoln) and (not eof) and (i < lexemesize - 3) do
            begin
              read(filename[i]);
              fname := fname + filename[i];
              i := i + 1
            end;
            readln
        end
      else
        begin
          l := length(paramstr(1));
          if l >= lexemesize - 3 then l := lexemesize - 4;
          fname := copy(paramstr(1), 1, l);
          while i <= l do
            begin
              filename[i] := fname[i];
              i := i + 1
            end
        end;
      filename[i] := filenameterminator;

      write('Processing "');
      outputfilename(filename);
      write('"');

      assign(ratfor, fname);
      reset(ratfor);
      if ioresult <> 0 then
        begin
          writeln;
          writeln('I failed to open the specified RATFOR-77 file.');
          halt
        end;
      ratforopen := true;

      { Locate the start of a possible filename extension }
      extension := i;
      while i > 0 do
        begin
          if filename[i] = '.' then
            begin
              extension := i;
              i := 0 { leave the loop }
            end;
          i := i - 1
        end;
      { and replace it with .f; if there was no extension }
      { we simply append one: }
      filename[extension]   := '.';
      filename[extension+1] := 'f';
      filename[extension+2] := filenameterminator;

      { HighSpeed Pascal's assign cannot handle packed array of char }
      fname := copy(fname, 1, extension-1);
      fname := fname + '.f';
			
      write(' -> "');
      outputfilename(filename);
      writeln('"');

      assign(fortran, fname);
      rewrite(fortran);
      if ioresult <> 0 then
        begin
          writeln('I couldn''t open the FORTRAN-77 output file.');
          close(ratfor);
          halt
        end;
      fortranopen := true
    end;


  { Some compilers require files to be closed explicitly. Turbo Pascal
    is one of them; if we don't do it, the last part of the output will
    never be written to the disk!

    Replace to calls of close() with whatever is applicable for your
    compiler. An empty procedure body is enough on many systems...
  }
  procedure closefiles;
    begin
      if fortranopen then
        begin
          fortranopen := false;
          close(fortran)
        end;
      if ratforopen then
        begin
          ratforopen := false;
          close(ratfor)
        end;
      if includeopen[1] then
        begin
          includeopen[1] := false;
          close(include1)
        end;
      if includeopen[2] then
        begin
          includeopen[2] := false;
          close(include2)
        end;
      if includeopen[3] then
        begin
          includeopen[3] := false;
          close(include3)
        end;
      if includeopen[4] then
        begin
          includeopen[4] := false;
          close(include4)
        end
    end;


  { This procedure is called if RATFOR-77 detects an error
    in its input. The procedure 'halt' is a Turbo Pascal specific
    builtin procedure that halts the program. Replace it with a similar
    call suitable for your system or with a non-local jump (goto) to
    the end of the main program. This is actually the way to do it
    in ISO Pascal, but Turbo Pascal does not support such goto's, sigh!
    (Compiler vendors, why are you ommiting things from the standard?)
  }
  procedure error;
    begin
      writeln('Error at line ', linenumber[filenumber]:1, '.');
      closefiles;
      halt(1)
    end;

  { This function is also system dependent. If your compiler
    doesn't support the assignment of names to external files,
    you will probably have to remove references to include1-4
    and replace this procedure with one that simply informs
    the user that include files are not supported.

    There is no reason to change the code of the parser proper.

    The reason why it looks so clumsy is that we cannot rely
    on compiler support for arrays of type text.
  }
  procedure fileopen;
    var
      fname: string[lexemesize];
      i: integer;
    begin
      if filenumber = 4 then
        begin
          write('I am sorry, but include files can only be nested');
          writeln(' to ', filenumber:1, ' levels.');
          error
        end;
       
      { This is necessary because HighSpeed Pascal's assign doesn't
        accept a packed array of char. }
        
      i := 1; fname := '';
      while filename[i] <> filenameterminator do
        begin
          fname := fname + filename[i];
          i := i + 1
        end;
        
      filenumber := filenumber + 1;
      linenumber[filenumber] := 1;
      if filenumber = 1 then
        begin assign(include1, fname); reset(include1) end
      else if filenumber = 2 then
        begin assign(include2, fname); reset(include2) end
      else if filenumber = 3 then
        begin assign(include3, fname); reset(include3) end
      else if filenumber = 4 then
        begin assign(include4, fname); reset(include4) end;
      if ioresult <> 0 then
        begin
          writeln('I couldn''t open the include file.');
          filenumber := filenumber - 1;
          error
        end;
        
      if filenumber in [1..4] then includeopen[filenumber] := true
    end;

  { This procedure closes the include files when we are done with them.
    It, too, is inherently system dependent.
  }
  procedure fileclose;
    begin
      if filenumber = 1 then close(include1)
      else if filenumber = 2 then close(include2)
      else if filenumber = 3 then close(include3)
      else if filenumber = 4 then close(include4);
      includeopen[filenumber] := false;
      filenumber := filenumber - 1
    end;

  { The next four files are used by 'getchar' -- an array of files
    would really make things easier.

    Notice how 'eofile' hides the end-of-file status of include files
    from the rest of the program.
  }
  function eofile: boolean;
    begin
      eofile := false;
      if filenumber = 1 then begin if eof(include1) then fileclose end
      else if filenumber = 2 then begin if eof(include2) then fileclose end
      else if filenumber = 3 then begin if eof(include3) then fileclose end
      else if filenumber = 4 then begin if eof(include4) then fileclose end
      else if filenumber = 0 then eofile := eof(ratfor)
    end;

  function eoline: boolean;
    begin
      if filenumber = 0 then eoline := eoln(ratfor)
      else if filenumber = 1 then eoline := eoln(include1)
      else if filenumber = 2 then eoline := eoln(include2)
      else if filenumber = 3 then eoline := eoln(include3)
      else if filenumber = 4 then eoline := eoln(include4)
    end;

  procedure readline;
    begin
      if filenumber = 0 then readln(ratfor)
      else if filenumber = 1 then readln(include1)
      else if filenumber = 2 then readln(include2)
      else if filenumber = 3 then readln(include3)
      else if filenumber = 4 then readln(include4)
    end;

  procedure readchar(var c: char);
    begin
      if filenumber = 0 then read(ratfor, c)
      else if filenumber = 1 then read(include1, c)
      else if filenumber = 2 then read(include2, c)
      else if filenumber = 3 then read(include3, c)
      else if filenumber = 4 then read(include4, c)
    end;



  { The following functions hide some aspects of the character
    set from the rest of the program.
    Replace their bodies with suitable code if your system
    uses a different character encoding scheme, like EBCDIC on
    IBM mainframes, instead of the ASCII char set used here.

    Remember to change the relevant assignments in initializeglobals
    as well.
  }

  function isletterordigit(c: char): boolean;
    begin
      isletterordigit := c in ['0'..'9', 'a'..'z', 'A'..'Z']
    end;

  function isdigit(c: char): boolean;
    begin
      isdigit := c in ['0'..'9']
    end;

  function isnumber(s: lexstring; len: integer): boolean;
    var
      i: integer;
    begin
      isnumber := true;
      for i := 1 to len do
        if not isdigit(s[i]) then isnumber := false;
    end;

  function todigit(c: char): integer;
    begin
      todigit := ord(c) - ord('0')
    end;

  function toupper(c: char): char;
    begin
      if c in ['a'..'z'] then toupper := chr(ord(c) - (ord('a') - ord('A')))
      else toupper := c
    end;

  { End of character set specific stuff.

    The rest of the program, to the best of the author's knowledge,
    complies with the ISO standard.
  }


  procedure greeting;
    begin
      writeln('This is RATFOR-77, Version 1.3 [3-Aug-94].');
      writeln('Copyright (C) 1994 Torsten Poulin. Email: <torsten@diku.dk>')
    end;


  function tonumber(s: lexstring; len: integer): integer;
    var
      number, digit, i: integer;
    begin
      number := 0;
      for i := 1 to len do
        begin
          digit := todigit(s[i]);
          if (number > maxint div 10) or
             (number = maxint div 10) and (digit > maxint mod 10)
          then
            begin
              writeln('This number is too big for me (overflow).');
              error
            end;
          number := number * 10 + digit
        end;
      tonumber := number
    end;


  function equal(s1: lexstring; l1: integer;
                   s2: string10; l2: integer): boolean;
    var
      i: integer;
      matching: boolean;
    begin
      matching := true;
      if l1 <> l2 then matching := false
      else
        i := 1;
        while (i <= l1) and matching do
          begin
            if toupper(s1[i]) <> s2[i] then matching := false;
            i := i + 1
          end;
      equal := matching
    end;

  procedure emitchar(ch: char);
    begin
      if charcount > 72 then
        begin
          { output a continuation line }
          writeln(fortran);
          write(fortran, '     &');
          charcount := 6
        end;
      if foldletters then write(fortran, toupper(ch))
      else write(fortran, ch);
      charcount := charcount + 1
    end;

  procedure emitstring10(s: string10; len: integer);
    var
      i: 1..10;
    begin
      for i := 1 to len do
        emitchar(s[i])
    end;

  procedure emitlexeme(lex: lexstring; len: integer);
    var
      i: 1..lexemesize;
    begin
      for i := 1 to len do
        emitchar(lex[i])
    end;


  procedure pushbackchar(c: char);
    begin
      if pushedback = buffersize then
        begin
          writeln('I''m sorry, but my internal buffer is full.');
          writeln('Maybe one of your defines is too long.');
          error
        end
      else
        begin
          if c = eolnchar then
            linenumber[filenumber] := linenumber[filenumber] - 1;
          if linenumber[filenumber] < 1 then
            linenumber[filenumber] := 1;
          pushedback := pushedback + 1;
          pushbackbuffer[pushedback] := c
        end
    end;

  procedure pushbackstring10(str: string10; len: integer);
    var
      i: integer;
    begin
      for i := len downto 1 do
        pushbackchar(str[i])
    end;


  procedure pushbacklexeme(lex: lexstring; len: integer);
    var
      i: integer;
    begin
      for i := len downto 1 do
        pushbackchar(lex[i])
    end;


  procedure getchar(var ch: char);
    begin
      if pushedback > 0 then
        begin
          ch := pushbackbuffer[pushedback];
          if ch = eolnchar then
            linenumber[filenumber] := linenumber[filenumber] + 1;
          pushedback := pushedback - 1
        end
      else
        begin
          if eofile then ch := eofchar
          else if eoline then
            begin
              linenumber[filenumber] := linenumber[filenumber] + 1;
              readline;
              ch := eolnchar
            end
          else
            begin
              readchar(ch);
              if ch in whitespaceset then ch := ' '
            end
        end
    end;


  { The following procedures handles the lookup table used for definitions.

    The strings are stored in the pool one after another without sentinels.
  }

  procedure storechar(c: char);
    begin
      if nextfree = poolsize then
        begin
          writeln('My internal string storage pool is full.');
          error
        end;
      stringpool[nextfree] := c;
      nextfree := nextfree + 1
    end;

  function retrievechar(position: integer): char;
    begin
      retrievechar := stringpool[position]
    end;

  function storelexeme(s: lexstring; l: integer): integer;
    var
      i: integer;
    begin
      storelexeme := nextfree; { starting position }
      for i := 1 to l do
        storechar(s[i]);
    end;

  procedure pushbackstored(position: integer);
    var
      i, replacelen: integer;
    begin
      replacelen := definetable[position].replacelen;
      position := definetable[position].replacement;
      for i := replacelen - 1 downto 0 do
        pushbackchar(retrievechar(position + i))
    end;

  function comparestored(s: lexstring; len, position: integer): boolean;
    var
      i, namelen: integer;
      matching: boolean;
    begin
      namelen := definetable[position].namelen;
      position := definetable[position].name;
      matching := true;
      if len <> namelen then matching := false
      else
        i := 1;
        while (i <= len) and matching do
          begin
            if s[i] <> retrievechar(position) then matching := false;
            position := position + 1;
            i := i + 1
          end;
      comparestored := matching
    end;


  { Look for a lexstring in the define table.
    It is currently implemented as a hash search.
  }
  procedure lookup(s: lexstring; l: integer;
                   var position: integer; var found, full: boolean);

    function hash(s: lexstring; l: integer): integer;
      begin { this definitely needs improvement: }
        hash := (ord(s[1]) + ord(s[l])) mod lookupsize
      end;

    var
      probe, place: integer;
      test: boolean;
    begin
      probe := hash(s, l);
      position := probe;
      if definetable[position].name = 0 then test := false
      else test := comparestored(s, l, position);
      if test then found := true { got it the first time }
      else
        begin
          full := true; { no place to insert yet }
          repeat
            position := (position + 1) mod (lookupsize + 1);
            if full and (definetable[position].name = 0) then
              begin
                full := false;
                place := position { first available location for insertion }
              end;
            if definetable[position].name = 0 then test := true
            else test := comparestored(s, l, position);
          until test or (position = probe);

          if definetable[position].name = 0 then test := false
          else test := comparestored(s, l, position);
          if test then found := true
          else
            begin
              found := false;
              position := place { use the place we found above }
            end
        end
    end;

  function insertdefine(s: lexstring; l: integer): integer;
    var
      position: integer;
      found, full: boolean;
    begin
      lookup(s, l, position, found, full);
      if not found then
        if full then
          begin
            writeln('There is no more room for definitions.');
            error
          end
        else
          begin
            with definetable[position] do
              begin
                name := storelexeme(s, l);
                namelen := l
              end;
            insertdefine := position
          end
      else
        begin
          writeln('You are not allowed to redefine a definition.');
          error
        end
    end;

  { end of lookup table material }


  procedure syntacticsugar;
    var
      lookahead: char;
    begin
      if ch in ['=', '!', '~', '^', '>', '<', '$', '&', '|', '[', ']'] then
        begin
          case ch of
            '=':
              begin
                getchar(lookahead);
                if lookahead = '=' then pushbackstring10('.EQ.      ', 4)
                else
                  begin
                    pushbackchar(lookahead);
                    pushbackchar(ch)
                  end
              end;
            '!', '~', '^':
              begin
                getchar(lookahead);
                if lookahead = '=' then pushbackstring10('.NE.      ', 4)
                else
                  begin
                    pushbackchar(lookahead);
                    pushbackstring10('.NOT.     ', 5)
                  end
              end;
            '>':
              begin
                getchar(lookahead);
                if lookahead = '=' then pushbackstring10('.GE.      ', 4)
                else
                  begin
                    pushbackchar(lookahead);
                    pushbackstring10('.GT.      ', 4)
                  end
              end;
            '<':
              begin
                getchar(lookahead);
                if lookahead = '=' then pushbackstring10('.LE.      ', 4)
                else
                  begin
                    pushbackchar(lookahead);
                    pushbackstring10('.LT.      ', 4)
                  end
              end;
            '$':
              begin
                getchar(lookahead);
                if lookahead = '(' then pushbackchar('{')
                else if lookahead = ')' then pushbackchar('}')
                else
                  begin
                    pushbackchar(lookahead);
                    pushbackchar(ch)
                  end
              end;
            '&': pushbackstring10('.AND.     ', 5);
            '|': pushbackstring10('.OR.      ', 4);
            '[': pushbackchar('{');
            ']': pushbackchar('}');
          end;
          getchar(ch); { get the first pushed back character }
        end
    end;


  procedure gettoken(var token: tokenkind);
    var
      lookahead, terminator: char;
      where: integer;
      found, dummy, done: boolean;
    begin
      repeat
        done := true;
        getchar(ch);
        if ch = eofchar then token := eoftoken
        else
          begin
            repeat
              done := true;
              while ch = ' ' do getchar(ch);
              if ch in ['=', '+', '-', '*', ',', '|', '&', '(', '_'] then
                begin
                  repeat getchar(lookahead)
                  until lookahead <> ' ';
                  if lookahead <> eolnchar then pushbackchar(lookahead);
                  if ch = '_' then
                    begin
                      ch := ' ';
                      done := false
                    end
                end;

              { Handle a string constant.
                The following examples of Ratfor code
                 'don''t'
                 'don'  't'
                 "don't"
                are all transformed into 'don''t'.
              }
              if ch in ['''', '"'] then
                begin
                  foldletters := false;
                  emitchar('''');
                  terminator := ch;
                  repeat
                    getchar(ch);
                    if ch = '''' then
                      if terminator = '"' then emitchar('''');
                    if ch = terminator then emitchar('''')
                    else emitchar(ch);
                    if ch in [eolnchar, eofchar] then
                      begin
                        writeln('End of line inside a string.');
                        error
                      end
                  until ch = terminator;
                  foldletters := true;
                  getchar(ch);
                  done := false
                end
            until done;

            syntacticsugar;

            lexeme.len := 1;
            lexeme.str[1] := ch;
            while isletterordigit(ch) and (lexeme.len <= lexemesize) do
              begin
                getchar(ch);
                lexeme.len := lexeme.len + 1;
                lexeme.str[lexeme.len] := ch
              end;

	    if lexeme.len > 1 then
	      begin
	        { we have read one character too far }
                pushbackchar(ch);
                lexeme.len := lexeme.len - 1;

	        { determine what kind of token we have got }
	        if isnumber(lexeme.str, lexeme.len) then
		  token := labeltoken
	        else if equal(lexeme.str, lexeme.len, 'IF        ', 2) then
		  token := iftoken
                else if equal(lexeme.str, lexeme.len, 'ELSE      ', 4) then
		  token := elsetoken
                else if equal(lexeme.str, lexeme.len, 'FOR       ', 3) then
		  token := fortoken
                else if equal(lexeme.str, lexeme.len, 'DO        ', 2) then
		  token := dotoken
                else if equal(lexeme.str, lexeme.len, 'REPEAT    ', 6) then
		  token := repeattoken
                else if equal(lexeme.str, lexeme.len, 'UNTIL     ', 5) then
		  token := untiltoken
                else if equal(lexeme.str, lexeme.len, 'WHILE     ', 5) then
                  token := whiletoken
                else if equal(lexeme.str, lexeme.len, 'BREAK     ', 5) then
                  token := breaktoken
	        else if equal(lexeme.str, lexeme.len, 'NEXT      ', 4) then
		  token := nexttoken
	        else if equal(lexeme.str, lexeme.len, 'SWITCH    ', 6) then
		  token := switchtoken
	        else if equal(lexeme.str, lexeme.len, 'CASE      ', 4) then
		  token := casetoken
	        else if equal(lexeme.str, lexeme.len, 'DEFAULT   ', 7) then
		  token := defaulttoken
	        else if equal(lexeme.str, lexeme.len, 'RETURN    ', 6) then
		  token := returntoken
	        else if equal(lexeme.str, lexeme.len, 'FUNCTION  ', 8) or
                        equal(lexeme.str, lexeme.len, 'SUBROUTINE', 10) then
		  token := functiontoken
	        else if equal(lexeme.str, lexeme.len, 'DEFINE    ', 6) then
		  token := definetoken
	        else if equal(lexeme.str, lexeme.len, 'INCLUDE   ', 7) then
		  token := includetoken
                else token := unknown;

                lookup(lexeme.str, lexeme.len, where, found, dummy);
                if found then
                  begin
                    pushbackstored(where);
                    done := false
                  end
                else done := true
	      end
	    else if ch = '#' then
	      begin
	        getchar(ch);
	        while (ch <> eolnchar) and (ch <> eofchar) do
		  getchar(ch);
                lexeme.str[1] := eolnchar; lexeme.len := 1;
	        token := newlinetoken
	      end
	    else if ch in ['%', '$'] then
	      begin
	        { copy the rest of the line verbatim to the output }
                foldletters := false;
                if ch = '$' then emitchar(ch);
	        repeat
		  getchar(ch);
		  emitchar(ch)
	        until (ch = eolnchar) or (ch = eofchar);
                foldletters := true;
                lexeme.str[1] := eolnchar; lexeme.len := 1;
	        token := newlinetoken
	      end
            else if ch = eolnchar then token := newlinetoken
            else if ch = '{' then token := leftbracetoken
            else if ch = '}' then token := rightbracetoken
            else if ch = ',' then token := commatoken
            else if ch = ':' then token := colontoken
            else if ch = ';' then token := semicolontoken
            else token := unknown
        end
      until done
    end;


  procedure pushjump(next, break: integer);
    begin
      jumps := jumps + 1;
      if jumps > jumpstacksize then
        begin
          writeln('The loops are nested too deeply for me.');
          writeln('You can either rethink your strategy or');
          writeln('recompile me with a higher nesting limit.');
          writeln('It is currently ', jumpstacksize:1, '.');
          error
        end;
      jumpstack[jumps].next := next;
      jumpstack[jumps].break := break
    end;

  procedure popjump;
    begin
      if jumps <= 0 then
        begin
          writeln('Internal error encountered while processing loop.');
          error
        end;
      jumps := jumps - 1
    end;

  procedure retrievejump(var next, break: integer; levels: integer);
    var here: integer;
    begin
      if levels > jumps then
        begin
          writeln('The loops are not nested that deeply.');
          error
        end;
      here := jumps - levels + 1;
      next := jumpstack[here].next;
      break := jumpstack[here].break
    end;

  { Parser }

  function generatelabel: integer;
    begin
      if labelnumber = maxint then
        begin
          writeln('I''m sorry, but I have run out of labels.');
          error
        end;
      generatelabel := labelnumber;
      labelnumber := labelnumber + 1
    end;

  procedure emitendcard;
    begin
      writeln(fortran);
      charcount := 1
    end;

  procedure tocolumn7;
    var i: integer;
    begin
      if charcount < 7 then
        for i := charcount + 1 to 7 do
          emitchar(' ')
    end;

  procedure emitlabel(l: integer);
    begin
      write(fortran, l:5);
      charcount := charcount + 5
    end;

  procedure emitcontinue;
    begin
      if charcount > 1 then { only if there is a label }
        begin
          tocolumn7;
          emitstring10('CONTINUE  ', 8);
          emitendcard
        end
    end;

  procedure emitexpression;
    var
      parentheses: integer;
      reachedend: boolean;
    begin
      gettoken(token);
      if lexeme.str[1] <> '(' then
        begin
          writeln('Missing left parenthesis.');
          error
        end;
      emitlexeme(lexeme.str, lexeme.len);

      parentheses := 1;
      reachedend := false;
      repeat
        gettoken(token);

        if token in [semicolontoken, eoftoken,
                     leftbracetoken, rightbracetoken] then
          begin
            pushbacklexeme(lexeme.str, lexeme.len);
            reachedend := true
          end
        else if token = newlinetoken then lexeme.len := 0
        else if lexeme.str[1] = '(' then parentheses := parentheses + 1
        else if lexeme.str[1] = ')' then parentheses := parentheses - 1;

        if lexeme.len > 0 then emitlexeme(lexeme.str, lexeme.len)
      until reachedend or (parentheses <= 0);

      if parentheses <> 0 then
        begin
          writeln('A parenthesis is missing from this expression.');
          error
        end
    end;


  procedure emitunknown(forincrement: boolean);
    var
      reachedend, storename: boolean;
      parentheses: integer;
      c: char;
    begin
      if token = functiontoken then storename := true
      else storename := false;
      reachedend := false;
      if forincrement then parentheses := 1
      else parentheses := 0;
      repeat
        gettoken(token);
        c := lexeme.str[1];
        if storename then
          begin
            functionname := lexeme;
            storename := false
          end;

        if token in [newlinetoken, semicolontoken, rightbracetoken] then
          reachedend := true
        else if token = functiontoken then storename := true
        else if token = leftbracetoken then
          begin
            writeln('Excuse me, but I don''t think you want to');
            writeln('put an opening brace here.');
            error
          end
        else if token = eoftoken then
          begin
            writeln('Whoa! I didn''t expect the program to end just now.');
            error
          end
        else if c = '(' then parentheses := parentheses + 1
        else if c = ')' then
          begin
            parentheses := parentheses - 1;
            if forincrement and (parentheses < 1) then
              reachedend := true
          end;

        if not reachedend then emitlexeme(lexeme.str, lexeme.len)
      until reachedend;

      if parentheses <> 0 then
        begin
          write('The parentheses in this expression are ');
          writeln('not balanced properly.');
          error
        end
    end;


  procedure emitifgoto(l: integer);
    begin
      tocolumn7;
      emitstring10('IF (.NOT. ', 9);
      emitexpression;
      emitstring10(') GO TO   ', 8);
      emitlabel(l);
      emitendcard
    end;

  procedure emitifthen;
    begin
      tocolumn7;
      emitstring10('IF        ', 3);
      emitexpression;
      emitstring10(' THEN     ', 5);
      emitendcard
    end;

  procedure emitelse;
    begin
      tocolumn7;
      emitstring10('ELSE      ', 4);
      emitendcard
    end;

  procedure emitendif;
    begin
      tocolumn7;
      emitstring10('END IF    ', 6);
      emitendcard
    end;


  procedure emitgoto(l: integer);
    begin
      tocolumn7;
      emitstring10('GO TO     ', 6);
      emitlabel(l);
      emitendcard
    end;


  procedure emitcasecondition(selector: integer);

    procedure emitoneexpression;
      var
        parentheses: integer;
        reachedend: boolean;
      begin
        parentheses := 0;
        reachedend := false;
	repeat
	  gettoken(token);
	  if token in [eoftoken, commatoken, colontoken] then
	    begin
	      pushbacklexeme(lexeme.str, lexeme.len);
	      reachedend := true
	    end
	  else if token = newlinetoken then lexeme.len := 0
	  else if lexeme.str[1] = '(' then parentheses := parentheses + 1
	  else if lexeme.str[1] = ')' then parentheses := parentheses - 1;

	  if lexeme.len > 0 then emitlexeme(lexeme.str, lexeme.len)
	until reachedend or (parentheses <= 0);

	if parentheses <> 0 then
	  begin
	    writeln('A parenthesis is missing from this expression.');
	    error
          end
      end;

    begin { emitcasecondition }
      emitstring10('IF (      ', 4);
      repeat
        emitchar('I'); emitlabel(selector);
        emitstring10 ('.EQ.(     ', 5);
        emitoneexpression;
        emitchar(')');
        gettoken(token);
        if token = commatoken then emitstring10('.OR.      ' , 4)
      until token <> commatoken;
      emitstring10(') THEN    ', 6);
      emitendcard;
      if token <> colontoken then
        begin
          writeln('Missing colon in CASE.');
          error
        end
    end;


  function howmany: integer;
    var
      lookahead: tokenkind;
      savelex: lextype;
      levels: integer;
    begin
      savelex := lexeme;
      gettoken(lookahead);
      if not (lookahead in [newlinetoken, semicolontoken, rightbracetoken])
      then
        begin
          if lookahead = labeltoken then { we have a number }
            levels := tonumber(lexeme.str, lexeme.len)
          else
            begin
              write('I expected BREAK/NEXT to be followed by');
              writeln(' a positive integer.');
              error
            end
        end
      else
        begin
          levels := 1; { the default is the current loop }
          pushbacklexeme(lexeme.str, lexeme.len);
          lexeme := savelex
        end;
      howmany := levels
    end;


  procedure programrule(stopat: tokenset);

    procedure statementrule;

      procedure unknownrule;
	begin
	  tocolumn7;
	  emitlexeme(lexeme.str, lexeme.len);
          emitunknown(false);
          emitendcard
	end;

      procedure nextrule;
        var
          nextlabel, dummy: integer;
        begin
          retrievejump(nextlabel, dummy, howmany);
          emitgoto(nextlabel);
        end;

      procedure breakrule;
        var
          dummy, breaklabel: integer;
        begin
          retrievejump(dummy, breaklabel, howmany);
          emitgoto(breaklabel);
        end;

      procedure returnrule;
        var
          lookahead: tokenkind;
          savelex: lextype;
          hasexpression: boolean;
        begin
          tocolumn7;
          savelex := lexeme;
          gettoken(lookahead);
          if lexeme.str[1] = '(' then hasexpression := true
          else hasexpression := false;
          pushbacklexeme(lexeme.str, lexeme.len);
          lexeme := savelex;
          if hasexpression then
            begin
              if functionname.len = 0 then
                begin
                  writeln('Hmm, are you sure this is a function?');
                  error
                end;
              emitlexeme(functionname.str, functionname.len);
              emitchar('=');
              emitexpression;
              emitendcard;
              tocolumn7
            end;
          emitstring10('RETURN    ', 6);
          emitendcard
        end;

      procedure labelrule;
	begin
          if tonumber(lexeme.str, lexeme.len) > 23000 then
	    writeln('This label may conflict with the generated ones.');
	  emitlexeme(lexeme.str, lexeme.len);
          statementrule
	end;

      procedure whilerule;
        var
          nextlabel, breaklabel: integer;
	begin
          nextlabel := generatelabel;
          breaklabel := generatelabel;
          pushjump(nextlabel, breaklabel);
	  emitcontinue;
          emitlabel(nextlabel);

          emitifgoto(breaklabel);
          statementrule;
          emitgoto(nextlabel);

          emitlabel(breaklabel);
          emitcontinue;
          popjump
        end;

      procedure repeatrule;
        var
          repeatlabel, nextlabel, breaklabel: integer;
          lookahead: tokenkind;
          savelex: lextype;
        begin
          repeatlabel := generatelabel;
          nextlabel := generatelabel;
          breaklabel := generatelabel;
          pushjump(nextlabel, breaklabel);
	  emitcontinue;
          emitlabel(repeatlabel);
	  emitcontinue;

          statementrule;
          emitlabel(nextlabel);

          savelex := lexeme;
          gettoken(lookahead);
          if lookahead = untiltoken then emitifgoto(repeatlabel)
          else
            begin
              emitgoto(repeatlabel);
              pushbacklexeme(lexeme.str, lexeme.len);
              lexeme := savelex
            end;
          emitlabel(breaklabel);
	  emitcontinue;
          popjump
        end;

      procedure forrule;
        var
          nextlabel, breaklabel, skiplabel, auxlabel: integer;
          hascondition: boolean;
          lookahead: tokenkind;
          savelex: lextype;
        begin
          nextlabel := generatelabel;
          breaklabel := generatelabel;

          emitcontinue;
          gettoken(token);
          if lexeme.str[1] <> '(' then
            begin
              writeln('Missing opening parenthesis in FOR statement.');
              error
            end;

          { Handle possible initializer }
          gettoken(lookahead);
          if lookahead <> semicolontoken then
            begin
              tocolumn7;
              emitlexeme(lexeme.str, lexeme.len);
              emitunknown(false);
              emitendcard;
            end;

          { Handle possible condition }
          savelex := lexeme;
          gettoken(lookahead);
          if lookahead <> semicolontoken then
            begin
              pushbacklexeme(lexeme.str, lexeme.len);
              lexeme := savelex;
              emitlabel(nextlabel);
              tocolumn7;
              emitstring10('IF (.NOT.(', 10);
              emitunknown(false);
              emitstring10(')) GO TO  ', 9);
              emitlabel(breaklabel);
              emitendcard;
              hascondition := true
            end
          else hascondition := false;

          { Handle possible increment part }
          gettoken(lookahead);
          if lexeme.str[1] <> ')' then
            begin
              pushbacklexeme(lexeme.str, lexeme.len);
              skiplabel := generatelabel;
              emitgoto(skiplabel);
              if hascondition then
                begin
                  auxlabel := nextlabel;
                  nextlabel := generatelabel
                end;
              emitlabel(nextlabel);
              tocolumn7;
              emitunknown(true);
              emitendcard;
              if hascondition then emitgoto(auxlabel);
              emitlabel(skiplabel);
              emitcontinue
            end
          else if not hascondition then
            begin
              { We have neither a condition nor an increment statement }
              emitlabel(nextlabel);
              emitcontinue
            end;

          pushjump(nextlabel, breaklabel);
          statementrule;
          emitgoto(nextlabel);
          emitlabel(breaklabel);
          emitcontinue;
          popjump
        end;

      procedure dorule;
        var
          nextlabel, breaklabel: integer;
        begin
          nextlabel := generatelabel;
          breaklabel := generatelabel;
          pushjump(nextlabel, breaklabel);

	  tocolumn7;
	  emitlexeme(lexeme.str, lexeme.len);
          emitchar(' ');
          emitlabel(nextlabel);
          emitchar(' ');
          emitunknown(false);
          emitendcard;
          statementrule;
          emitlabel(nextlabel);
          emitcontinue;
          emitlabel(breaklabel);
          emitcontinue;
          popjump
        end;

      procedure switchrule;
        var
          selector: integer;
          first: boolean;
        begin
          selector := generatelabel;
          tocolumn7;
          emitchar('I'); emitlabel(selector); emitchar('=');
          emitexpression;
          emitendcard;

          repeat
            gettoken(token)
          until token <> newlinetoken;
          if token <> leftbracetoken then
            begin
              writeln('Missing opening brace in SWITCH statement.');
              error
            end;

          repeat
            gettoken(token)
          until token <> newlinetoken;
          first := true;

          { Handle CASE ...,: }
          while token = casetoken do
            begin
              tocolumn7;
              if not first then emitstring10('ELSE      ', 5);
              emitcasecondition(selector);
              programrule([eoftoken, rightbracetoken,
                           casetoken, defaulttoken]);
              first := false
            end;

          { Handle possible DEFAULT: }
          if token = defaulttoken then
            begin
              gettoken(token);
              if token <> colontoken then
                begin
                  writeln('Missing colon after DEFAULT.');
                  error
                end;
              if not first then emitelse;
              tocolumn7;
              programrule([eoftoken, rightbracetoken,
                           casetoken, defaulttoken]);
            end;

          if token <> rightbracetoken then
            begin
              writeln('Missing closing brace in SWITCH.');
              error
            end;
          gettoken(token); { so we don't fall through ... }
          pushbacklexeme(lexeme.str, lexeme.len);

          if not first then emitendif
        end;

      procedure ifrule;
        var
          lookahead: tokenkind;
          savelex: lextype;
        begin
          emitifthen;
          statementrule;

          savelex := lexeme;
          repeat
            gettoken(lookahead)
          until lookahead <> newlinetoken;
          if lookahead = elsetoken then
            begin
              emitelse;
              statementrule
            end
          else
            begin
              pushbacklexeme(lexeme.str, lexeme.len);
              lexeme := savelex
            end;
          emitendif
        end;

      procedure definerule;
        var
          where, count, parentheses: integer;
        begin
          gettoken(token);
          if lexeme.str[1] = '(' then
            begin { DEFINE (name, replacement) }
              gettoken(token);
              where := insertdefine(lexeme.str, lexeme.len);
              definetable[where].replacement := nextfree;
              gettoken(token);
              if token <> commatoken then
                begin
                  writeln('Comma expected in parenthesized definition.');
                  error
                end;
              repeat getchar(ch)
              until ch <> ' ';
              { get replacement text upto closing parenthesis }
              count := 0;
              parentheses := 1;
              repeat
                if ch = '(' then parentheses := parentheses + 1
                else if ch = ')' then parentheses := parentheses - 1;
                if parentheses > 0 then
                  begin
                    storechar(ch);
                    count := count + 1;
                    getchar(ch)
                  end
              until (parentheses <= 0) or (ch = eofchar);
              if parentheses <> 0 then
                begin
                  writeln('Replacement text contains unbalanced parentheses.');
                  error
                end;
              definetable[where].replacelen := count
            end
          else
            begin { DEFINE name replacement }
              where := insertdefine(lexeme.str, lexeme.len);
              definetable[where].replacement := nextfree;
              repeat getchar(ch)
              until ch <> ' ';
              { get replacement text upto end of line }
              count := 0;
              while not (ch in [eolnchar, eofchar]) do
                begin
                  storechar(ch);
                  count := count + 1;
                  getchar(ch)
                end;
              pushbackchar(ch);
              definetable[where].replacelen := count
            end
        end;

      procedure includerule;
        var
          i: integer;
        begin
          repeat getchar(ch)
          until ch <> ' ';
          i := 0;
          while (not (ch in [';', ' ', eolnchar, eofchar])) and
                (i < lexemesize) do
            begin
              i := i + 1;
              filename[i] := ch;
              getchar(ch)
            end;
          if i = 0 then
            begin
              writeln('I expected INCLUDE to be followed by a filename.');
              error
            end
          else filename[i+1] := filenameterminator;
          write(' ':filenumber+1, 'including "');
          outputfilename(filename);
          writeln('"');
          fileopen
        end;


      begin { statementrule }
        repeat gettoken(token)
        until token <> newlinetoken;

        if token = whiletoken then whilerule
        else if token = repeattoken then repeatrule
        else if token = fortoken then forrule
        else if token = dotoken then dorule
        else if token = switchtoken then switchrule
        else if token = iftoken then ifrule
        else if token = labeltoken then labelrule
        else if token = returntoken then returnrule
        else if token = breaktoken then breakrule
        else if token = nexttoken then nextrule
        else if token = includetoken then includerule
        else if token = definetoken then definerule
        else if token = leftbracetoken then
          begin { handle compound statement (grouping) }
            programrule([eoftoken, rightbracetoken]);
            if token <> rightbracetoken then
              begin
                writeln('Missing closing brace.');
                error
              end;
            token := newlinetoken { so we don't fall through ... }
          end
        else if token in [unknown, functiontoken] then unknownrule
      end;

    begin { programrule }
      repeat statementrule
      until token in stopat
    end;


  { Main program }
  begin
    greeting;
    initializeglobals;
    openfiles;
    writeln(fortran, '* -- Machine generated FORTRAN 77');
    writeln(fortran, '* -- code created by RATFOR-77.');
    writeln(fortran, '* -- Not intended for human consumption.');
    programrule([eoftoken]); { start the parser }
    closefiles;
    writeln('Done.')
  end.
