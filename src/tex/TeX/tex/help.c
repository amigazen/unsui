/* help.c */

char *help_messages[] = { 0L,

/* buildbox.c: 1 */
"You should say `\\leaders <box or rule><hskip or vskip>'.\n\
I found the <box or rule>, but there's no suitable\n\
<hskip or vskip>, so I'm ignoring these leaders.",

"Sorry; this \\lastbox will be void.",

"Sorry...I usually can't take things from the current page.",
"This \\lastbox will therefore be void.",

"I'm working on `\\vsplit<box number> to <dimen>';\n\
will look for the <dimen> next.",

"I was expecting to see \\hbox or \\vbox or \\copy or \\box or\n\
something like that. So you might find something missing in\n\
your output. But keep trying; you can fix this later.",

/* dump.c: 7 (12) */

"`{...\\dump}' is a no-no.",

/* gettoken.c: 8 (13) */

"A funny symbol that I can't read has just been input.\n\
Continue, and I'll forget that it ever happened.",


/* itex.c: 9 (15) */

"All patterns must be given before typesetting begins.",
"(See Appendix H.)",


/* overflow.c: 11 (17) */

"If you really absolutely need more capacity,\n\
you can ask a wizard to enlarge me.",


/* math.c: 12 (19) */

"Somewhere in the math formula just ended, you used the\n\
stated character from an undefined font family. For example,\n\
plain TeX doesn't allow \\it or \\sl in subscripts. Proceed,\n\
and I'll try to forget that I needed that character.",

"Sorry, but I can't typeset math unless \\textfont 2\n\
and \\scriptfont 2 and \\scriptscriptfont 2 have all\n\
the \\fontdimen values needed in math symbol fonts.",

"Sorry, but I can't typeset math unless \\textfont 3\n\
and \\scriptfont 3 and \\scriptscriptfont 3 have all\n\
the \\fontdimen values needed in math extension fonts.",

"The `$' that I just saw supposedly matches a previous `$$'.\n\
So I shall assume that you typed `$$' both times.",


/* scanning.c: 16 (31) */

"A left brace was mandatory here, so I've put one in.\n\
You might want to delete and/or insert some corrections\n\
so that I will find a matching right brace soon.\n\
(If you're confused by all this, try typing `I}' now.)",

"I'm going to assume that 1mu=1pt when they're mixed.",

"A number should have been here; I inserted `0'.\n\
(If you can't figure out why I needed to see a number,\n\
look up `weird error' in the index to The TeXbook.)",

"You can refer to \\spacefactor only in horizontal mode;\n\
you can refer to \\prevdepth only in vertical mode; and\n\
neither of these is meaningful inside \\write. So",

"I'm forgetting what you said and using zero instead.",

/* 21 (43) */

"I changed this one to zero.",

"A register number must be between 0 and 255.",
"A character number must be between 0 and 255.",
"Since I expected to read a number between 0 and 15,",
#if 0 /* TeX 3.141 */
"A numeric math code must be between 0 and 32767.",
#else
"A mathchar number must be between 0 and 32767.",
#endif
"A numeric delimiter code must be between 0 and 2^{27}-1.",

"A one-character control sequence belongs after a ` mark.\n\
So I'm essentially inserting \\0 here.",

"I can only go up to 2147483647='17777777777=\"7FFFFFFF,\n\
so I'm using that number instead of yours.",

/* 29 (53) */

"I dddon't go any higher than filll.",

"The unit of measurement in math glue must be mu.",

"Dimensions can be in units of em, ex, in, pt, pc,\n\
cm, mm, dd, cc, bp, or sp; but yours is a new one!\n\
I'll assume that you meant to say pt, for printer's points.",

"To recover gracefully from this error, it's best to\n\
delete the erroneous units; e.g., type `2' to delete\n\
two letters. (See Chapter 27 of The TeXbook.)",

"I can't work with sizes bigger than about 19 feet.\n\
Continue and I'll use the largest value I can.",

"I was looking for a control sequence whose\n\
current meaning has been defined by \\font.",

"To increase the number of font parameters, you must\n\
use \\fontdimen immediately after the \\font is loaded.",


/* shipout.c: 36 (67) */

"I can't handle that very well; good luck.",

"On this page there's a \\write with fewer real {'s than }'s.",

"The page just created is more than 18 feet tall or\n\
more than 18 feet wide, so I suspect something went wrong.",


/* tex8.c: 39 (71) */

"You've closed more groups than you opened.\n\
Such booboos are generally harmless, so keep going.",

"Your sneaky output routine has problematic {'s and/or }'s.",

"Your \\output commands should empty \\box255,\n\
e.g., by saying `\\shipout\\box255'.\n\
Proceed; I'll discard its present contents.",

"I'm guessing that you meant to end an alignment here.",

"I'll pretend you didn't say \\long or \\outer here.",

"I'll pretend you didn't say \\long or \\outer or \\global.",

"You should have said `\\read<number> to \\cs'.\n\
I'm going to look for the \\cs now.",

"I'm going to use 0 instead of that illegal code value.",


/* tex7.c: 47 (83) */

"To put a horizontal rule in an hbox or an alignment,\n\
you should use \\leaders or \\hrulefill (see The TeXbook).",

"I'm changing to \\insert0; box 255 is special.",

"Try `I\\vskip-\\lastskip' instead.",
"Try `I\\kern-\\lastkern' instead.",
"Perhaps you can make the output routine do it.",

"Sorry, Pandora. (You sneaky devil.)\n\
I refuse to unbox an \\hbox in vertical mode or vice versa.\n\
And I can't open any boxes in math mode.",

"Sorry: The third part of a discretionary break must be\n\
empty, in math formulas. I had to delete your third part.",

"Wow---I never thought anybody would tweak me here.\n\
You can't seriously need such a huge discretionary list?",

"Discretionary lists must contain only boxes and kerns.",

"I've put in what seems to be necessary to fix\n\
the current column of the current alignment.\n\
Try to go on, since this might almost work.",

"I can't figure out why you would want to use a tab mark",
"here. If you just want an ampersand, the remedy is\n\
simple: Just type `I\\&' now. But if some right brace",
"up above has ended a previous alignment prematurely,\n\
you're probably due for more error messages, and you\n\
might try typing `S' now just to see what is salvageable.",
"or \\cr or \\span just now. If something like a right brace",

"I expect to see \\noalign only after the \\cr of",
"an alignment. Proceed, and I'll ignore this case.",
"I expect to see \\omit only after tab marks or the \\cr of",

"I'm ignoring this, since I wasn't doing a \\csname.",

/* 65 (111) */

"I'm ignoring this misplaced \\limits or \\nolimits command.",

"I was expecting to see something like `(' or `\\{' or\n\
`\\}' here. If you typed, e.g., `{' instead of `\\{', you\n\
should probably delete the `{' by typing `1' now, so that\n\
braces don't get unbalanced. Otherwise just proceed.\n\
Acceptable delimiters are characters whose \\delcode is\n\
nonnegative, or you can use `\\delimiter <delimiter code>'.",

"I'm changing \\accent to \\mathaccent here; wish me luck.\n\
(Accents are not the same in formulas as they are in text.)",

"I treat `x^1^2' essentially like `x^1{}^2'.",
"I treat `x_1_2' essentially like `x_1{}_2'.",

/* 70 (122) */

"I'm ignoring this fraction specification, since I don't\n\
know whether a construction like `x \\over y \\over z'\n\
means `{x \\over y} \\over z' or `x \\over {y \\over z}'.",

"I'm ignoring a \\right that had no matching \\left.",

"Please don't say `\\def cs{...}', say `\\def\\cs{...}'.\n\
I've inserted an inaccessible control sequence so that your\n\
definition will be completed without mixing me up too badly.\n\
You can recover graciously from this error, if you're\n\
careful; see exercise 27.2 in The TeXbook.",

"I'm forgetting what you said and not changing anything.",

"I can't carry out that multiplication or division,\n\
since the result is out of range.",

"I allow only values in the range 1..32767 here.",
"I allow only nonnegative values here.",

"I can only handle fonts at positive sizes that are\n\
less than 2048pt, so I've changed what you said to 10pt.",

"(That was another \\errmessage.)",

"This error message was generated by an \\errmessage\n\
command, so I can't give any explicit help.\n\
Pretend that you're Hercule Poirot: Examine all clues,\n\
and deduce the truth by order and method.",

"This isn't an error message; I'm just \\showing something.\n\
Type `I\\show...' to show more (e.g., \\show\\cs,\n\
\\showthe\\count10, \\showbox255, \\showlists).",

"And type `I\\tracingonline=1\\show...' to show boxes and\n\
lists on your terminal as well as in the transcript file.",

/* 82 (148) */

"I can handle only one magnification ratio per job. So I've\n\
reverted to the magnification you used earlier on this run.",

"The magnification ratio must be between 1 and 32768.",


/* tex6.c: 84 (151) */

"Hyphenation exceptions must contain only letters\n\
and hyphens. But continue; I'll forgive and forget.",

"Letters in \\hyphenation words must have \\lccode>0.\n\
Proceed; I'll ignore the character I just read.",

"The box you are \\vsplitting contains some infinitely",

"shrinkable glue, e.g., `\\vss' or `\\vskip 0pt minus 1fil'.\n\
Such glue doesn't belong there; but you can safely proceed,",

"The box you are trying to split is an \\hbox.\n\
I can't split such a box, so I'll leave it alone.",

"Tut tut: You're trying to \\insert into a\n\
\\box register that now contains an \\hbox.",

"Proceed, and I'll discard its present contents.",

"The page about to be output contains some infinitely",

"The correction glue for page breaking with insertions\n\
must have finite shrinkability. But you may proceed,",

"You shouldn't use \\box255 except in \\output routines.",

"I've concluded that your \\output is awry; it never does a\n\
\\shipout, so I'm shipping \\box255 out myself. Next time\n\
increase \\maxdeadcycles if you want me to be more patient!",

"I've inserted a begin-math/end-math symbol since I think\n\
you left one out. Proceed, with fingers crossed.",

/* 96 (172) */

"Sorry, but I'm not programmed to handle this case;\n\
I'll just pretend that you didn't ask for it.\n\
If you're in the wrong mode, you might be able to\n\
return to the right one by typing `I}' or `I$' or `I\\par'.",

"I've inserted something that you may have forgotten.\n\
(See the <inserted text> above.)\n\
With luck, this will get me unwedged. But if you\n\
really didn't forget anything, try typing `2' now; then\n\
my insertion and my current dilemma will both disappear.",

"Things are pretty mixed up, but I think the worst is over.",

"I've deleted a group-closing symbol because it seems to be\n\
spurious, as in `$x}$'. But perhaps the } is legitimate and\n\
you forgot something else, as in `\\hbox{$x}'. In such cases\n\
the way to recover is to insert both the forgotten and the\n\
deleted material, e.g., by typing `I$}'.",

"since the offensive shrinkability has been made finite.",


/* tex0.c: 101 (188) */

"I have just deleted some text, as you asked.\n\
You can now delete more, or insert, or whatever.",

"Sorry, I don't know how to help in this situation.",
"Sorry, I already gave what help I could...",
"Maybe you should try asking a human?",
"An error might have occurred before I noticed any problems.\n\
``If all else fails, read the instructions.''",

"I'm broken. Please show this to someone who can fix can fix",

"One of your faux pas seems to have wounded me deeply...\n\
in fact, I'm barely conscious. Please fix it and try again.",

"You rang?\n\
Try to insert some instructions for me (e.g.,`I\\showlists'),\n\
unless you just want to quit by typing `X'.",


/* fatalerror()  109 (201) */

"End of file on the terminal!",
"(interwoven alignment preambles are not allowed)",
"*** (job aborted, no legal \\end found)",
"*** (cannot \\read from terminal in nonstop modes)",
"*** (job aborted, file error in nonstop mode)",


/* tex1.c: 114 (206) */

"A forbidden control sequence occurred in skipped text.",

"This kind of error happens when you say `\\if...' and forget\n\
the matching `\\fi'. I've inserted a `\\fi'; this might work.",

"The file ended while I was skipping conditional text.",

"I suspect you have forgotten a `}', causing me\n\
to read past where you wanted me to stop.\n\
I'll try to recover; but if the error is serious,\n\
you'd better type `E' or `X' now and fix your file.",

"The control sequence at the end of the top line\n\
of your error message was never \\def'ed. If you have\n\
misspelled it (e.g., `\\hobx'), type `I' and the correct\n\
spelling (e.g., `I\\hbox'). Otherwise just continue,\n\
and I'll forget about whatever was undefined.",

"The control sequence marked <to be read again> should\n\
not appear between \\csname and \\endcsname.",

/* 120 (212) */

"I've run across a `}' that doesn't seem to match anything.\n\
For example, `\\def\\a#1{...}' and `\\a}' would produce\n\
this error. If you simply proceed now, the `\\par' that\n\
I've just inserted will cause me to report a runaway\n\
argument that might be the root of the problem. But if\n\
your `}' was spurious, just type `2' and it will go away.",

"I suspect you've forgotten a `}', causing me to apply this\n\
control sequence to too much text. How can we recover?\n\
My plan is to forget the whole thing and hope for the best.",

"If you say, e.g., `\\def\\a1{...}', then you must always\n\
put `1' after `\\a', since control sequence names are\n\
made up of letters only. The macro here has not been\n\
followed by the required stuff, so I'm ignoring it.",

"I'm ignoring this; it doesn't match any \\if.",

/* tex5.c: 124 (216) */

"Displays can use special alignments (like \\eqalignno)\n\
only if nothing but the alignment itself is between $$'s.",

"So I've deleted the formulas that preceded this alignment.",

"There should be exactly one # between &'s, when an\n\
\\halign or \\valign is being set up. In this case you had",

"none, so I've put one in; maybe that will work.",

"more than one, so I'm ignoring all but the first.",

"You have given more \\span or & marks than there were\n\
in the preamble to the \\halign or \\valign now in progress.\n\
So I'll assume that you meant to type \\cr instead.",

"The paragraph just ended includes some glue that has\n\
infinite shrinkability, e.g., `\\hskip 0pt minus 1fil'.\n\
Such glue doesn't belong there---it allows a paragraph\n\
of any length to fit on one line. But it's safe to proceed,",

/* tex4.c */
/* tex3b.c: 131 (223) */

"I wasn't able to read the size data for this font,\n\
so I will ignore the font specification.\n\
[Wizards can fix TFM files using TFtoPL/PLtoTF.]\n\
You might try inserting a different font spec;\n\
e.g., type `I\\font<same font id>=<substitute font name>'.",

"I'm afraid I won't be able to make use of this font,\n\
because my memory for character-size data is too small.\n\
If you're really stuck, ask a wizard to enlarge me.\n\
Or maybe try `I\\font<same font id>=<name of loaded font>'.",


/* tex3.c: 133 (225) */

"Where was the left brace? You said something like `\\def\\a}',\n\
which I'm going to interpret as `\\def\\a{}'.",

"I'm going to ignore the # sign you just used.",

"I've inserted the digit you should have used after the #.\n\
Type `1' to delete what you did use.",

"You meant to type ## instead of #, right?\n\
Or maybe a } was forgotten somewhere earlier, and things\n\
are all screwed up? I'm going to assume that you meant ##.",

"This \\read has unbalanced braces.",

"I was expecting to see `<', `=', or `>'. Didn't.",

/* TeX 3.141: 139 */

"Sorry, \\setbox is not allowed after \\halign in a display,\n\
or between \\accent and an accented character.",

0L
};

/* -- end -- */
