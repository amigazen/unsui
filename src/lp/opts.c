/*
 *  OPTS.C
 */

static char *usage_array[] = {

  "INDENT",      "-i", "<indent> ", "set indention, left margin (default: 0)",
  "WIDTH",       "-w", "<charct> ", "set page width, right margin (default: 80)",
  "HEIGHT",      "-h", "<linect> ", "set page height in #of lines per page (default: 66)",
  "FILE",        "-u", "<infile> ", "force next argument to be the input filename",
  "TO",          "-o", "<outfile>", "set output device or file (default is PRT:)",
  "APPEND",      "-a", "         ", "append output to print destination (default for @tocfiles)",
  "NOAPPEND",    "+a", "         ", "overwrite print destination (default)",
  "REPLACE",     "-r", "         ", "replace the input file by the output file",
  "TABSIZE",     "-t", "[tabsize]", "set tabsize and convert tabs to spaces",
  "USETAB",      "+t", "         ", "send tabs, don't convert tabs to spaces",
  "CRONLY",      "-e", "         ", "print with AUTO CR/LF option (send CR only)",
  "LFONLY",      "+e", "         ", "turn off AUTO CR/LF option (send normal LF)",
  "USEFF",       "-f", "         ", "use formfeed (FF) characters for new pages",
  "NOFF",        "+f", "         ", "convert a formfeed into a sequece of linefeeds (default)",
  "PLAIN",       "-p", "         ", "print plain, don't (ENV:) init printer",
  "NOPLAIN",     "+p", "         ", "add ENV:InitPrinter to the top of the file (default)",
  "MINI",        "-m", "         ", "print one line superscript, next subscript...",
  "NOMINI",      "+m", "         ", "turn off MINI option '-m' (default)",
  "SKIPTOPAGE",  "-b", "<pageno> ", "begin printing with the first line of page <pageno>",
  "SKIPTOLINE",  "-s", "<lineno> ", "start with line <lineno> of the input file (default: 1)",
  "NOCLEANUP",   "-d", "         ", "don't delete spaces or tabs at the end of a line",
  "CLEANUP",     "+d", "         ", "delete spaces or tabs at the end of a line (default)",
  "LEADER",      "-l", "<string> ", "print <string> in front of each output line",
  "NOLEADER",    "+l", "         ", "do not print any leading string",
  "HEADER",      "-n", "         ", "print page header (filename, date)",
  "NOHEADER",    "+n", "         ", "turn off HEADER option '-n', don't print any header (default)",
  "SINGLESIDED", "-1", "[pageno] ", "single sided page numbering beginning with page [pageno]",
  "DOUBLESIDED", "-2", "[pageno] ", "page numbering for double sided output.",
  "NOFOOTINGS",  "+0", "         ", "no page numbering, no footings (default)",
  "COLUMNS",     "-c", "<columns>", "set #of columns of equal width (default: 1, max: 2)",
  "FEEDOUT",     "-x", "         ", "expand each file via LF to fill up its last sheet",
  "NOFEEDOUT",   "+x", "         ", "do not add anything to the end of file (default)",
  "WITH",        "@",  "<tocfile>", "print a list of files (each with it's arguments)",
  (char *)0L, (char *)0L, (char *)0L, (char *)0L
};

char **howtouse= &usage_array[0];

/*
 * AmigaDOS options:
 *
 *         /S Switch      /A Required            /T Toggle
 *         /K Keyword     /F Rest of line
 *         /N Number      /M Multiple strings
 *
 */
