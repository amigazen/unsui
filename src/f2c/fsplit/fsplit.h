# fsplit.h - This file is part of FSPLIT.

include machdep.h

# We won't accept lines longer than this.
# Note that this also limits the length
# of the input file name!

define LINELEN    80

# File descriptors

define FORTRAN  1
define SCRATCH  2
define OUTPUT   3

define OTHER   0
define BLOCK   1
define ENDSTAT 2

# Default filenames. They are used if fsplit cannot
# determine the name of the current program unit.
# E.g., 6 characters which will leave room for
# the two-digit generation number under MS-DOS


define SUBNAME 'subrou'
define FUNNAME 'functn'
define PRGNAME 'progrm'
define BLKNAME 'blkdta'

define MAXNAME 6

