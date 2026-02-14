VERSION = 1
REVISION = 1

.macro DATE
.ascii "2.11.2009"
.endm

.macro VERS
.ascii "mkfile 1.1"
.endm

.macro VSTRING
.ascii "mkfile 1.1 (2.11.2009)"
.byte 13,10,0
.endm

.macro VERSTAG
.byte 0
.ascii "$VER: mkfile 1.1 (2.11.2009)"
.byte 0
.endm
