#
#    	File Name :	login.sh   
#
#			- (hard disk version )tailored for WB 2.04
#
#    GMD - 14 Apr 92
#
# NOTE:  do not use AMIGADOS Run with csh4.0 , arp v39 ; it endcli's
#
echo ""
echo "login.sh - 14 Apr 92"
echo ""
#
#<<<<<<<<<<<<<<<<<<<<<<<<<   set up func keys 
#
#   - unset any default Func key settings
#
unset f1 f2 f3 f4 f5 f6 f7 f8 f9 f10
unset F1 F2 F3 F4 F5 F6 F7 F8 F9 F10
#
#  NOTE : Function key settings (mostly) need a "^M" sequence to
#	  cause a NewLine and thus command execution.
#
#
#
set _abbrev n

set f1 "cd sys:"^M
set f2 "cd df0:"^M
set f3 "cd df2:"^M
set f4 "cd ram:"^M
set f5 "cd C_dev:;lsl"^M
set f6 info^M
set f7 "pwd | input a; echo set F8 cdl $a | input b ; exec $b^M"^M
###set f7 "pwd | input a; echo cdl $a \>sys:work.sh | input b ; exec $b^M"^M
set f8  ";dir -s"^M
set f9  ";lsl "
set f10 "cdl "
set F1 "lsl sys:"^M
set F2 "lsl df0:"^M
set F3 "lsl df2:"^M
set F4 "lsl ram:"^M
set F5 "cdl sys:xpersonal"^M
set F6 "run Xoper"^M
set F7  memfrag^M
source s:F8.sh
set F9 "cdl sys:xpersonal/letters"^M
set F10 "sh make.sh"^M
set _titlebar "Csh519-GMD [ %n ]"
#
#
#>>>>>> _path ; csh uses it to track commands...
#
set _path RAM:,C:,C_bin:,D:,LC:,CDEV:bin/,SYS:system/,SYS:Tools/,SYS:xtoys/,WORK:JM/  
#
#
#>>>>>> PATH ; AmigaDross uses it 
#
PATH RESET ; PATH  RAM: C: D: LC: SYS:system SYS:tools C_AZTEC:bin  TOYS: 
#
##echo "loading alias , sets , SETS ...."
#
#	remove inbuilt aliases !!
#
unalias cls
unalias dswap
unalias exit
unalias cdir
unalias q
unalias manlist
#
#	Hard Disc backups - to df0: or df1: , %x = -days to start from
#
alias aa assign
alias addr "cdl sys:xpersonal/Addresses;em addr.lst"
alias amic "cdl amic:;run amic"
alias awk gawk
alias b	   "beep"
alias bbs "cdl sys:xbbs"
alias bb   "Backup"
alias bd "date >>K: ; b ; "
alias buh "%x Backup  sys: -U$x -FBackup:      -O -J -Z -f800;
alias buhI "%x Backup  sys: -I  -F             -O -J -Z -f800;
alias buhi "%x Backup  sys: -i  -F           -d\"*.o\"  -O    -Z -f800;
alias bu0 "%x Backup  sys: -U$x -Fdf0:       -d\"*.o\"  -O -J -Z -f800;
alias bu1 "%x Backup  sys: -U$x -Fdf2:       -d\"*.o\"  -O -J -Z -f800;
alias buL "%x Backup  sys: -U$x -L           -d\"*.o\"  ;
alias cdl "%d cd $d;lsl "
alias clogs "cp df0:*.log logs: ; cp df2:*.log logs: ; lsl logs:"
alias ch  run chessmaster:chessmaster
alias c "%d cd $d ; lsl "
alias comm "cd sys:ncomm/ncomm; ncomm"
alias copy cp
#
# cpi copies given files from current dir to mirror dir on DHn:
#
#usage:    cpi <n>  *		; copy all dirs files to DHn:
#
alias cpi "%dhx%Z  set DIR \@strmid( `pwd` 5 ) ; \
 set F \@files( $Z ) ; \
 set C \"cp -ouq\" $F "dh"$dhx:$DIR ; \
 echo $C ; \
 exec $C ; set C \"\" ; set F \"\" "
#
# cpir recursively copies current dir to mirror dir on DHn:
# usage : cpir <n>   ; where n is DHn:
#
alias cpir "set D \@strmid( `pwd` 5 ) ; \
 set C \"cp -qour \" \"dh0:\"$D  \"dh1:\"$D ; \
 echo $C ; \
 exec $C ; set C \"\" "
#
# cdi0, cdi1 changes dir to mirror image dir on other hard disk
#
alias cdi0 "set D \@strmid( `pwd` 5 ) ; \
 set C \"cd dh0:\"$D \"; lsl\" ; \
 echo $C ; \
 exec $C ; set C \"\" "

alias cdi1 "set D \@strmid( `pwd` 5 ) ; \
 set C \"cd dh1:\"$D \"; lsl\"  ; \
 echo $C ; \
 exec $C ; set C \"\" "

alias cra  " %x CR -a $x"^M
alias crr  " %x CR -r $x"^M
alias cron "newcli con:0/0/630/120/Cron/CLOSE FROM Cron:CronStart"
alias doc "%f cd Doc:$f ; lsl"
alias dm "%x Dmouse -s$x"
alias dcopy Turbobackup
alias dildor toys:Dildor/dildor
alias e ram:em
alias email "e sys:xpersonal/addresses/e-mail.lst"
alias env "cdl s: ; em go.sh"
alias f  "%f  Find df0: df2: -name $f -print" 
alias fhd  "%f  Find sys: ( -name "$f" )  -print"
alias fh "%f grep $f sys:du0.lst"
alias f0  "%f  Find df0: ( -name $f )  -print" 
alias f2  "%f  Find df2: ( -name $f )  -print" 
alias fred "cdl sys:FredFish"
alias form "%d Format DRIVE df$d: NAME GMD NOICONS"
alias formq "%d Format DRIVE df$d: NAME GMD NOICONS QUICK"
alias dform "%d MFormat DRIVE DI$d: NAME DOS"
alias fish "cdl pisces:cat"
alias g "%t set _back $_cwd ; cd $t ; pwd"
alias gb "cd $_back  ;pwd"
alias h howmany
alias lsl dir
alias l dir
alias lp lpr
alias ls dir -s
alias ll dir -S
alias login "start 1 ; start 2 ; start 0 ;"
alias logout "stop 1 ; stop 2 ; stop 0 ; cd ram: "
alias lo logout
alias li login
alias life "cdl sys:xlife; LIFE -o -t -p2 xmp/bomb"
alias logs "cdl logs:"
alias mm muchmore
alias mandelvroom "cd mandelbrot:;lsl; Mandelvroom"
alias makedir "%d mkdir $d ; echo `date` >$d/.mkdate ; protect $d/.mkdate -d"
alias mc "run mousecoords"
alias menu_refresh "menu -n ; sh s:menu.sh"
alias moria "cdl moria:; stack 50000 ; Moria"
alias p "%p  search -q  -n sys:xpersonal/Addresses/* $p"
alias pa cat "sys:xpersonal/addresses/*"
alias pp "%p search -q -c   -n sys:xpersonal/Addresses/* $p"
alias q "echo \"Type quit to exit Csh\""
alias res "%x Backup -R    $x"
alias rel "%x Backup -R -L $x"
alias rml "%l set f @files( * ) ; foreach j ( $f ) \"if @filelen( $j ) <= $l ; echo $j ;rm $j\ ; endif""
alias swl "%d cd /$d;lsl"
alias say "%f cp $f SPEAK:opt/f/s160"
alias sh source
alias setdate "cat s:setdate"
alias s "set;SET"
alias show "%f lha e $f  T:; set temp @nameroot( $f ) ; e T:$temp ; rm T:$temp "
alias ss search -c 
alias stop  "%n ram:motor $n 0 ; echo SCSI id=$n stopped" 
alias stopall "cd RAM: ; stop 2 ; stop 1 ; stop 0 ;"
alias startall "start 2 ; start 1 ; start 0 ;"
alias start "%n ram:motor $n 1 ; echo SCSI id=$n started" 
alias toys "cdl toys:"
alias tt "date"
alias touchall  "ram:Find sys: ( -name \\* )  -print -exec ram:Touch {} \\; " 
alias u  "cd .. ; lsl"
alias uu "cd .. ; cd .. ; lsl"
alias uuu "cd .. ; cd .. ; cd .. ; lsl"
alias virus  "assign K: ; echo >ram:sink; assign K: ram:sink; cp d:find d:kv ram:; date >>K:;  ram:Find sys: -type f -exec ram:kv -I  {} \\; >>K:;  bd"
alias win "window 0 0 639 240; lsl"
alias w pwd
alias world "cd sys:world; wdb"
alias ww "cd sys:walter; em ww" 
#
#  toys next
#
alias dk 	toys:dk
alias flip 	toys:flip
alias mondrian 	toys:mondrian
alias oing 	toys:oing
alias suck	toys:suck
alias closeme	toys:closeme
#
set _history 25
#
#	AZTEC env variables
#

MANX_SET INCLUDE=C_inc:             
MANX_SET CLIB=c_aztec:lib!c_aztec:libD;
# 						AZTEC SDB
MANX_SET "SDBOPT=-cs1,3,2,0 -cd3,2 -cc0,0,1,2" 		
#
#  set up the menus
#
source s:menu.sh
#
# shrink window a bit to expose bottom of hard disk icons
#
window 0 0 639 240
version
dir -s
info
echo " " 
echo "The bluebird of happiness does not alight on the hand that seeks... "
echo " "
date
