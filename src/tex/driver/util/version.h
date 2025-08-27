/*
 * The programs ShowDVI and DVIprint are copyright Georg Heßmann.
 * You can change and distribute the programs under the terms of
 * the GNU general public license. See the file COPYING.
 *
 * Don't delete the copyright notice from the programs.
 *
 * If you spread a changed version of the programs, they still have to
 * call "WarningStr(NULL)" to show my copyright message.
 * But you should append a short notice to the message to show, that
 * this is no original version.
 *
 * Don't delete or change this header.
 *
 * Georg Heßmann (hessmann@informatik.uni-hamburg.de)
 *               (http://tech-www.informatik.uni-hamburg.de/Personal/hessmann)
 * 26.May.95
 *
 */


/**********************************************************************/
/************************  Program Version  ***************************/
/**********************************************************************/
/***********************   13.11.89  (hes)   **************************/
/**********************************************************************/


/* Change log:
 *
 * Early 1985, (nmh) -- ported sun version to Apollo.
 * 30-Mar-85 (nmh) -- added -a option to specify a different PXL area
 * 12-Jun-85 (nmh) -- v1.02 - process DVI pages in reverse order
 * 20-Aug-85 (nmh) -- v1.03
 * 24-Aug-85 (nmh) -- v2.00
 * 15-Mar-88 (rbs) -- ported sun version to atari st
 *                 -- v3.00
 * 06-Apr-88 (rbs) -- v3.12
 * 30-Apr-88 (rbs) -- v3.13
 * 31-May-88 (rbs) -- v3.14
 * 02-Jun-88 (rbs) -- ported for screen-output (Atari ST 1040)
 *                 -- v3.99a
 * 04-Jul-88 (hes) -- ported on AMIGA 1000
 *                 -- v3.99m
 * 23-Jul-88 (hes) -- added functions to jump trough the dvi-file
 *                 -- v3.991m
 * 28-Jul-88 (hes) -- added functions to change the displayed dvi-file
 *                 -- v3.992m
 * 19-Aug-88 (rbs) -- reported on ATARI ST
 *                 -- v3.99n
 * 20-Aug-88 (rbs) -- revised list functions
 *                    added scanning for pointer to the begin of pages
 *                     in the dvifile (to find a specified page faster)
 *                 -- v3.991n
 * 26-Jan-89 (hes) -- little changes in the message system,
 *		      report the loading (removing) of the fonts,
 *		      Changed name of the logfile.
 *		      Add in globals.h SHOWDVI_LOGFILE
 *		      CHANGED the Version number!! (for better increasing)
 *		   -- v0.50	(without index!)
 * 12-Feb-89 (hes) -- merged dviprint, showdvi and flib in one dir. system
 *		      add file 'dvihand.c', a part of 'dviprint.c' which
 *		      used in showdvi and dviprint.
 *		      Added a new Fatal-Mes. in 'liste.c' (in_list).
 *		      Make the arrows on the (AMIGA) scrollbars.
 *		   -- v0.51
 * 09-Jun-89 (hes) -- Inserted author names (crypted).
 *		      Updating prog-arguments, add -? help text.
 *		      Changing the default hoffset and voffset.
 *		      Correcting the offsets by changing to the draft-modus
 *		      in dviprint.c.
 *		   -- v0.52
 * 11-Jul-89 (hes) -- Changing the Fatal-handling (errorn numbers).
 *		      Many changes in amscreen.c.
 *		      Adding page_counter function in showdvi.c.
 *		   -- v0.53
 * August 89 (rbs) -- porting code to ANSI compiler (Turbo C 1.1 for ATARI ST)
 *                    fixing a few bugs
 * 		      now IMG files can be include within the special command
 *		   -- v0.53
 * 04-Sep-89 (rbs) -- pre-release of an ATARI-version for screen previewing
 *		   -- v0.54
 * 09-Oct-89 (hes) -- Adding Pop-Up-Menu to AMIGA-Version (am_menu.c),
 *		      little changes in amscreen.c and gadget.c,
 *		      added define FONTS_IN_CHIPMEM for using system
 *		      CopyBitArray (but, it is a little bit to slow,
 *                    too much overhead).
 *		   -- v0.54
 * 05-Nov-89 (hes) -- Adding color/about/help-requester (am_requ.c),
 *                    Add KOMM+4L : load NEW DVI-File,
 *                    change load file again - do not change page-number!
 *                 -- v0.55
 * 26-Nov-89 (hes) -- all change logs now here in util/version.h,
 *		      dviprint: now module newhard.c, make the same as
 *				hardcopy.c but FASTER.
 *				split the giant switch-command in
 *				three pieces.
 *				Add -z option to change the dpi.
 *		   		-- v0.60
 *		      showdvi:  add filerequester+little changes in
 *				AMIGA part.
 *				Add -z option to change the dpi.
 *				-- v0.60
 *		      flib:     change version number :->
 *				-- v0.60
 * 03-Dec-89 (hes) -- add SAVE_BITS_X,Y in bitmap.[ch] for adding
 *		      1/4 inch distance to the right/lower border.
 *		      h/v-offset now in any unit (in,cm,mm,...) available.
 *		      showdvi:	submenus for the popup-menu,
 *				change resolution 83dpi - 100dpi - 120dpi
 *				not totaly implemented, because init_fontmt
 *				work with fixed resolution :-(
 *				correct the screen-sizing alg. in amscreen.c.
 *				-- v0.61
 *		      dviprint: Revising DecodeArgs and Print Help,
 *				add -u option,
 *				fix bug in fast_copy.SetChar(),
 *				add SAVE_BITS_X,Y in bitmap.c for adding
 *				1/5 inch distance to the lower and 
 *				1/2 inch to the right border.
 *				-- v0.61
 * 07-Dec-89 (hes) -- showdvi:	AMIGA-Version...
 *				work correct with pages which are smaller than
 *				the window.
 *				-- v0.62
 *		      dviprint: add -d option.
 *				-d 1 prints on a HP-DeskJet.
 *				-- v0.62
 * 02-Jan-90 (hes) -- showdvi:  bug fix in full-screen modi,
 *				add '!' command, open a new shell-window.
 *				-- v0.63
 * 13-Feb-90 (rbs) -- adapting Georg's code to Atari ST
 *				-- showdvi: using only GEM routines (should work
 *				            on all monitors)
 *				            implemented windows
 *				-- v0.64
 * 06-Mar-90 (hes) -- fixed bug in fontmt.c (crash if flib not found).
 *		      fixed bug in fontmt.c/alloc_char return value incorrect.
 *		      dviprint: add -m and -e options to control the memory
 *				usage of the program.
 *				add -d 2 option. Prints on a Epson-FX printer.
 *			  	-- v0.63
 *		      showdvi:	add three new lines to the config-file:
 *				beep on warnings, default resolution and resolution menu.
 *				revised page-scroll-modus. Now it works correct!
 *				-- v0.65
 * 11-Mar-90 (hes) -- adapting it for AMIGA Aztec-C 5.0,
 *		      change all function definitions to ANSI-style.
 *		      dviprint: --v0.64
 *		      showdvi:	--v0.66
 * 20-Mar-90 (hes) -- brand new fonthandling!!
 *		      Now works with _ALL_ magsteps or whatever!
 *		      New files flmt.[chi] (flib handling) and new_font.[chi]
 *		      (fonthandling). pkload.c changed to newpk.c.
 *		      Complex search strategy for font searching:
 *			1. search for defined fonts; in flibs or pk-files (defined in user-file)
 *			2. search for defined flibs (defined in user-file)
 *			3. search flib in searchlist of directories (environment var)
 *			4. search _pk-file_ in searchlist of directories (env. var)
 *		      Define the config-directory in an environment-variable.
 *		      dviprint:
 *				-- v0.70
 *		      showdvi:	implement the change resolution menu.
 *				add AMIGA-w key-function for save_configuration.
 *				-- v0.70
 * 23-Mar-90 (rbs) -- improved Georg's fonthandling, fixed a few bugs
 *		        in new_font.c.
 *		      substituted fonts are now shared with the orig fonts
 *                      (changed font structures in new_font.h)
 *		      showdvi:
 *				implemented file selecetor box for ATARI
 * 				-- v0.74
 * 29-Mar-90 (rbs) -- fixed a few bugs in showdvi (list handling)
 *		      now corrupted DVI files can be printed and displayed
 *		      showdvi:
 *				-- v0.76
 *		      dviprint:
 *				-- v0.72
 * 03-Apr-90 (hes) -- fixed little bugs in new_font.c and other little changes.
 *		      create font groups (saves memory).
 *		      change the ^C handling for the lattice compiler (AMIGA)
 *		      dviprint:
 *				revised the fonthandling (define and load)
 *				-- v0.74
 *		      showdvi:	(AMIGA)
 *				now the file select box works correct,
 *				implement an ARexx Port,
 *				implement function-key assignment.
 *				new define variable: esc_exit (on/off)
 *				-- v0.78
 * 10-Apr-90 (rbs) -- changed the order of font structures in new_font.h
 *		        (Turbo C V1.1 for Atari ST bombed with the old order !?)
 *		      fixed a few bugs in new_font.c (swapping of fonts)
 *		      showdvi:
 *				-- v0.80
 *		      dviprint:
 *				-- v0.76
 *		PS: Georg, what's about version number 1.0?
 * 20-Apr-90 (rbs) -- fixed a few int-long conversions in newhard.c
 *		      dviprint:
 *				-- v0.78
 * 24-Apr-90 (hes) -- fixed little bugs in new_font.c and showdvi.c
 *		      dviprint: -- v0.79
 *		      showdvi:  (AMIGA)
 *				new modus for the pop-up menu.
 *				new entry in the config file (int-menu)
 *				-- v0.81
 * 02-Mai-90 (hes) -- add a 'special' string handling for the AMIGA.
 *		      Works with an auxiliary program witch the special string
 *		      evalute and returns a bitmap.
 *		      New source files: util/amiga/special.[ch] and
 *		      an example as aux. program: util/amiga/specialhost.c 
 *		      dviprint:	Add default printer, resolution and
 *				print direction to the files globals.h and
 *				dviprint.c.
 *				-- v0.80
 *		      showdvi:	Fixed bug in the full-screen modi (div 0).	
 *				Copy the key-handling from amscreen.c to amkey.c
 *				and improve it.
 *				-- v0.82
 * 18-Jun-90 (hes) -- dviprint: Change the -d numbers of the printers.
 *				Add a new Epson and a new Deskjet driver.
 *				Improve ^C handling.
 *				-- v0.83
 *		      showdvi:	add skippage.[ch] to skip pages without preamble.
 *				(AMIGA)
 *				Do not leave the program, if a other window
 *				is on the screen.
 *				Add AmigaTeX ARexx commands.
 *				-- v0.83
 * 23-Jun-90 (hes) -- Add the font-caching algoithmus from AmigaTeX to our
 *		      Amiga Version. Look in the file TeX:pk/fontvols and
 *		      copy pk-files to TeX:pk/...
 *		      dviprint: Change _abort for dviprint/AMIGA.
 *				-- v0.84
 *		      showdvi:	Load font topaz 11, use it for the popup-menu.
 *				(AMIGA)
 *				-- v0.84
 * 26-Jun-90 (hes) -- This is the pre-release Version vor c't (AMIGA).
 *		      dviprint:	-- v0.90
 *		      showdvi:	-- v0.90
 * 02-Jul-90 (hes) -- Fixed bug in new_font.c, initialize font location in
 *		      common elements.
 *		      Possible compiler fault: Error in parsit (new_font.c).
 *		      New search list structure (it's smaller and works).
 *		      New entrys in Logfile with -s option (new_font.c).
 *		      dviprint:	-- v0.91
 *		      showdvi:	change the algotithmus of parse_line in config.c
 *				(only AMIGA)
 *				-- v0.91
 * 16-Jul-90 (hes) -- showdvi:  Fix open font bug, close fonts, refresh gadgets
 *				if the window becomes activated. (only AMIGA)
 *				-- v0.92
 *		      dviprint: Add landscape code (module landscap.[ci])
 *				(but still dosn't work)
 *				-- v0.92
 * 01-Aug-90 (hes) -- Change the \special-handling (AMIGA Version).
 *		      This is the release Version!
 *		      showdvi:	unset pscro if printing (AMIGA).
 *				-- v1.00
 *		      dviprint:	Landscape modus now works.
 *				Add -i Option to print to IFF-Files (AMIGA).
 *				-- v1.00
 * 19-Aug-90 (hes) -- Add a new ARexx-Port to ShowDVI and DVIprint to call
 *		      Metafont, if an font dosn't exists.
 *		      Use the environment var "CALLMF". (only ENV: variable!)
 *		      showdvi:	Fixed bug in arexx.c/loadagain. (AMIGA)
 *				Change 'realy' to 'really' in am_requ.h.
 *				-- v1.01
 *		      dviprint:	-- v1.01
 * 06-Sep-90 (hes) -- Add %x to the font format-strings.
 *		      Change call_mf. Call it once if a font aren't found.
 *		      Use only ENV: Environment-Variables.
 *		      showdvi:	close the dvi-file while scrolling (fclose-fopen).
 *				-- v1.02
 *		      dviprint:	-- v1.02
 * 19-Sep-90 (hes) -- Add %h %v and %y to the font format-strings.
 *		      %x now works korrect.
 *		      Add 'pkdir_str' and 'pkdir' keywords to the fontdef files.
 *		      showdvi and dviprint now 'pure'.
 *		      showdvi:	fixed many small bugs (scrollbars, requester..)
 *				add keymap handling to amkey.c
 *				don't make fatal if printing dosn't work
 *				-- v1.03
 *		      dviprint: -- v1.03
 * 17-Nov-90 (hes) -- Change new_font.[ch]. Every font/lib has now it's own
 *		      format-string. 
 *		      AMIGA: Fix bug in DoSpecial.
 *		      Both, DVIprint and ShowDVI now uses only 4kB stack.
 *		      AMIGA: Delete stack checking code.
 *		      list.
 *		      dviprint:	fix little bug in dviprint.c, don't call
 *				DoSpecial in SkipMode.
 *				-- v1.04
 *		      showdvi:	fix bug of confused about-requester.
 *				Change liste.[chi] and showdvi.c.
 *				Store the physical page number in the list.
 *				-- v1.04
 * 27-Nov-90 (hes) -- Fix some little bugs. :-)
 *		      showdvi:	Add physical pagenumbering code.
 *				I.e. new var 'current_page_phy'
 * 29-Nov-90 (hes) -- showdvi:	Physical page numbers now work correct.
 *				Add 'secundary_nr' page number to list structure.
 *				Add 'screen-mode' to the config file. Now you
 *				can contol the type of the ShowDVI screen (under
 *				Amiga Dos 2.0).
 *				-- v1.05
 * 06-Dec-90 (hes) -- dviprint: Updated newhard.c from J.C. Hoehle.
 *				New Epson Modus....
 *				Fix ^C bug (AMIGA) (also thank's to J.C. Hoehle!) !!
 *				-- v1.05
 *		      showdvi:	Add 'physikal-number' to the config file.
 *				Fix empty line bug in the config file (AMIGA).
 * 17-Dec-90 (hes) -- Changes for the "Level 0 DVI Driver Standard"
 *		      Add the new rounding scheme. See MoveOver, MoveDown, setmotion.
 *		      Set NPXLCHARS to 256. Don't store the unpacked font into the
 *		      internal mem. Only the packed chars are stored and unpacked if
 *		      needet. Get the memory for the unpacked chars via 'malloc'.
 *		      dviprint:	New NEC P6 modus (without 'ESC $' skips).
 *				-- v1.06
 *		      showdvi:	AMIGA: Add application window for OS 2.0.
 *				-- v1.06
 * 24-Jan-91 (hes) -- Change all (execpt in bitmap.c) malloc's to xmalloc's.
 *		      Don't search always font's at the default path.
 *		      If env-vars FLIBDIR or PKDIR set, delete the default path.
 *		      (see define ALWAYS_DEFAULT in new_font.c)
 *		      AMIGA: if Stats, close/open the Logfile after every line.
 *		      AMIGA: fix bug in do_special/draw_border.
 *		      dviprint: Add Epson-LQ mode (does not jet working!).
 *				-- v1.07
 *		      showdvi:	Fix little bug at page change without scrollbars.
 *				-- v1.07
 * 10-Feb-91 (hes) -- dviprint: Fix malloc bug with HP-part.
 *		      showdvi:	Fix little bug with menu-change-res and fullpage.
 *				-- PasTeX 1.2a release
 * 14-Feb-91 (hes) -- dviprint:	Add -d 0 for AMIGA (generic printer)!
 *				Fix Bugs in EpsonLQ part (draft <> highq).
 *				Add -S option for fclose/fopen logfile (AMIGA).
 *				New feature with -z option. Now you can
 *				specify a horizontal and vertical resolution.
 *				-- v1.08
 *		      showdvi:	Add -S option. (AMIGA)
 *				-- v1.08
 * 30-Mai-91 (hes) -- new_font.[ch] von Bernd eingebaut.
 *		      thinspace/backspace/.space Berechnung von Bernd eingebaut.
 *		      Reload Font wird nicht mehr am Bildschirm angezeigt.
 *		      Reload Font ueberarbeitet. Ein Font wird nun nur noch
 *		      dann wieder in's fontmem geladen, wenn ein Char
 *		      wirklich gebraucht wird.
 *		      DVIprint: Fix bug: 'IFFBase == NULL' (AMIGA)
 *				Add -O option (DENSITY), -1 -2 option and -> option.
 *				-- v1.09
 *		      ShowDVI:	(AMIGA)
 *				Add AMIGA-Menues (pop-up-menue off),
 *				add auto-load-again (on/off).
 *				add screen-size 0,0.
 *				Save the app-win position in the '.info' file.
 *				Change the follow_gad. Set ModifyIDCMP(MOUSEMOVE).
 *				Change the 'newcli' function for OS2.0.
 *				Fix little bug (SPACE-'b' handling).
 *				-- v1.09
 * 24-Jun-91 (hes) -- Das 'reload' Prinzip verbessert/Bug freier gemacht.
 *		      Den maxstring fuer Message... vergroessert.
 *		      ShowDVI:	(AMIGA)
 *				Color-File Requester nun aus der "req.library".
 *				AboutRequester zu einem AboutWindow mit IText Strukturen
 *				umgebaut.
 *				Screen-Font auf NULL gesetzt. Das Programm kann nun mit
 *				beliebig grossen System-Fonts umgehen.
 *				FullPage Modus so umgebaut, dass er nicht mehr direkt in einen
 *				Window/Screen Rast-Port schreibt. Allociert nun seinen eigenen
 *				RastPort. Damit ist ShowDVI voellig 'clean' und schreibt
 *				nirgens mehr direkt in eine Bitmap!
 * 24-Jun-91 (jch) -- parsef Modul zum lesen von Konfigurationsdateien
 *		      new_font.c benutzt parsef.
 *		      DVIprint: Ansteuerung der Nadeldrucker nun ueber config-File
 *				DVIprint.printers.
 *
 *  9-Jul-91 (hes) -- hoffset_in_fix, voffset_in_fix eingefuehrt.
 *		      h/v-offset wird nun an die Magnification angepasst.
 *		      Neues GetOpt eingebaut. Vollkommen neue
 *			Parameter. Nun auch WB Unterstuetzung
 *		      ENV: Variablen "ShowDVI-dir" und "ShowDVI-file" gesetzt.
 *		      DVIprint: 
 *				-- v1.10
 *		      ShowDVI:	(AMIGA)
 *				Timer.device ausgebaut, verwende nun INTUITICKS.
 *				Full-screen Algorithmus stark verbessert.
 *				Neue Features:
 *				- PublicScreen unter 2.0
 *				- 4 Farben Screen
 *				- AutoScroll wenn screen-size angegeben
 *				- middle-menu unter 2.0
 *				- Measure Window
 *				- Unit Menu
 *				- border line Anzeige
 *				- run cmmand + script eingebaut (Name im config-File).
 *				-- v1.10
 *  9-Jul-91 (jch) -- DVIprint: Bessere Optimierung bei der Druckausgabe.
 *				Unterer Rand wird nicht geskippt sondern es wird sofort FF
 *				gemacht.
 *				HP Treiber wieder angepasst.
 * 15-Jul-91 (jch) -- DVIprint: Fuer jeden Drucker in DVIprint.printers Breite
 *				definiert, dadurch rechter Rand setzbar, keine zu
 *				lange Grafik mehr an Drucker geschickt.
 *				Preferences Drucker skaliert grosse Seiten *nicht*
 *				mehr, daher grosser Geschw. Gewinn. (thank @cbmvax)
 *				'printer help' Option nun verfuegbar.
 *		      ShowDVI:  GetOpt Optionen angepasst
 * 18-Jul-91 (hes) -- ShowDVI:	(AMIGA)
 *				add SubMenu "Shell commands" / SpecialHost Menu-entry
 *				add new ARexx commands: getstring, getlong, okay1, okay2
 *				and menu.
 * 19-Jul-91 (hes) -- ShowDVI:	(AMIGA)
 *				New for V37.x and higher: Menu-Help
 *				Help text for every menu entry in the file TeX:config/ShowDVI.help
 *  7-Aug-91 (hes) -- Changed GetOpt (WB Args).
 *		      Changed startup-code (ShowUmain.c, PrintUmain.c).
 *		      ShowDVI:	(AMIGA)
 *				Fixed little bug in arexx.c ("" problem)
 *				Fixed bug in ShowDVI.c. Call "showdvi dir/file" cause problems.
 *				-- v1.11
 *		      DVIprint: Some little changes.
 *				-- v1.11
 * 12-Aug-91 (hes) -- Switched to OLDARGS in new_font.c. Changed "pkdir" and "basepkdir". 
 *		      They have now two or three arguments (+ vertical resolution).
 *		      Changed FontVols alg. Now it uses the basepkdir format-string to search
 *		      the font on the FontVol-disks.
 *		      ShowDVI:	Now always Amiga-DOS menu available. If pop-up menu, then
 *				use MENUVERIFY and use the Amiga-DOS menu in the top window-
 *				border.
 *				Change open Koo. for the about-window. No it is *always* in
 *				the mid of the screen! (Thanks MWeyer)
 *				Fix bug in init_images()!! Occures if the screen is larger than
 *				1000 pixels.
 *				-- v1.12
 *		      DVIprint: -- v1.12
 * 31-Aug-91 (hes) -- Change Logging() in dvihand.c, because SAS/C vsprintf() dosn't
 *		      work with '%f'!
 *		      new_font.c fontcaching. Set the path of the font to the
 *		      destination, not to the source!
 *		      ShowDVI:	New entries in the config-file: clone-wb-colors,
 *				quick-exit and new screen mode: workbench.
 *				New menu entry: Clone WB color
 *				New keycodes for the help-window.
 *				If new DVI-file, scroll right (hoffset).
 *				Fix bug in computing of the right border.
 *				Change center screen if new-dvi-file and change res.
 *				-- v1.13	(PasTeX 1.3 release version)
 *		      DVIprint: New keywords for DVIprint.printers: NR100 NR10 and NR1
 *				-- v1.13	(PasTeX 1.3 release version)
 * 09-Sep-91 (hes) -- Change OpenConfigFile(), now try to replace old files.
 *		      ShowDVI:	New entry in config file: monitor-size
 *				Change MOUSEMOVE handling.
 *				Fix enforcer hit ('i' with measure window active).
 *				-- v1.14
 *		      DVIprint:	Add accounting possibility.
 *				Do not open printer.device when iffprint or outtofile.
 *				-- v1.14
 *
 * 14-Sep-91 (hes) -- Add null_x/y in special_dmap message.
 *		      ShowDVI:	Fix two bugs around OpenConfigFile()
 *				Full-page now only in the display-rect.
 *				Add menu entry "set screen size".
 *				Add config file entry "border-line".
 *				Add ARexx command "getpubscreenname".
 *				Add priority parameter.
 *				-- v1.15
 *		      DVIprint:
 *				Add priority parameter.
 *				-- v1.15
 * 19-Sep-91 (hes) -- Small changes in special.[ch].
 *		      ShowDVI:	Add 'Screen Pref' window.
 *				-- v1.16
 *		      DVIprint: New printer entry 'LaserJet'.
 *				Add compress mode for DeskJet and LaserJet.
 *				-- v1.16
 * 28-Sep-91 (hes) -- Tiny change in GetOpt.c, there is a bug in ENV reading, but where??
 *		      ShowDVI:	Add notify machanism for Auto-Load-Again (under 2.0).
 *				-- v1.17
 *		      DVIprint: -- v1.17
 * 12-Okt-91 (hes) -- Change new_font.c/read_atex_file(). Alloc arrays per xmalloc() to
 *		      save stack space. So, 4kB stack should be enough.
 *		      ShowDVI:	Little change with notify. Don't open the DVI file
 *				immediatly after the notify-signal arrives. Check first,
 *				if the file exists
 * 13-Nov-91 (hes) -- ShowDVI:	Change scrollbars and page-scroll gadget.
 *				Add 'use-own-screen' line to the config file.
 *				-- v1.18
 *		      DVIprint: Change MAXGROUPING to 6. (Now should work with 48pins to)
 *				Change 'generic' printing. Don't use NOFORMFEED at the last
 *				pass.
 *				-- v1.18
 * 22-Mai-92 (hes) -- ShowDVI:	Many many small changes...ShowDVI in a window...2.0 look...
 *				Try to fix a non-freed signal (notify)
 *				-- v1.19 ß
 *		      DVIprint:	Fix bug in landscape mode.
 *				Add number of pages to print option.
 *				Add Canon mode (not working yet).
 *				Small cosmetic changes in showprinters output.
 *				-- v1.19 ß
 * 22-Sep-92 (hes) -- ShowDVI:	2.0 only program!!
 *				Boopsi images for the arrows.
 *				Screenmode requestor/save numeric ID's (V38).
 *				Village support, bitmap in fast-ram.
 *				Set top/left border command.
 * 29-Sep-92 (hes) -- ShowDVI:  Make main window to a app-window, if it is on a WB screen.
 * 15-Jan-93 (hes) -- ShowDVI:  Delete all gadgets in the window title-bar.
 *				Add ability to load the DVI file into ram.
 *				Add a border to the page.
 *				(Achtung...bei print-page muss der Rand weg!!)
 * 17-Jan-93 (hes) -- ShowDVI:	Include a boopsi gadget for the left/bottom border.
 *				It's a replacement for the old page scrolling mechanism.
 *				Link *not* with catchres.o, because this will cause GURU.
 *				(make 020+ieee version)
 *				Convert scrollbars to newlook + 3D look.
 *				Delete all old pgscro/images.
 *				(full-page error is still present :-( )
 *				-- v1.20ß
 *		      DVIprint: 020+ieee version
 *				Also possible to load DVI file into ram...but always disabled.
 *				(Should look for iff-save routines!!)
 *				-- v1.20ß
 * 26-Feb-93 (hes) -- ShowDVI:  Add DOMINO, PICASSO screen mode detection.
 *				Add new scroll mode, use ScrollRaster().
 *				Use ScrollRaster() scrolling if screen is a PICASSO screen.
 *				Not pure! (because of amiga.lib)
 *				-- v1.21ß
 * 03-Mar-93 (hes) -- ShowDVI:	Add Save-As-Iff and Copy menus.
 *				Use RKM functions for scrollbar values.
 *				Not pure! (because of amiga.lib)
 *				-- v1.22ß
 *		      DVIprint:	Add new IFF save routines.
 *				Change hoff/voff behavior in Landscape mode.
 *				-- v1.22ß
 * 12-Mar-93 (hes) -- DVIprint: Add PHY option.
 * 15-Mar-93 (hes) -- ShowDVI:	Full localized.
 *				Not pure! (because of amiga.lib)
 *				-- v1.23ß 
 *		      DVIprint: Partly localized.
 *				-- v1.23ß
 * 06-Jun-93 (hes) -- Add FatalStr(), MessageStr(), LoggingStr(), WarningStr() functions.
 *		      Replace all Message(GetTeXString(...)) with Message(...) and all
 *		      Message("...") with MessageStr("..."). Same with the other functions.
 *		      Change struct Options {} to localize the help texts.
 *		      ShowDVI:	Really full localized (CLI parameters).
 *				Fix problem with the menu colors.
 *				-- v1.24ß
 *		      DVIprint: Full localized.
 *				-- v1.24ß
 * 18-Jun-93 (hes) -- DVIprint: Add LaserJet4 (draft=300dpi, hq=600dpi) (not tested)
 *
 *
 * ******************  F I V E    Y E A R S    D E V E L O P M E N T   ******************
 *
 *
 * 18-Jul-93 (hes) -- Add WIDTH/HEIGHT options to define page size.
 *		      Add MARK options to mark all used fonts.
 *		      ShowDVI:	Fix enforcer hit in SetShowDVIVars().
 *				Add AmigaGuide support/GadgetHelp.
 *				Page can now also in FAST-RAM!
 *				Add two new config-lines for scrolling/fast-ram.
 *				-- v1.25ß
 *		      DVIprint:	LaserJet4 seems to work.
 *				-- v1.25ß
 *
 * 23-Oct-93 (hes) -- ShowDVI:  Use C-version of fast_cp.c (hope this fixes the missing chars)
 *				-- v1.26ß
 *		      DVIprint: Add TWOup, MOFFset and BOOK (not working!) options.
 *				-- v1.26ß
 * 31-Dec-93 (hes) -- ShowDVI:	Fix bug with fullpage/lace. Comment out some stupit code.
 *				Page smaller than window...delete one line of the
 *				scrollbar...fixed.
 *				Compiled with SAS/C 6.50.
 *		      DVIprint: Compiled with SAS/C 6.50.
 * 01-Jan-94 (hes) -- ShowDVI:	Make CURSOR behavior style-guide compatible.
 *				ScreenPref window: toggle ownscreen -- enable/disable gadgets.
 *				Use ?SetScrollerValues/?FindScrollerTop in amscreen.c.
 * 11-Jan-94 (hes) -- ShowDVI:	Fix bug in show_full_page if window larger than page.
 *				-- v1.27ß
 *		      DVIprint: -- v1.27ß
 * 18-Jan-94 (hes) -- ShowDVI:  Small changes in sleep-pointer handling. Add os3 function.
 *				-- v1.28ß
 * 22-Jan-94 (hes) -- ShowDVI:	Compiled with SAS/C 6.51.
 *				-- v1.29ß
 *		      DVIprint: Compiled with SAS/C 6.51.
 *				-- v1.29ß
 * 26-Jan-94 (hes) -- ShowDVI:  sleep_win does now also make block_win.
 *                    DVIprint:	Fix some things with \special + border + twopage.
 * 06-Feb-94 (hes) -- ShowDVI:	Fix some things with the mess-win.
 *				Catalog: totaly -> totally
 *				Fix AREXX commands: getfile/getdir/loadnew
 *				-- v1.30ß
 *		      DVIprint: -- v1.30ß
 * 16-Mar-94 (hes) -- Free distributable BETA version. No BETACOPYRIGHT.
 *		      Fix some things in GetOpt.c (HIDDEN+'true'in)
 *		      ShowDVI:	Delete Interlace button in Screen-config.
 *				Fix bug with filerequester and empty file gad.
 *				Make PageGad work correctly.
 *				-- v1.31ß
 *		      DVIprint: -- v1.31ß
 *                    --BETA 1--
 * 01-Apr-94 (hes) -- ShowDVI:	Move ReplyMsg() before the switch statement.
 *				Fixes some bugs (crashes!) is the screen is closed and reopened.
 *				Add PUBSCREEN arg.
 *				Fix some bugs in the prefwin.
 *				-- v1.32ß
 *		      DVIprint: Add PUBSCREEN arg (hidden).
 *				Add PAPER option.
 *				-- v1.32ß
 *
 *		      --BETA 2--
 * 05-Apr-94 (hes) -- ShowDVI:	Fix Bug in win_mov. SetWriteMask is a V39 function!
 *				-- v1.33ß
 * 13-Apr-94 (hes) -- ShowDVI:	Make black-on-white work.
 *				Fix pot_x gad bug (ghost gadget).
 *				Better update of the mess-window (key event).
 *				Replace 'interlace' with 'black on white' in pref-win.
 *				Add config entry for black on white writing.
 * 15-Apr-94 (hes) -- Change offset-mode in fast_cp.c and dospecial.c
 *		      Fix WIDTH/HEIGHT options.
 *		      Change catalog version number to the number of the programs.
 *		      ShowDVI:	Add PAPER option.
 *				Fix problems in messwin if page smaller than window.
 *				-- v1.34ß
 *		      DVIprint: Fix (?) offsets in landscape / twoup mode (hint: use neg. voffset).
 *				Forbit TWOUP + REVERSE.
 *				Fix ODD/EVEN in TWOUP.
 *				Fix printing of last leftpage in TWOUP.
 *				Increase MAX_GROUPING to 12.
 *				-- v1.34ß
 * 19-Apr-94 (hes) -- ShowDVI:	ARexx command getfile: remove .dvi extension.
 *		      --BETA 3--
 * 03-May-94 (hes) -- ShowDVI:	Add set_checked...() to the messwin procs.
 *				Fix two small bugs with WriteMask and ClipIt in win_mov().
 * 08-May-94 (hes) -- ShowDVI:	Fix small bug (Workbech) of german locale file.
 *				Deactivate LACE menu, if !own_scr
 * 12-May-94 (hes) -- Change badpk() in loadfont. Only one GetTeXString().
 *		      ShowDVI:	Add NameFromLock() in init_task_name(). (Stefan Scherer)
 * 		      --BETA 4--
 * 24-May-94 (hes) -- Change PAPER option to PAPERSIZE (Dan Barret).
 * 09-Jun-94 (hes) -- ShowDVI:  -- v1.36ß
 *		      DVIprint: Add _alpha_ MUI GUI. (option GUI)
 *				Recognice directories/.tex files as input.
 *				-- v1.36ß
 *                    --BETA 5--
 * 16-Jun-94 (hes) -- ShowDVI:	Fix 'confuse program' with wrong option (uninitialized menu) (YET).
 *				(This fixes also the program with start of second ShowDVI)
 *				Add black on white for fullpage.
 *				Fix mungwall hit if stopping the program within fullpage mode (Stefan Schmieta).
 *				Add ARexx command 'getnumofpages' (arma).
 *				ARexx 'getpage' returns error rc=5, if no dvi-file loaded.
 * 22-Jun-94 (hes) -- Break Fatal() loop. Don't call cleanupt, if already done.
 *		      ShowDVI:	Now use the real width/height of with scroller arrows and sizing gadget.
 *				Should now work together with 'sysihack' (reported from olk).
 *				Fix checkmark menu entries. Now doppel-klick does nothing. (reported from Dan Barret)
 * 29-Jun-94 (hes) -- Fix TPIC special call: Wrong offset in twoup mode.
 *		      ShowDVI:	Add AMIGA-H shortcut for 'start SpecialHost'
 *		   		-- v1.37ß
 *		      DVIprint: Add PUBSCREEN arg. Works only with the GUI option.
 *				Use MUI 2.1 (version 8). Change lokale files.
 *		   		-- v1.37ß
 * 10-Jul-94 (hes) -- Do a bit profiling and make Read?Byte() inline. Some other small changes in fast_cp.c
 *		      loadfont.c newfont.c and unpack.c. Hope it makes things a bit faster.
 *		      ShowDVI:	Add search string feature (menu+'s')
 * 14-Jul-94 (hes) -- ShowDVI:	Now really sysihack compatible.
 * 20-Jul-94 (hes) -- ShowDVI:	Fix some problems with the physical page numbering (reported from Matthias Berberich)
 * 21-Jul-94 (hes) -- ShowDVI:	Highlight the found string.
 * 07-Aug-94 (hes) -- Add version check into the locale function.
 *		      ShowDVI:	Add Search-Window.
 *				Fix something about starting second ShowDVI.
 *				Show BETA requester only once.
 *				-- v1.38ß
 *                    DVIprint:	-- v1.38ß
 * 10-Aug-94 (hes) -- Add SPECIALhost option.
 *                    --BETA 6--
 * 29-Sep-94 (hes) -- Add some things into the special-message. (Check the size of the message!)
 *		      ShowDVI:	-- v1.39ß
 *		      DVIprint:	-- v1.39ß
 * 12-Nov-94 (hes) -- Use EVPaths for OpenConfigFile.
 *
 * 25-May-95 (hes) -- ********* RELEASE THE SOURCE TO THE PUBLIC !!! **********
 *		      ShowDVI:	-- v1.40
 *		      DVIprint:	-- v1.40
 * 05-Jul-95 (hes) -- Fix magnification problem in dospecia.c/draw_border().
 *		      Both programs need new PasTeX.catalog version.
 *		      ShowDVI:	Make scrolling on graphic cards (esp. CyBER*) faster.
 *				Use BltTemplate() in ClipIt().
 *				Two new lines in the config file. You can set now the a/b pens.
 *				So if you use ShowDVI on a public screen with more colors, you can choose
 *				your favorite color combination.
 *				Shorten the filename in the window titelbar.
 *				Bug? Border lines doesn't work on CyBERvision.
 *				-- v1.41
 *		      DVIprint:	-- v1.41
 *
 */




/* If you want uncrypted version strings, undef it */

#define CRYPT


#define  VERSION_FORMAT_STRING	"%s V%s%c %s"

#define COMPILER		' '	/* no more 'ß' */

#define DVIPRINT_VERSION	"1.41"
#define SHOWDVI_VERSION		"1.41"

#define PROGRAMTITLE_SHOW	"ShowDVI"
#define PROGRAMTITLE_FLIB	"FontLib Manager"
#define PROGRAMTITLE_PRINT	"DVIPrint"

#define PROGRAMKENNUNG		"Copyright © 1990-1995 by Georg Heßmann. All Rights Reserved."


#define AUTHOR_D_FORMAT		"\n\t%s\n\t\t%s\n\t\t%s\n"
#define AUTHOR_S_FORMAT		"\n\t%s\n\t\t%s\n"
#define AUTHOR_TITLE		"The Authors:"
#define AUTHOR1			"Georg Heßmann (Hessmann@Informatik.Uni-Hamburg.De)"
#define AUTHOR3			"Jörg Höhle (hoehle@cs.uni-sb.de)"


#if defined(BETACOPYRIGHT)
# define COPYRIGHT		"This is *no* public version!! \"PLEASE DON'T SPREAD IT\""
#endif

