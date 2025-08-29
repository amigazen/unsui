%%
%% This file is part of the Standard LaTeX `Graphics Bundle'.
%%
%% It should be distributed *unchanged* and together with all other
%% files in the graphics bundle. The file 00readme.txt contains a list
%% of all of these files.
%%
%% A modified version of this file may be distributed, but it should
%% be distributed with a *different* name. Changed files must be
%% distributed *together with a complete and unchanged* distribution
%% of these files.
%%


The LaTeX Colour and Graphics Packages
========================================
                    1996/06/01

This is a collection of LaTeX packages for:
 * producing colour
 * including graphics (eg PostScript) files 
 * rotation and scaling of text
in LaTeX documents.

=======================================================================


THIS DIRECTORY CONTAINS 
======================

00readme.txt    This File
changes.txt     Log of changes to the packages.
graphics.ins    Install file for docstrip.

Standard packages
=================
color.dtx       Source for color package
graphics.dtx    Source for graphics package
trig.dtx        Source for trig package (required by graphics)

Non Standard Packages
=====================
graphicx.dtx    Source for graphicx package (extension of graphics)
epsfig.dtx      Source for epsfig package (extension of graphicx)
keyval.dtx      Source for keyval pacakge (required by both the above)
pstcol.sty      Small wrapper for pstricks package.
lscape.sty      Produce landscape pages in a (mainly) portrait document.

Driver Files
============
drivers.dtx     Source for driver files for supported drivers.
textures.def    Textures file supplied by Arthur Ogawa.

User Documentation
==================
grfguide.tex    User Guide to all the packages in this bundle.
                WARNING: 
                This file calls color and graphics packages
                without a driver option. 
                You *must* set up two files 
                                 color.cfg and graphics.cfg
                containing (for example)
                                \ExecuteOptions{dvips}
                Before running this file.

grfguide.ps     PostScript version of the above (Using PostScript fonts)
                You may use this to see examples that your driver may
                not be able to achieve.


=============================================

TO UNPACK THE PACKAGES
======================

latex graphics.ins

This will produce the 

.sty package files
.def driver files.

=============================================

USING THE PACKAGES
==================

Move files ending in .sty .def  to a standard TeX input directory.

Make a default option for your site by creating two files
     color.cfg and graphics.cfg
containing (if dvips is your default driver)
\ExecuteOptions{dvips}

You may then LaTeX the user guide:
latex grfguide.tex.

==============================================
