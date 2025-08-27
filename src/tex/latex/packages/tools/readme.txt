
README for the `tools' bundle  (June 1996)
=============================

This `bundle' consists of LaTeX2e packages written and supported by
members of the LaTeX3 Project Team.

The documented source code of each package is in a file with extension
`.dtx'.  Running LaTeX on the file tools.ins will produce all the
package files, and some associated files.

So you should first process tools.ins:

  latex tools.ins

The files with extensions `.sty' and `.tex' (including a file whose
name is just `.tex') should then be moved to a directory on LaTeX's
standard input path.

Documentation for the individual packages may then be obtained by
running LaTeX on the `.dtx' files.

For example:

  latex array.dtx

will produce the file array.dvi, documenting the array package.

***NOTE****
Copyright is maintained on each of these packages by the author(s)
of the package. 

Unless otherwise mentioned in the package file, all the packages in
this bundle are released under the restrictions detailed below. 

In particular, the multicol package is distributed under special
terms that restrict commercial use, as explained in multicol.dtx. 

The file manifest.txt contains a list of the main files in the
distribution together with a one-or-two line summary of each package.


Reporting Bugs
==============

If you wish to report a problem or bug in any of these packages,
use the latexbug.tex program that comes with the standard LaTeX
distribution.  Please ensure that you enter `1' when prompted with a
menu of categories, so that the message will be automatically
forwarded to the appropriate part of our database.

When reporting bugs, please produce a small test file that shows the
problem, and ensure that you are using the current version of the
package, and of the base LaTeX software.


Distribution of unchanged versions
==================================
  
  Redistribution of unchanged files is allowed provided that this
  readme file is included and all the files for a package are
  distributed together.

  The individual packages may bear additional restrictions on
  modification and distribution which supersede these general
  conditions.

Generation and distribution of changed versions
===============================================

  The generation of changed versions of the files included in these
  packages is allowed under the restrictions listed in the file
  legal.txt in the base LaTeX distribution.  In particular you should: 

  - rename the file before you make any changes to it.  

  - change the error report address so that we do not get sent error
    reports for files *not* maintained by us.


  The distribution of changed versions of the files included in these
  packages is allowed under the restrictions listed in the file
  legal.txt in the base LaTeX distribution.  In particular you should: 

  - also distribute the unmodified version of the file.
