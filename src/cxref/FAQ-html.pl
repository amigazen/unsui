#!/usr/bin/perl
#-*-perl-*-

#
# Copyright Andrew M. Bishop 1996.97.
#
# Usage: FAQ-html.pl < FAQ > FAQ.html
#

$_=<STDIN>;
s/^ *//;
s/ *\n//;
$first=$_;

print "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n";
print "<HTML>\n";
print "\n";
print "<HEAD>\n";
print "<TITLE>$first</TITLE>\n";
print "</HEAD>\n";
print "\n";
print "<BODY>\n";
print "\n";
print "<h1>$first</h1>\n";

$questions=0;
$answers=0;
$pre=0;
$blank=1;

while(<STDIN>)
  {
   chop;

   s/</&lt;/g;
   s/>/&gt;/g;
   s/&/&amp;/g;
   s/\"/&quot;/g;

   next if(m/^ *=+ *$/);
   next if ($_ eq "--------------------");
   $pre++,$blank=0,next if($pre==1 && $_ eq "");
   $blank=1,next        if($pre!=1 && $_ eq "");
   $pre++ if($pre);

   if ($_ eq "--------------------------------------------------------------------------------")
       {
        $pre=0,print "</pre>\n" if($pre);
        print "<hr>\n";
        $answers++              if( $answers);
        $questions=0,$answers=1 if( $questions);
        $questions=1            if(!$questions && !$answers);
       }
   else
       {
        if(m/^(Section [0-9]+)/)
            {
             $section = $1;
             $section =~ tr/ /-/;

             $pre=0,print "</pre>\n" if($pre);
             print "<p><b><a href=\"#$section\">$_</a></b>\n" if($questions);
             print "<h2><a name=\"$section\">$_</a></h2>\n"       if($answers);
            }
        else
            {
             if(m/^(Q [0-9]+.[0-9]+[a-z]*)/)
                 {
                  $question = $1;
                  $question =~ tr/ /-/;

                  $blank=0,$pre=0,print "</pre>\n" if($pre);
                  print "<p><a href=\"#$question\">$_</a>\n"  if($questions);
                  print "<h3><a name=\"$question\">$_</a></h3>\n" if($answers);
                  $pre=1,print "<pre>\n" if($answers);
                 }
             else
                 {
                  $blank=0,print "\n" if($blank);
                  print "$_\n";
                 }
            }
       }
  }

print "</BODY>\n";
print "\n";
print "</HTML>\n";
