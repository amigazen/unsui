BEGIN {p = 1}
$1 == "%%BeginFont:" { p = 0 }
{ if (p==1) print $0 }
$1 == "%%EndFont" { print "bn"; p = 1 }
$1 == "%%EndFont:" { print "bn"; p = 1 }
