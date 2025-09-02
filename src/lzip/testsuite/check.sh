#! /bin/sh
# check script for Lzip - A data compressor based on the LZMA algorithm
# Copyright (C) 2008, 2009 Antonio Diaz Diaz.
#
# This script is free software: you have unlimited permission
# to copy, distribute and modify it.

objdir=`pwd`
testdir=`cd $1 ; pwd`
LZIP=${objdir}/lzip
framework_failure() { echo 'failure in testing framework'; exit 1; }

if [ ! -x ${LZIP} ] ; then
	echo "${LZIP}: cannot execute"
	exit 1
fi

if [ -d tmp ] ; then rm -r tmp ; fi
mkdir tmp
echo -n "testing lzip..."
cd ${objdir}/tmp

cat ${testdir}/../COPYING > in || framework_failure
fail=0

${LZIP} -cd ${testdir}/COPYING.lz | cmp ${testdir}/../COPYING - || fail=1

for i in 1 2 3 4 5 6 7 8 9; do
	${LZIP} -k -$i in || fail=1
	mv -f in.lz copy.lz || fail=1
	${LZIP} -df copy.lz || fail=1
	cmp in copy || fail=1
	echo -n .
done

for i in 1 2 3 4 5 6 7 8 9; do
	${LZIP} -c -$i in > out || fail=1
	${LZIP} -cd out > copy || fail=1
	cmp in copy || fail=1
	echo -n .
done

for i in 1 2 3 4 5 6 7 8 9; do
	${LZIP} -c -$i < in > out || fail=1
	${LZIP} -d < out > copy || fail=1
	cmp in copy || fail=1
	echo -n .
done

for i in 1 2 3 4 5 6 7 8 9; do
	${LZIP} -f -$i -o out < in || fail=1
	${LZIP} -df -o copy < out.lz || fail=1
	cmp in copy || fail=1
	echo -n .
done

echo
if test ${fail} = 0; then
	echo "tests completed successfully."
	if cd ${objdir} ; then rm -r tmp ; fi
else
	echo "tests failed."
fi
exit ${fail}
