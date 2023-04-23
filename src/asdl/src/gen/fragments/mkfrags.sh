#!/bin/sh
#
# wrapper script for MkFrags.mkFragments function
#

PROG=mkfrags

if [ $# != 1 ] ; then
  echo "usage: $PROG.sh <dir>"
  exit 1
fi

DIR=$1

SRC=/workspaces/ml-yacc-calc/src/asdl/src/gen/fragments/sources.cm

if test "smlnj" = "smlnj" ; then
/workspaces/ml-yacc-calc/src/bin/sml -32 $SRC 2> /dev/null 1>/dev/null <<XXXX
MkFrags.mkFragments "$DIR";
XXXX
exit $?
elif test "smlnj" = "mlton" ; then
  HERE=$(pwd)
  cd /workspaces/ml-yacc-calc/src/asdl/src/gen/fragments
  make -s $PROG || exit $1
  cd $HERE
  /workspaces/ml-yacc-calc/src/asdl/src/gen/fragments/$PROG $DIR || exit $1
  exit 0
else
  echo "unknown SML implementation"
  exit 1
fi
