#!/bin/sh
set -eu

BASE=$1
INP=$BASE.inp
OUT=$BASE.out
EXP=$BASE.exp
DIFF=${DIFF:-diff -u}

if ! test -f "$INP" ; then
    INP=/dev/null
fi

./"$BASE" <"$INP" >"$OUT" &&
    $DIFF "$OUT" "$EXP" &&
    rm -f "$OUT"
