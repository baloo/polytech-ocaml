#!/bin/sh

BIN=./exec
DIR=tests.d

echo "Running tests"

for file in $(ls $DIR); do
    NAME=`echo $file | sed -r 's/([a-z0-9]*)\.([a-z0-9]*)\.([a-z0-9]*)/\1/'`
    RESULT=`echo $file | sed -r 's/([a-z0-9]*)\.([a-z0-9]*)\.([a-z0-9]*)/\2/'`
    NUMBER=`echo $file | sed -r 's/([a-z0-9]*)\.([a-z0-9]*)\.([a-z0-9]*)/\3/'`

    echo "Testing $NAME #$NUMBER "
    
    cat $DIR/$file | $BIN

    echo "Result should be : $RESULT";
done
