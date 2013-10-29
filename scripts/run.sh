#!/bin/bash

set -e

PATH=$1
PORT=$2
OPTS=$3

HYPHENS=${PATH//\//-}
NAME=${HYPHENS:1}
PID_FILE="./logs/$NAME.pid"
STDOUT_FILE="./logs/$NAME.out"
STDERR_FILE="./logs/$NAME.err"

/usr/bin/java $OPTS -jar ".$PATH/target/scala-2.10/nlptools-$NAME-assembly-2.4.4-SNAPSHOT.jar" --server --port $PORT > $STDOUT_FILE 2> $STDERR_FILE &

PID=$!

echo "Running $PATH on :$PORT with ($OPTS): $PID"

echo $PID > $PID_FILE

wait $PID

/bin/rm $PID_FILE
