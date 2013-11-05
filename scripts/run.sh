#!/bin/bash

PATH=$1
PORT=$2
OPTS=$3

HYPHENS=${PATH//\//-}
NAME=${HYPHENS:1}
PID_FILE="./logs/$NAME.pid"
STDOUT_FILE="./logs/$NAME.out"
STDERR_FILE="./logs/$NAME.err"

if [ -f "$PID_FILE" ]
then
  echo "stop running instance or remove $PID_FILE"
  exit
fi

/bin/rm -f $PID_FILE
/bin/rm -f $STDOUT_FILE
/bin/rm -f $STDERR_FILE

CMD="/usr/bin/java $OPTS -jar .$PATH/target/scala-2.10/nlptools-$NAME-assembly-*.jar --server --port $PORT >> $STDOUT_FILE 2> $STDERR_FILE &"

echo "$CMD" > $STDOUT_FILE
eval "$CMD"

PID=$!

echo "Running $PATH on :$PORT with ($OPTS): $PID"

echo $PID > $PID_FILE

wait $PID

/bin/rm -f $PID_FILE
