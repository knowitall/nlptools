#!/bin/bash

CLASS_NAME="edu.knowitall.tool.parse.ClearDependencyParserMain"
PORT=12001

SCRIPT_DIR=`dirname $0`
SHORT_NAME=`basename $0 .sh`
APP_ROOT="$SCRIPT_DIR/.."
JVM_ARGS="-Xmx3G"

. "${SCRIPT_DIR}/nlptools-run-server.sh" "$CLASS_NAME" "$SHORT_NAME" "$PORT" "$@"
