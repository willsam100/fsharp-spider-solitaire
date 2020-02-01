#! /bin/sh
# kill -9 -$(ps -o pgid= $1 | grep -o '[0-9]*')
pkill -TERM -P $1