#!/bin/sh

# Colorize output if it's a terminal
if [ -t 1 ]; then
    HLINT_FLAGS='--colour'
fi

DIRS=''
for D in 'src' 'test' 'tests'; do
    if [ -d "$D" ]; then
        if [ -z "$DIRS" ]; then
            DIRS="$D"
        else
            DIRS="$DIRS $D"
        fi
    fi
done

# Use current directory if no known subdirectories with source code were found.
if [ -z "$DIRS" ]; then
    DIRS='.'
fi

hlint $HLINT_FLAGS .
