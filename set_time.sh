#!/bin/bash

f=State.hs

t=$(cat "$f")
s="(time \"$(date +'%Y-%m-%d %H:%M:%S %:z (%Z)')\")"
t=${t//NOW/$s}
echo "$t" > "$f"
