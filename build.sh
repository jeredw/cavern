#!/bin/bash

dasm cavern.s -f3 -I../dasm-2.20.11/machines/atari2600 \
  -ocavern.bin -scavern.sym -lcavern.txt
