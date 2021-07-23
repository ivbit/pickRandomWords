#!/bin/mksh

# Intellectual property information START
# 
# Copyright (c) 2021 Ivan Bityutskiy 
# 
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
# 
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
# 
# Intellectual property information END

# Description START
#
# The script prints results to STDOUT
# with amount of words specified in outWordsAmount variable.
# Input file source.txt must contain 1 word per line.
# The script is used to fetch random words from input file
# and create a single line containing all words.
# This script is written for mksh.
#
# Description END

# Shell settings START
umask 077
# Shell settings END

# Declare variables START
# Compute the full path to this script.
typeset scriptRoot="$(cd "$(dirname "$0")"; pwd)/"
# source.txt is a text file with 1 word per line.
typeset inputFile="source.txt"
typeset inputFPath="$scriptRoot$inputFile"
# array to store 8-byte unsigned values as strings
typeset arrEightBytes
# variable to represent array's item in for loop
typeset itemEightBytes
# sedArgs will contain all arguments for sed
typeset sedArgs=''

# number of lines (1 word per line) in input file
integer inputLinesAmount=$(wc -l < "$inputFPath")
# number of words in output file
integer outWordsAmount=100
# number of byter to read by od
integer numBytes=$(( outWordsAmount * 8 ))
# result, produced by dc (random line number to pick by sed)
integer dcResult
# Declare variables END

# BEGINNING OF SCRIPT
# populate an array with 8-byte random unsigned numbers
#
set -A arrEightBytes -- $(od -A 'n' -N $numBytes -t 'u8' -v /dev/urandom)

# start dc as a co-process
dc |&

for itemEightBytes in ${arrEightBytes[@]}
do
  # print into dc: random8bitUInt % inputFileLinesAmount + 1 (reverse polish notation)
  print -p -- "1 $itemEightBytes $inputLinesAmount % + p"
  # read result from dc
  read -p -- dcResult
  # populate a string with arguments for sed
  sedArgs="${sedArgs};${dcResult}p"
done

# close dc co-process
print -p -- 'q'

# sed returns alphabetically sorted output,
# sort -R randomizes it,
# tr transforms it into a single string
sed -n "$sedArgs" "$inputFPath" |
  sort -R |
    tr '\n' ' ' | less

# END OF SCRIPT

