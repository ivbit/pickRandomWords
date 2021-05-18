#!/bin/ksh

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
# This script is written for OpenBSD's pdksh.
#
# Description END

# Shell settings START
umask 077
# Shell settings END

# Declare variables START
# source.txt is a text file with 1 word per line.
typeset inputFile='./source.txt'

# number of lines (1 word per line) in input file
integer inputLinesAmount=$(wc -l < $inputFile)
# number of words in output
integer outWordsAmount=100

# sedArgs will contain all arguments for sed
typeset sedArgs="$(jot -r -s ';' -w '%dp' $outWordsAmount '1.01' ${inputLinesAmount}'.99')"
# Declare variables END

# BEGINNING OF SCRIPT
# sed returns alphabetically sorted output,
# sort -R randomizes it,
# tr transforms it into a single string
sed -n $sedArgs $inputFile |
  sort -R |
    tr '\n' ' ' | less

# END OF SCRIPT

