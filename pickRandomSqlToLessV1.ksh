#!/usr/bin/mksh

# Intellectual property information START
#
# Copyright (c) 2023 Ivan Bityutskiy 
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
# The script prints results to 'less' program
# with amount of words specified in outWordsAmount variable.
# Input file source.db has table words (id integer primary key, word text).
# The script is used to fetch random words from input database file
# and create a single line containing all words.
# This script is written for mksh shell.
# DEPENDENCY: sqlite3
#
# Description END

# Shell settings START
# On old shell 'errexit' may cause buggy behavior: shell exits during 'for' loop
# set -o errexit
# set -o nounset
# set -o noglob
set -euf
# Shell settings END

# Declare variables START
typeset tRed='\033[31m'
typeset tNorm='\033[0m'
# Compute the full path to this script.
typeset scriptRoot="$(cd "$(dirname "$0")"; pwd)"
# source.db is a database file with 1 word per line.
typeset inputFile="source.db"
typeset inputFPath="${scriptRoot}/${inputFile}"
# String to store 8-byte unsigned values separated by space ' '
typeset allEightBytes=''
# Variable to represent item in for loop
typeset itemEightBytes=''
# sqlite3 options
typeset optString='.mode list'
# sqlString will contain all arguments for sqlite3
typeset sqlString='SELECT word FROM words WHERE'
# SELECT word FROM words WHERE id = 1 OR id = 2 OR id = 3
typeset condition='id ='

# number of rows in input database
integer inputLinesAmount=574241
# number of words in output file
integer outWordsAmount=100
# number of bytes to read by od
integer numBytes=$(( $outWordsAmount * 8 ))
# result, produced by dc (random number)
integer dcResult=1
# Declare variables END

# Define functions START
# Print 1st argument to STDERR and exit the script
function errorM
{
  print -u2 -- "\n\t${tRed}${$1}${tNorm}\n\n"
  exit 1
}
# Define functions END

# BEGINNING OF SCRIPT
> /dev/null 2>&1 ls "$inputFPath" || errorM 'DB file does not exist!'
> /dev/null 2>&1 command -v sqlite3 || errorM 'sqlite3 is not installed!'

# Populate a string with 8-byte random unsigned numbers
allEightBytes="$(od -A 'n' -N $numBytes -t 'u8' -v /dev/urandom)"

# start dc as a co-process
dc |&

for itemEightBytes in $allEightBytes
do
  # print into dc: random8bitUInt % inputFileLinesAmount + 1 (reverse polish notation)
  print -p -- "1 $itemEightBytes $inputLinesAmount % + p"
  # read result from dc
  read -p -- dcResult
  # Populate a string with arguments for sqlite3
  sqlString="${sqlString} ${condition} ${dcResult}"
  # First condition='id =', others condition='OR id ='
  condition='OR id ='
done
# close dc co-process
print -p -- 'q'
# Add semicolon at the end of sqlString
sqlString="${sqlString};"

# tr transforms output into a single string
sqlite3 "$inputFPath" "$optString" "$sqlString" |
  tr '\n' ' ' | less

# END OF SCRIPT

