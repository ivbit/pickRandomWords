#!/usr/bin/dash

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
# The script prints results to STDOUT
# with amount of words specified in outWordsAmount variable.
# Input file source.db has table words (id integer primary key, word text).
# The script is used to fetch random words from input database file
# and create a single line containing all words.
# This script is written for POSIX shell.
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
tRed='\033[31m'
tNorm='\033[0m'
# Compute the full path to this script.
scriptRoot="$(cd "$(dirname "$0")"; pwd)"
# source.db is a database file with 1 word per line.
inputFile="source.db"
inputFPath="${scriptRoot}/${inputFile}"
# String to store 8-byte unsigned values separated by space ' '
allEightBytes=''
# Variable to represent item in for loop
itemEightBytes=''
# sqlite3 options
optString='.mode list'
# sqlString will contain all arguments for sqlite3
sqlString='SELECT word FROM words WHERE'
# SELECT word FROM words WHERE id = 1 OR id = 2 OR id = 3
condition='id ='

# number of rows in input database
inputLinesAmount=574241
# number of words in output file
outWordsAmount=100
# number of bytes to read by od
numBytes=$(( $outWordsAmount * 8 ))
# result, produced by dc (random number)
dcResult=1
# Declare variables END

# Define functions START
# Print 1st argument to STDERR and exit the script
errorM()
{
  >&2 printf -- "\n\t${tRed}%s${tNorm}\n\n" "$1"
  exit 1
}

# Clear just the screen, not the whole console
fnClear()
{
  >&2 printf -- '\033[1;1H\033[0J'
}
# Define functions END

# BEGINNING OF SCRIPT
> /dev/null 2>&1 ls "$inputFPath" || errorM 'DB file does not exist!'
> /dev/null 2>&1 command -v sqlite3 || errorM 'sqlite3 is not installed!'

# Populate a string with 8-byte random unsigned numbers
allEightBytes="$(od -A 'n' -N $numBytes -t 'u8' -v /dev/urandom)"

for itemEightBytes in $allEightBytes
do
  # Print into dc: random8bitUInt % inputFileLinesAmount + 1 (reverse polish notation)
  dcResult="$(printf -- '%s' "1 $itemEightBytes $inputLinesAmount % + p" | dc)"
  # Populate a string with arguments for sqlite3
  sqlString="${sqlString} ${condition} ${dcResult}"
  # First condition='id =', others condition='OR id ='
  condition='OR id ='
done
# Add semicolon at the end of sqlString
sqlString="${sqlString};"

fnClear
printf -- '\n'
# tr transforms output into a single string
sqlite3 "$inputFPath" "$optString" "$sqlString" | tr '\n' ' '
printf -- '\n\n'

# END OF SCRIPT

