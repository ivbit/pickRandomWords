#!/bin/ksh

# Intellectual property information START
# 
# Copyright (c) 2022 Ivan Bityutskiy 
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
# The script uses ed editor to create a words source file
# out of /usr/share/dict/words system file.
# Why using "ed" instead of "sed -i"?
# Because "sed" is not a text editor, "sed" is a stream editor,
# using "sed -i" to edit large files in place
# may and will cause problems.
# Defaults are stored in 3 variables:
# sourceFile
# shortWords
# longWords
# Defaults need to be changed in both
# section: "Declare variables"
# and in function: "defaults".
# If defaults are changed, the script is able to process any
# file with unix file format, which contain a single word
# on each separate line.
# Description END

# Shell settings START
set -o noglob
umask 077
# Shell settings END

# Define functions START
function defaults
{
  case "$1" in
    s)
      shortWords=3
      ;;
    l)
      longWords=11
      ;;
    b)
      shortWords=3
      longWords=11
      ;;
  esac
}

function syntax
{
  defaults 'b'
  print -u2 -- "\nUsage:
    ./${1##*/} -h
    \tTo print this help\n
    ./${1##*/} -s
    \tTo set the maximum length of short words
    \tto be removed from word source file.
    \tOnly single digit is accepted.\n
    ./${1##*/} -l
    \tTo set the minimum length of long words
    \tto be removed from source file.
    \tOnly single or double digit is accepted.\n
    ./${1##*/}
    \tIf there are no options, or the length
    \tof short word is greater then or equal
    \tto the length of long word,
    \tthe defaults will be used:
    \tshort word: ${shortWords} letters,
    \tlong word: ${longWords} letters.\n
    Example:
    ./${1##*/} -s ${shortWords}
    ./${1##*/} -l ${longWords}
    ./${1##*/} -s ${shortWords} -l ${longWords}\n"
  exit 1
}
# Define functions END

# Declare variables START
typeset sourceFile='source.txt'
typeset -Z1 shortWords=3
typeset -Z2 longWords=11
# Declare variables END

# Get the options START 
if (( $# ))
then
  while getopts :hs:l: anOption
  do
    # Prevent user from entering first option
    # as -ssssssssssssssssssssssssssssss.
    (( ${#1} != 2 )) && syntax "$0"
    case $anOption in
      h)
        syntax "$0"
        ;;
      :)
        # Option is missig an argument, set
        # defaults for that option.
        defaults "$OPTARG"
        ;;
      s)
        # Surplus arguments to the script
        (( $# > 4 )) && syntax "$0"
        shortWords="${OPTARG}"
        # Olny a single digit allowed for the length of shord words.
        [[ "${shortWords}" != [[:digit:]] ]] && syntax "$0"
        ;;
      l)
        # Surplus arguments to the script
        (( $# > 4 )) && syntax "$0"
        longWords="${OPTARG}"
        # One or more digits allowed for the length of long words.
        [[ "${longWords}" != +([[:digit:]]) ]] && syntax "$0"
        ;;
     \?)
       # Incorrect option is passed to the script.
       syntax "$0"
       ;;
    esac
  done
  shift 'OPTIND - 1'
fi
# Get the options END

# If script has surplus arguments START
(( $# > 1 )) && syntax "$0"
# If script has surplus arguments END
(( shortWords >= longWords )) && defaults 'b'

# BEGINNING OF SCRIPT
print -u2 -- "\nWords with length of \033[91m${shortWords}\033[0m letters or less\n\twill be removed from words source file."
print -u2 -- "Words with length of \033[91m${longWords#0}\033[0m letters or more\n\twill be removed from words source file."
print -u2 -- "Depending on performance of your system,\n\tthis process may take \033[91m3 to 5 minutes\033[0m.\n\tPlease be patient."

# Copy file to the current directory, overwriting existing file.
cp -f /usr/share/dict/words "./${sourceFile}"
# Only owner of the file has access to it.
chmod 600 "./${sourceFile}"
# Edit file with "ed" text editor.
# Suppress all messages from "ed" by sending
# them to /dev/null
> /dev/null 2>&1 ed -s "${sourceFile}" <<EOF
g/^.\{0,${shortWords}\}$/d
g/^.\{${longWords#0},\}$/d
w
q
EOF

print -u2 -- "\nFile \"$PWD/\033[91m${sourceFile#*/}\033[0m\"\n\toverwritten successfully!\n"

# Shell settings START
set +o noglob
# Shell settings END

# END OF SCRIPT

