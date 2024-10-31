#! /bin/sh
# launch \
exec tclsh "$0" ${1+"$@"}

# Intellectual property information START
#
# Copyright (c) 2024 Ivan Bityutskiy
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
# Creating passphrase with 100 words randomly selected out of list of words.
# https://en.wikipedia.org/wiki/Passphrase
#
# Usage:
# ./getPassphrase.tcl
# OR
# Start tclsh and source ./getPassphrase.tcl, then call genPw, or 'puts [genPw]'
# tclsh
# source ./getPassphrase.tcl
# genPw
# for {set i 0} {$i < 100} {incr i} {puts [genPw]}
# Description END

# Huge list of words
set words {
  huge list of words goes here in form of
  single words separated by whitespace
  it better be more than a million words
  the actual list is too huge to upload
  if run as is the program will pick
  passphrase from this desciption
}

# Getting true random numbers from /dev/urandom on *nix systems.
# Maximum limit for the range
set max [llength $words]
# How many bytes to read at once (8 bytes = 64 bits = 64 bit integer)
set readSingle 8
# How many integers are needed
set readAll [expr {$readSingle * 100}]

# Procedure will read all the data from /dev/urandom in a single step and store
# it in a list. This is a better solution than reading from /dev/urandom every
# single time, opening and closing the file. For example instead of opening and
# closing /dev/urandom 100 times, it will be done only once.
proc genRandList {maxValue {amtBytes 800}} {
  set devUrandom [open /dev/urandom rb]
  # Converting binary data into unsigned integers for future use:
  # Order can be 'little endian', 'big endian', or 'native' for the CPU;
  # m = 64 bit integer in native order; n = 32 bit integer in native order;
  # u = unsigned flag; * = count, all bytes will be stored in one variable
  binary scan [chan read $devUrandom $amtBytes] mu* randList
  chan close $devUrandom
  # Storing random numbers (0..=$maxValue-1) in a list
  foreach {num} $randList {
    lappend randResult [expr {$num % $maxValue}]
  }
  return $randResult
}

# Procedure will generate the passphrase
proc genPw {} {
  global words
  # Using newline character around passphrase for better readability
  set pwStr \n
  foreach {randNum} [genRandList $::max $::readAll] {
    append pwStr [lindex $words $randNum] { }
  }
  # Trimming the last space character from a string
  set pwStr [string trimright $pwStr]
  # Using newline character around passphrase for better readability
  append pwStr \n
  # Returning the passphrase
  return $pwStr
}

# Printing the passphrase
chan puts stdout [genPw]

# END OF SCRIPT

