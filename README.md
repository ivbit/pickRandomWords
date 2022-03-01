# pickRandomWords

The script creates text file **fetch.txt** with amount of words specified in **outWordsAmount** variable.

Input file **source.txt** must contain **1 word per line** and be **unix file format**.

makeWordsSource.ksh and makeWordsSourceSpecial.ksh will create file **source.txt** out of

**/usr/share/dict/word** system file. It is better to create your own word source file with a couple of millions words in it.

The script is used to fetch random words from input file and create a single line containing all words in output file.

This script is written for OpenBSD's **pdksh**.

The scripts with *Special* suffix will work on linux through **mksh** shell.

