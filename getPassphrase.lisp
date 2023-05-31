#| Intellectual property information START

Copyright (c) 2023 Ivan Bityutskiy 

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

Intellectual property information END

Description START

Create passphrase with 100 words randomly selected out of 781910 words.
https://en.wikipedia.org/wiki/Passphrase

Usage:
clisp getPassphrase.lisp
sbcl --script getPassphrase.lisp
ccl64 -l getPassphrase.lisp -e '(quit)'

Works on Unix-like systems

Description END |#

(defparameter *words* (make-array 781910 :initial-contents '(
  words in form of lisp sumbols go here
  original array contains 781910 words and file is too huge to upload
)))

(defun true-random-array (&optional (bit-amt 64))
  (with-open-file
    ( ; parameter definitions START
      urandom "/dev/urandom"
      :direction :input
      :element-type (list 'unsigned-byte bit-amt)
    ) ; parameter definitions END
    (let*
      ( ; variable definition START
        (uint-array (make-array 100 :element-type (list 'unsigned-byte bit-amt)))
      ) ; variable definition END
      (read-sequence uint-array urandom)
      uint-array
    ) ; let* END
  ) ; with-open-file END
) ; defun true-random END

(defun get-passphrase ()
  (princ #\Newline)
  (princ
    (string-downcase
      (string-trim "()"
        (prin1-to-string
          (loop for item across (true-random-array)
            collect (aref *words* (1+ (mod item 781910)))
          ) ; loop END
        ) ; prin1-to-string END
      ) ; string-trim END
    ) ; string-downcase END
  ) ; princ END
  (fresh-line)
  (princ #\Newline)
) ; defun get-passphrase END

(get-passphrase)

;; END OF SCRIPT

