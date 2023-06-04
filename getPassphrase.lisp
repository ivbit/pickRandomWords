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

Create passphrase with 100 words randomly selected out of huge array of words.
https://en.wikipedia.org/wiki/Passphrase

Usage:
clisp getPassphrase.lisp
sbcl --script getPassphrase.lisp
ccl64 -l getPassphrase.lisp -e '(quit)'
OR
(load "getPassphrase.lisp")
(loop repeat 10 do (get-passphrase))
(dotimes (i 10) (get-passphrase))
(dotimes (i 10) (progn (princ (1+ i)) (get-passphrase)))
Description END |#

;; If list/array is huge, print only the first 100 elements to the repl
(setf *print-length* 100)

;; On Unix-like systems, system file '/usr/share/dict/words' can be used to
;; produce words array: remove single quotes and unusual characters from file
(defparameter *words*
#(
  huge array of words goes here in form of lisp symbols
  it better be more than a million words
  the actual array is too huge to upload
  if run as is the program will pick passphrase from this desciption
) ; #() array END
) ; defparameter *words* END

(defparameter *indices* (1- (length *words*)))

;; clear screen (Unix-like systems) START
(defun clear ()
 #+clisp
  (ext:run-program "/usr/bin/clear")
 #+sbcl
  (sb-ext:run-program "/usr/bin/clear" nil :input nil :output *standard-output*)
 #+clozure
  (ccl:run-program "/usr/bin/clear" nil :input nil :output *standard-output*)
 '(screen was cleared)
) ; defun clear END
(defun clear0 () (clear))
(defun cls () (clear))
;; clear screen (Unix-like systems) END

(defun true-random-array (&optional (bit-amt 64))
  (with-open-file
    ( ; parameter definitions START
      urandom "/dev/urandom"
      :direction :input
      :element-type (list 'unsigned-byte bit-amt)
    ) ; parameter definitions END
    (let*
      ( ; variable definition START
        (uint-arr (make-array 100 :element-type (list 'unsigned-byte bit-amt)))
      ) ; variable definition END
      (read-sequence uint-arr urandom)
      uint-arr
    ) ; let* END
  ) ; with-open-file END
) ; defun true-random END

(defun get-passphrase ()
  (terpri)
  (princ
    (string-downcase
      (string-trim "()"
        (prin1-to-string
          (loop for item across (true-random-array)
            collect (aref *words* (mod item *indices*))
          ) ; loop END
        ) ; prin1-to-string END
      ) ; string-trim END
    ) ; string-downcase END
  ) ; princ END
  (terpri)
  (terpri)
) ; defun get-passphrase END

(get-passphrase)

;; END OF SCRIPT


