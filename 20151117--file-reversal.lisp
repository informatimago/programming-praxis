
;; http://programmingpraxis.com/2015/11/17/file-reversal/
;;
;;
;; File Reversal
;; November 17, 2015
;;
;; Given a file containing lines of text that fits into memory, it is
;; easy to write the file with lines in reverse order: read the lines of
;; text into memory, then write them in reverse. It is harder to reverse
;; the lines of a file if the file is too big to fit into memory.
;;
;; Your task is to write a program that reverses the lines of a text file
;; that is too big to fit in memory. When you are finished you are
;; welcome to read or run a suggested solution, or to post your own
;; solution or discuss the exercise in the comments below.


#|

A text file that is too big to fit in memory can contain lines that
are too big to fit in memory.

MMMMMMMMMMMMMM
|_______________________________________| <- first line.
…
|_|___|_|___|___|___|_|_|_|_|_|_|_|_|___| <- last line.
 ^
 |
14th before last line


MMMMMMMMMMMMMM
|_______________________________________| <- first line.
…
|_|___|_|___|_____________________|_|___| <- last line.
 ^                    ^
 |                    |
 |                 bigger than memory :-P.
 |
6th before last line.

MMMMMMMMMMMMMM
|_______________________________________| <- first line.
|___| <- second and last line.

|_______________________________________| <- first line.
|___________________| <- second and last line.


|#

(defparameter *line-separator* #\newline)
(defparameter *block-size* 4096)
(defparameter *half-memory-size* (min (* 16 1024 1024)
                                      (* *block-size*
                                         (truncate array-total-size-limit
                                                   *block-size*))))

(defun make-buffer ()
  (make-array *half-memory-size*
              :element-type '(unsigned-byte 8)
              :fill-pointer *half-memory-size*))
