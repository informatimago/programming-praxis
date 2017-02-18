;; [sourcecode lang="css"]
;; I can see from my statistics that the new academic year is beginning.
;; Again, as in a previous exercise, in the spirit of helping programming
;; students who are just starting a new school year, we have two typical
;; homework problems.
;;
;; Your task is to write programs to solve these problems. When you
;; are finished, you are welcome to read or run a suggested solution,
;; or to post your own solution or discuss the exercise in the
;; comments below.


;; 1. Given an array of positive integers, find the inflection point
;;    where the total of the integers before the inflection point and the
;;    total of the integers after the inflection point are least
;;    different. For instance, given the array [3, 7, 9, 8, 2, 5, 6], the
;;    inflection point is between the 9 and 8, which leaves a total of 19
;;    before the inflection point and 21 after, a difference of 2.


;; (3 1 3)
;;   ^        3 4
;;
;;  (3 1 2 3)
;;      ^     4 5

;;  (3 2 1 3)
;;      ^     5 4

;; (2 2 3 1 1 1 4 1 1 8 2 2)
;;               ^           14 14

(defun sum (vector min max)
  (loop
    :for i :from min :to max
    :sum (aref vector i)))

(defun inflection-point (vector)
  (let ((i -1)
        (j (length vector))
        (l 0)
        (r 0))
    (loop :while (< (1+ i) j) :do
      (if (< (abs (- (+ l (aref vector (1+ i))) r))
             (abs (- l (+ r (aref vector (1- j))))))
          (progn (incf i) (incf l (aref vector i)))
          (progn (decf j) (incf r (aref vector j)))))
    (values i l r (abs (- l r)))))

(progn ;; tests
  (assert (=  0 (inflection-point #(3 1 3))))
  (assert (=  1 (inflection-point #(3 2 1 3))))
  (assert (=  2 (inflection-point #(3 7 9 8 2 5 6))))
  (assert (=  6 (inflection-point #(2 2 3 1 1 1 4 1 1 8 2 2))))
  (assert (= -1 (inflection-point #())))
  (assert (= -1 (inflection-point #(42)))))

(inflection-point #(42))
;; --> -1
;;     0
;;     42
;;     42


(inflection-point #(3 7 9 8 2 5 6))
;; --> 2
;;     19
;;     21
;;     2



;; 2. Write a program that reads a file from disk and writes the last n
;;    lines of the file, where n is an input parameter.


;; Of course, we may use a library, loading the whole file in RAM, and
;; processing the list of lines with the language primitives:

(defun last-lines* (file n)
  (last (com.informatimago.common-lisp.cesarum.file:string-list-text-file-contents file)
        n))

;; (last-lines #P"~/src/lisp/encours/programming-praxis--20150821.lisp" 5)
;; --> ("(defun last-lines (file n)"
;;      "  (last (com.informatimago.common-lisp.cesarum.file:string-list-text-file-contents file)"
;;      "        n))"
;;      ""
;;      "(last-lines #P\"~/src/lisp/encours/programming-praxis--20150821.lisp\" )")

;; This is what you would do in a "professional" setting (use libraries).


;; But at school, we're learning to write programs, not to use
;; libraries.  Also there is the objection that the file could be
;; bigger than the RAM, and that N is big too (the tail could also
;; represent a bigger than RAM size if this tail was loaded at once).

;; Therefore we will write a function reading the file, nothing the
;; last N line positions, and re-reading the tail. Instead of
;; collecting the whole tail into a buffer, we call a processing
;; function provided by the caller.

(defun last-lines (file n fun)
  (let ((last-lines (make-array (1+ n) :initial-element nil))
        (last-index -1))
    ;; LAST-LINES will be used as a circular buffer, with LAST-INDEX
    ;; pointing to the last entry.  We need one more slot for the EOF.
    (flet ((modinc (x) (mod (1+ x) (1+ n))))
      (with-open-file (stream file)
        (loop
          :do (setf last-index                   (modinc last-index)
                    (aref last-lines last-index) (file-position stream))
          :while (read-line stream nil nil))
        (loop
          :repeat n
          :for index := (modinc last-index) :then (modinc index)
          :when (aref last-lines index)
            :do (file-position stream (aref last-lines index))
                (funcall fun (read-line stream)))))))

;; (last-lines #P"~/src/lisp/encours/programming-praxis--20150821.lisp" 20 (function print))
;;
;; "  (let ((last-lines (make-array (1+ n) :initial-element nil))"
;; "        (last-index -1))"
;; "    ;; LAST-LINES will be used as a circular buffer, with LAST-INDEX"
;; "    ;; pointing to the last entry.  We need one more slot for the EOF."
;; "    (flet ((inc (x) (mod (1+ x) (1+ n))))"
;; "      (with-open-file (stream file)"
;; "        (loop"
;; "          :do (setf last-index                   (inc last-index)"
;; "                    (aref last-lines last-index) (file-position stream))"
;; "          :while (read-line stream nil nil))"
;; "        (loop"
;; "          :repeat n"
;; "          :for index := (inc last-index) :then (inc index)"
;; "          :when (aref last-lines index)"
;; "            :do (file-position stream (aref last-lines index))"
;; "                (funcall fun (read-line stream)))))))"
;; ""
;; "(last-lines #P\"~/src/lisp/encours/programming-praxis--20150821.lisp\" 20 (function print))"
;; ""
;; ""

;; [/sourcecode]

;; [sourcecode lang="css"]

;; Now, if the file is really that big (bigger than RAM), perhaps
;; there's no point in reading it all just to find the tail.  We could
;; read it "from the end".

;; In Common Lisp, calling file-position with a position not obtained
;; from file-position is not conforming (it could fail on "strange"
;; systems), but it will work as expected on implementations
;; targetting posix systems (with some care for file position falling
;; in the middle of a CR-LF newline).  The following code is not
;; strictly conforming Common Lisp, but it will work on the usual
;; implementations and systems.

(defvar *disk-buffer-size* 4096)
(defvar *default-line-size* 134) ;; columns + newline

(defun estimate-lines-size (n &key last-size last-count)
  "Return an estimation of the size of N lines.
If LAST-SIZE and LAST-COUNT are given, then this estimation takes into
account the fact that only LAST-COUNT lines could be read from
LAST-SIZE bytes."
  (let ((estimation (ceiling (* n (if (and last-size last-count)
                                      (+ 2 (/ last-size last-count))
                                      *default-line-size*)))))
    #+DEBUG (format t "Estimating ~D lines ~:[~*~;~:*given ~D lines took ~D characters ~]to ~D~%"
                    n last-count last-size estimation)
    estimation))

;; (progn (estimate-lines-size 50 :last-size 8192 :last-count 30)
;;        (estimate-lines-size 50))
;; Estimating 50 lines given 30 lines took 8192 characters to 13754
;; Estimating 50 lines to 6700

(defun big-file-last-lines (file n fun)
  (catch 'short
    (return-from big-file-last-lines
      (with-open-file (stream file)
        (let ((file-size  (file-length stream))
              (lines-size (estimate-lines-size n)))
          (when (<= file-size lines-size)
            ;; close the file and try again with the normal procedure.
            (throw 'short nil))
          (flet ((count-lines (stream n from to old-count)
                   "Return the file position of the Nth line before the end,
                    starting from the FROM file position, and assuming
                    there are OLD-COUNT lines between the TO file position
                    and the end of file."
                   (let* ((modulo     (- n old-count))
                          (last-lines (make-array modulo :initial-element nil))
                          (last-index -1))
                     (file-position stream from) ; could be in the middle of a line
                     (read-line stream) ; so throw it away.
                     (flet ((modinc (x) (mod (1+ x) modulo)))
                       (let ((new-count (loop
                                          :for line-position := (file-position stream)
                                          :while (and (< line-position to)
                                                      (read-line stream nil nil))
                                          :do (setf last-index                   (modinc last-index)
                                                    (aref last-lines last-index) line-position)
                                          :count 1)))
                         (if (<= modulo new-count)
                             (values n
                                     (aref last-lines (modinc last-index)))
                             (values (+ old-count new-count)
                                     (aref last-lines (mod (- last-index new-count) modulo))))))))
                 (round-file-position (file-position)
                   (* (floor file-position *disk-buffer-size*) *disk-buffer-size*)))
            (loop
              :with line-position := file-size
              :with line-count    := 0
              :for from := (round-file-position (- file-size lines-size))
              :until (minusp from)
              :do (multiple-value-setq (line-count line-position)
                    (count-lines stream n from line-position line-count))
                  (setf lines-size (estimate-lines-size n :last-size (- file-size line-position)
                                                          :last-count line-count))
              :while (< line-count n)
              :finally (file-position stream line-position)
                       (loop :repeat line-count
                             :do (funcall fun (read-line stream)))))))))
  ;; Only called when 'short has been thrown.
  (last-lines file n fun))

;; cl-user> (ls :l  #P"/tmp/actors.list" )
;; -  900710027 Aug 21 19:07 /tmp/actors.list
;; ; No value
;; cl-user> (time (big-file-last-lines #P"/tmp/actors.list" 12 'write-line))
;;          5. CD-ROM  distribution  is  prohibited  without  written
;;             permission from the Internet Movie Database Ltd
;;
;;     Distribution by e-mail, BBS and  Internet  systems  is  positively
;;     encouraged within these limitations.
;;
;;     The files and software which make up the  movie  database  may  be
;;     uploaded  to  commercial  BBS  systems  providing  that  the above
;;     conditions are met and no *additional* fees are applied above  the
;;     standard connect time or downloading charges.
;;
;;     For further info visit http://www.imdb.com/licensing/contact
;; (big-file-last-lines #P"/tmp/actors.list" 12 'write-line)
;; took 379 microseconds (0.000379 seconds) to run.
;; During that period, and with 8 available CPU cores,
;;        0 microseconds (0.000000 seconds) were spent in user mode
;;        0 microseconds (0.000000 seconds) were spent in system mode
;;  22,528 bytes of memory allocated.
;;  1 minor page faults, 0 major page faults, 0 swaps.
;; nil
;; cl-user>

;; [/sourcecode]

