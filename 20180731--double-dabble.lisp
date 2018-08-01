;; https://programmingpraxis.com/2018/07/31/double-dabble/
;; 
;; Double Dabble
;; 
;; July 31, 2018
;; 
;; Over at ComputerPhile, Professor Brailsford gives a beautiful
;; explanation of the double-dabble algorithm for converting a number
;; from binary to binary-coded decimal. If you don’t want to watch the
;; video — you should — there is also an explanation at Wikipedia, or you
;; can read the original description by C. B. Falconer.
;; 
;; Your task is to implement the double-dabble algorithm as shown by
;; Professor Brailsford. When you are finished, you are welcome to read
;; or run a suggested solution, or to post your own solution or discuss
;; the exercise in the comments below.
;;
;; https://www.youtube.com/watch?v=eXIfZ1yKFlA
;; https://en.wikipedia.org/wiki/Double_dabble
;; http://cbfalconer.home.att.net/download/dubldabl.txt


;; [sourcecode lang="css"]

;; That algorithm is obviously nice when implemented in assembler (it
;; uses only one MUTABLE register, it implements a simple and short loop,
;; and foremost, it avoids a costly division by ten).
;; 
;; However, when implement in a high level programming language (EVEN in
;; C!), it is bound to be worse than any other algorithm, because in high
;; level programming languages, numbers are not mutable registers, they
;; are immutable. Therefore manipulating bits in integers is costly!
;; Processing the BCD digits (check if >4 and add 3) n times will be a
;; big waste of time, compared to only (ceilling n 3) divisions by ten!
;;
;; On my system double-dabble takes more than double the time of bcd, for
;; 4-digits, 16-bit BCDs.

(defun double-dabble (n w)
  (let ((b (* 4 (ceiling w 3))))
    (flet ((adjust (n)
             (loop
               :for p :from w :below (+ w b) :by 4
               :for d := (ldb (byte 4 p) n)
               :when (< 4 d)
                 :do (setf n (dpb (+ 3 d) (byte 4 p) n))
               :finally (return n))))
      (declare (inline adjust))
      (loop
        :repeat w
        :do (setf n (ash (adjust n) 1))
        :finally (return (ldb (byte b w) n))))))

(defun test/double-dabble ()
  (flet ((check (n)
           (assert (string= (prin1-to-string n) (format nil "~X" (double-dabble n 16))))))
    (check 0)
    (check 1) (check 10) (check 100) (check 1000)
    (check 2) (check 20) (check 200) (check 2000)
    (check 3) (check 30) (check 300) (check 3000)
    (check 4) (check 40) (check 400) (check 4000)
    (check 5) (check 50) (check 500) (check 5000)
    (check 6) (check 60) (check 600) (check 6000)
    (check 7) (check 70) (check 700) (check 7000)
    (check 8) (check 80) (check 800) (check 8000)
    (check 9) (check 90) (check 900) (check 9000)
    (check 55) (check 5500) (check 1234) (check 4321)
    (check 99) (check 9900) (check 6789) (check 9876)
    (check 91) (check 19)  (check 9191) (check 1919))
  :success)

(defun bcd (n w)
  (let ((b (* 4 (ceiling w 3))))
    (loop
      :with r := 0
      :repeat b
      :for d :from 0 :by 4
      :do (multiple-value-bind (q digit) (truncate n 10)
            (setf r (dpb digit (byte 4 d) r)
                  n q))
      :finally (return r))))

(defun test/bcd ()
  (flet ((check (n)
           (assert (string= (prin1-to-string n) (format nil "~X" (bcd n 16))))))
    (check 0)
    (check 1) (check 10) (check 100) (check 1000)
    (check 2) (check 20) (check 200) (check 2000)
    (check 3) (check 30) (check 300) (check 3000)
    (check 4) (check 40) (check 400) (check 4000)
    (check 5) (check 50) (check 500) (check 5000)
    (check 6) (check 60) (check 600) (check 6000)
    (check 7) (check 70) (check 700) (check 7000)
    (check 8) (check 80) (check 800) (check 8000)
    (check 9) (check 90) (check 900) (check 9000)
    (check 55) (check 5500) (check 1234) (check 4321)
    (check 99) (check 9900) (check 6789) (check 9876)
    (check 91) (check 19)  (check 9191) (check 1919))
  :success)


(defun bench/double-dabble ()
  (loop :for i :to 9999
        :do (double-dabble i 16)))

(defun bench/bcd ()
  (loop :for i :to 9999
        :do (bcd i 16)))

;; (load (compile-file #P"~/src/lisp/encours/double-dabble.lisp"))
;; (time (bench/double-dabble))
;; (time (bench/bcd))

;; [/sourcecode]
