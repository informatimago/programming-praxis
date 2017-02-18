(in-package :user1)


(defun collect-letters (string)
  (loop
    :with table := (make-hash-table)
    :for letter :across string
    :do (incf (gethash letter table 0))
    :finally (let ((letters '()))
               (maphash (lambda (letter count)
                          (push (cons letter count) letters))
                        table)
               (return letters))))

(defun make-palindrome (string)
  (let ((letters (collect-letters string)))
    (when (<= (count-if (function oddp) letters :key (function cdr)) 1)
      (let* ((middle)
             (left (with-output-to-string (out)
                     (loop :for (letter . count) :in letters
                           :do (format out "~V,,,V<~>" (truncate count 2) letter)
                           :when (oddp count)
                             :do (setf middle letter)))))
        (concatenate 'string left (when middle (list middle)) (reverse left))))))


(defun palindrome-from-string (string)
  (loop for ch across string
        with singl = '()
        and  chars = '()
        do (cond ((member ch singl)
                  (setf singl (remove-if (lambda (c) (eql c ch)) singl))
                  (push ch chars))
                 (t (push ch singl)))
        finally (cond ((<= (length singl) 1)
                       (return (map 'string 'identity (append chars singl (reverse chars)))))
                      (t (return nil)))))


(defun palindromep (string)
  (and (stringp string)
       (loop
         :for i :from 0
         :for j :from (1- (length string)) :by -1
         :while (< i j)
         :always (char= (aref string i) (aref string j)))))

(dolist (fun '(make-palindrome palindrome-from-string))
  (loop :for input :in '("" "x" "eelloo" "Heelloo")
        :always (palindromep (funcall fun input)))
  (loop :for input :in '("xy" "Not a palidrome.")
        :always (null  (funcall fun input))))


(make-palindrome "")
;; --> ""
(make-palindrome "x")
;; --> "x"
(make-palindrome "xy")
(make-palindrome "xyaabb")
;; --> nil
(make-palindrome "eelloo")
;; --> "eolloe"
(make-palindrome "Heelloo")
;; --> "eolHloe"
(make-palindrome "World")
;; --> nil

