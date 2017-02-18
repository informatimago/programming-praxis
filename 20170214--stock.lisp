;; Today's exercise is a classic of both algorithm classes and corporate
;; interviews:
;;
;; Given a list of daily stock prices for a month, find the day on which
;; you can buy a stock and the day on which you can sell the stock that
;; gives the maximum gain.
;;
;; Your task is to write a program that finds the optimal starting and
;; ending dates for stock purchase and sale. When you are finished, you
;; are welcome to read or run a suggested solution, or to post your own
;; solution or discuss the exercise in the comments below.



(in-package :cl-user)

(defmacro maximizing (expression &key such-as)
  (destructuring-bind (order min v1 v2 max) such-as
    (let ((vmax     (gensym "max-"))
          (vmax-val (gensym "max-val-"))
          (vmax-v1  (gensym (format nil "max-~A-" v1)))
          (vmax-v2  (gensym (format nil "max-~A-" v2))))
      `(loop :with ,vmax := ,max
             :with ,vmax-val
             :with ,vmax-v1
             :with ,vmax-v2
             :for ,v1 :from ,min ,(if (eq order '<)
                                      :below
                                      :to) ,vmax
             :do (loop :for ,v2 :from (+ ,v1 ,(if (eq order '<)
                                                  1 0))
                         :to ,vmax
                       :do (let ((cur-val (let ((,v1 ,v1)
                                                (,v2 ,v2))
                                            ,expression)))
                             (when (or (null ,vmax-val)
                                       (< ,vmax-val cur-val))
                               (setf ,vmax-val cur-val
                                     ,vmax-v1 ,v1
                                     ,vmax-v2 ,v2))))
             :finally (return (values ,vmax-v1 ,vmax-v2 ,vmax-val))))))


(defun best-trade (seq)
  (maximizing (- (aref seq sell) (aref seq buy))
              :such-as (<= 0 buy sell (- (length seq) 1))))


(defun save-data-for-plot (path seq)
  (with-open-file (*standard-output* path :direction :output
                                          :if-does-not-exist :create
                                          :if-exists :supersede)
    (map nil (function print) seq)
    (terpri)))

(let ((quotes (coerce (loop :repeat 22
                            :for quote := 1000/100
                              :then (+ quote (/ (- (random 100) 50) 100))
                            :collect (coerce quote 'single-float))
                      'vector)))
  (save-data-for-plot "/tmp/quotes.dat" quotes)
  (asdf:run-shell-command "gnuplot -e 'plot \"/tmp/quotes.dat\" with lines'")
  (best-trade quotes))
