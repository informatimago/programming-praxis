;; From: programmingpraxis <post@gwene.org>
;; Subject: 357 Numbers
;; Newsgroups: gwene.com.yahoo.pipes.lisp
;; Date: Fri, 06 Mar 2015 10:00:09 +0100 (4 hours, 1 minute, 35 seconds ago)
;; Message-ID: <x1-CMuvSQ5uR6vr70m9ywJFJ7rNsw8@gwene.org>
;;
;; This question arose at a job-interview site: Find all numbers
;; divisible only by 3, 5 and 7. For instance, 35 = 5 × 7 is included in
;; the set, but 30 = 2 × 3 × 5 is not because of the factor of 2. Your
;; task is to write the requested program and determine how many numbers
;; in the set are less than a million. When you are finished, you are
;; welcome to read or run a suggest solution, or to post your own
;; solution or discuss the exercise in the comments below.

(length (sort (let ((max 1000000))
                (loop
                  :for 3s = 1 then (* 3 3s)
                  :while (<= 3s max)
                  :append (loop
                            :for 5s = 1 then (* 5 5s)
                            :for 35s = (* 3s 5s)
                            :while (<= 35s max)
                            :append (loop
                                      :for 7s = 1 then (* 7 7s)
                                      :for 357s = (* 3s 5s 7s)
                                      :while (<= 357s max)
                                      :collect 357s))))
              (function <)))
;; 203

