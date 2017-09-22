;; Comma Quibbling
;; 
;; September 22, 2017
;; 
;; Eric Lippert, at his blog Fabulous Adventures in Coding, discusses the
;; problem of comma quibbling, which turns a list of words like (“ABC”
;; “DEF” “G” “H”) into the string “ABC, DEF, G and H” with commas between
;; each pair of words except the last pair, which gets the word “and”
;; (without a comma). Here are the rules:
;; 
;; 
;; 
;; If the input is empty, so is the output.
;; 
;; If the input has a single word, so does the output.
;; 
;; If the input has two words, the output is the two words separated by
;; “and”.
;; 
;; If the input has more than two words, the output is all the words,
;; separated by commas, but with the last comma replaced by “and”.
;; 
;; A word is a maximal sequence of characters not containing a comma or a
;; space.
;; 
;; 
;; 
;; Your task is to write a function that adds commas to a list of words,
;; using the rules described above. When you are finished, you are
;; welcome to read or run a suggested solution, or to post your own
;; solution or discuss the exercise in the comments below.

(defun commated-list (words)
  (format nil "~[~;~*~A~;~{~A~} and ~A~:;~{~A~^, ~} and ~A~]"
          (length words)
          (butlast words)
          (first (last words))))

(mapcar (function commated-list)
        '(()
          ("Apple")
          ("Apple" "Banana")
          ("Apple" "Banana" "Cherry")
          ("Apple" "Banana" "Cherry" "Date")))
;; --> ("" "Apple" "Apple and Banana" "Apple, Banana and Cherry" "Apple, Banana, Cherry and Date")

