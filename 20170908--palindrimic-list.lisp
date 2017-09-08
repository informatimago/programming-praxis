;; Linked List Palindrome
;; 
;; by programmingpraxis
;; 
;; Today's exercise is an Amazon interview question from Career Cup (this
;; is the exact question asked):
;; 
;; 
;; 
;; There is a given linked list where each node can consist of any number
;; of characters :- For example
;; 
;; a-->bcd-->ef-->g-->f-->ed-->c-->ba.
;; 
;; Now please write a function where the linked list will return true if
;; it is a palindrome .
;; 
;; Like in above example the linked list should return true
;; 
;; 
;; 
;; Your task is to write a program to determine if a list of strings
;; forms a palindrome. When you are finished, you are welcome to read or
;; run a suggested solution, or to post your own solution or discuss the
;; exercise in the comments below.

;; [sourcecode lang="css"]

;; Simple solution.
;; O(n) in time. (assuming amortized O(n) with-output-to-string implementation).
;; 2n space. (for string and its reverse).
;; programmer time: the time to write it, correct on the first write.

(defun palindromic-list-p (list-of-strings)
  (flet ((list-to-string (los)
           (with-output-to-string (*standard-output*)
             (dolist (string los)
               (write-string string)))))
    (let ((string (list-to-string list-of-strings)))
      (string= string (reverse string)))))

(assert      (palindromic-list-p '("a" "bcd" "ef" "g" "f" "ed" "c" "ba")))
(assert (not (palindromic-list-p '("a" "bcd" "ef" "g" "f" "Xd" "c" "ba"))))



;; Sophisticated solution: the characters are compared in place.
;; O(n) in time
;; O(e) in space with e = (length list-of-strings), which may be much smaller than n if the strings are big.
;; programmer time: a lot; debugging and tuning.

(defun palindromic-list-p (list-of-strings)
  (labels ((equal-palindromic-string (size a start-a b start-b)
             ;; (print (list 'equal-palindromic-string size a start-a b start-b))
             (loop
               :repeat size
               :for i :from start-a
               :for j :from (- (length b) start-b 1) :downto 0
               :always (char= (aref a i) (aref b j))))
           (equal-list-of-strings (a start-a b start-b)
             (cond
               ((endp a) (endp b))
               ((endp b) nil)
               (t        (let* ((size (min (- (length (first a)) start-a)
                                           (- (length (first b)) start-b)))
                                (new-start-a (+ start-a size))
                                (new-start-b (+ start-b size)))
                           (and (equal-palindromic-string size (first a) start-a (first b) start-b)
                                (equal-list-of-strings
                                 (if (< new-start-a (length (first a)))   a           (rest a))
                                 (if (< new-start-a (length (first a)))   new-start-a 0)
                                 (if (< new-start-b (length (first b)))   b           (rest b))
                                 (if (< new-start-b (length (first b)))   new-start-b 0))))))))
    (equal-list-of-strings list-of-strings 0
                           (reverse list-of-strings) 0)))


(assert      (palindromic-list-p '("a" "bcd" "ef" "g" "f" "ed" "c" "ba")))

(assert (not (palindromic-list-p '("a" "bcd" "ef" "g" "f" "Xd" "c" "ba"))))

(assert (palindromic-list-p
         '("a" "man" "a" "plan" "a" "caret" "a" "ban" "a" "myriad" "a" "sum"
           "a" "lac" "a" "liar" "a" "hoop" "a" "pint" "a" "catalpa" "a" "gas"
           "an" "oil" "a" "bird" "a" "yell" "a" "vat" "a" "caw" "a" "pax" "a"
           "wag" "a" "tax" "a" "nay" "a" "ram" "a" "cap" "a" "yam" "a" "gay" "a"
           "tsar" "a" "wall" "a" "car" "a" "luger" "a" "ward" "a" "bin" "a"
           "woman" "a" "vassal" "a" "wolf" "a" "tuna" "a" "nit" "a" "pall" "a"
           "fret" "a" "watt" "a" "bay" "a" "daub" "a" "tan" "a" "cab" "a"
           "datum" "a" "gall" "a" "hat" "a" "fag" "a" "zap" "a" "say" "a" "jaw"
           "a" "lay" "a" "wet" "a" "gallop" "a" "tug" "a" "trot" "a" "trap" "a"
           "tram" "a" "torr" "a" "caper" "a" "top" "a" "tonk" "a" "toll" "a"
           "ball" "a" "fair" "a" "sax" "a" "minim" "a" "tenor" "a" "bass" "a"
           "passer" "a" "capital" "a" "rut" "an" "amen" "a" "ted" "a" "cabal"
           "a" "tang" "a" "sun" "an" "ass" "a" "maw" "a" "sag" "a" "jam" "a"
           "dam" "a" "sub" "a" "salt" "an" "axon" "a" "sail" "an" "ad" "a"
           "wadi" "a" "radian" "a" "room" "a" "rood" "a" "rip" "a" "tad" "a"
           "pariah" "a" "revel" "a" "reel" "a" "reed" "a" "pool" "a" "plug" "a"
           "pin" "a" "peek" "a" "parabola" "a" "dog" "a" "pat" "a" "cud" "a"
           "nu" "a" "fan" "a" "pal" "a" "rum" "a" "nod" "an" "eta" "a" "lag"
           "an" "eel" "a" "batik" "a" "mug" "a" "mot" "a" "nap" "a" "maxim" "a"
           "mood" "a" "leek" "a" "grub" "a" "gob" "a" "gel" "a" "drab" "a"
           "citadel" "a" "total" "a" "cedar" "a" "tap" "a" "gag" "a" "rat" "a"
           "manor" "a" "bar" "a" "gal" "a" "cola" "a" "pap" "a" "yaw" "a" "tab"
           "a" "raj" "a" "gab" "a" "nag" "a" "pagan" "a" "bag" "a" "jar" "a"
           "bat" "a" "way" "a" "papa" "a" "local" "a" "gar" "a" "baron" "a"
           "mat" "a" "rag" "a" "gap" "a" "tar" "a" "decal" "a" "tot" "a" "led"
           "a" "tic" "a" "bard" "a" "leg" "a" "bog" "a" "burg" "a" "keel" "a"
           "doom" "a" "mix" "a" "map" "an" "atom" "a" "gum" "a" "kit" "a"
           "baleen" "a" "gala" "a" "ten" "a" "don" "a" "mural" "a" "pan" "a"
           "faun" "a" "ducat" "a" "pagoda" "a" "lob" "a" "rap" "a" "keep" "a"
           "nip" "a" "gulp" "a" "loop" "a" "deer" "a" "leer" "a" "lever" "a"
           "hair" "a" "pad" "a" "tapir" "a" "door" "a" "moor" "an" "aid" "a"
           "raid" "a" "wad" "an" "alias" "an" "ox" "an" "atlas" "a" "bus" "a"
           "madam" "a" "jag" "a" "saw" "a" "mass" "an" "anus" "a" "gnat" "a"
           "lab" "a" "cadet" "an" "em" "a" "natural" "a" "tip" "a" "caress" "a"
           "pass" "a" "baronet" "a" "minimax" "a" "sari" "a" "fall" "a" "ballot"
           "a" "knot" "a" "pot" "a" "rep" "a" "carrot" "a" "mart" "a" "part" "a"
           "tort" "a" "gut" "a" "poll" "a" "gateway" "a" "law" "a" "jay" "a"
           "sap" "a" "zag" "a" "fat" "a" "hall" "a" "gamut" "a" "dab" "a" "can"
           "a" "tabu" "a" "day" "a" "batt" "a" "waterfall" "a" "patina" "a"
           "nut" "a" "flow" "a" "lass" "a" "van" "a" "mow" "a" "nib" "a" "draw"
           "a" "regular" "a" "call" "a" "war" "a" "stay" "a" "gam" "a" "yap" "a"
           "cam" "a" "ray" "an" "ax" "a" "tag" "a" "wax" "a" "paw" "a" "cat" "a"
           "valley" "a" "drib" "a" "lion" "a" "saga" "a" "plat" "a" "catnip" "a"
           "pooh" "a" "rail" "a" "calamus" "a" "dairyman" "a" "bater" "a"
           "canal" "panama")))


;; To see how this works, let's trace equal-palindromic-string:
#||

cl-user>       (palindromic-list-p '("a" "bcd" "ef" "g" "f" "ed" "c" "ba"))

(equal-palindromic-string 1 "a" 0 "ba" 0) 
(equal-palindromic-string 1 "bcd" 0 "ba" 1) 
(equal-palindromic-string 1 "bcd" 1 "c" 0) 
(equal-palindromic-string 1 "bcd" 2 "ed" 0) 
(equal-palindromic-string 1 "ef" 0 "ed" 1) 
(equal-palindromic-string 1 "ef" 1 "f" 0) 
(equal-palindromic-string 1 #1="g" 0 #1# 0) 
(equal-palindromic-string 1 "f" 0 "ef" 0) 
(equal-palindromic-string 1 "ed" 0 "ef" 1) 
(equal-palindromic-string 1 "ed" 1 "bcd" 0) 
(equal-palindromic-string 1 "c" 0 "bcd" 1) 
(equal-palindromic-string 1 "ba" 0 "bcd" 2) 
(equal-palindromic-string 1 "ba" 1 "a" 0) 
t
cl-user>       (palindromic-list-p '("a" "bcd" "ef" "g" "f" "Xd" "c" "ba"))

(equal-palindromic-string 1 "a" 0 "ba" 0) 
(equal-palindromic-string 1 "bcd" 0 "ba" 1) 
(equal-palindromic-string 1 "bcd" 1 "c" 0) 
(equal-palindromic-string 1 "bcd" 2 "Xd" 0) 
(Equal-palindromic-string 1 "ef" 0 "Xd" 1) 
nil
cl-user>

||#
;; [/sourcecode]




