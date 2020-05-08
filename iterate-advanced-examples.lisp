
;; This file both collects example code from the Iterate manual, adds
;; examples for constructs without exemplars and has several advanced
;; examples of drivers and generators.


;; 2.1.2 Sequence iteration
;;
;; for var in list &optional by step-function

;; Print every item in a list (from iterate-examples.lisp)
(iterate (for item in '(1 2 3))
   (print item))
;; >>  1
;; >>  2
;; >>  3
;; =>  NIL

;; Print every other item in a list. (from iterate-examples.lisp)
(iterate (for item in '(1 2 3 4 5) by #'cddr)
   (print item))
;; >>  1
;; >>  3
;; >>  5
;; =>  NIL

;; for var on list &optional by step-function

;; Collect successive tails of a list. (from iterate-examples.lisp)
(iterate (for sublist on '(a b c d))
   (collect sublist))
;; =>  ((A B C D) (B C D) (C D) (D))

;; Search list for symbol and return subsequent item.
(iterate (for sublist on '(:a 1 :b 2 :c 3) by #'cddr)
	 (when (eq (car sublist) :b)
	   (return (cadr sublist))))
;; => 2

;; for var in-vector vector &sequence
;; &sequence := from | upfrom | downfrom | to | downto | above | below | by

;; Print sequence of letters
(let ((letters (vector 'a 'b 'c 'd 'e 'f)))
	   (iterate (for letter in-vector letters)
		    (format t "~s~%"  letter)))

;; Print sequence of letters using from and downto forms, i.e. from
;; 'z' down to 'u' (zero-based) character in the alphabet, specifying
;; that the index variable be lexically bound to i.
(let ((letters "abcdefghijklmnopqrstuvwxyz"))
	   (iterate (for letter in-vector letters with-index i from 25 downto 20)
		    (format t "~s: ~a~%"  i (string letter))))

;; likewise except using upfrom, characters in the interval ['u', 'z']
(let ((letters "abcdefghijklmnopqrstuvwxyz"))
	   (iterate (for letter in-vector letters with-index i upfrom 20)
		    (format t "~s: ~a~%"  i (string letter))))

;; likewise except using downfrom, characters in the interval ['k',
;; 'a']
(let ((letters "abcdefghijklmnopqrstuvwxyz"))
	   (iterate (for letter in-vector letters with-index i downfrom 10)
		    (format t "~s: ~a~%"  i (string letter))))

;; Likewise except characters in the interval [p, q], note from and
;; upfrom are synonyms
(let ((letters "abcdefghijklmnopqrstuvwxyz"))
	   (iterate (for letter in-vector letters with-index i from 15)
		    (format t "~s: ~a~%"  i (string letter))))

(let ((letters "abcdefghijklmnopqrstuvwxyz"))
	   (iterate (for letter in-vector letters with-index i upfrom 15)
		    (format t "~s: ~a~%"  i (string letter))))

;; likewise
(let ((letters "abcdefghijklmnopqrstuvwxyz"))
	   (iterate (for letter in-vector letters with-index i from 15 downto 5)
		    (format t "~s: ~a~%"  i (string letter))))
