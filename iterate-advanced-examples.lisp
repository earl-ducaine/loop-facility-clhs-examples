
;; This file both collects example code from the Iterate manual, adds
;; examples for constructs without exemplars and has several advanced
;; examples of drivers and generators.

;; Getting started -- load iterate from Quicklisp and import all
;; exported symbols into your current package. iterate-keywords
;; (separate from Common Lisp keywords) are symbols rather than
;; designators of some kind and therefor must be refered to by their
;; package name if they aren't imported, something that quickly
;; becomes tedious, e.g.
;;
;; (itererate::iter (itererate::for i itererate::from 1 itererate::to 10)
;;                  (itererate::collect i))
;;
;; For more information on installing quicklisp see the main website:
;;
;; https://www.quicklisp.org/

(ql:quickload :iterate)
(use-package :iterate)

;; 1
;;
;; simple example
(iter (for i from 1 to 10)
      (collect i))
;; >> (1 2 3 4 5 6 7 8 9 10)

;; collect odd numbers in a list
(let ((list '(1 2 3 4 5 6 7 8 9 10)))
  (iter (for el in list)
	(if (and (numberp el) (oddp el))
	    (collect el))))
;; >> (1 3 5 7 9)

;; Example of iterating over alist, of creation a new variable
;; bindings, stepping over multiple sequences at once, using compiler
;; declarations of variable types, and collecting values
(let ((alist '((a 1) (b 2) (c 3) (d 4))))
  (iter (for (key . item) in alist)
	(for i from 0)
	(declare (fixnum i))
	(collect (cons i key))))
;; >> ((0 . A) (1 . B) (2 . C) (3 . D))

;; 2.1 Drivers
;;
;; Example of repeat, the only iteration driver clause that doesn't
;; begin with for.
(iter (repeat 3)
      (print "I will not talk in class."))
;; >> "I will not talk in class."
;; >> "I will not talk in class."
;; >> "I will not talk in class."


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
