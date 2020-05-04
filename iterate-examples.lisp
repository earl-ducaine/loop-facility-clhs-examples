



;;;; 6.1.2.1.1 The for-as-arithmetic subclause

(let ((x 1))
  (iterate (for i from x by (incf x) to 10)
     (collect i)))
;; =>  (2 4 6 8 10)
;; =>  Note the subtle difference to loop which produces =>(1 3 5 7 9)
;; roughly the difference between (++i + 1) and (i++ + 1) in c. Moral:
;; Don't use sideeffects in your by clause for either.

(let ((x 1))
  (iterate (for i by (incf x) from x to 10)
     (collect i)))
;; =>  (2 4 6 8 10)
;; See above note about subtle difference with loop.


;;; 6.1.2.1.1.1 Examples of for-as-arithmetic subclause

;; Print some numbers.
(iterate (for i from 1 to 3)
   (print i))
;; >>  1
;; >>  2
;; >>  3
;; =>  NIL

;; Print every third number.
(iterate (for i from 10 downto 1 by 3)
   (print i))
;; >>  10
;; >>  7
;; >>  4
;; >>  1
;; =>  NIL

;; Step incrementally from the default starting value.
(iterate (for i below 3)
   (print i))
;; >>  0
;; >>  1
;; >>  2
;; =>  NIL

;;; 6.1.2.1.2.1 Examples of for-as-in-list subclause

;; Print every item in a list.
(iterate (for item in '(1 2 3))
   (print item))
;; >>  1
;; >>  2
;; >>  3
;; =>  NIL

;; Print every other item in a list.
(iterate (for item in '(1 2 3 4 5) by #'cddr)
   (print item))
;; >>  1
;; >>  3
;; >>  5
;; =>  NIL

;; Destructure a list, and sum the x values using fixnum arithmetic.
(iterate (for (item . x) in '((a . 1) (b . 2) (c . 3)))
	 (declare (fixnum x))
	 (unless (eq item 'b)
	   (sum x)))
;; =>  4

;; Note the use of the ordinary declare vs loop's use of the
;; of-type. Also note that in neither case is the return type
;; specified, i.e. 'the' form. Below is how you would declare that
;; too. See Chapter 4 of the Iterate Manual for more details.
(iterate (declare (iterate:declare-variables))
  (for (item . x) in '((a . 1) (b . 2) (c . 3)))
	 (declare (fixnum x))
	 (unless (eq item 'b)
	   (sum x into my-result))
	 (declare (fixnum my-result))
	 (finally (return my-result)))
;; =>  4

;;;; 6.1.2.1.3.1 Examples of for-as-on-list subclause

;; Collect successive tails of a list.
(iterate (for sublist on '(a b c d))
   (collect sublist))
;; =>  ((A B C D) (B C D) (C D) (D))

;; Print a list by using destructuring with the loop keyword ON.
(iterate (for (item) on '(1 2 3))
   (print item))
;; >>  1
;; >>  2
;; >>  3
;; =>  NIL


;;;; 6.1.2.1.4.1 Examples of for-as-equals-then subclause

;; Collect some numbers.
(iterate (for item initially 1 then (+ item 10))
	 (for iteration from 1 to 5)
	 (collect item))
;; =>  (1 11 21 31 41)
;; Note difference to loop which uses =
;; ... for item = 1 then (+ item 10) ...
;; See Chapter 2 of the Iterate Manual for important differences
;; between: for ... initially; for ... =; and for ... next


;;;; 6.1.2.1.5.1 Examples of for-as-across subclause

(iterate (for char in-vector "abcdefghijkl")
   (write-char char t))
;; => abcdefghijkl
;; NIL
;; Note, iterate uses in-voctor rather than loop's across

;;; 6.1.2.1.6 The for-as-hash subclause

(let ((hash-table (make-hash-table)))
  (setf (gethash "one" hash-table) 1)
  (setf (gethash "two" hash-table) 2)
  (setf (gethash "three" hash-table) 3)
  (iterate (for (k v) in-hashtable hash-table)
	   (collect v)))
;; => (1 2 3)
;; Note the mispelling, in in-hashtable rather than in-hash-table as
;; would be consistent with the rest of CL.


;;;6.1.2.1.7 The for-as-package subclause
(iterate (for s in-package :cl-user)
	 (count s))
;; => 1408
;; Of course the actual number computed will vary based on the
;; symbols you've imported into your cl-user package.


;;; 6.1.2.2 Local Variable Initializations

(iterate (with a = 1)
	 (with b = (+ a 2))
	 (with c = (+ b 3))
	 (return (list a b c)))
;; =>  (1 3 6)
;; Note, iterate doesn't have the equivelent of the 'and' clause (see
;; example below) and thus there is no mechanism for computing 'with'
;; values in parallel. See section 3.5 in the Iterate Manual for
;; details on why.

(loop with a = 1
   and b = 2
   and c = 3
   return (list a b c))
;; =>  (1 2 3)


;;;; 6.1.2.2.1 Examples of WITH clause

;; These bindings occur in parallel.
(setq a 5 b 10)

;; =>  10
(loop with a = 1
   and b = (+ a 2)
   and c = (+ b 3)
   return (list a b c))
;; =>  (1 7 13)
;; See above comment (6.1.2.2) re: with clause.

;; This example shows a way to declare local variables that are of
;; different types.
(iterate (with (a b c))
	 (declare (float a)
		  (integer b)
		  (float c))
	 (return (format nil "~A ~A ~A" a b c)))
;; =>  "0.0 0 0.0"

;; And of the same type, in the usual way
(iterate (with (a b c))
   (declare (float a b c))
   (return (format nil "~A ~A ~A" a b c)))
;; =>  "0.0 0.0 0.0"

;;; 6.1.3 Value Accumulation Clauses

;; Collect every name and the kids in one list by using
;; cons and appending.
(iterate (for name in '(fred sue alice joe june))
	 (for kids in '((bob ken) () () (kris sunshine) ()))
	 (appending (cons name kids)))
;; =>  (FRED BOB KEN SUE ALICE JOE KRIS SUNSHINE JUNE)

;;;; 6.1.3.1 Examples of COLLECT clause

;; Collect all the symbols in a list.
(iterate (for i in '(bird 3 4 turtle (1 . 4) horse cat))
	 (when (symbolp i)
	   (collect i)))
;; =>  (BIRD TURTLE HORSE CAT)

;; Collect and return odd numbers.
(iterate (for i from 1 to 10)
	 (if (oddp i)
	     (collect i)))
;; =>  (1 3 5 7 9)

;; Collect items into local variable, but don't return them.
(iterate (for i in '(a b c d) by #'cddr)
   (collect i into my-list)
   (finally (print my-list)))
;; >>  (A C)
;; =>  NIL

;;;; 6.1.3.2 Examples of APPEND and NCONC clauses

;; use appending to concatenate some sublists.
(iterate (for x in '((a) (b) ((c))))
   (appending x))
;; =>  (A B (C))

;; nconc some sublists together. Note that only lists made by the call
;; to list are modified.
(iterate (for i upfrom 0)
	 (as x in '(a b (c)))
	 (nconcing
	  (if (evenp i)
	      (list x)
	      nil)))
;; =>  (A (C))

;;;; 6.1.3.3 Examples of COUNT clause

(iterate (for i in '(a b nil c nil d e))
   (count i))
;; =>  5

;;;; 6.1.4.1 Examples of REPEAT clause

(iterate (repeat 3)
	 (format t "~&What I say three times is true.~%"))
;; >>  What I say three times is true.
;; >>  What I say three times is true.
;; >>  What I say three times is true.
;; =>  NIL
(iterate (repeat -15)
	 (format t "What you see is what you expect~%"))
;; =>  NIL

;;; 6.1.4.2 Examples of ALWAYS, NEVER, and THEREIS clauses

;; Make sure i is always less than 11 (two ways).
;; the for construct terminates these loops.
(iterate (for i from 0 to 10)
   (always (< i 11)))
;; =>  N

(iterate (for i from 0 to 20)
	 (always (< i 11)))
;; =>  NIL

(iterate (for i from 0 to 10)
   never (> i 11))
;; =>  T

(iterate (for i from 0 to 20)
   (never (> i 11)))
;; =>  NIL

;; If i exceeds 10 return i; otherwise, return nil.  the thereis
;; construct terminates this loop.
(iterate (for i from 0)
   (thereis (when (> i 10) i)))
;; =>  11

;; the finally clause is not evaluated in these examples.
(iterate (for i from 0 to 10)
   (always (< i 9))
   (finally (print "you won't see this")))
;; =>  NIL

(iterate (never t)
   (finally (print "you won't see this")))
;; =>  NIL

(iterate (thereis "Here is my value")
   (finally (print "you won't see this")))
;; =>  "Here is my value"

;; The for construct terminates this loop, so the finally clause
;; is evaluated.
(iterate (for i from 1 to 10)
   (thereis (> i 11))
   (finally (prin1 'got-here)))
;; >>  GOT-HERE
;; =>  NIL

;; If this code could be used to find a counterexample to Fermat's
;; last theorem, it would still not return the value of the
;; counterexample because all of the THEREIS clauses in this example
;; only return T.  But if Fermat is right, that won't matter
;; because this won't terminate.
(iterate (for z upfrom 2)
	 (thereis
	  (iterate (for n from 3 below (log z 2))
		   (thereis
		    (iterate (for x below z)
			     (thereis
			      (iterate (for y below z)
				       (thereis (= (+ (expt x n) (expt y n))
						   (expt z n))))))))))
;; <runs forever>

;;;; 6.1.4.3 Examples of WHILE and UNTIL clauses

(let ((hungry nil))
  (flet ((hungry-p ()
	   hungry)
	 (eat ()
	   (setf hungry t)))
    (iterate (while (hungry-p))
	     (eat))))
;; => NIL

;; until not is equivalent to while.
(let ((hungry nil))
  (flet ((hungry-p ()
	   hungry)
	 (eat ()
	   (setf hungry t)))
    (iterate (until (not (hungry-p)))
	     (eat))))
;; => NIL

;; Collect the length and the items of STACK.
(let ((stack '(a b c d e f)))
  (iterate (for item initially (length stack) then (pop stack))
	   (collect item)
	   (while stack)))
;; =>  (6 A B C D E F)
;; Note, the subtle distinction between loop's '=' and iterate's
;; 'initially' See Chapter 2 of the Iterate Manual for additional
;; details on the use of 'initially'

;; Use while to terminate a loop that otherwise wouldn't terminate.
;; note that while occurs after the when.
(iterate (for i from 3)
	 (declare (fixnum i))
	 (when (oddp i)
	   (collect i))
	 (while (< i 5)))
;; =>  (3 5)


;;;; 6.1.5.1 Examples of unconditional execution

;; Print numbers and their squares.
(iterate (for i from 1 to 3)
	 (print i)
	 (print (* i i)))
;; >>  1
;; >>  1
;; >>  2
;; >>  4
;; >>  3
;; >>  9
;; =>  NIL
;; Note, compare iterate's implicit 'progn' with loop's 'do' form.


;;; 6.1.6.1 Examples of when clause

;; Signal an exceptional condition.
(iterate (for item in '(1 2 3 a 4 5))
	 (when (not (numberp item))
	   (return (cerror "enter new value"
			   "non-numeric value: ~s" item))))
;; Error: non-numeric value: A

;; This example parses a simple printed string representation from
;; buffer (which is itself a string) and returns the index of the
;; closing double-quote character.
(let ((buffer "\"a\" \"b\""))
  (iterate (unless (char= (char buffer 0) #\")
	     (finish))
	   (for i from 1 below (length (the string buffer)))
	   (declare (fixnum i))
     (when (char= (char buffer i) #\")
       (return i))))
;; =>  2

;; The collected value is returned.
(iterate (for i from 1 to 10)
	 (when (> i 5)
	   (collect i))
	 (finally (prin1 'got-here)))
;; >>  GOT-HERE
;; =>  (6 7 8 9 10)

;; Return both the count of collected numbers and the numbers.
(iterate (for i from 1 to 10)
   (when (> i 5)
     (collect i into number-list)
     (count i into number-count)
     (finally (return (values number-count number-list)))))
;; =>  5
;;     (6 7 8 9 10)

;;;; 6.1.7.1 Control Transfer Clauses
;;; 6.1.7.1.1 Examples of named clause

;; Named block.
(iterate  max
	  (for i from 1 to 10)
	  (print i)
	  (return-from max 'done))
;; >>  1
;; =>  DONE

;;;; 6.1.7.2 Initial and Final Execution
;; <TODO>


;;;; 6.1.8 Examples of Miscellaneous Loop Features

(let ((i 0))
  (iterate (incf i)
	   (if (= i 3)
	       (return i))))
;; =>  3)

(let ((i 0)(j 0))
  (tagbody
     (iterate (incf j 3)
	      (incf i)
	      (if (= i 3)
		  (go exit)))
   exit)
  j)
;; =>  9

(iterate (for x from 1 to 10)
   (for y first nil then x)
   (collect (list x y)))
;; =>  ((1 NIL) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9) (10 10))

(iterate (for x from 1 to 10)
	 (for y initially nil then x)
	 (collect (list x y)))
;; =>  ((1 NIL) (2 1) (3 2) (4 3) (5 4) (6 5) (7 6) (8 7) (9 8) (10 9))

;;;; 6.1.8.1 Examples of clause grouping

;; Group conditional clauses.
(iterate (for i in '(1 324 2345 323 2 4 235 252))
	 (cond ((oddp i)
		(print i)
		(collect i into odd-numbers)
		(terpri))
	       (t
		(collect i into even-numbers)))
	 (finally
	  (return (values odd-numbers even-numbers))))
;; >>  1
;; >>
;; >>  2345
;; >>
;; >>  323
;; >>
;; >>  235
;; =>  (1 2345 323 235)
;;     (324 2 4 252)

;; Collect numbers larger than 3.
(iterate (for i in '(1 2 3 4 5 6))
	 (when (and (> i 3) i)
	   ;; it refers to (and (> i 3) i).
	   (collect i)))
;; =>  (4 5 6)

;; Find a number in a list.
(iterate (for i in '(1 2 3 4 5 6))
   (when (and (> i 3) i)
     (return i)))
;; =>  4

;; The above example is similar to the following one.
(iterate (for i in '(1 2 3 4 5 6))
   (thereis (and (> i 3) i)))
;; =>  4


;; Nest conditional clauses.
(let ((list '(0 3.0 apple 4 5 9.8 orange banana)))
  (iterate (for i in list)
     (if (numberp i)
       (if (floatp i)
	 (collect i into float-numbers)
	 (collect i into other-numbers))
       (if (symbolp i)
	   (collect i into symbol-list)
	   (error "found a funny value in list ~S, value ~S~%" list i)))
     (finally (return (values float-numbers other-numbers symbol-list)))))
;; =>  (3.0 9.8)
;;     (0 4 5)
;;     (APPLE ORANGE BANANA)

;; Without the end preposition, the last 'and' would apply to the
;; inner if rather than the outer one.
(iterate (for x from 0 to 3)
   (print x)
   (when (zerop (mod x 2))
     (princ " a")
     (when (zerop (floor x 2))
       (princ " b"))
     (princ " c")))
;; >>  0  a b c
;; >>  1
;; >>  2  a c
;; >>  3
;; =>  NIL
