



;;;; 6.1.2.1.1 The for-as-arithmetic subclause

(let ((x 1)) (loop for i from x by (incf x) to 10 collect i))
;; =>  (1 3 5 7 9)
(let ((x 1)) (loop for i by (incf x) from x to 10 collect i))
;; =>  (2 4 6 8 10)

;;; 6.1.2.1.1.1 Examples of for-as-arithmetic subclause

;; Print some numbers.
(loop for i from 1 to 3
   do (print i))
;; >>  1
;; >>  2
;; >>  3
;; =>  NIL

;; Print every third number.
(loop for i from 10 downto 1 by 3
   do (print i))
;; >>  10
;; >>  7
;; >>  4
;; >>  1
;; =>  NIL

;; Step incrementally from the default starting value.
(loop for i below 3
   do (print i))
;; >>  0
;; >>  1
;; >>  2
;; =>  NIL

;;; 6.1.2.1.2.1 Examples of for-as-in-list subclause

;; Print every item in a list.
(loop for item in '(1 2 3) do (print item))
;; >>  1
;; >>  2
;; >>  3
;; =>  NIL

;; Print every other item in a list.
(loop for item in '(1 2 3 4 5) by #'cddr
   do (print item))
;; >>  1
;; >>  3
;; >>  5
;; =>  NIL

;; Destructure a list, and sum the x values using fixnum arithmetic.
(loop for (item . x) of-type (t . fixnum) in '((A . 1) (B . 2) (C . 3))
   unless (eq item 'B) sum x)
;; =>  4


;;;; 6.1.2.1.3.1 Examples of for-as-on-list subclause

;; Collect successive tails of a list.
(loop for sublist on '(a b c d)
   collect sublist)
;; =>  ((A B C D) (B C D) (C D) (D))

;; Print a list by using destructuring with the loop keyword ON.
(loop for (item) on '(1 2 3)
   do (print item))
;; >>  1
;; >>  2
;; >>  3
;; =>  NIL


;;;; 6.1.2.1.4.1 Examples of for-as-equals-then subclause

;; Collect some numbers.
(loop for item = 1 then (+ item 10)
   for iteration from 1 to 5
   collect item)
;; =>  (1 11 21 31 41)


;;;; 6.1.2.1.5.1 Examples of for-as-across subclause

(loop for char across (the simple-string (find-message channel))
   do (write-char char stream))


;;; 6.1.2.1.6 The for-as-hash subclause
;; <TODO>


;;;6.1.2.1.7 The for-as-package subclause
;; <TODO>


;;; 6.1.2.2 Local Variable Initializations

(loop with a = 1
   with b = (+ a 2)
   with c = (+ b 3)
   return (list a b c))
;; =>  (1 3 6)

(block nil
  (let* ((a 1)
	 (b (+ a 2))
	 (c (+ b 3)))
    (tagbody
       (next-loop (return (list a b c))
		  (go next-loop)
		  end-loop))))

(loop with a = 1
   and b = 2
   and c = 3
   return (list a b c))
;; =>  (1 2 3)

(block nil
  (let ((a 1)
	(b 2)
	(c 3))
    (tagbody
       (next-loop (return (list a b c))
		  (go next-loop)
		  end-loop))))

;;;; 6.1.2.2.1 Examples of WITH clause

;; These bindings occur in sequence.
(loop with a = 1
   with b = (+ a 2)
   with c = (+ b 3)
   return (list a b c))
;; =>  (1 3 6)

;; These bindings occur in parallel.
(setq a 5 b 10)
;; =>  10
(loop with a = 1
   and b = (+ a 2)
   and c = (+ b 3)
   return (list a b c))
;; =>  (1 7 13)

;; This example shows a shorthand way to declare local variables
;; that are of different types.
(loop with (a b c) of-type (float integer float)
   return (format nil "~A ~A ~A" a b c))
;; =>  "0.0 0 0.0"

;; This example shows a shorthand way to declare local variables
;; that are the same type.
(loop with (a b c) of-type float
   return (format nil "~A ~A ~A" a b c))
;; =>  "0.0 0.0 0.0"

;;; 6.1.3 Value Accumulation Clauses

;; Collect every name and the kids in one list by using
;; COLLECT and APPEND.
(loop for name in '(fred sue alice joe june)
   for kids in '((bob ken) () () (kris sunshine) ())
   collect name
   append kids)
;; =>  (FRED BOB KEN SUE ALICE JOE KRIS SUNSHINE JUNE)

;;;; 6.1.3.1 Examples of COLLECT clause

;; Collect all the symbols in a list.
(loop for i in '(bird 3 4 turtle (1 . 4) horse cat)
   when (symbolp i) collect i)
;; =>  (BIRD TURTLE HORSE CAT)

;; Collect and return odd numbers.
(loop for i from 1 to 10
   if (oddp i) collect i)
;; =>  (1 3 5 7 9)

;; Collect items into local variable, but don't return them.
(loop for i in '(a b c d) by #'cddr
   collect i into my-list
   finally (print my-list))
;; >>  (A C)
;; =>  NIL

;;;; 6.1.3.2 Examples of APPEND and NCONC clauses

;; Use APPEND to concatenate some sublists.
(loop for x in '((a) (b) ((c)))
   append x)
;; =>  (A B (C))

;; NCONC some sublists together.  Note that only lists made by the
;; call to LIST are modified.
(loop for i upfrom 0
   as x in '(a b (c))
   nconc (if (evenp i) (list x) nil))
;; =>  (A (C))

;;;; 6.1.3.3 Examples of COUNT clause

(loop for i in '(a b nil c nil d e)
   count i)
;; =>  5

;;;; 6.1.4.1 Examples of REPEAT clause

(loop repeat 3
   do (format t "~&What I say three times is true.~%"))
;; >>  What I say three times is true.
;; >>  What I say three times is true.
;; >>  What I say three times is true.
;; =>  NIL
(loop repeat -15
   do (format t "What you see is what you expect~%"))
;; =>  NIL

;;; 6.1.4.2 Examples of ALWAYS, NEVER, and THEREIS clauses

;; Make sure I is always less than 11 (two ways).
;; The FOR construct terminates these loops.
(loop for i from 0 to 10
   always (< i 11))
;; =>  T
(loop for i from 0 to 10
   never (> i 11))
;; =>  T

;; If I exceeds 10 return I; otherwise, return NIL.
;; The THEREIS construct terminates this loop.
(loop for i from 0
   thereis (when (> i 10) i) )
;; =>  11

;;; The FINALLY clause is not evaluated in these examples.
(loop for i from 0 to 10
   always (< i 9)
   finally (print "you won't see this"))
;; =>  NIL
(loop never t
   finally (print "you won't see this"))
;; =>  NIL
(loop thereis "Here is my value"
   finally (print "you won't see this"))
;; =>  "Here is my value"

;; The FOR construct terminates this loop, so the FINALLY clause
;; is evaluated.
(loop for i from 1 to 10
   thereis (> i 11)
   finally (prin1 'got-here))
;; >>  GOT-HERE
;; =>  NIL

;; If this code could be used to find a counterexample to Fermat's
;; last theorem, it would still not return the value of the
;; counterexample because all of the THEREIS clauses in this example
;; only return T.  But if Fermat is right, that won't matter
;; because this won't terminate.

(loop for z upfrom 2
   thereis
     (loop for n upfrom 3 below (log z 2)
	thereis
	  (loop for x below z
	     thereis
	       (loop for y below z
		  thereis (= (+ (expt x n) (expt y n))
			     (expt z n))))))

;;;; 6.1.4.3 Examples of WHILE and UNTIL clauses

(loop while (hungry-p) do (eat))

;; UNTIL NOT is equivalent to WHILE.
(loop until (not (hungry-p)) do (eat))

;; Collect the length and the items of STACK.
(let ((stack '(a b c d e f)))
  (loop for item = (length stack) then (pop stack)
     collect item
     while stack))
;; =>  (6 A B C D E F)

;; Use WHILE to terminate a loop that otherwise wouldn't terminate.
;; Note that WHILE occurs after the WHEN.
(loop for i fixnum from 3
   when (oddp i) collect i
   while (< i 5))
;; =>  (3 5)


;;;; 6.1.5.1 Examples of unconditional execution

;; Print numbers and their squares.
;; The DO construct applies to multiple forms.
(loop for i from 1 to 3
   do (print i)
     (print (* i i)))
;; >>  1
;; >>  1
;; >>  2
;; >>  4
;; >>  3
;; >>  9
;; =>  NIL

;;; 6.1.6.1 Examples of when clause

;; Signal an exceptional condition.
(loop for item in '(1 2 3 a 4 5)
   when (not (numberp item))
   return (cerror "enter new value" "non-numeric value: ~s" item))
;; Error: non-numeric value: A

;; The previous example is equivalent to the following one.
(loop for item in '(1 2 3 a 4 5)
   when (not (numberp item))
   do (return
	(cerror "Enter new value" "non-numeric value: ~s" item)))
;; Error: non-numeric value: A

;; This example parses a simple printed string representation from
;; buffer (which is itself a string) and returns the index of the
;; closing double-quote character.
(let ((buffer "\"a\" \"b\""))
  (loop initially (unless (char= (char buffer 0) #\")
		    (loop-finish))
     for i of-type fixnum from 1 below (length (the string buffer))
     when (char= (char buffer i) #\")
     return i))
;; =>  2

;; The collected value is returned.
(loop for i from 1 to 10
   when (> i 5)
   collect i
   finally (prin1 'got-here))
;; >>  GOT-HERE
;; =>  (6 7 8 9 10)

;; Return both the count of collected numbers and the numbers.
(loop for i from 1 to 10
   when (> i 5)
   collect i into number-list
   and count i into number-count
   finally (return (values number-count number-list)))
;; =>  5, (6 7 8 9 10)

;;;; 6.1.7.1 Control Transfer Clauses
;;; 6.1.7.1.1 Examples of named clause

;; Just name and return.
(loop named max
   for i from 1 to 10
   do (print i)
   do (return-from max 'done))
;; >>  1
;; =>  DONE

;;;; 6.1.7.2 Initial and Final Execution
;; <TODO>

;;;; 6.1.8 Examples of Miscellaneous Loop Features

(let ((i 0))                     ; no loop keywords are used
  (loop (incf i) (if (= i 3) (return i)))) =>  3
(let ((i 0)(j 0))
  (tagbody
     (loop (incf j 3) (incf i) (if (= i 3) (go exit)))
   exit)
  j)
;; =>  9

(loop for x from 1 to 10
   for y = nil then x
   collect (list x y))
;; =>  ((1 NIL) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9) (10 10))

(loop for x from 1 to 10
   and y = nil then x
   collect (list x y))
;; =>  ((1 NIL) (2 1) (3 2) (4 3) (5 4) (6 5) (7 6) (8 7) (9 8) (10 9))

;;;; 6.1.8.1 Examples of clause grouping

;; Group conditional clauses.
(loop for i in '(1 324 2345 323 2 4 235 252)
   when (oddp i)
   do (print i)
   and collect i into odd-numbers
   and do (terpri)
   else                              ; I is even.
   collect i into even-numbers
   finally
     (return (values odd-numbers even-numbers)))
;; >>  1
;; >>
;; >>  2345
;; >>
;; >>  323
;; >>
;; >>  235
;; =>  (1 2345 323 235), (324 2 4 252)

;; Collect numbers larger than 3.
(loop for i in '(1 2 3 4 5 6)
   when (and (> i 3) i)
   ;; it refers to (and (> i 3) i).
   collect it)
;; =>  (4 5 6)

;; Find a number in a list.
(loop for i in '(1 2 3 4 5 6)
   when (and (> i 3) i)
   return it)
;; =>  4

;; The above example is similar to the following one.
(loop for i in '(1 2 3 4 5 6)
   thereis (and (> i 3) i))
;; =>  4


;; Nest conditional clauses.
(let ((list '(0 3.0 apple 4 5 9.8 orange banana)))
  (loop for i in list
     when (numberp i)
     when (floatp i)
     collect i into float-numbers
     else                                  ; Not (floatp i)
     collect i into other-numbers
     else                                    ; Not (numberp i)
     when (symbolp i)
     collect i into symbol-list
     else                                  ; Not (symbolp i)
     do (error "found a funny value in list ~S, value ~S~%" list i)
     finally (return (values float-numbers other-numbers symbol-list))))
;; =>  (3.0 9.8), (0 4 5), (APPLE ORANGE BANANA)

;; Without the end preposition, the last 'and' would apply to the
;; inner if rather than the outer one.
(loop for x from 0 to 3
   do (print x)
   if (zerop (mod x 2))
   do (princ " a")
   and if (zerop (floor x 2))
   do (princ " b")
   end
   and do (princ " c"))
;; >>  0  a b c
;; >>  1
;; >>  2  a c
;; >>  3
;; =>  NIL
