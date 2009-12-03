;; -*- auto-recompile: t -*-
;;; choose.el --- some pseudo-nondeterminism in elisp.
;; Time-stamp: <2009-04-01 15:28:10 deego>
;; Copyright (C) Deepak Goel 2001
;; Emacs Lisp Archive entry
;; Filename: choose.el
;; Package: choose
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 0.9dev


;; Author's homepage: http://www.glue.umd.edu/~deego
;; For latest version:

(defvar choose-home-page  
  "http://www.glue.umd.edu/~deego/emacspub/choose/")


 
;; This file is NOT (yet) part of GNU Emacs.
 
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 

;; See also:


;; Quick start:
(defvar choose-quick-start
  "Drop into your load-path and insert \(require 'choose\) in .emacs.

Type M-x choose-introduction for more."
)

(defun choose-quick-start ()
  "Provides electric help for function `choose-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert choose-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar choose-introduction
  "
choose.el provides (some) non-determinism in elisp.   Does anyone know if
such a thing exists already?

The use of this facility is best illustrated by the example-function
choose-find-mul.  Where you can accomplish the task without a single
loop, whereas otherwise you would need 9 nested loops (of course,
emacs will internally loop for you, sombody's gotta do it..).  For
more example(s), type M-x choose-example.

Inspiration from Paul Graham's book 'On Lisp'.  choose/fail
essentially allows the programmer to simply ask lisp to choose a
choice rather than looping over the possibilities.  And EMACS,
with its infinite foresight :), will always ensure that the
choice it made, was the right one, the one you wanted.  It never
takes the false path of (choose-fail). (Okay, there are a lot
of caveats to this, see commentary for more details..).

Conceptual and coding help with the TODO or any other help most
welcome. 

See choice-iterate for some code that goes beyond choice-with. 

"
)

(defun choose-introduction ()
  "Provides electric help for function `choose-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert choose-introduction) nil) "*doc*"))

;;; Commentary:
(defvar choose-commentary
  "Please first see M-x choose-quick-start and M-x choose-introduction.

It seems to me that any code that uses choose will run much faster if
compiled, since then the macro-expansions will already have been
done..  For an illustration, look at the time it takes to compile
choose-find-mul. 

Now: here are the caveats mentioned in the introduction..  Whereas if
you were using a language like scheme with an inbuilt support for
continutions, you can expect the wrong path to be (effectively) never
taken. In other languages, any decent choose/fail implementation would
atleast try to simulate continuations, as Graham does in his.  In
choose.el, we simply chicken out and not even simulate
continuations. All that happens is that the paths are iterated over
unless the right one is found. You had better keep track of any
important environment variables yourself..  Just think of choose as
asking emacs to iterate your code over all possibilities unless one
'goes through' without running into failures.

In essence, choose just saves you loops, and provides you an
arbitrarily invokable fail ability --- you can fail anywhere in the
program.  In particular, the body of your code will be evaluated once
for each possible combination until the right one is found.  You had
better not make any (careless) side-effects, unless you really want
them executed once for each (wrong) possibility as well.

Also note that there is an alternative syntax available for choose,
available by typing M-x `choose-toggle-syntax'.

Also see bugs. which also contains a TODO.  Any coding or conceptual
help is most welcome for the TODO. Feature-requests and suggestions are
also welcome. 


A Reminder: When a construct like (choice-with (setq var (choose '(1
2))) ... ) evaluates to 'choose-failed, that doesn't mean that var
also is setq'ed to choose-failed.  The var is simply setq'ed to the
last possible value before choice-with finally gave up.   That is why
you use choose-failed-p, see the code of choose-invertilble for an
example. 

Gotchas: 

 (1) Remember that the body of the code within a choice-* may be
 evall'd many times. Thus, don't rely on any side-effects from the
 code, ever.  Only rely on the return value from the code. Also, think
 carefully for any side-effect that may affect variables from outside
 the code. 


"
)

(defun choose-commentary ()
  "Provides electric help for function `choose-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert choose-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;; For a casual user (like myself), there are no bugs.. just follow
;; the examples, and the usage of choose should be perfectly
;; clear.. For an advanced user who is used to the true meaning of
;; nondeterminism, here are the bugs: (again, don't worry if u can't
;; parse some of what is written below)

;; There being no support for continuations in elisp (or even common
;; lisp), this is obviously a very incomplete implementation of
;; nondeterminism, and perhaps doesn't even do justice to the term.

;; This code assumes a pretty rigorous syntax for choose.  Thus, we
;; can forget about (apply 'choose (lists.. )), the only syntax is
;; (choose <stuff>). 
;; 
;; A little bit of a remedy to this situation is the provision of
;; choose-OLDER-n construct, which essentially allows you to do an
;; equivalent of (mapcar 'choose).. Moreover, the syntax for choose
;; and choose-OLDER-n is chosen such that you will never need (hopefully) to
;; use apply with choose...   of course, this is still not as general
;; as you would like..  when using choose-OLDER-n, the value of n should be
;; known before the wrapping choice-with.    This is a bit of a
;; drawback since you sometimes don't know the value of n deep into
;; the code.  But version 0.7 will seek to overcome this drawback as
;; follows:  whereas multiple nested choice-with's have always been
;; allowed, the inner ones really have no effect, for the outer ones
;; used to take care of all the choose's in between, but from now on,
;; the outer ones will leave any blobk of inner code beginning with
;; another choice-with alone.. thus, if you determine the value of n
;; deep inside a choice-with, all you gotta do is start another blovk
;; of choice-with.


;;; Note that making collections of all possible results is easy. 
;; After all tests have passed, simply add-to-list current-result
;; all-results, and then call choose-fail.



;;; New features:
(defvar choose-new-features
  "
0.5: not only do we have a choose-OLDER-n \(see 0.1\), but the program is way
way faster now..  the equivalent looping is more intuitive, and one of
the macros has now been converted to a function. 

0.1: has also implemented a choose-OLDER-n.
     Earlier, you had to know how many chooses you were going to use
     in the program.  So, you if the number of variables to be 'chosen'
     were to be determined at run-time, there was no way.

     Now, you can type use a choose-OLDER-n, which will create a list of n
     objects, each 'chosen'.  Moreover, the second element of the list
     can refer to the first, the third can refer to the first and
     second, and so on while choosing.

"
)

(defun choose-new-features ()
  "Provides electric help for function `choose-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert choose-new-features) nil) "*doc*"))

(defvar choose-version "0.9dev"

"
0.8: will provide a choice-with-cc which is an enhancement of choice-with..

0.5: will try to redefine choose-internal-with-choose so that the innermost
choose gets iterated over first..
0.4..")

;;==========================================
;;; Code:
(defvar choose-internal-inside-p nil
  "DO not change. Internal to choose.")

(defvar choose-before-load-hook)
(defvar choose-after-load-hook)
(run-hooks 'choose-before-load-hook)
(eval-when-compile (require 'cl))


(defvar choose-internal-catch-tag 'choose-internal-catch-tag
 "Internal.  Will be a symbol holding the current `choose-internal-catch-tag'."
)


(defun choose-fail ()
  "Use this inside programs to fail, see examples."
  (throw choose-internal-catch-tag choose-internal-fail-symbol)
  ;;choose-internal-fail-symbol
)


(defun choose-internal-throw-maybe (sym)
  (when 
      (choose-failed-p sym)
    (throw choose-internal-catch-tag choose-internal-fail-symbol))
  sym)

(defvar choose-internal-fail-symbol 'choose-failed)

(defvar choose-choose-symbol 'choose)


;;;###autoload
(defalias 'choose-randomize-list 'choose-permute)

;;;###autoload
(defun choose-permute (ls)
  "Randomize a list... So that (1 2 3) may return (2 3 1).
You may often want to use things like..
\(choice-with ... (choose (choose-permute ls)) .."
  (choose-internal-permute-1 (copy-list ls) (length ls)))

(defun choose-internal-permute-1 (ls n)
  
  ;; First select the 1st element.
  (if (<= n 1)
      ls
    (let* ((m (random n))
	   ;; prev cell.
	   cell1
	   elt
	   ;; wasteful.
	   ;;(e1 (nth m ls))
	   (n1 (- n 1)))
      (cond 
       ((= m 0) (setq elt (car ls) ls (cdr ls)))
       (t
	(setq cell1 (nthcdr (- m 1) ls) elt (cadr cell1))
	(setcdr cell1 (cddr cell1))))
      (cons elt (choose-internal-permute-1 ls n1)))))
  

;; Note that Everything beginning with choice- is special .. in the
;; way that macros expand.
(defmacro choice-with-thistimeout (secs &rest body)
  "secs is an unquoted form."
  `(with-timeout
       (,secs
	;; Send a failure to our parent as necc.
	(choice-with (choose-fail)))
     (choice-with 
      (sit-for 0)
      ;; In case body was empty, continue to return nil...
      nil
      ,@body)))






;; Note that Everything beginning with choice- is special .. in the
;; way that macros expand.
(defmacro choice-with-thistimeout-scored (secs score &rest body)
  "Take as much time as we need to within SECS. Then, return the
result of the body for the choices corresponding to the largest
score. Score should be a number.

Score is an unquoted form that is evaluated at the END of computation
for each BODY, with body's own result being returned.  This seems
buggy atm, in that score does not seem to be maximized."
  (let ((acc (gensym "--choose-accumulator--"))
	(nbest (gensym "--choose-acc--"))
	;;(anext (gensym "--choose-acc--"))
	(res (gensym "--choose-acc--")))
    `(let ((,acc  nil))
       (choice-with-thistimeout ,secs 
				  (push 
				   (list (progn ,@body) ,score)
				   ,acc)
				  (choose-fail))
       (if (null ,acc)
	   (choice-with (choose-fail))
	 (setq ,nbest (second (first ,acc)) ,res (caar ,acc)
	       ,acc (cdr ,acc))
	 (while ,acc
	   (if (> (second (first ,acc)) ,nbest)
	       (setq ,nbest (second (first ,acc)) ,res (caar ,acc)))
	   (setq ,acc (cdr ,acc)))
	 ,res))))



(defmacro choice-with-timeout-scored (score &rest body)
  "This will probbaly be the main AI function used by the user. 
This probably works best when each choice-with has at most one choose in it.."
  `(choice-with-thistimeout-scored
    choose-seconds
    ,score
    (let 
	((choose-seconds (* choose-seconds
			     choose-seconds-modifier)))
	,@body)))


  





(defvar choose-seconds 240
  "The main timeout should finish within these many seconds.
Feel free to modify this variable in your AIish programs.")

(defvar choose-seconds-modifier 0.6)

(defmacro choice-with-timeout (&rest body)
  "See also choose-with-thistimout.
Finish the body within a certain time.  Do this by ensuring that
our children inherit a lower timeout.

This design is very interesting.  This will ensure, for example, that
a top-level choice-with finishes in 6mins. It may have 3 children
choice-withs, each of which then has a 4-min upper limit.  Each
children may further children, and so on.

Essentially, at any level, we won't got too bogged down if we don't
find an answer. This should also prevent infinite looping in
circular-tree problems..
something is funny.. see choose-example-AI-100-398.
"
  (let ((ccym (gensym "--choose-timeout--")))
    `(let ((ccym ,choose-seconds))
       (choice-with-thistimeout
	choose-seconds
	(let 
	    ;; Note that this will get repeated many times..
	    ;; therefore.. be careful.. 
	    ((choose-seconds (* ccym
				choose-seconds-modifier)))
	  ,@body)))))



;;;###autoload
(defmacro choice-with (&rest body)
  "All code expecting to use choose should begin with this, see examples.
BODY is your code.



Note in particular the syntax for choose-OLDER-n:

\(choose-OLDER-n 'n 'var-name  \(list of n lists\) &optional initial\)
'n is an expression that should evaluate to a number.  This expression
will be evaluate exactly once, in the beginning.

The var-name, itself can be accessed within the list of n lists.


Initial is the initial list that var should be bound to.  If INITIAL
is not a list or is not n element long, then we shall assume that
INITIAL refers to each element of the list, and will create a n-long
list by repeating initial n times.


Also note that the way we have implemented choose, the first
encountered choose is the one that runs through all possibilities
before a different possibility is considered for the second choose..

The body of choice-with gets evaluated as many times as needed, until
everything works.  The body following each (choose... ) gets evaluated
as many times as needed until everything works.


Aother way is to invoke choose-n instead of choose.  There,
given a list of lists, we shall return a list of choices, one
from each list.

Note, code such as 
 (choice-with (choose b ...)
              (choose c (list b b)))
will not work as expected, because c is not aware of b. A subsequent
 choose within a choice-with cannot depend on a previous choose's
 value.

The workaround is simple:


 (choice-with (choose b ...)
              (choice-with (choose c (list b b ))))

In other words, when in doubt, always surround a CHOOSE with a choice-with.

If we detect a choice-unwith, we will attempt to stop any further
substitutions, and treat that choice-unwith as if it was a progn.


We ourselves throw the tag to our own parent if one exists.  In
other words, this will not return t, rather, a choose-fail:
 (choice-with (symbolp (choice-with (choose-fail))))

"
  ;;`(eval (choose-internal-with-choose (choose-internal-with-choose-OLDER-n '(progn ,@body)))))
  (let ((ccsym (gensym "--choose-internal--")))
    `(let ((,ccsym
	    ,(choose-internal-with-choose 
	      (choose-internal-with-choose-n
	       ;;(choose-internal-with-choose-OLDER-n `(progn ,@body)))))
	       `(progn ,@body)))))
       ;; If failed, attempt to throw a tag to our parent.  Instead of
       ;; using the condition-case hack, we could also have done this:
       ;; We could have established whether we are in a choose-catch,
       ;; whenever we call catch via a variable such as in-choose-p. In
       ;; other words, use a new macro for choose-catch
       ;; The above is our older way of throwing tag.
       ;; The better method is to use choose-internal-inside-p.
       ;; (if 
       ;; 	   (choose-failed-p ,ccsym)
       ;; 	   (condition-case 
       ;; 	       ccsym2 (throw choose-internal-catch-tag choose-internal-fail-symbol)
       ;; 	     (error choose-internal-fail-symbol))
       ;; 	 ,ccsym))))
       (when (and (choose-failed-p ,ccsym)
		  (choose-internal-inside-p))
	 (throw choose-internal-catch-tag choose-internal-fail-symbol))
       ,ccsym)))

  ;;`(choose-internal-with-choose (progn ,@body)))


(defun choose-internal-with-choose-n-possibilities (expr)
  "Return a list of possibilities that we will evaluate."
  (if expr
      (choose-internal-with-choose-n-possibilities-1 expr (list nil))
    nil))

(defun choose-internal-with-choose-n-possibilities-1 (expr lastlist)
  "Return a list of possibilities that we will evaluate."
  (if expr
      (choose-internal-with-choose-n-possibilities-1
       (butlast expr)
       (let ((en (car (last expr))) (ls nil))
	 (dolist (ee en)
	   (dolist (ll lastlist)
	     (push (cons ee ll) ls)))
	 (reverse ls)))
      lastlist))


(defun choose-internal-with-choose (expr)
  "Internal. Just transforms the expression into something else..
Is called from `choice-with', EXPR is the expression which gets
formatted.  


"
  (let* ((replace (gensym "--choose--"))
	 (replaced-return (choose-internal-replace-choose
			   expr replace)))
    (if replaced-return
	`(choose-keep-trying
	  (lambda (,replace)
	    ,(choose-internal-with-choose (first replaced-return)))
	  ,(cadr (second replaced-return))
	  )


      ;; we should still invoke choose-keep-trying, just in case
      ;; someone throws us a tag back.  Else, something like
      ;; (choice-with (choose-fail)) yields an error, which can lead
      ;; to further problems with, for example, choice-iterate.
      ;;expr))
      `(choose-keep-trying 
	(lambda (,replace)
	  ,expr)
	(list nil)))))
      



(defun choose-internal-with-choose-n (expr)
  "Internal. Just transforms the expression into something else..
Is called from `choice-with', EXPR is the expression which gets
formatted.  


"
  (let* ((replace (gensym "--choose--"))
	 (replaced-return (choose-internal-replace-choose-n
			   expr replace)))
    (if replaced-return
	`(choose-keep-trying
	  (lambda (,replace)
	    ,(choose-internal-with-choose-n (first replaced-return)))
	  (choose-internal-with-choose-n-possibilities
	   ,(cadr (second replaced-return)))
	  )


      ;; we should still invoke choose-keep-trying, just in case
      ;; someone throws us a tag back.  Else, something like
      ;; (choice-with (choose-fail)) yields an error, which can lead
      ;; to further problems with, for example, choice-iterate.
      ;;expr))
      `(choose-keep-trying 
	(lambda (,replace)
	  ,expr)
	(list nil)))))
      





(defmacro choose-catch (&rest body)
  `(let ((choose-internal-inside-p t))
     (catch ,@body)))

(defun choose-keep-trying (fcn args)
  "choice-with macroexpands to this.
FCN  accepts only  one arg..ARGS is a list of arguments.
keeps  trying until  one arg  doesn't
fail.. else  fails.. args be a list  of args.. the arg  is actually an
expression which will be evaled.
Thus (let ((x 1)) (choose (+ x 2) (+ x 3)))
will  work  because  the  expression  '(+  x  2)  will  be  passed  to
`choose-keep-trying' and will be evaled at run-time.."
  (if (null args)
      ;; not only should we simply return the failed symbol, but throw the
      ;; tag back to our OWN parent if any. Which would be useful if
      ;; this choice-with was a child of another choice-with..
      ;; We could attempt a ignore-errors throw-tag here, but we shall
      ;; avoid that, to provide maximum flexibility to the user. 
      choose-internal-fail-symbol
    (let* ((choose-internal-catch-tag (gensym "choose-catch"))
	   (try (choose-catch choose-internal-catch-tag (funcall fcn (car args)))))
      (if (choose-failed-p try)
	  (choose-keep-trying fcn (cdr args))
	try))))


(defun choose-internal-replace-choose (expr replace)
  "Internal.
replaces one (choose... ) or (chooser) in an EXPR.  REPLACE is
what gets inserted there.."
  (let (
	newlist-res sec)
    (setq newlist-res
	  (choose-internal-replace-one-if 
	   ;; ok to replace if choose
	   #'(lambda (arg) 
	       (and (listp arg)
		  (or 
		   (equal (car arg) 'choose)
		   ;; Bad: This will get evalled many times in the
		   ;; tree function. 
		   ;;(and (equal (car arg) 'chooser) (setq randomp t) t))))
		   (equal (car arg) 'chooser))))
	   
	   ;; supply the expr.
	   expr 
	   ;; cause the replacement.
	   (lambda (arg) replace
	     )
	   ;; this repl was meant to not descend over our children
	   ;; choice-with's. Let's comment it replace choice-with with
	   ;; choice-unwith
	   ;; supply the exception.

	   #'(lambda (arg)
	     (and (listp arg)
		  ;;(equal (car arg) 'choice-with)))
		  (string-match
		   "^choice-.*" (format "%s" (car arg)))))))
    (setq sec (second newlist-res))
    (if (eq (car sec) 'chooser)
	(list (first newlist-res)
	      (list (first sec) 
		    (cons 'choose-permute
			  (cdr sec))))
      newlist-res)))





(defun choose-internal-replace-choose-n (expr replace)
  "Internal.
replaces one (choose... ) in an EXPR.  REPLACE is what gets inserted there.."
  (choose-internal-replace-one-if (lambda (arg) (and (listp arg)
					(equal (car arg) 'choose-n)))
		     expr 
		     (lambda (arg) replace
		       )
		     (lambda (arg)
		       (and (listp arg)
			    (equal (car arg) 'choice-unwith)))
		     ))





(defun choose-internal-replace-one-if (pred tree repl-pred &optional cont-pred)
  "A general lisp function. 
Returns a new TREE with replaced elements..
The new  tree has  the first element  that satisfies PRED  replaced by
the funcall of repl-pred on that element.

This  function  returns  \(NEWLIST  REPLACED\).  Where  NEWLIST  is  the
replaced list.   REPLACED is  the first element  of the LIST  that got
replaced.

Note carefully,  that when  this function is  being applied,  it tests
each  element \(the tree  as well  as all  of its  sublists as  well as
atoms\) to see if they satisfy the pred..

If could not find any match, returns nil.

If cont-pred is supplied, the element satisfying this pred is never
descended further...


"
  (if (null cont-pred)
      (setq cont-pred 
	    (lambda (arg) nil)))
  (if (funcall pred (copy-tree tree))
      (list (funcall repl-pred (copy-tree tree))
	    (copy-tree tree))
    (if 
	(or (not (listp tree)) (funcall  cont-pred tree))
	nil
      (let*
	  ((subreplaceds
	    (mapcar (lambda (arg)
		      (choose-internal-replace-one-if
		       pred arg repl-pred cont-pred))
		    tree))
	   (index
	    (position-if (lambda (arg)
			   arg)
			 subreplaceds)))
	(choose-ifn
	 index 
	 ;; if no match, please return nil
	 nil
	 (let ((sub (nth index subreplaceds)))
	   (list
	    (append (subseq tree  0 index)
		    (list (car sub))
		    (subseq tree (+ index 1) (length tree)))
	    (second sub))))))))


  
(defmacro choose-ifn (a b c)
  "A general utility. Like if but reverse.  A B C."
  `(if ,a ,c ,b))

(defun choose-failed-p (arg)
  "A good way to test if something resulted in choose-failure.
Preferably use this rather than testing yourself.  I don't know why.
Tells if ARG is same as 'choose-failed-symbol."

  (equal arg choose-internal-fail-symbol))



  

(defun choose-OLDER-n (varq ls &optional initlist nolet-p
			     index)
  " Behaves differently whether used inside or outside choice-with.
The intended use is inside choice-with.

This shall return an equivalent choose-expression..

varq is a
quoted variable.  ls is a list of sublists.  Choose will choose one
item from each of those sublists.  initlist is an optional list
\(ideally of the same length as ls.  If supplied, this is what varq
gets initially assigned to.  varq is normally initialized using a let.
But if nolet-p is true, then it is not.  \(used in recursive
self-calls\).  Similarly, if for some reason you want only the 5th
through 7th entries of varq to be 'chosen', then make the initlist 7
elements long, make the ls only 3 elements long.  And make the
index 4.  Thus, index is the starting index for which we will start
doing \(set \(nth index varq\) \(choose....\)\).  Again, this is
useful because called by the function for self. 

Thus, remember that each member of ls is itself a list of expressions..
Note that choose-OLDER-n will go and set the variable you specify to the
chosen values.  If you intended to do that only locally, you will have
to use your own let wrapper around choose-OLDER-n.




"
  (if (null index) 
      (setq index 0))
  (if (null initlist)
      (setq initlist 
	    (make-list (length ls) choose-internal-fail-symbol)))
  (choose-ifn 
   nolet-p
   `(progn 
      (setq ,varq ',initlist)
      ,(choose-OLDER-n varq ls initlist t index))
   (if (>= index (length ls))
       nil
     `(progn
	;; This one is to prevent unintended consequences for the sick
	;; few :-)  who use choose-OLDER-n to loop rather than to choose.
	(setf ,varq (copy-tree ,varq))
	(setf (nth ,index ,varq) (choose ',(nth index ls)))
	,(choose-OLDER-n varq ls (cdr initlist) t (+ index 1))))))


(defun choose-internal-replace-if (pred tree repl-pred &optional continue-p
breadth-first-p)
  "Replaces all matches in the tree that satisfy pred..returns the
replaced-tree.. see also the doc. of choose-internal-replace-one-if..  returns
the original tree if no match could be found.   If not-root is true,
the immediate root is not tested against pred. 

If continue-after is non-nil, the choose-internal-replace-if keeps calling
on even on the replaced elements of the list, until such time as pred
returns nil on the list..   Otherwise, (default), choose-internal-replace-if is
not called on the replaced elements..
 "
  (if breadth-first-p
      (choose-internal-replace-if-breadth-first 
       pred tree repl-pred continue-p)
    (choose-internal-replace-if-depth-first pred tree repl-pred continue-p)))

(defun choose-internal-replace-if-depth-first 
  (pred tree repl-pred &optional continue-p)
  "This one not tested..  

for continue-p:
The logic: if a list, we apply to all the sub-elements, until pred
fails on all of them.. once pred fails on all of them, we finally
apply it to the tree itself.  Again, if the pred succeeds on the tree,
call the function again, until it stops succeeding.
For not continue-p: call it exactly once on the chidren, and then call
it exactly once on the tree. 

"
  (let* 
      ((new-tree
	(if (listp tree)
	    (mapcar
	     (lambda (arg)
	       (choose-internal-replace-if-depth-first pred arg repl-pred
					      continue-p))
	     tree)
	  tree))
       (foundp
	(funcall pred new-tree)))
    (if foundp
	(if continue-p
	    (choose-internal-replace-if-depth-first
	     pred (funcall repl-pred new-tree)
	     repl-pred continue-p)
	  (funcall repl-pred new-tree))
      new-tree)))
  
(defun choose-internal-replace-if-breadth-first
  (pred tree repl-pred &optional continue-p)
"logic:

if not continue-p, we simply apply it once to the parent, if succeeds,
return, apply it to children.  

If continue-p, we will apply it first to the parent, if succeeds, call
it again.  If does not succeed on parent, start applying to children.
If any one child succeeds, at any level, we immediately abort and start with
the highest parent once again!  Note that applying it to the first
child also implies that if it does not succeed on the first child, it
is tried on the first grandchild.  And this happens before it is
applied to the second child... in other words, though we are assured
that children process after parents, they *can* get processed before
uncles.  In that sence, is not a true breadth-first. 

"

 (if continue-p 
     (choose-internal-replace-if-breadth-first-hairy
      pred tree repl-pred)
   (choose-internal-replace-if-breadth-first-simple
    pred tree repl-pred)))

(defun choose-internal-replace-if-breadth-first-simple
  (pred tree repl-pred)
  "Internal.."
  (if (funcall pred tree)
      (funcall repl-pred tree)
    (if (listp tree)
	(mapcar
	 (lambda (arg)
	   (choose-internal-replace-if-breadth-first-simple
	    pred arg repl-pred))
	 tree)
      tree)))

(defun choose-internal-replace-if-breadth-first-hairy
  (pred tree repl-pred)
  "Is internal.
If pred on tree, replace and apply again.
If not, try descendents one by one.. as soon as any one descendent
succeeds, replace it by that value and start again with the
tree.. Since descendent can be arbitrarily deep, you see that this can
be hairy.. 
"
  (let* ((first-res
	  (choose-internal-replace-if-breadth-first-hairy-once
	   pred tree repl-pred)))
    (if (second first-res)
	(choose-internal-replace-if-breadth-first-hairy
	 pred (car first-res) repl-pred)
      tree)))

(defun choose-internal-replace-if-breadth-first-hairy-once 
  (pred tree repl-pred)
  "internal"
  (if (funcall pred tree)
      (list (funcall repl-pred tree) t)
    (if (not (listp tree))
	(list tree nil)
      ;; tree is a list..
      (let 
	  ((foundp (list nil nil))
	   (n (length tree))
	   (index 0))
	(while (and (< index n)
		    (not (second foundp)))
	  (setq index (+ index 1))
	  (setq foundp (choose-internal-replace-if-breadth-first-hairy-once
			pred (nth (- index 1) tree) repl-pred)))
	(if (not (second foundp))
	    (list tree nil)
	  (list
	   (append
	    (subseq tree 0 (- index 1))
	    
	    ;;(list (elt tree (- index 1)))
	    (list (first foundp))
	    (subseq tree index n)) 
	   t))))))
	   

(defun choose-internal-replace-continue-if-breadth-first 
  (pred tree repl-cont-pred)
  "Even if cont-pred always returns t, this still not  same as
choose-internal-replace-id-breadth-first!  In the sense, that once a node
fails on pred, it is *never* applied to that node again, even if the
cont-pred is true or the children satisfy pred.   

Logic: this first considers the tree... if satisfies pred, will be
replaced.. if cont is found to t, the children will be considered for
replacement else not.. and so on..  The tree itself will *never* be
considered again....    In the above, cont is determined as the second
element of the list returned by applying repl-cont-pred to tree.  The
replacement value for the tree is the first argument of the list
returned by the application of repl-cont-pred on tree. 

"
  (if (funcall pred tree)
      (let* ((val (funcall repl-cont-pred tree))
	     (contp (second val))
	     (new-tree (first val)))
	(if (and contp (listp tree))
	    (mapcar
	     (lambda (arg)
	       (choose-internal-replace-continue-if-breadth-first
		pred arg repl-cont-pred))
	     new-tree)
	  new-tree))
    (if (listp tree)
	(mapcar 
	 (lambda (arg)
	   (choose-internal-replace-continue-if-breadth-first
	    pred arg repl-cont-pred))
	 tree)
      tree)))

   
(defun choose-internal-replace-continue-if (&rest args)
  (apply 'choose-internal-replace-continue-if-breadth-first args))




(defmacro choice-iterate (&rest body)
  "Rather than return the result of the first successful body, iterate
  over all possibilities.  For successful cases, store body. At the
  end, return a list of all such successful bodies. This can be used
  as an alternative to choice-with.

Note that if there's another choice-with within the body of this
macro, this macro will NOT see the choices for that choice-with, and
will simply assume that that one returns just one result.

See also choice-with-iterate
"
  (let ((acc (gensym "--choose-accumulator--")))
    `(let ((,acc nil))
       (choice-with 
	(push 
	 (progn ,@body)
	 ,acc)
	(choose-fail))
       (reverse ,acc))))


(defmacro choice-with-iterate (&rest body)
  "This macro is equivalent to choice-with at all times, except when
this is itself the top-level choose invoker.  In that case, this is
equivalent to choice-iterate. "
  `(if 
      (choose-internal-inside-p)
       (choice-with ,@body)
     (choice-iterate ,@body)))


(defun choose-internal-inside-p ()
  choose-internal-inside-p)


(defmacro choice-unwith (&rest body)
  "This function is provided only as a convenience to the user if they want to
 stop our continuous throwing for some reason. We normally
 don't envision a use for this. 

To take an example, consider
  (choice-with (symbolp (choice-unwith (choose-fail))))
If you use progn instead of choice-unwith, you will obtain a fail. In
 the current case, you simply obtain t.
"

  ;; We need to catch any tags emanating from our body and not
  ;; throw them any further.
  ;;(let ((cusym (gensym "--choose-internal--")))
  `(catch choose-internal-catch-tag 
     (progn
       ,@body)))





(defun choose-nil-if-failed (arg)
  (if (choose-failed-p arg)
      nil
    arg))


;;;====================================================
;; The rest of this file is some examples.

;;;###autoload
(defun choose-example ()
  "Run this."
  (interactive)
  (message "Try: various examples below."))


;; Why is this broken? 
;;;###autoload
(defun choose-example-find-mul-broken ()
  "The problem is to choose 3 3-digit numbers.  Their sum should be
divisible by 7 and 13.  Each of the numbers should be progressive.
Let's define a progressive number as one in which every digit is
either 1+ or twice the digit to the right of it.  Thus, 632 is a
progressive number..  now imagine writing this program without using
choose.. the 9 nested loops you would need.. and now look at the
0-loop program below...  There, you just ask lisp to choose.. choose
wisely, that is. With foresight, lisp chooses such that you will never
encounter choose-fail.

Thus, we have 3 progressive numbers, added ...
 a b c
 d e f
+g h i
=====
<multiple of 7 and 13>
=====

this example also illustrates that expressions can
be used inside choose's..  "
  (interactive)
  (message "hang on for 2 minutes..")
  (choice-with
   (let* (
	  (c (choose '(0 1 2 3 4 5 6 7 8 9)))
	  (f (choose '(0 1 2 3 4 5 6 7 8 9)))
	  (i (choose '(0 1 2 3 4 5 6 7 8 9)))
	  (b (choose '((* c 2) (+ c 1))))
	  (a (choose '((* b 2) (+ b 1))))
	  (e (choose '((* f 2) (+ f 1))))
	  (d (choose '((* e 2) (+ e 1))))
	  (h (choose '((* i 2) (+ i 1))))
	  (g (choose '((* h 2) (+ h 1)))))
     (if (or (> a 9) (> d 9) (> g 9) (= a 0) (= d 0) (= g 0))
	 (choose-fail))
     (message "Trying a:%S, b:%S, c:%S, d:%S, e:%S, f:%S, g:%S, h:%s i:%s"
	      a b c d e f g h i)
     (let ((sum
	    (+ (* 100 (+ a d g))
	       (* 10 (+ b e h))
	       (+ c f i))))
       (if (and (zerop (% sum 7)) (zerop (% sum 13)))
	   (message
	    (concat 
	     "Progressives %s, %s and %s sum to %s "
	     "which is divisible by 7 and 13")
	    (+ (* 100 a) (* 10 b) c)
	    (+ (* 100 d) (* 10 e) f)
	    (+ (* 100 g) (* 10 h) i)
	    sum)
	 (choose-fail)))))
  (sit-for 5)
  (message "Done.."))


(defun choose-example-find-mul ()
  "The problem is to choose 3 3-digit numbers.  Their sum should be
divisible by 7 and 13.  Each of the numbers should be progressive.
Let's define a progressive number as one in which every digit is
either 1+ or twice the digit to the right of it.  Thus, 632 is a
progressive number..  now imagine writing this program without using
choose.. the 9 nested loops you would need.. and now look at the
0-loop program below...  There, you just ask lisp to choose.. choose
wisely, that is. With foresight, lisp chooses such that you will never
encounter choose-fail.

Thus, we have 3 progressive numbers, added ...
 a b c
 d e f
+g h i
=====
<multiple of 7 and 13>
=====

this example also illustrates that expressions can
be used inside choose's..  "
  (interactive)
  (message "hang on for 2 minutes..")
  (choice-with
   (let* (
	  (c (choose '(0 1 2 3 4 5 6 7 8 9)))
	  (f (choose '(0 1 2 3 4 5 6 7 8 9)))
	  (i (choose '(0 1 2 3 4 5 6 7 8 9)))
	  b
	  a
	  e
	  d
	  h
	  g)
     (choice-with
      (setq b (choose (list (* c 2) (+ c 1)))
	    e (choose (list (* f 2) (+ f 1)))
	    h (choose (list (* i 2) (+ i 1))))
      (choice-with
       (setq a (choose (list (* b 2) (+ b 1)))
	     d (choose (list (* e 2) (+ e 1)))
	     g (choose (list (* h 2) (* h 1))))
       (if (or (> a 9) (> d 9) (> g 9) (= a 0) (= d 0) (= g 0))
	   (choose-fail))
       (message "Trying %S%S%S, %S%S%S, %S%s%s"
		a b c d e f g h i)
       (let ((sum
	      (+ (* 100 (+ a d g))
		 (* 10 (+ b e h))
		 (+ c f i))))
	 (if (and (zerop (% sum 7)) (zerop (% sum 13)))
	     (progn
	       (setq debugchoose 1)
	       (message
		(concat 
		 "Progressives %s, %s and %s sum to %s "
		 "which is divisible by 7 and 13")
		(+ (* 100 a) (* 10 b) c)
		(+ (* 100 d) (* 10 e) f)
		(+ (* 100 g) (* 10 h) i)
		sum))
	   (choose-fail)))))
     (sit-for 5)
     (message "Done.."))))
  
  
;;;###autoload
(defun choose-example-invertible-4 ()
  "Get a 4-digit number which should be a factor of its reverse..
Defining this function using loops might be more cumbersome.. (unless
you simply start with 1001 and use incf.. which is really a trick in
the current context..)"
  (interactive)
  (message "Hang on for a minute..")
  (choice-with
   (let (
	 (a (choose '(1 2 3 4 5 6 7 8 9)))
	 (b (choose '(0 1 2 3 4 5 6 7 8 9)))
	 (c (choose '(0 1 2 3 4 5 6 7 8 9)))
	 (d (choose '(0 1 2 3 4 5 6 7 8 9))))
     (message "Trying %S %S %S %S " a b c d)
     (let ((number
	    (+ d (* 10 c) (* 100 b) (* 1000 a)))
	   (revnum
	    (+ a (* 10 b) (* 100 c) (* 1000 d))))
       (if (= number revnum) (choose-fail))
       (if (= (% revnum number) 0)
	   (message "%S divides its reverse" number)
	 (choose-fail)))))
  (sit-for 5)
  (message "Done"))



(defun choose-example-maze-solve ()
  "Still in progress.."
  (interactive)
  (setq choose-good-syntax-p t)
  (require 'maze)
  (call-interactively 'maze)
  (message "waiting for 3 seconds...")
  (sit-for 3)
  (while
      (not (maze-check-win
	    (let ((choices (choose-maze-get-choices)))
	      (funcall (choose choices)))))))















(defun choose-example-nogrouping-3-numbers ()
  "Attempts to find 3 good numbers. No subgroup of these three numbers
  should sum up to the result of any other subgroup. Broken." 
  (interactive)
  (require 'oct)
  (require 'utils4)
  (let* ((nmax 20)
	 (biglist (oct-colon 1 nmax))
	 a b c d e result)
    (setq result
	  (choice-with
	   (setq a (choose biglist))
	   (setq b (choose biglist))
	   (setq c (choose biglist))
	   (unless
	       (and (< a b ) (< b c))
	     (choose-fail))
	   (when (choose-example-nogrouping-find-relation
		  (list a b c))
	     (choose-fail))))
    (message "%s"
	     (if (choose-failed-p result) result
	       (list a b c)))))


(defun choose-example-nogrouping-find-relation (nums)
  "Given a bunch of distinct numbers in a list, finds if the sum
  of any bunch of them equals that of any other bunch."

  (require 'utils4)
  (require 'oct)
  (let* ((pset (utils-powerset nums))
	 set1 set2 result)
    (setq 
     result
     (choice-with
      (setq set1 (choose pset))
      (setq set2 (choose pset))
      (when (or (null set1) (null set2)
		(intersection set1 set2)
		(not
		 (=
		  (apply #'+ set1)
		  (apply #'+ set2))))
	(choose-fail))
      (list set1 set2)))
    (if (choose-failed-p result) nil result)))



(defun choose-example-invertible (&optional k)
  "Works just fine now.."
  ;;(interactive)
  (if (or (null k) (< k 2))
      (setq k 2))
  (message "Trying k= %s" k)
  (if (choose-failed-p
       (choice-with
	(let (num number reverse)
	  (choose-k 'num (cons '(1 2 3 4 5 6 7 8 9)
			       (make-list (- k 1)
					  '(0 1 2 3 4 5 6 7 8 9))))
	  (setq number (choose-make-number num)
		reverse (choose-make-number (reverse num)))
	  (message "Considering %s" number)
	  (when (= number reverse)
	    (message "number %S equals its reverse, failing.." number)
	    (choose-fail))
	  (if (zerop
	       (% reverse number))
	      (progn
		(message
		 "Number %s divides its reverse %S" number reverse)
		(sit-for 5))
	    (choose-fail)
	    ))))
      (choose-invertible (+ k 1))))

(defun choose-example-nongrouping-n-numbers-givenmax (m mmax)
  "UNIMPLEMENTED. Attempt to find m numbers, each <= mmax, such that no subgroup among
  these m numbers sums up to any other subgroup.  

There was a new scientist 'enigma' problem related to this.
"
  (let*
      ((choices (oct-colon 1 mmax))
       (possibilities nil)
       result numbers ls2 ls3)
    (setq 
     result 
     (choice-with
      (choose-OLDER-n 'numbers 
		(make-list m choices))
      (setq ls2 (sort (remove-duplicates (copy-tree numbers)) #'<))
      ;; ensure increasing order and uniqueness
      (unless (equalp ls2 numbers)
	(choose-fail))
      
      
      (if (choose-nogrouping-find-relation (copy-tree numbers))
	  (choose-fail))
      (push (copy-tree numbers) possibilities)
      (message "Found a possibility: %S" numbers)
      (sit-for 1)
      (if (> (length possibilities) 20)
	  (progn
	    (message "too many possibilities, bye")
	    (sit-for 1))
	(choose-fail))))
    (if possibilities
	(car (reverse possibilities)))))
 


(defun choose-example-nongrouping-n-numbers (n)
  "Attempt to find n numbers, each <= max, such that no subgroup among
  these n numbers sums up to any other subgroup.  

There was a new scientist 'enigma' problem related to this.
"
  (let*
      ((max (- n 1))
       numbers)
    (while (not numbers)
      (incf max)
      (message "Examining max: %S" max)
      (setq numbers 
	    (choose-example-nongrouping-n-numbers-givenmax n max)))
    numbers))




     
       
	 
(defun choose-example-complex-2 ()
  "This example demonstrates that the choose-list can depend on
previous values chosen"
  (interactive)
  (let (a b c)
    (choice-with
     (setq a (choose '((1 2) (3 4))))
     (setq b (set-difference '(1 2 3 4) a))
     (choice-with
      (setq c (choose b))
      (unless (member c '(1 2)) (choose-fail)))
     "extra stuff here"
     (message "%s" (list a b c)))))




     
       
	 
(defun choose-example-complex-1 ()
  "This example demonstrates that the choose-list can depend on
previous values chosen"
  (interactive)
  (let (a b c)
    (choice-with
     (setq a (choose '((1 2) (3 4))))
     (setq b (set-difference '(1 2 3 4) a))
     (choice-with
      (setq c (choose b))
      (unless (member c '(1 2)) (choose-fail))))
    (message "%s" (list a b c))))


(defun choose-example-iterate-1 ()
  "Return a list of all possible pairings of 2 sets of three
  numbers, except those that match 2 with b.  The result is:
\((1 a)
 (1 b)
 (1 c)
 (2 a)
 (2 c)
 (3 a)
 (3 b)
 (3 c))

"
  (interactive)
  (message "%s"
	   (choice-iterate
	    (let ((n (choose (list 1 2 3)))
		  (m (choose (list 'a 'b 'c))))
	      (when (and (= n 2) (eq m 'b))
		(choose-fail))
	      (list n m)))))

(defun choose-example-iterate-2 ()
  "Shows how an internal choice-with-iterate acts as a choice-with. 
Thus, b returns with the very first list of values. We thus return a
  list of only 3 pairings."
  (interactive)
  (let ((a (list 1 2 3))
	(b (list 4 5 6))
	x y z)
    (message "%s"
	     (choice-with-iterate
	      (setq x (choose a))
	      (if (= x 3) (choose-fail))
	      (choice-with-iterate 
	       (setq y (choose b)))
	      (list x y)))))


(defun choose-example-iterate-3 ()
  "Just another example of iterate. We return a list of 9 pairings."
  (interactive)
  (let ((a (list 1 2 3))
	(b (list 4 5 6))
	x y z)
    (message "%s"
	     (choice-with-iterate
	      (setq x (choose a))
	      (setq y (choose b))
	      (list x y)))))




(defun choose-example-bishop-problem-2-enumerate-interactive  ()
  (interactive)
  (message "Found %s answers" 
	   (length 
	    (choose-example-bishop-problem-2-enumerate))))

(defun choose-example-bishop-problem-2-enumerate  ()
  "The problem is to enumarate all possibilies of placing n bishops on
  a nxn chess board such that they control every square.
  For nxn, the number of possiblies is, according to my calculations,
  (factorial(n/2) * 0.25 * (n+6))^2.  Thus, for a 2x2, we should have
  4 possibilies.  Let's find out. We shall return, for each
  possibility, a list of bishop placements. 
  The usual chessboard convention is [a-h][1-8]. We shall rather
  use 0-7 for each. Thus b3 is denoted by [02].  Furthermore, we
  represent the numbers in base n."
  (let* ((evens (list 0 3))
	 (odds (list 1 2))
	 ;; bare => need to be covered.
	 ;;(bare (append odds evens)))
	 bare
	 e0 o0 )
    (choice-iterate 
     ;; Place the even bishops .. e0 and e1.
     (setq e0 (choose evens))
     
     
     ;; Place the odd bishops.
     (setq o0 (choose odds))
     ;;(setq o1 (choose odds))
     ;;(when (= o0 o1) (choose-fail))
     ;; Now, need to re-initialize bares, because body may have messed
     ;; with it. 
     (setq bare (append evens odds))
     
     ;; For each bishop, remove the bishop and (n-1) surrounding
     ;; squares in each direction from bare.
     (setq bare (choose-example-bishop-problem-2-remove-bishop o0 bare))
     (setq bare (choose-example-bishop-problem-2-remove-bishop e0 bare))
     
     ;; should have chosen so nothing is left uncovered. 
     (when bare (choose-fail))
     
     (list e0 o0))))


(defun choose-example-bishop-problem-4-iterate-interactive  ()
  (interactive)
  (message "Found %s answers" 
	   (length 
	    (choose-example-bishop-problem-n-iterate 4))))



(defun choose-example-bishop-problem-n-iterate  (n)
  "Repeat the 2 bishop problem, but for arbitrary n!
This function also displays the use of choose-n.

See the 2-enumerate function above for more details. 
  For nxn, the number of possiblies is, according to my calculations,
  (factorial(n/2) * 0.25 * (n+6))^2.  Thus, for a 2x2, we should have
  4 possibilies.  


2x2: 4
4x4: 25 , and indeed (length (choose-example-bishop-problem-n-enumerate 4)):
25. Must have done something wrong.
Let's find out. We shall return, for each
  possibility, a list of bishop placements. 
  The usual chessboard convention is [a-h][1-8]. We shall rather
  use 0-7 for each. Thus b3 is denoted by [02].  Furthermore, we
  represent the numbers in base n.

To save time, we can neatly divide the problem into even and odd
and thus sqrt our execution time. But, this function is meant as
a utility, so lists everything.
"
  (unless (evenp n)
    (error "n should be even, for now.."))
  (let* ((nn (* n n))
	 (nn1 (- nn 1))
	 (n1 (- n 1))
	 (n21 (- (/ n 2) 1))
	 (n22 (- (/ n 2) 2))
	 (n2 (/ n 2))
	 (alls (loop for i from 0 to nn1 collect i))
	 (evens (remove-if-not
		 #'(lambda (k)
		     (zerop 
		      (mod 
		       (+ (/ k n)
			  (mod k n))
		       2)))
		 alls))
	 (odds (set-difference alls evens))
	 (oddsxy (choose-example-chess-xy odds n))
	 (evensxy (choose-example-chess-xy evens n))
	 ;; bare => need to be covered.
	 ;;(bare (append odds evens)))
	 evenchoices oddchoices
	 bare
	 ;; es and os hold the lists for even and odd bishops.
	 es os esxy osxy)
    (loop for i from 0 to n21 do (push evens evenchoices))
    (loop for i from 0 to n21 do (push odds oddchoices))
    (choice-iterate
    ;; Place the even bishops .. and odd bishops.
     (setq es (choose-n evenchoices))
     (setq os (choose-n oddchoices))

     ;; Look for conflicts.  Moreover, eliminate simple reorderings,
     ;; which are just permutatinons!
     ;;(when (< (length (delete-duplicates es)) n2)
     ;;(choose-fail))
     (dotimes (i n21) ;; 0 to n/2-1
       (when (>= (nth i es) (nth (+ i 1) es))
	 (choose-fail)))


     (dotimes (i n21) ;; 0 to n-2
       (when (>= (nth i os) (nth (+ i 1) os))
	 (choose-fail)))



     (setq esxy (choose-example-chess-xy es n))
     (setq osxy (choose-example-chess-xy os n))
     ;; Now, need to re-initialize bares, because body may have messed
     ;; with it. 
     (if (choose-example-chess-bishop-uncovered-p esxy evensxy)
       (choose-fail))
     (if (choose-example-chess-bishop-uncovered-p osxy oddsxy)
	 (choose-fail))
     (append es os))))

(defun choose-example-chess-xy (poss n)
  (loop for p in poss collect
	(list (/ p n) (mod p n))))

(defun choose-example-chess-bishop-uncovered-p (bs sqs)
  "bs are bishops. sqs are our squares. If any square is left
  uncovered, we return with a t, else nil."
  ;; For each square, examine if uncovered.  As soon as a good bishop
  ;; found, move on to the next one.
  (block b
    (loop for s in sqs do
	  (unless
	      ;; we should get a t from this progn, else we have found a problem.
	      (block a
		(progn
		  (loop for b in bs do
			(when (= (abs (- (car b) (car s)))
				 (abs (- (second b) (second s))))
			  (return-from a t)))
		  nil))
	    
	    (return-from b t)))
    ;; no problem found.
    nil))



(defun choose-example-bishop-problem-2-remove-bishop (pos sqrs)
  (let ((s (copy-list sqrs)))
    ;;(setq s (delete pos sqrs))
    ;; right diagonal 2+1 = 3
    (loop for i from -1 to 1 do
	  (setq s (delete (+ pos (* i 3)) s)))
    ;; left diagonal 2 -1 = 1
    (loop for i from -1 to 1 do
	  (setq s (delete (+ pos (* i 1)) s)))
    s))
    


(defun choose-example-abuse-choice-iterate-1 ()
  "Abuse it to \"multiply\" three lists."
  (interactive)
  (message "%S"
	   (choice-iterate
	    (choose-n '((A B)
			(1 2)
			("aa" "bb" "cc"))))))


    


(defun choose-example-abuse-choice-iterate-2 ()
  "Abuse it to \"multiply\" three lists."
  (interactive)
  (message "%S"
	   (choice-iterate
	    (list 
	     (choose '(A B))
	     (choose '(1 2))
	     (choose '("aa" "bb" "cc"))))))


    
    
     
    

  
  

(defun choose-example-make-number (ls)
  (if (null ls)
      (error "null list")
    (if (= (length ls) 1)
	(car ls)
      (choose-example-make-number
       (cons (+ (* 10 (car ls))
		(cadr ls))
	     (cddr ls))))))





(defun choose-example-tree-path-to-node (tree node)
  "See Paul Graham's On Lisp descent p. 303.
Given a tree TREE, list the path (list of nodes) from head to node, if
  node is a member of the tree.  ELSE nil.  

Tree means that car is the node, and cdr is a list of the children trees.
"
  ;; This one might as well be a (progn), except that we have a
  ;; choose-fail in the null clause, so this is an acceptor. 
  (choice-with
   (cond
    ((null tree) (choose-fail))
    ((eq (car tree) node)
     (list (car tree)))
    (t 
     ;; Note that a choice-with needs to be here (or, of course, at
     ;; both places.) rather than at the beg. of defun.
     ;;
     ;; If it is at the beg. of the defun instead, we can have this
     ;; problem: This clause could be evaluated for all values of cdr
     ;; tree. Sometimes, cdr tree is null, in which case, of course,
     ;; the defun-level choice-with will think that there are no
     ;; choices, and instantly die. The problem is that the
     ;; defun-level choice-with shouldn't have been called at all for
     ;; some of such cases, like (when the tree is (8), and node
     ;; itself is 8).
     ;; 
     (choice-with
      ;; Ask lisp to simply choose the right path from among the ones
      ;; in (cdr tree).
       (let ((childpath 
	      (choose-example-tree-path-to-node 
	       (choose (cdr tree))
	       node)))
	 ;; We need to throw tag from right here. 
	 (if (choose-failed-p childpath) 
	     (choose-fail)
	   (cons (car tree)
		 childpath))))))))


(defun choose-example-tree-path-to-node-run ()
  (interactive)
  (let 
      ((tree
	'(1 
	  (2 
	   (3) 
	   (4) 
	   (5))
	  (6)
	  (7
	   (8
	    (9)
	    (10)
	    )
	   )))
       )

    ;;(setq tree '(9))
    (message "(1 7 8 9): %s" 
	     (choose-example-tree-path-to-node tree 9))))



(defun choose-example-AI-tree-path-to-node (tree node)
  "Just like choose-example-tree-path-to-node, but more AIish.
The tree can even be circular. We won't get bogged down in an infinite
  loop. Rather, for most real life trees, we will actually return an
  answer!

Of course, a better program should actually mark nodes and verify that
we are not traversing the same node again. But, that would not
illustrate our point. 
"
  ;; This one might as well be a (progn), except that we have a
  ;; choose-fail in the null clause, so this is an acceptor. 
  (choice-with
   (cond
    ((null tree) (choose-fail))
    ((eq (car tree) node)
     (list (car tree)))
    (t 
     ;; Note that a choice-with needs to be here (or, of course, at
     ;; both places.) rather than at the beg. of defun.
     ;;
     ;; If it is at the beg. of the defun instead, we can have this
     ;; problem: This clause could be evaluated for all values of cdr
     ;; tree. Sometimes, cdr tree is null, in which case, of course,
     ;; the defun-level choice-with will think that there are no
     ;; choices, and instantly die. The problem is that the
     ;; defun-level choice-with shouldn't have been called at all for
     ;; some of such cases, like (when the tree is (8), and node
     ;; itself is 8).
     ;; 

     ;; Get us the answer, but please do not take too long.
     (choice-with-timeout
      ;; Ask lisp to simply choose the right path from among the ones
      ;; in (cdr tree).
       (let ((childpath 
	      (choose-example-tree-path-to-node 
	       (choose (choose-permute (cdr tree)))
	       node)))
	 ;; We need to throw tag from right here. 
	 (if (choose-failed-p childpath) 
	     (choose-fail)
	   (cons (car tree)
		 childpath))))))))



(defun choose-example-AI-toeh ()
  "The first choice took too long. But, we found an alternative solution."
  (interactive)
  (message "Wait 2 secs.")
  (message "%s"
	   (let ((choose-seconds 1))
	     (or 
	      (choose-nil-if-failed
	       (choice-with-timeout
		
		(sit-for 2)
		(choose (list 1))))
	      (choice-with-timeout
	       (choose (list 3)))))))

(defun choose-example-AI-xyz ()
  "We have an endless supply of lists. We need to choose a number from
  each list. The sum of these numbers needs to sum up to > 2.
  The conundrum (not known in advance), is that each list happens to
  start with a 0. A normal depth-first loop will end in an infinite
  run.
  Not only is emacs smart enough to \"choose\" rather than looping,
  it also times out and eventually returns us an answer!

Note that a better AI program should also replace calls to (choose.. ) with 
calls to \(choose (choose-permute ..)). A shorthand for the latter is
to simply use (chooser).
"



  (interactive)
  (let ((choose-seconds 6))
    (message "%s"
	     (choose-example-AI-xyz-1 nil))))

(defun choose-example-AI-xyz-1 (prevnums)
  (let ((eachlist 
	 (list 0 1 2))
	(sum 0)
	nthis)
    (choice-with-timeout
     ;; this is important!
     (sit-for 0)
    (setq 
      nthis (choose eachlist))
     (setq sum (+ nthis (apply #'+ prevnums)))
     (if (>= sum 2)
	 ;; If sum > 2, we are ok.
	 (cons nthis prevnums)
       ;; If not, descend and see if our children can help any. 
       (choose-example-AI-xyz-1 (cons nthis prevnums))))))
    
     
      
       



(defun choose-example-AI-teh-wrong ()
  "Choose within 1 sec, numbers from 3 lists such that their sum is
  maximized. Also, randomize the order of looping.. (using chooser.)
See the next example for the right way of doing this."
  (interactive)
  ;; The reason this failed is the whole evaluation thing. 
  ;; The others, the bad choices, get evalueted too. Choose finally
  ;; returns the final result from a good choice, so, yes, the
  ;; expression returned will be (3,3,3), but that doesn't mean a b c
  ;; have been set to these values. 
  (let ((ls (loop for i downfrom 3 to 0 collect i)))
    (choice-with-thistimeout-scored 
     100
     (+ a b c)
     (setq a (choose (copy-tree ls))
	   ;; Or equivalently:
	   b (choose (copy-tree ls))
	   c (choose (copy-tree ls))))
    (message "%s: %s" (+ a b c) (list a b c))))


(defun choose-example-AI-ab-439879 ()
  (interactive) 
  (let ((ls '(2 3 2 4 3)))
  (message "%s"
	   (choice-with-thistimeout-scored 3 (+ a b)
					     (setq a (choose ls)
						   b (chooser ls))
					     (list a b)))))

(defun choose-example-AI-100-98389 ()
  "Choosoe three numbers quickly from large lists.
Notice how a is == b most of the time..  So, we never really had the
  chance to even slightly optimize the inner ones.. and timed out
  quite before that.. 
"
  (interactive) 
  (let ((ls (choose-permute (loop for ii from 0 to 1000 collect ii))))
    (message "%s"
	     (choice-with-thistimeout-scored 0.3 (+ a b c)
					     (sit-for 0)
					     (setq a (choose ls)
						   b (choose ls)
						   c (choose ls)) 
					     (list (+ a b c) a b
  c)))))


(defun choose-example-AI-100-398 ()
  "Choose three numbers quickly from large lists.
"
  (interactive) 
  (let ((ls (choose-permute (loop for ii from 0 to 1000 collect ii)))
	(choose-seconds 20))
    (message "%s"
	     (choice-with-timeout
	      (+ a b c)
	      (sit-for 0)
	      (list 
	       ;; Hm, do we even know the value of b here that we have
	       ;; a (+ a b c) in here? 
	       (setq a (choice-with-timeout (+ a b c) (setq a (choose ls))))
	       (setq b (choice-with-timeout (+ a b c) (setq b (choose ls))))
	       (setq c (choice-with-timeout (+ a b c) (setq c (choose  ls)))))))))

					       



(run-hooks 'choose-after-load-hook)




(provide 'choose)

;;; choose.el ends here

