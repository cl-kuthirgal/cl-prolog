
(in-package :cl-prolog-sys)

(defvar *valid-uc-atom-scanner*
  (cl-ppcre:create-scanner "^[A-Z0-9_]*$")
  "Matches uppercase names of Lisp symbols which may be mapped to
Prolog atoms.")
(defvar *valid-mc-atom-scanner*
  (cl-ppcre:create-scanner "^[a-z][a-zA-Z0-9_]*$")
  "Matches mixed case names of Lisp symbols which may be mapped to
unquoted Prolog atoms.")

(defvar *symbol-atom-scanner*
  (cl-ppcre:create-scanner "^[-#$&*+./:<=>?@^`~!]+$")
  "Matches names of Lisp symbols which may be mapped to unquoted
Prolog atoms.")

(defun pl-atom-p (arg)
  "Returns T if ARG is a symbol capable of representing a Prolog atom,
or NIL otherwise. The rule applied is that calling the function with
symbols whose name is all upper case without digits or underscores,
all lower case with or without digits or underscores, mixed case with
or without digits or underscores but with a lower case first letter
will return T. Calling with a symbol whose name is mixed case with an
upper case first letter will return NIL."
  (if (symbolp arg)
      (let ((name (symbol-name arg)))
        (or ; (string= "!" name)  -- now moved into regex
            (cl-ppcre:scan *valid-uc-atom-scanner* name)
            (cl-ppcre:scan *valid-mc-atom-scanner* name)
            (cl-ppcre:scan *symbol-atom-scanner* name)))
    nil))

(defun pl-variable-p (arg)
  "Returns T if ARG is a symbol capable of representing a Prolog
variable. Such symbols must start with a question mark."
  (and (symbolp arg)
       (char= #\? (char (symbol-name arg) 0))))

(defun anon-pl-variable-p (arg)
  "Returns T if ARG is the symbol ?_, representing the Prolog
anonymous variable."
  (and (symbolp arg)
       (string= "?_" (symbol-name arg))))

(defun pl-variable-name (arg)
  "Returns the Prolog name of symbol ARG."
  (cond ((anon-pl-variable-p arg)
         "_")
        ((pl-variable-p arg)
         (string-capitalize (subseq (lisp-symbol-to-pl arg) 1)
                            :start 0 :end 1))
        (t
         (error 'prolog-error
                :text (format nil
                              "~a does not map to a Prolog variable" arg)))))

(defun maps-to-pl-atom-p (arg)
  "Returns T if ARG is a symbol capable of representing a Prolog atom,
or NIL otherwise. The rule applied is that calling the function with
symbols whose name is all upper case without digits or underscores,
all lower case with or without digits or underscores, mixed case with
or without digits or underscores but with a lower case first letter
will return T. Calling with a symbol whose name is mixed case with an
upper case first letter will return NIL."
  (if (symbolp arg)
      (let ((name (symbol-name arg)))
        (or (string= "!" name) ; support the cut operator
            (cl-ppcre:scan *valid-uc-atom-scanner* name)
            (cl-ppcre:scan *valid-mc-atom-scanner* name)))
    nil))

(defun pl-atom-name (arg)
  "Returns the Prolog name of symbol ARG."
  (if (pl-atom-p arg)
      (lisp-symbol-to-pl arg)
    (symbol-name arg)))

;; Three types of atoms don't need quotes.
;; Those satisfying the regex [a-z][a-zA-Z0-9_]*, the regex
;; [-#$&*+./:<=>?@^`~]+ (the 'symbol' characters) and finally
;; single-character atoms formed from a `solo' character (!;).

(defun pl-quote-atom-p (atom-name)
  "Returns T if the Prolog atom ATOM-NAME requires quoting."
  (not (or (cl-ppcre:scan *valid-mc-atom-scanner* atom-name)
           (cl-ppcre:scan *symbol-atom-scanner* atom-name))))

(defun pl-functor (predicate)
  "Returns a string which is the name portion of the name/arity string
PREDICATE."
  (subseq predicate 0 (position #\/ predicate)))

(defun pl-arity (predicate)
  "Returns an integer which is the arity portion of name/arity string
PREDICATE."
  (parse-integer predicate :start (1+ (position #\/ predicate))))

(defun lisp-symbol-to-pl (symbol)
  "Returns the symbol name of SYMBOL which will be converted to lower
case if every letter in the symbol-name is upper case."
  (let ((name (lisp-name-to-pl (symbol-name symbol))))
    (if (every #'upper-case-p name)
        (string-downcase name)
      name)))

(defun pl-name-to-lisp (name)
  "Returns a copy of NAME with underscores replaced with hyphens."
  (substitute #\- #\_ name))

(defun lisp-name-to-pl (name)
  "Returns a copy of NAME with hyphens replaced with underscores."
  (substitute #\_ #\- name))

(defun proper-list-p (expr)
  "Returns T if EXPR is a proper list, or otherwise NIL."
  (or (null expr)
      (and (consp expr)
           (proper-list-p (rest expr)))))

(defun dotted-pair-p (expr)
  (and (consp expr)
       (cdr expr)
       (not (consp (cdr expr)))))

(defun dotted-list-head (arg)
  (append (butlast arg) (list (car (last arg)))))

(defun dotted-list-tail (arg)
  (cdr (last arg)))

(defun pl-expr-p (expr)
  "Returns T if EXPR is a proper list whose CAR is a symbol which is
bound to a function, or otherwise NIL."
  (and (proper-list-p expr)
       (symbolp (first expr))
       (fboundp (first expr))))

;; FIXME -- think of a better name for this function
(defun apply-expression (expr)
  (if (pl-expr-p expr)
      ;; FIXME -- check fboundp before apply
      (apply (first expr) ; the Lisp function representing the Prolog
                          ; functor
             (mapcar #'(lambda (x)
                         (cond ((pl-expr-p x)
                                (apply-expression x))
                               ((proper-list-p x)
                                (mapcar #'apply-expression x))
                               (t
                                x)))
                     (rest expr))) ; the arguments to the Lisp function
    expr))

(defmethod create-prolog (args prolog-type)
  (error "Unknown Prolog type ~a" prolog-type))

(defmethod call-prolog (expr module prolog)
  nil)