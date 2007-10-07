
(in-package #:cl-swi)

(cffi:define-foreign-library libpl
  (t (:default "libpl")))

(cffi:use-foreign-library libpl)

(defun pl-init (prolog arg-list)
  "Initialises the SWI-Prolog backend PROLOG with the arguments in
ARG-LIST. The arguments must be strings. Returns T on successful
initialisation, or NIL otherwise. There are issues with
re-initialising once pl-cleanup has been called (via pl-destroy) as
this currently causes Prolog to segfault. Corresponence on the
SWI-Prolog mailing list suggests that cycling through
pl-initialise/pl-cleanup is bad because for two reasons. Firstly,
calls to pl-initialise are expensive and secondly, pl-cleanup is not
guaranteed to free memory used by any Prolog C extensions and may
cause memory leaks. The course of action advised is to use
retractall/1 to clear the database instead."
  (let ((argc (length arg-list))
        (argv-ptr (create-foreign-argv arg-list)))
    (if (swi-true-p (pl-initialise argc argv-ptr))
        (setf (argv-ptr-of prolog) argv-ptr)
      (cffi:foreign-free argv-ptr))
    (pl-is-init)))

(defun pl-is-init ()
  "Returns T if the SWI-Prolog backend has been initialised, or NIL
otherwise."
  (cffi:with-foreign-objects ((argc-ptr :pointer)
                              (argv-ptr :pointer))
    (swi-true-p (pl-is-initialised argc-ptr argv-ptr))))

(defun pl-destroy (prolog)
  "Frees resources used by the SWI-Prolog backend PROLOG and returns T
if successful, or NIL otherwise."
  (cffi:foreign-free (argv-ptr-of prolog))
  (setf (argv-ptr-of prolog) nil)
  (swi-true-p (pl-cleanup 0)))

(defun create-foreign-argv (arg-list)
  "Creates the argv array for calling the SWI-Prolog initialisation
function. The foreign memory is not freed because the SWI-Prolog
foreign API states that these values may be accessed at any time by
the Prolog engine."
  (cffi:foreign-alloc :string
                      :initial-contents arg-list
                      :null-terminated-p t))

;; Lazily create module ref
(defmethod ref-of :before ((obj swi-module))
  (unless (slot-boundp obj 'ref)
    (setf (slot-value obj 'ref)
          (pl-new-module (pl-new-atom (name-of obj))))))

;; Lazily create functor ref
(defmethod ref-of :before ((obj swi-functor))
  (unless (slot-boundp obj 'ref)
    (setf (slot-value obj 'ref)
          (pl-new-functor (pl-new-atom (name-of obj)) (arity-of obj)))))

;; Lazily create predicate ref
(defmethod ref-of :before ((obj swi-predicate))
  (unless (slot-boundp obj 'ref)
    (setf (slot-value obj 'ref)
          (pl-pred (ref-of (functor-of obj)) (ref-of (module-of obj))))))

(defmethod print-object ((obj swi-term) stream)
  (with-slots (ref) obj
    (format stream "<SWI-TERM ref: ~a>"
            ref)))

(defmethod print-object ((obj swi-module) stream)
  (with-slots (name) obj
    (format stream "<SWI-MODULE name: ~a>"
            name)))

(defmethod print-object ((obj swi-functor) stream)
  (with-slots (name arity ref) obj
    (format stream "<SWI-FUNCTOR name: ~a, arity: ~a, ref: ~a>"
            name arity ref)))

(defmethod print-object ((obj swi-predicate) stream)
  (with-slots (functor module ref) obj
    (format stream "<SWI-PREDICATE ~a, ~a, ref: ~a>"
            functor module ref)))

(defmethod write-term ((obj swi-term) (arg symbol))
  (if (pl-variable-p arg)
      (ensure-variable arg obj)
    (progn
      (pl-put-atom-chars (ref-of obj) (pl-atom-name arg))
      (pl-atom-name arg)))
  obj)

(defmethod write-term ((obj swi-term) (arg integer))
  (pl-put-integer (ref-of obj) arg)
  obj)

(defmethod write-term ((obj swi-term) (arg float))
  (pl-put-float (ref-of obj) arg)
  obj)

(defmethod write-term ((obj swi-term) (arg string))
  ;; (pl-put-string-chars (ref obj) arg)
  (pl-put-atom-chars (ref-of obj) arg)
  obj)

(defmethod write-term ((obj swi-term) (arg swi-term))
  (pl-put-term (ref-of obj) (ref-of arg))
  obj)

(defmethod write-term ((obj swi-term) (arg (eql nil)))
  (pl-put-nil (ref-of obj))
  obj)

(defmethod write-term ((obj swi-term) (arg cons))
  (cond ((dotted-pair-p arg)
         (let ((head (make-instance 'swi-term :prolog (prolog-of obj)
                                    :ref (pl-new-term-ref)))
               (tail (make-instance 'swi-term :prolog (prolog-of obj)
                                    :ref (pl-new-term-ref))))
           (write-term head (car arg))
           (write-term tail (cdr arg))
           (pl-cons-list (ref-of obj) (ref-of head) (ref-of tail))))
        ((proper-list-p arg)
         (let ((head (make-instance 'swi-term :prolog (prolog-of obj)
                                    :ref (pl-new-term-ref))))
           (pl-put-nil (ref-of obj))
           (dolist (elt (reverse arg))
             (write-term head elt)
             (pl-cons-list (ref-of obj) (ref-of head) (ref-of obj)))))
        (t
         (let ((head (make-instance 'swi-term :prolog (prolog-of obj)
                                    :ref (pl-new-term-ref)))
               (tail (make-instance 'swi-term :prolog (prolog-of obj)
                                    :ref (pl-new-term-ref))))
           (write-term head (car (reverse (dotted-list-head arg))))
           (write-term tail (dotted-list-tail arg))
           (pl-cons-list (ref-of obj) (ref-of head) (ref-of tail))
           (dolist (elt (cdr (reverse (dotted-list-head arg))))
             (let ((node (make-instance 'swi-term :prolog (prolog-of obj)
                                        :ref (pl-new-term-ref))))
               (write-term node elt)
               (pl-cons-list (ref-of obj) (ref-of node) (ref-of obj)))))))
  obj)

(defmethod read-term ((obj swi-term))
  (let ((term-type (pl-term-type (ref-of obj))))
    (cond ((= +swi-variable+ term-type)
           (read-variable obj))
          ((= +swi-atom+ term-type)
           (read-atom obj))
          ((= +swi-integer+ term-type)
           (read-integer obj))
          ((= +swi-float+ term-type)
           (read-float obj))
          ((= +swi-string+ term-type)
           (read-string obj))
          ((= +swi-compound-term+ term-type)
           (if (swi-true-p (pl-is-list (ref-of obj)))
               (read-list obj)
             (read-functor obj)))
          (t (error 'prolog-error :text "Unknown term type ~a" term-type)))))

(defmethod cl-prolog-sys:create-prolog ((args list)
                                        (prolog-type (eql :swi-prolog)))
  (let ((prolog (make-instance 'swi-prolog)))
    (unless (pl-is-init)
      (unless (pl-init prolog args)
        (error 'prolog-error :text "Failed to start SWI-Prolog")))
    prolog))

(defmethod cl-prolog-sys:destroy-prolog ((prolog swi-prolog))
  (if (is-enabled-p prolog)
      (pl-destroy prolog)
    (error 'prolog-error :text "SWI-Prolog is not active")))

(defmethod cl-prolog-sys:is-enabled-p ((prolog swi-prolog))
  (pl-is-init))

(defmethod cl-prolog-sys:create-compound-term ((name string) (args list)
                                               (prolog swi-prolog))
  (let ((functor (ensure-functor name (length args) prolog))
        (terms (mapcar #'write-term
                       (create-swi-term-list (length args) prolog) args))
        (ref (pl-new-term-ref)))
    (progn
      (when (null terms) ; we seem to need an empty term here
        (push (make-instance 'swi-term :prolog prolog
                             :ref (pl-new-term-ref)) terms))
      (pl-cons-functor-v ref (ref-of functor) (ref-of (first terms)))
      (make-instance 'swi-compound-term :prolog prolog
                     :ref ref :functor functor :args terms))))

;; (defmethod cl-prolog-sys:create-rule ((head swi-term) (body list)
;;                                       (prolog swi-prolog))
;;   (flet ((pl-and (&rest terms)
;;            (create-compound-term "," terms prolog)))
;;     (if (< (length body) 2)
;;         (create-compound-term ":-" (adjoin head body) prolog)
;;       (create-compound-term ":-" (list head (reduce #'pl-and body
;;                                                     :from-end t)) prolog))))

(defmethod cl-prolog-sys:create-rule ((head swi-term) (body list)
                                      (prolog swi-prolog))
  (if (< (length body) 2)
      (create-compound-term ":-" (adjoin head body) prolog)
    (create-compound-term ":-" (list head (and/n body)) prolog)))

(defmethod cl-prolog-sys:find-prolog-module ((name string) (prolog swi-prolog))
  (ensure-module name prolog))

(defmethod cl-prolog-sys:call-prolog ((expr list) (module swi-module)
                                      (prolog swi-prolog))
  (swi-true-p (call-swi-prolog (apply-expression expr) module prolog)))

(defmethod cl-prolog-sys:call-prolog :around ((expr list) (module swi-module)
                                              (prolog swi-prolog))
  (declare (ignore expr module))
  (setf (vars-terms-of prolog) (make-hash-table)
        (terms-vars-of prolog) (make-hash-table))
  (call-next-method))

;;; FIXME - check opening/closing foreign frames
(defmethod cl-prolog-sys:prolog-query ((expr list) (module swi-module)
                                       (prolog swi-prolog))
  (let (;; (prolog-frame (pl-open-foreign-frame))
        (query (open-prolog-query expr module prolog)))
    (multiple-value-bind (solution bindings)
        (prolog-next-solution query)
      (close-prolog-query query)
      ;; (pl-close-foreign-frame prolog-frame)
      (values solution bindings))))

;;; FIXME - check opening/closing foreign frames
(defmethod cl-prolog-sys:open-prolog-query ((expr list) (module swi-module)
                                            (prolog swi-prolog))
  (let (;; (prolog-frame (pl-open-foreign-frame))
        (query (open-swi-query (apply-expression expr) module prolog)))
    ;; (make-instance 'swi-query :prolog prolog :ref query :frame prolog-frame)))
    (make-instance 'swi-query :prolog prolog :ref query)))

(defmethod cl-prolog-sys:open-prolog-query :around ((expr list)
                                                    (module swi-module)
                                                    (prolog swi-prolog))
  (declare (ignore expr module))
  (setf (vars-terms-of prolog) (make-hash-table)
        (terms-vars-of prolog) (make-hash-table))
  (call-next-method))

(defmethod cl-prolog-sys:prolog-next-solution ((query swi-query))
  (let ((solution (swi-true-p (pl-next-solution (ref-of query))))
        (exception (pl-exception (ref-of query))))
    (cond (solution
           (values t (prolog-variable-bindings (prolog-of query))))
          ((swi-false-p exception)
           (values nil nil))
          (t
           (let ((exception-term
                  (make-instance 'swi-term :prolog (prolog-of query)
                                 :ref exception)))
             (error 'prolog-error :text
                   0 (format nil "~a" (read-term exception-term))))))))

;;; FIXME - check opening/closing foreign frames
(defmethod cl-prolog-sys:close-prolog-query ((query swi-query))
  (pl-close-query (ref-of query))
  (setf (is-open query) nil)
  (when (frame-of query)
    (pl-close-foreign-frame (frame-of query)))
  t)

(defun swi-true-p (arg)
  "Returns T if ARG is the SWI-Prolog true value, otherwise NIL."
  (= +swi-true+ arg))

(defun swi-false-p (arg)
  "Returns T if ARG is the SWI-Prolog false value, otherwise NIL."
  (= +swi-false+ arg))

(defun create-term-ref-list (arity)
  "Creates a list of length ARITY containing consecutive term
references."
  (labels ((recurse-terms (term arity)
             (if (zerop arity)
                 nil
               (cons term (recurse-terms (1+ term) (1- arity))))))
    (recurse-terms (pl-new-term-refs arity) arity)))

(defun create-swi-term-list (arity prolog)
  "Creates a list of length ARITY containing consecutive swi-terms."
  (mapcar #'(lambda (r)
              (make-instance 'swi-term :prolog prolog :ref r))
          (create-term-ref-list arity)))

(defun ensure-functor (name arity prolog)
  "Returns an SWI-Prolog functor of NAME and ARITY."
  (let ((key (cons name arity)))
    (multiple-value-bind (functor cached)
        (gethash key (functor-cache-of prolog))
      (if cached
          functor
        (setf (gethash key (functor-cache-of prolog))
              (make-instance 'swi-functor :name name :arity arity))))))

(defun ensure-module (name prolog)
   "Returns an SWI-Prolog module of NAME."
   (multiple-value-bind (module cached)
       (gethash name (module-cache-of prolog))
     (if cached
         module
       (setf (gethash name (module-cache-of prolog))
             (make-instance 'swi-module :name name)))))

(defun ensure-predicate (name arity module prolog)
  "Returns an SWI-Prolog predicate of NAME and ARITY in MODULE, which
defaults to the SWI-Prolog user module."
  (let ((key (list name arity module)))
    (multiple-value-bind (predicate cached)
        (gethash key (predicate-cache-of prolog))
      (if cached
          predicate
        (let ((functor (ensure-functor name arity prolog)))
          (setf (gethash key (predicate-cache-of prolog))
                (make-instance 'swi-predicate
                               :functor functor :module module)))))))

(defun ensure-variable (symbol term)
  "Associates SYMBOL with a free Prolog variable TERM and returns
TERM. If SYMBOL is already associated with a Prolog variable, the ref
slot of the cached value is used to set TERM's ref slot."
  (let ((prolog (prolog-of term)))
    (if (anon-pl-variable-p symbol)
        (pl-put-variable (ref-of term))
    (multiple-value-bind (value cached)
        (gethash symbol (vars-terms-of prolog))
      (if cached
          (pl-put-term (ref-of term) (ref-of value))
        (progn
          (pl-put-variable (ref-of term))
          (setf (gethash term (terms-vars-of prolog)) symbol
                (gethash symbol (vars-terms-of prolog)) term))))))
  term)

(defun prolog-variable-bindings (prolog)
  (let ((bindings nil))
    (maphash #'(lambda (term var) ; FIXME -- don't include no-reporting vars
                 (push (cons var (read-term term)) bindings))
             (terms-vars-of prolog))
    (nreverse bindings)))

(defun read-variable (term)
  "Reads a Lisp string representation of an unbound variable from
TERM."
  (cffi:with-foreign-object (string-ptr :string)
    (pl-get-chars (ref-of term) string-ptr (boole boole-ior
                                                  *swi-cvt-variable*
                                                  *swi-buf-discardable*))
    (cffi:mem-aref string-ptr :string)))

(defun read-atom (term)
  "Reads a Prolog atom from TERM."
  (cffi:with-foreign-object (atom-ptr 'atom-t)
    (pl-get-atom (ref-of term) atom-ptr)
    (let ((name (pl-atom-chars (cffi:mem-aref atom-ptr 'atom-t))))
      (if (string= "[]" name)
          '(nil)
        ;; (intern name)))))
        name))))

(defun read-integer (term)
  "Reads a Prolog integer from TERM."
  (cffi:with-foreign-object (integer-ptr :int)
    (pl-get-integer (ref-of term) integer-ptr)
    (cffi:mem-aref integer-ptr :int)))

(defun read-float (term)
  "Reads a Prolog float from TERM."
  (cffi:with-foreign-object (float-ptr :double)
    (pl-get-float (ref-of term) float-ptr)
    (cffi:mem-aref float-ptr :double)))

(defun read-string (term)
  "Reads a Prolog string from TERM."
  (cffi:with-foreign-objects ((string-ptr :string)
                              (integer-ptr :unsigned-int))
    (pl-get-string (ref-of term) string-ptr integer-ptr)
    (cffi:mem-aref string-ptr :string)))

(defun read-functor (term)
  "Reads a Prolog functor and arguments from TERM."
  (cffi:with-foreign-objects ((atom-ptr 'atom-t)
                              (integer-ptr :int))
    (pl-get-name-arity (ref-of term) atom-ptr integer-ptr)
    (let ((name (pl-atom-chars (cffi:mem-aref atom-ptr 'atom-t)))
          (arity (cffi:mem-aref integer-ptr :int)))
      (cons (name-of (ensure-functor name arity (prolog-of term)))
            (read-args term arity)))))

(defun read-args (term arity)
  "Reads a Lisp list of arguments of a Prolog functor of ARITY from
TERM."
  (let ((arg (make-instance 'swi-term :prolog (prolog-of term)
                            :ref (pl-new-term-ref)))
        (args nil))
    (dotimes (n arity)
      (pl-get-arg (1+ n) (ref-of term) (ref-of arg))
      (push (read-term arg) args))
    (reverse args)))

(defun read-list (term)
  "Reads a Prolog list from TERM."
  (let ((prolog (prolog-of term)))
    (if (swi-true-p (pl-get-nil (ref-of term)))
        nil
      (let ((head (make-instance 'swi-term :prolog prolog
                                 :ref (pl-new-term-ref)))
            (tail (make-instance 'swi-term :prolog prolog
                                 :ref (pl-copy-term-ref (ref-of term))))
          (elements nil))
      (do ((has-more (pl-get-list (ref-of tail) (ref-of head) (ref-of tail))
                     (pl-get-list (ref-of tail) (ref-of head) (ref-of tail))))
          ((swi-false-p has-more) (nreverse elements))
        (push (read-term head) elements))))))

(defun call-swi-prolog (term module prolog)
  "Calls TERM in context of MODULE in PROLOG."
  (let ((predicate (ensure-predicate (name-of (functor-of term))
                                     (arity-of (functor-of term))
                                     module prolog)))
    (pl-call-predicate (ref-of module) *swi-q-normal*
                       (ref-of predicate)
                       (ref-of (first (args-of term))))))

(defun open-swi-query (term module prolog)
  "Opens query TERM in context of PROLOG."
  (let ((predicate (ensure-predicate (name-of (functor-of term))
                                     (arity-of (functor-of term))
                                     module prolog)))
    (pl-open-query (ref-of module) *swi-q-normal*
                   (ref-of predicate)
                   (ref-of (first (args-of term))))))

(cffi:defcallback wibble foreign-t ((arg0 term-t))
  (let ((term (make-instance 'swi-term :ref arg0)))
    (format t "Got ~a~%" (read-term term)))
  +swi-true+)

;; (pl-register-foreign "wibble" 1 (cffi:callback wibble) *)