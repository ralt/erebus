(defpackage #:erebus
  (:use #:cl)
  (:export #:when-let
           #:when-let*
           #:if-let
           #:if-let*))

(in-package #:erebus)

(defmacro when-let* (bindings &body body)
  "Bind `bindings` serially and execute `body`, short-circuiting on `nil`.

  This macro combines `when` and `let*`.  It takes a list of bindings
  and binds them like `let*` before executing `body`, but if any
  binding's value evaluates to `nil` the process stops and `nil` is
  immediately returned.

  Examples:

    (when-let* ((a (progn (print :a) 1))
                (b (progn (print :b) (1+ a)))
      (list a b))
    ; =>
    :A
    :B
    (1 2)

    (when-let* ((a (progn (print :a) nil))
                (b (progn (print :b) (1+ a))))
      (list a b))
    ; =>
    :A
    NIL

  "
  (alexandria:with-gensyms (block)
    `(block ,block
       (let* ,(loop :for (symbol value) :in bindings
                    :collect `(,symbol (or ,value
                                           (return-from ,block nil))))
         ,@body))))

(defmacro if-let* (bindings then else)
  "Bind `bindings` serially and execute `then` if all are true, or `else` otherwise.

  This macro combines `if` and `let*`.  It takes a list of bindings and
  binds them like `let*` before executing `then`, but if any binding's
  value evaluates to `nil` the process stops and the `else` branch is
  immediately executed (with no bindings in effect).

  Examples:

    (if-let* ((a (progn (print :a) 1))
              (b (progn (print :b) (1+ a)))
      (list a b)
      'nope)
    ; =>
    :A
    :B
    (1 2)

    (if-let* ((a (progn (print :a) nil))
              (b (progn (print :b) (1+ a))))
      (list a b)
      'nope)
    ; =>
    :A
    NOPE

  "
  (alexandria:with-gensyms (outer inner)
    `(block ,outer
       (block ,inner
         (let* ,(loop :for (symbol value) :in bindings
                      :collect `(,symbol (or ,value
                                             (return-from ,inner nil))))
           (return-from ,outer ,then)))
       ,else)))

(defmacro when-let (bindings &body body)
  "Bind `bindings` in parallel and execute `body`, short-circuiting on `nil`.

  This macro combines `when` and `let`.  It takes a list of bindings and
  binds them like `let` before executing `body`, but if any binding's value
  evaluates to `nil` the process stops and `nil` is immediately returned.

  Examples:

    (when-let ((a (progn (print :a) 1))
               (b (progn (print :b) 2))
      (list a b))
    ; =>
    :A
    :B
    (1 2)

    (when-let ((a (progn (print :a) nil))
               (b (progn (print :b) 2)))
      (list a b))
    ; =>
    :A
    NIL

  "
  (alexandria:with-gensyms (block)
    `(block ,block
       (let ,(loop :for (symbol value) :in bindings
                   :collect `(,symbol (or ,value
                                          (return-from ,block nil))))
         ,@body))))

(defmacro if-let (bindings then else)
  "Bind `bindings` in parallel and execute `then` if all are true, or `else` otherwise.

  This macro combines `if` and `let`.  It takes a list of bindings and
  binds them like `let` before executing `then`, but if any binding's value
  evaluates to `nil` the process stops and the `else` branch is immediately
  executed (with no bindings in effect).

  Examples:

    (if-let ((a (progn (print :a) 1))
             (b (progn (print :b) 2))
      (list a b)
      'nope)
    ; =>
    :A
    :B
    (1 2)

    (if-let ((a (progn (print :a) nil))
             (b (progn (print :b) 2)))
      (list a b)
      'nope)
    ; =>
    :A
    NOPE

  "
  (alexandria:with-gensyms (outer inner)
    `(block ,outer
       (block ,inner
         (let ,(loop :for (symbol value) :in bindings
                     :collect `(,symbol (or ,value
                                            (return-from ,inner nil))))
           (return-from ,outer ,then)))
       ,else)))
