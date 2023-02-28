#lang racket
(require racket)

;; syracuse video lectures

(call/cc (λ (k0)
           (+ 1 (call/cc (λ (k1)
                           (+ 1 (k0 3)))))))

;; call/cc called with (λ (k0) ...)
;; (+ 1 ...) is the surrounding expression for
;; call/cc called with (λ (k1) ...)
;; k1 is the continuation with (+ 1 (k0 3))
;; but when (k0 3) gets evaluated, it jumps to
;; ('here (call/cc (λ (k0) ...)))
;; 'here is empty, so we get 3

(call/cc (λ (k0)
           (+ 1 (call/cc (λ (k1)
                           (+ 1 (k0 (k1 3))))))))

(let/cc k0
         (+ 1 (let/cc k1
                       (+ 1 (k0 (k1 3))))))

;; call/cc called with (λ (k0) ...)
;; (+ 1 (call/cc (λ (k1) ...))) evaled
;; (+ 1 (k0 (k1 3))) evaled
;; k0 is called
;; we get 'here (k1 3)
;; k1 is called with 3
;; we get the surrounding expression, which is
;; (call/cc (λ (k0) (+ 1 3)))
;; (call/cc (λ (k0) 4)) => 4

(call/cc (λ (z1) 4))

;; call/cc called with (λ (k0) ...)
;; eval (+ 1 (call/cc (λ (k1) ...)))
;; (+ 1 (k0 (k1 3))) evaled
;; k0 sends (k1 3) back to the start of the program,
;;  i.e. 'here (call/cc (λ (k0) ...))
;; so we get (k1 3)
;; k1 sends 3 to (call/cc (λ (k0) (+ 1 'here)))
;; so we have (call/cc (λ (k0) (+ 1 3))) => (call/cc (λ (k0) 4))
;; we end up with a continuation k0 that evaluates to 'here (call/cc (λ (k0) 4))


(call/cc (λ (k0)
           (+ 1 (call/cc (λ (k1)
                           (+ 1 (k1 3))))
              (k0 1))))

;; call/cc called with (λ (k0) ...)
;; eval (+ 1 (call/cc (λ (k1) ...)))
;; eval (+ 1 (k1 3))
;; k1 jumps to surrounding area with 3:
;;  (call/cc (λ (k0) (+ 1 3 (k0 1))))
;; we eval our expression and return 1

(+ 1 (call/cc (λ (k) (k 3) (print 0))))

#| notes from https://courses.cs.washington.edu/courses/cse341/04wi/lectures/15-scheme-continuations.html |#

(+ 4 (+ 1 2))
; => ((λ (v) (+ 4 v)) 3)
((λ (v) (+ 4 v)) 3)
(+ 4 3) ; => 7

#|
(call/cc expr)  does the following:
1. Captures the current continuation
2. Constructs a function `c` that takes one argument, and applies the current continuation with the argument value
3. Passes this function as an argument to `expr` -- (expr c)
4. Returns the result of evaluating (expr c), unless expr calls c, in which case the value that is passed to c is returned
|#

(+ 4 (call/cc (λ (cont) (cont (+ 1 2))))) ; => (+ 4 (+ 1 2)) => 7
((λ (v) (+ 4 v)) (call/cc (λ (cont) (cont (+ 1 2)))))

#| continuations are first class |#

;; continuations don't have to be called immediately
(define x '())
(define (put-cont-in-x cont) (set! x cont))
(define (print-line x) (display x) (newline))

(print-line (call/cc put-cont-in-x))
(x "hi!")

;; x is '()
;; eval (print-line (call/cc put-cont-in-x))
;; (call/cc put-cont-in-x) calls put-cont-in-x
;; put-cont-in-x sets x to the continuation, which is
;; (print-line cont)
;; put-cont-in-x returns #<void> since it just sets x
;; then when we call (x <string>) we get (print-line <string>)

#| continuations for "early exit" from nested evaluation |#

(define (naive-find predicate? x)
  (cond [(null? x) '()]
        [(predicate? (car x)) (car x)]
        [else (naive-find predicate? (cdr x))]))

(naive-find number? '(animal danimal canimal 8 banimal zanimal))

(define (find predicate? a-list)
  (call/cc (λ (cont)
             (letrec ((helper (λ (x)
                                (cond ((null? x) (cont '()))
                                      ((predicate? (car x)) (cont (car x)))
                                      (else (helper (cdr x)))))))
               (helper a-list)))))

(find number? '(animal danimal canimal 8 banimal zanimal))

;; when we find a terminating case, this skips the computation directly to the top level
;; and ignores the remaining recursive calls. This isn't such a big deal in languages with tail recursion

;; We can see this more clearly with print statements
(define (printing-naive-find predicate? x)
  (cond [(null? x) '()]
        [(predicate? (car x)) (car x)]
        [else (let ((retval (printing-naive-find predicate? (cdr x))))
                (displayln (format "returning from recursive call at: ~a" (car x)))
                retval)]))

(printing-naive-find number? '(animal danimal canimal 8 banimal zanimal))

(define (printing-find predicate? a-list)
  (call/cc (λ (cont)
             (letrec ((helper (λ (x)
                                (cond ((null? x) (cont '()))
                                      ((predicate? (car x)) (cont (car x)))
                                      (else (let ((retval (helper (cdr x))))
                                              (displayln (format "returning from call at: ~a" (car x)))
                                              retval))))))
               (helper a-list)))))

(printing-find number? '(animal danimal canimal 8 banimal zanimal))

;; printing-naive-find prints values "on the way back" from the last recursive call, whereas printing-find does not
;; because the continuation is captured before entering the recursive function

;; naive-find is tail-recursive so there is no need to pass the end result directly to the caller of the original invocation
;; but for non-tail recursive functions, continuations can make returning the last call constant rather than linear-time

;; breaking out of deeply nested evaluations is often useful when returning errors
(define (divide-or-error a-list divisor-list error-value)
  (cond [(null? a-list) '()]
        [(eq? (car divisor-list) 0) error-value]
        [else
         (let ([result (divide-or-error (cdr a-list) (cdr divisor-list) error-value)])
           (if (eq? result error-value)
               error-value
               (cons (/ (car a-list) (car divisor-list)) result)))]))

(divide-or-error '(1000 559 383 210 103) '(2 2 2 2 2) 'nope)
(divide-or-error '(1000 559 383 210 103) '(2 2 0 2 2) 'nope)

(define (divide-or-error2 a-list divisor-list error-value)
  (call/cc (λ (cont)
             (letrec ((helper (λ (values divisors)
                                (cond [(null? values) '()]
                                      [(eq? (car divisors) 0) (cont error-value)]
                                      [else
                                       (cons (/ (car values) (car divisors))
                                             (helper (cdr values) (cdr divisors)))]))))
               (helper a-list divisor-list)))))


(divide-or-error2 '(1000 559 383 210 103) '(2 2 2 2 2) 'nope)
(divide-or-error2 '(1000 559 383 210 103) '(2 2 0 2 2) 'nope)

#| The dynamic-wind function |#

#|
Sometimes we want to make sure that a function gets executed regardless of whether a continuation bypasses part of the evaluating expression

The dynamic-wind function takes 3 args, each of which must be a no-argument function value

The semantics are:
(dynamic-wind in-fn body-fn out-fn)

- call body-fn and return its value
- whenever body-fn is entered, either during current eval or though eval of any continuation captured inside body-fn, call in-fn beforehand
- whenever body-fn is exited, either during current eval or though eval of any continuation captured inside body-fn, call in-fn afterwards
|#

(define (safe-process input-file process-fn)
  (let ((p (open-input-file input-file)))
    (dynamic-wind
      (λ () #f)
      (λ () (process-fn p))
      (λ ()
        (begin
          (displayln "closing input port")
          (close-input-port p))))))

;; here the in fn does nothing, the out function closes the input port, and the body fn applies process-fn to the opened input port

;; now we can apply safe-process to a function that exits prematurely
(define exit-to-toplevel 'dummy)
(call/cc (λ (cont) (set! exit-to-toplevel cont)))
(define (process p) (toplevel-exit))

;; dynamic-wind still closes the input port
(safe-process "example.ss" process)

#| Exceptions |#

;; here is how we would like to use the raise and handle functions
(define (find-or-raise pred x)
  (cond [(null? x) (raise '(empty "No such element in list."))]
        [(pred (car x)) (car x)]
        [else (find-or-raise pred (cdr x))]))

(define (gt0 x) (> x 0))
(define find-result
  (handle 'empty
          (λ () (find-or-raise gt0 '(-1 -2 -3 -4)))
          (λ (exception) -1)))

#|

We would like the arguments to be:

1. a symbol naming the exception to be handled
2. a no-argument function that evaluates some body expression
3. a single argument function that takes an exception and produces a
   value to be returned when that exception is raised

|#

#| The handler stack |#
;; at any given point in evaluation, there is a stack of "active" handlers
;; when an exception is raised, we pop handlers off the stack until we reach
;; one that can handle the current exception
(define handler-stack '())

(define (push-handler handler)
  (set! handler-stack (cons handler handler-stack)))

(define (pop-handler)
  (when (not (null? handler-stack))
    (let ((top (car handler-stack)))
      (begin
        (set! handler-stack (cdr handler-stack))
        top))))

;; unhandled exceptions
(define exit-to-toplevel 'dummy)
(call/cc (λ (cont) (set! exit-to-toplevel cont)))

(if #t
    (if #t
        (if #t
            (if #t
                (if #t
                    (begin
                      (println "hopping!")
                      (exit-to-toplevel))
                    #f)
                #f)
            #f)
        #f)
    #f)

;; The raise function
(define (raise exception)
  (if (null? handler-stack)
      (begin
        (displayln (format "uncaught exception: ~a" exception))
        (exit-to-toplevel))
      (let* ((exn-name (car exception))
             (handler (pop-handler))
             (handler-exn-name (car handler)))
        (if (eq? exn-name handler-exn-name)
            (let ((handler-cont (cadr handler))
                  (handler-fn (caddr handler)))
              (handler-cont (handler-fn exception)))
            (raise exception)))))

;; the handle function
(define (handle exception-name body-fn handler-fn)
  (call/cc (λ (cont)
             (dynamic-wind (λ () (push-handler (list exception-name cont handler-fn)))
                           body-fn
                           (λ () (pop-handler))))))
