#lang racket
(require racket)

#| What's in Store? |#

(define (the-empty-table name)
  (abort (cons 'no-answer (cons name '()))))

(define global-table
  ... the-empty-table ...)

(define (lookup table name)
  (table name))

(define (extend name1 value table)
  (λ (name2)
    (cond [(eq? name2 name1) value]
          [else (table name2)])))

(define (value e)
  ...
  (cond [(define? e) (*define e)]
        [else (the-meaning e)])
  ...)

(define (define? e)
  (cond [(atom? e) #f]
        [(atom? (car e))
         (eq? (car e) 'define)]
        [else #f]))

(define (*define e)
  (set! global-table
        (extend (name-of e)
                (box (the-meaning
                      (right-side-of e)))
                global-table)))

(define (box it)
  (λ (sel)
    (sel it (λ (new)
              (set! it new)))))

(define (setbox box new)
  (box (λ (it set) (set new))))

(define (unbox box)
  (box (λ (it set) it)))

(define xbox (box 'hello))

(unbox xbox)
(setbox xbox 'halo)
(unbox xbox)

(define (the-meaning e)
  (meaning e lookup-in-global-table))

(define (lookup-in-global-table name)
  (lookup global-table name))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (*quote e table)
  (text-of e))

(define (*identifier e table)
  (unbox (lookup table e)))

(define (*set e table)
  (setbox
   (lookup table (name-of e))
   (meaning (right-side-of e) table)))

(define (*lambda e table)
  (λ (args)
    (beglis (body-of e)
            (multi-extend
             (formals-of e)
             (box-all args)
             table))))

(define (beglis es table)
  (cond [(null? (cdr es)) (meaning (car es) table)]
        [else ((lambda val)
               (beglis (cdr es) table))
              (meaning (car es) table)]))

(define (box-all vals)
  (cond [(null? vals) '()]
        [else (cons (box (car vals))
                    (box-all (cdr vals)))]))

(define (multi-extend names values table)
  (cond [(null? names) table]
        [else
         (extend (car names)
                 (car values)
                 (multi-extend (cdr names)
                               (cdr values)
                               table))]))

(define (*application e table)
  ((meaning (function-of e) table)
   (evlis (arguments-of e) table)))

(define (evlis args table)
  (cond [(null? args) '()]
        [else
         ((λ (val) (cons val (evlis (cdr args) table)))
          (meaning (car args) table))]))

(define (:car args-in-a-list)
  (car (car args-in-a-list)))

(define (a-prim p)
  (λ (args-in-a-list)
    (p (car args-in-a-list))))

(define (b-prim p)
  (λ (args-in-a-list)
    (p (car args-in-a-list)
       (car (cdr args-in-a-list)))))

(define *const
  (let ((:cons (b-prim cons))
        (:car (a-prim car))
        (:cdr (a-prim cdr))
        (:null? (a-prim null?))
        (:eq? (b-prim eq?))
        (:atom? (a-prim atom?))
        (:number? (a-prim number?))
        (:zero? (a-prim zero?))
        (:add1 (a-prim add1))
        (:sub1 (a-prim sub1))
        (:number? (a-prim number?)))
    (λ (e table)
      (match e
        [#t #t]
        [#f #f]
        [v #:when (number? v) v]
        ['cons :cons]
        ['car :car]
        ['cdr :cdr]
        ['null? :null?]
        ['eq? :eq?]
        ['atom? :atom?]
        ['zero? :zero?]
        ['add1 :add1]
        ['sub1 :sub1]))))

(define (*cond e table)
  (evcon (cond-lines-of e) table))

(define (evcon lines table)
  (cond [(else? (question-of (car lines)))
         (meaning (answer-of (car lines)) table)]
        [(meaning (question-of (car lines)) table)
         (meaning (answer-of (car lines)) table)]
        [else (evcon (cdr lines) table)]))

(define (*letcc e table)
  (let/cc skip
    (beglis (ccbody-of e)
            (extend
             (name-of e)
             (box (a-prim skip))
             table))))

(define abort '())
(define (value e)
  (let/cc the-end
    (set! abort the-end)
    (cond [(define? e) (*define e)]
          [else (the-meaning e)])))

(define (expression-to-action e)
  (cond [(atom? e) (atom-to-action e)]
        [else (list-to-action e)]))

(define (atom-to-action e)
  (match e
        [#t *const]
        [#f *const]
        [v #:when (number? v) *const]
        ['cons *const]
        ['car *const]
        ['cdr *const]
        ['null? *const]
        ['eq? *const]
        ['atom? *const]
        ['zero? *const]
        ['add1 *const]
        ['sub1 *const]
        [_ *identifier]))

(define (list-to-action e)
  (match e
    [v #:when (atom? v)
       (match v
         ['quote *quote]
         ['lambda *lambda]
         ['letcc *letcc]
         ['set! *set]
         ['cond *cond]
         [_ *application])]
    [_ *application]))

(define (text-of x) (cadr x))
(define (formals-of x) (cadr x))
(define (body-of x) (cddr x))
(define (ccbody-of x) (cddr x))
(define (name-of x) (cadr x))
(define (right-side-of x) (if (null? (cddr x)) 0 (caddr x)))
(define (cond-lines-of x) (cdr x))
(define (else? x) (cond [(atom? x) (eq? x 'else)] [else #f]))
(define (question-of x) (car x))
(define (answer-of x) (cadr x))
(define (function-of x) (car x))
(define (arguments-of x) (cdr x))
