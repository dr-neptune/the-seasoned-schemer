#lang racket
(require racket)

#| Welcome Back to the Show |#

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (member? a lat)
  (match lat
    [(list _ ... (== a) _ ...) #t]
    [_ #f]))

(member? 'sardines '(italian sardines spaghetti parsley))

(define (two-in-a-row? lat)
  (match lat
    [(list _ ... a a _ ...) #t]
    [_ #f]))

(two-in-a-row? '(italian sardines spaghetti parsley))
(two-in-a-row? '(italian sardines spaghetti spaghetti parsley))

(define (cond/two-in-a-row? lat)
  (cond [(or (null? lat)
             (null? (rest lat))) #f]
        [(eq? (first lat) (first (rest lat))) #t]
        [else
         (cond/two-in-a-row? (rest lat))]))

(cond/two-in-a-row? '(italian sardines spaghetti parsley))
(cond/two-in-a-row? '(italian sardines spaghetti spaghetti parsley))

(define (is-first? a lat)
  (if (null? lat) #f (eq? a (car lat))))

(define (book/two-in-a-row? lat)
  (cond [(null? lat) #f]
        [else (or (is-first? (car lat) (cdr lat))
                  (book/two-in-a-row? (cdr lat)))]))

;; swing and a miss
(define (book/2/continue-search? a lat)
  (cond [(null? lat) #f]
        [(eq? a (car lat)) #f]
        [else #t]))

(define (book/2/two-in-a-row? lat)
  (when
      (book/2/continue-search? (car lat) (cdr lat))
    (book/2/two-in-a-row? (rest lat))))

(book/2/two-in-a-row? '(italian sardines spaghetti parsley))
(book/2/two-in-a-row? '(italian sardines spaghetti spaghetti parsley))

(define (book/3/two-in-a-row? lat)
  (cond [(null? lat) #f]
        [else
         (is-first-b? (car lat) (cdr lat))]))

(define (is-first-b? a lat)
  (cond [(equal? a (car lat)) #t]
        [else (book/3/two-in-a-row? (cdr lat))]))

(book/3/two-in-a-row? '(italian sardines spaghetti parsley))
(book/3/two-in-a-row? '(italian sardines spaghetti spaghetti parsley))

(define (two-in-a-row-b? lat)
  (cond [(or (null? lat) (null? (cdr lat))) #f]
        [(eq? (car lat) (cadr lat)) #t]
        [else (two-in-a-row-b? (cdr lat))]))

(two-in-a-row-b? '(italian sardines spaghetti parsley))
(two-in-a-row-b? '(italian sardines spaghetti spaghetti parsley))
(two-in-a-row-b? '(italian sardines spaghetti parsley parsley))

(define (book/4/two-in-a-row-b? preceding lat)
  (cond [(null? lat) #f]
        [else
         (or (eq? (car lat) preceding)
             (book/4/two-in-a-row-b? (car lat) (cdr lat)))]))

(define (book/4/two-in-a-row? lat)
  (cond [(null? lat) #f]
        [else
         (book/4/two-in-a-row-b? (car lat) (cdr lat))]))

(book/4/two-in-a-row? '(italian sardines spaghetti parsley))
(book/4/two-in-a-row? '(italian sardines spaghetti spaghetti parsley))
(book/4/two-in-a-row? '(italian sardines spaghetti parsley parsley))

(define (sum-of-prefixes tup)
  (cond [(null? tup) '()]
        [else
         (cons
          (sum-of-prefixes (rest tup))
          (+ (first tup)
             (sum-of-prefixes (rest tup))))]))

(define (sum-of-prefixes tup)
  (cond [(null? tup) '()]
        [else
         (sum-of-prefixes-b tup 0)]))

(define (sum-of-prefixes-b tup sonssf)
  (cond [(null? tup) '()]
        [else
         (cons (+ sonssf (car tup))
               (sum-of-prefixes-b (cdr tup) (+ sonssf (car tup))))]))

(sum-of-prefixes '(1 1 1 1 1))

(define (accumulate tup [running-total 0])
  (if (null? tup)
      '()
      (let ([tot (+ running-total (first tup))])
        (cons tot (accumulate (rest tup) tot)))))

(accumulate '(1 1 1 1 1))

(define (pick n lat)
  (cond [(eq? 1 n) (car lat)]
        [else
         (pick (sub1 n) (cdr lat))]))

(define (scramble-b tup rev-pre)
  (cond [(null? tup) '()]
        [else
         (cons (pick (car tup)
                     (cons (car tup)
                           rev-pre))
               (scramble-b (cdr tup)
                           (cons (car tup) rev-pre)))]))
