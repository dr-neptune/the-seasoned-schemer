#lang racket
(require racket)

#| Absconding with the Jewels |#

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(deep 6)

(define (deep m)
  (cond [(zero? m) 'pizza]
        [else (cons (deep (sub1 m)) '())]))

(define (six-layers p)
  (cons
   (cons
    (cons
     (cons
      (cons
       (cons p '())
       '())
      '())
     '())
    '())
   '()))

(six-layers 'neapolitan)

(define toppings '())
(define (deepB m)
  (cond [(zero? m)
         (let/cc jump
           (set! toppings jump)
           'pizza)]
        [else (cons (deepB (sub1 m)) '())]))

(deepB 6)
(toppings 'mozarella)
(toppings 'cake)

(cons (toppings 'cake) '())
(cons (cons (cons (cons (toppings 'mozarella)) '()) '()) '())

(cons (toppings 'cake)
      (toppings 'cake))

(define (deep&co m k)
  (cond [(zero? m) (k 'pizza)]
        [else
         (deep&co (sub1 m) (λ (x)
                             (k (cons x '()))))]))

(deep&co 4 toppings)

(deep&co 15 identity)
(deep&co 0 identity)

(define (deep&coB m k)
  (cond [(zero? m) (let ()
                     (set! toppings k)
                     (k 'pizza))]
        [else
         (deep&coB (sub1 m)
                   (λ (x) (k (cons x '()))))]))

(deep&coB 4 identity)

(toppings 'mozarella)

(cons (toppings 'cake)
      (toppings 'cake))

(cons (toppings 'cake)
      (cons (toppings 'mozarella)
            (cons (toppings 'pizza) '())))

(define two-in-a-row?
  (letrec ([W (λ (a lat)
                (cond [(null? lat) #f]
                      [else
                       (let ([nxt (car lat)])
                         (or (eq? nxt a)
                             (W nxt (cdr lat))))]))])
    (λ (lat)
      (cond [(null? lat) #f]
            [else (W (car lat) (cdr lat))]))))

(two-in-a-row? '(mozarella cake mozarella))
(two-in-a-row? '(mozarella mozarella pizza))

(define leave '())
(define (walk l)
  (cond [(null? l) '()]
        [(atom? (car l)) (leave (car l))]
        [else
         (begin
           (walk (car l))
           (walk (cdr l)))]))
