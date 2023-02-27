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

(define (start-it l)
  (let/cc here
    (set! leave here)
    (walk l)))

(start-it '((potato) (chips (chips (with))) fish))


(define fill '())

(define (waddle l)
  (cond [(null? l) '()]
        [(atom? (car l))
         (let ()
           (let/cc rest
             (set! fill rest)
             (leave (car l)))
           (waddle (cdr l)))]
        [else
         (begin
           (waddle (car l))
           (waddle (cdr l)))]))

(define (start-it2 l)
  (let/cc here
    (set! leave here)
    (waddle l)))

(start-it2 '((donuts)
             (cheerios (cheerios (spaghettios)))
             donuts))

(fill '((donuts)
        (cheerios (cheerios (spaghettios)))
        donuts))

(define (rest1 x)
  (waddle '(()
            (cheerios (cheerios (spaghettios)))
            donuts)))

(define (get-next x)
  (let/cc here-again
    (set! leave here-again)
    (fill 'go)))

(get-next 'go)

;; starting up again on 27feb23
(define leave '())

(define (walk l)
  (cond [(null? l) '()]
        [(atom? (car l)) (leave (car l))]
        [else
         (begin (walk (car l))
                (walk (cdr l)))]))

(define (start-it l)
  (let/cc here
    (set! leave here)
    (walk l)))

(start-it '((potato)
            (chips (chips (with)))
            fish))

(define fill '())

(define (waddle l)
  (cond [(null? l) '()]
        [(atom? (car l)) (begin
                           (let/cc rest
                             (set! fill rest)
                             (leave (car l)))
                           (waddle (cdr l)))]
        [else
         (begin (waddle (car l))
                (waddle (cdr l)))]))

(define (start-it2 l)
  (let/cc here
    (set! leave here)
    (waddle l)))

(start-it2 '((donuts)
             (cheerios (cheerios (spaghettios)))
             donuts))

(define (get-next x)
  (let/cc here-again
    (set! leave here-again)
    (fill 'go)))

(get-next 'go)

(define (get-first l)
  (let/cc here
    (set! leave here)
    (waddle l)
    (leave '())))


(get-first '())
(get-first '((pizza)))
(get-first '(fish (chips) chips))
(get-next 'go)

(define (two-in-a-row-*? l)
  (let ([fst (get-first l)])
    (if (atom? fst)
        (two-in-a-row-b*? fst)
        #f)))

(define (two-in-a-row-b*? a)
  (let ([n (get-next 'go)])
    (if (atom? n)
        (or (eq? n a)
            (two-in-a-row-b*? n))
        #f)))

(define two-in-a-row-*?
  (letrec
      ((T? (λ (a)
             (let ((n (get-next 0)))
               (if (atom? n)
                   (or (eq? n a) (T? n))
                   #f))))
       (get-next (λ (x)
                   (let/cc here-again
                     (set! leave here-again)
                     (fill 'go))))
       (fill identity)
       (waddle (λ (l)
                 (cond [(null? l) '()]
                       [(atom? (car l))
                        (begin
                          (let/cc rest
                            (set! fill rest)
                            (leave (car l)))
                          (waddle (cdr l)))]
                       [else (begin
                               (waddle (car l))
                               (waddle (cdr l)))]))))
    (λ (l)
      (let ((fst (let/cc here
                   (set! leave here)
                   (waddle l)
                   (leave '()))))
        (if (atom? fst) (T? fst) #f)))))


(two-in-a-row-*? '(((((peanut) butter)) and (marmalade ((((((jelly))))) jelly)))))
(two-in-a-row-*? '(((((peanut) butter)) and (marmalade ((((((jelly))))) jam)))))
(two-in-a-row-*? '(((food) ()) (((food)))))
