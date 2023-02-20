#lang racket
(require racket)

#| Let there be names |#
(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (leftmost l)
  (cond [(null? l) '()]
        [(atom? (car l)) (car l)]
        [else (leftmost (car l))]))

(leftmost '(((a) b) c))
(leftmost '(((a)) ()))
(leftmost '((() a) ()))
(leftmost '())

(define (leftmost l)
  (cond [(null? l) '()]
        [(atom? (car l)) (car l)]
        [else (cond [(atom? (leftmost (car l)))
                     (leftmost (car l))]
                    [else (leftmost (cdr l))])]))

(leftmost '(((a) b) (c d)))
(leftmost '(((a) ()) () (e)))
(leftmost '((() a) ()))

(define (leftmost l)
  (cond [(null? l) '()]
        [(atom? (car l)) (car l)]
        [else
         (let ([rc (leftmost (car l))])
           (cond [(atom? rc) rc]
                 [else (leftmost (cdr l))]))]))

(define (rember1* a l)
  (letrec ([R (λ (l)
                (cond [(null? l) '()]
                      [(atom? (car l))
                       (cond [(eq? (car l) a) (cdr l)]
                             [else (cons (car l) (R (cdr l)))])]
                      [else
                       (cond [(equal? (R (car l)) (car l))
                              (cons (car l) (R (cdr l)))]
                             [else (cons (R (car l)) (cdr l))])]))])
    (R l)))

(rember1* 'salad '((Swedish rye)
                   (French (mustard salad turkey))
                   salad))

(rember1* 'meat '((pasta meat) pasta (noodles meat sauce) meat tomatoes))


(define (rember1* a l)
  (letrec ([R (λ (l)
                (cond [(null? l) '()]
                      [(atom? (car l))
                       (cond [(eq? (car l) a) (cdr l)]
                             [else (cons (car l) (R (cdr l)))])]
                      [else
                       (let ([av (R (car l))])
                         (cond [(equal? av (car l))
                                (cons (car l) (R (cdr l)))]
                               [else (cons av (cdr l))]))]))])
    (R l)))

(define (depth* l)
  (cond [(null? l) 1]
        [(atom? (car l)) (depth* (cdr l))]
        [else
         (cond [(> (depth* (cdr l))
                   (add1 (depth* (car l))))
                (depth* (cdr l))]
               [else
                (add1 (depth* (car l)))])]))

(define (depth* l)
  (cond [(null? l) 1]
        [(atom? (car l)) (depth* (cdr l))]
        [else
         (let ([nest-first (add1 (depth* (car l)))]
               [nest-rest (depth* (cdr l))])
           (cond [(> nest-rest nest-first) nest-rest]
                 [else nest-first]))]))

(depth* '((pickled) peppers (peppers pickled)))
(depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter))
(depth* '(() ((bitter butter) (makes) (batter (bitter))) butter))

(define (depth* l)
  (cond [(null? l) 1]
        [(atom? (car l)) (depth* (cdr l))]
        [else
         (let ([nest-first (add1 (depth* (car l)))]
               [nest-rest (depth* (cdr l))])
           (max nest-first nest-rest))]))

(define (depth* l)
  (cond [(null? l) 1]
        [(atom? (car l)) (depth* (cdr l))]
        [else (max (add1 (depth* (car l))) (depth* (cdr l)))]))

(define (leftmost l)
  (let/cc skip
    (letrec ([lm (λ (l out)
                 (cond [(null? l) '()]
                       [(atom? (car l)) (out (car l))]
                       [else (begin (lm (car l) out)
                                    (lm (cdr l) out))]))])
      (lm l skip))))

(define (leftmost l)
  (let/cc skip
    (letrec ([lm (λ (l)
                   (cond [(null? l) '()]
                         [(atom? (car l)) (skip (car l))]
                         [else (begin (lm (car l))
                                      (lm (cdr l)))]))])
      (lm l))))

(leftmost '(((a) b) d))

(define (even-or-odd? i)
  (letrec ([even? (λ (n)
                    (if (= n 0)
                        'even
                        (odd? (sub1 n))))]
           [odd? (λ (n)
                   (if (= n 0)
                       'odd
                       (even? (sub1 n))))])
    (even? i)))

(even-or-odd? 100)  ; => 'even
(even-or-odd? 101)  ; => 'odd


(define (rm a l oh)
  (cond [(null? l) (oh 'no)]
        [(atom? (car l))
         (if (eq? (car l) a)
             (cdr l)
             (cons (car l)
                   (rm a (cdr l) oh)))]
        [else
         ...
         (let/cc oh
           (rm a (car l) oh))
         ...]))

(define (rm a l oh)
  (cond [(null? (oh 'no))]
        [(atom? (car l))
         (if (eq? (car l) a)
             (cdr l)
             (cons (car l)
                   (rm a (cdr l) oh)))]
        [else
         (if (atom? (letcc oh
                           (rm a (car l) oh)))
             (cons (car l) (rm a (cdr l) oh))
             (cons (rm a (car l) 0) (cdr l)))]))

(define (try a b)
  (let/cc success
    (let/cc x
      (success a))
    b))

(define (rember1* a l)
  (let/cc oh
    (try oh (rm a l oh) l)))

(rember1* 'noodles '(((noodles food) more (food))))

#| last example wasn't great |#
