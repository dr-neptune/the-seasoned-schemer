#lang racket
(require racket)

(define Y
  (λ (le)
    ((λ (f)
       (f f))
     (λ (f)
       (le (λ (x)
             ((f f) x)))))))

(define (multirember a lat)
  ((Y (λ (mr)
        (λ (lat)
          (cond [(null? lat) '()]
                [(eq? a (car lat)) (mr (cdr lat))]
                [else
                 (cons (car lat) (mr (cdr lat)))]))))
   lat))

(multirember 'meeses '(meeses cheeses for meeses the meeses))

(define length
  (Y (λ (length)
       (λ (l)
         (cond [(null? l) 0]
               [else
                (add1 (length (cdr l)))])))))

(length '(meeses cheeses for meeses the meeses))

(define (multirember a lat)
  (λ (a lat)
    ((letrec
         ((mr (λ (lat)
                (cond ((null? lat) '())
                      ((eq? a (car lat))
                       (mr (cdr lat)))
                      (else
                       (cons (car lat)
                             (mr (cdr lat))))))))
       mr)
     lat)))

(define mr
  (λ (lat)
    (cond [(null? lat) '()]
          [(eq? a (car lat)) (mr (cdr lat))]
          [else
           (cons (car lat)
                 (mr (cdr lat)))])))

(define multirember (λ (a lat) (mr lat)))

(multirember 'meeses '(meeses cheeses for meeses the meeses))

(define multirember
  (λ (a lat)
    (letrec
        ((mr (λ (lat)
               (cond ((null? lat) '())
                     ((eq? a (car lat))
                      (mr (cdr lat)))
                     (else
                      (cons (car lat)
                            (mr (cdr lat))))))))
      (mr lat))))

(multirember 'meeses '(meeses cheeses for meeses the meeses))

(define (multirember-f a lat test?)
  (letrec ([mrf (λ (lat)
                  (cond [(null? lat) '()]
                        [(test? a (car lat))
                         (mrf (cdr lat))]
                        [else
                         (cons (car lat)
                               (mrf (cdr lat)))]))])
    (mrf lat)))

(multirember-f 24 '(meeses 24 cheeses for 24 meeses the 24 meeses) eq?)

(define (multirember-f test?)
  (λ (a lat)
    (cond [(null? lat) '()]
          [(test? a (car lat))
           ((multirember-f test?) a (cdr lat))]
          [else
           (cons (car lat)
                 ((multirember-f test?) a (cdr lat)))])))

((multirember-f eq?) 24 '(meeses 24 cheeses for 24 meeses the 24 meeses))

(define (multirember-f test?)
  (letrec ([m-f (λ (a lat)
                  (cond [(null? lat) '()]
                        [(test? (car lat) a)
                         (m-f a (cdr lat))]
                        [else
                         (cons (car lat) (m-f a (cdr lat)))]))])
    m-f))

((multirember-f eq?) 24 '(meeses 24 cheeses for 24 meeses the 24 meeses))

(define (member? a lat)
  (cond [(null? lat) #f]
        [(eq? (car lat) a) #t]
        [else (member? a (cdr lat))]))

(define (match/member? a lat)
  (match lat
    [(list _ ... (== a) _ ...) #t]
    [_ #f]))

(member? 'ice '(salad greens with pears brie cheese frozen yogurt))
(match/member? 'greens '(salad greens with pears brie cheese frozen yogurt))

(define (member? a lat)
  ((letrec ([yes? (λ (l)
                   (cond [(null? l) #f]
                         [(eq? (car l) a) #t]
                         [else
                          (yes? (cdr l))]))])
    yes?)
  lat))

(member? 'ice '(salad greens with pears brie cheese frozen yogurt))
(member? 'pears '(salad greens with pears brie cheese frozen yogurt))

(define (union s1 s2)
  (cond [(null? s1) s2]
        [(member? (car s1) s2)
         (union (cdr s1) s2)]
        [else
         (cons (car s1)
               (union (cdr s1) s2))]))

(union '(tomatoes and macaroni casserole)
       '(macaroni and cheese))

(define (union s1 s2)
  (letrec ([U (λ (s1)
                (cond [(null? s1) s2]
                      [(member? (car s1) s2)
                       (U (cdr s1))]
                      [else
                       (cons (car s1) (U (cdr s1)))]))])
    (U s1)))

(union '(tomatoes and macaroni casserole)
       '(macaroni and cheese))

(define (union s1 s2)
  (letrec ([U (λ (s1)
                (cond [(null? s1) s2]
                      [(member? (car s1) s2)
                       (U (cdr s1))]
                      [else
                       (cons (car s1) (U (cdr s1)))]))]
           [member? (λ (a lat)
                      (cond [(null? lat) #f]
                            [(eq? (car lat) a) #t]
                            [else
                             (member? a (cdr lat))]))])
    (U s1)))

(union '(tomatoes and macaroni casserole)
       '(macaroni and cheese))

(define (union s1 s2)
  (letrec ([U (λ (s1)
                (cond [(null? s1) s2]
                      [(member? (car s1) s2)
                       (U (cdr s1))]
                      [else
                       (cons (car s1) (U (cdr s1)))]))]
           [M? (λ (a lat)
                 (letrec ([N? (λ (lat)
                                (cond [(null? lat) #f]
                                      [(eq? (car lat) a) #t]
                                      [else
                                       (N? (cdr lat))]))])
                   (N? lat)))])
    (U s1)))


(union '(tomatoes and macaroni casserole)
       '(macaroni and cheese))

(define (two-in-a-row? lat)
  (letrec ([W (λ (a lat)
                (cond [(null? lat) #f]
                      [else
                       (or (eq? (car lat) a)
                           (W (car lat) (cdr lat)))]))])
    (cond [(null? lat) #f]
          [else (W (car lat) (cdr lat))])))

(two-in-a-row? '(dog dog yes cat))
(two-in-a-row? '(dog yes cat))

(define (sum-of-prefixes tup)
  (letrec ([S (λ (sss tup)
                (cond [(null? tup) '()]
                      [else
                       (cons (+ sss (car tup))
                             (S (+ sss (car tup))
                                (cdr tup)))]))])
    (S 0 tup)))

(sum-of-prefixes '(1 2 3 4 5))

(define (scramble tup)
  (letrec ([P (λ (tup rp)
                (cond [(null? tup) '()]
                      [else
                       (cons (pick (car tup) (cons (car tup) rp))
                             (P (cdr tup)
                                (cons (car tup) rp)))]))])
    (P tup '())))
