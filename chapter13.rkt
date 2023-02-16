#lang racket
(require racket)
#| Hop, Skip, and Jump |#

(define (member? a lat)
  (cond [(null? lat) #f]
        [(eq? (car lat) a) #t]
        [else (member? a (cdr lat))]))

(define (intersect set1 set2)
  (cond [(null? set1) '()]
        [(member? (car set1) set2)
         (cons (car set1)
               (intersect (cdr set1) set2))]
        [else (intersect (cdr set1) set2)]))

(intersect '(tomatoes and macaroni)
           '(macaroni and cheese))

(define (intersect set1 set2)
  (letrec ([I (λ (s1)
                (cond [(null? s1) '()]
                      [(member? (car s1) set2)
                       (cons (car s1) (I (cdr s1)))]
                      [else (I (cdr s1))]))])
    (I set1)))

(intersect '(tomatoes and macaroni)
           '(macaroni and cheese))

(define (intersectall lset)
  (cond [(null? lset) '()]
        [(null? (cdr lset)) (car lset)]
        [else
         (intersect (car lset)
                    (intersectall (cdr lset)))]))

(intersectall '((tomatoes and macaroni)
                (macaroni and cheese)
                (fleas and bees)))

(define (intersectall lset)
  (if (null? lset) '()
      (letrec ([I (λ (ls)
                    (cond [(null? (cdr ls)) (car ls)]
                          [else
                           (intersect (car ls) (intersectall (cdr ls)))]))])
        (I lset))))

(intersectall '((3 mangos and)
                (3 kiwis and)
                (3 hamburgers)))

(intersectall '((3 mangos and)
                ()
                (3 diet hamburgers)))

(define (intersectall lset)
  (let/cc
      hop
    (letrec ((⊳ (λ (lset)
                  (cond [(null? (car lset)) (hop '())]
                        [(null? (cdr lset)) (car lset)]
                        [else
                         (intersect (car lset) (⊳ (cdr lset)))]))))
      (cond [(null? lset) '()]
            [else (⊳ lset)]))))

;; use let/cc to return values abruptly and promptly

(define (intersect set1 set2)
  (letrec ([⊳ (λ (set1)
                (cond [(null? set1) '()]
                      [(member? (car set1) set2)
                       (cons (car set1) (⊳ (cdr set1)))]
                      [else (⊳ (cdr set1))]))])
    (cond [(null? set2) '()]
          [else (⊳ set1)])))

(intersect '(a b c) '(b c d))
(intersect '(a b c) '())

(define (intersectall lset)
  (let/cc hop
    (letrec ([A (λ (lset)
                  (cond [(null? (car lset)) (hop '())]
                        [(null? (cdr lset)) (car lset)]
                        [else (I (car lset) (A (cdr lset)))]))]
             [I (λ (s1 s2)
                  (letrec ([J (λ (s1)
                                (cond [(null? s1) '()]
                                      [(member? (car s1) s2)
                                       (J (cdr s1))]
                                      [else (cons (car s1)
                                                  (J (cdr s1)))]))])
                    (cond [(null? s2) (hop '())]
                          [else (J s1)])))])
      (cond [(null? lset) '()]
            [else (A lset)]))))


(intersectall '((3 steaks and)
                (no food and)
                (three baked potatoes)
                (3 diet hamburgers)))

(intersectall '((3 mangos and)
                ()
                (3 diet hamburgers)))

(define (rember a lat)
  (letrec ((⊳ (λ (lat)
               (cond [(null? lat) '()]
                     [(eq? a (car lat)) (cdr lat)]
                     [else (cons (car lat) (⊳ (cdr lat)))]))))
    (⊳ lat)))

(rember 'spud '(spud dud bud hud))
(rember 'bud '(spud dud bud hud))

(define (rember-beyond-first a lat)
  (letrec ([⊳ (λ (lat)
                (cond [(null? lat) '()]
                      [(eq? a (car lat)) '()]
                      [else (cons (car lat) (⊳ (cdr lat)))]))])
      (⊳ lat)))

(rember-beyond-first 'roots '(noodles
                              spaghetti
                              spatzle
                              bean-thread
                              roots
                              potatoes
                              yam
                              others
                              rice))

(define (rember-upto-last a lat)
  (let/cc skip
    (letrec ([⊳ (λ (lat)
                  (cond [(null? lat) '()]
                        [(eq? a (car lat)) (skip (⊳ (cdr lat)))]
                        [else
                         (cons (car lat) (⊳ (cdr lat)))]))])
      (⊳ lat))))

(rember-upto-last 'roots '(noodles
                           spaghetti
                           spatzle
                           bean-thread
                           roots
                           potatoes
                           yam
                           others
                           rice))

(rember-upto-last 'cookies '(cookies
                             chocolate mints
                             caramel delight
                             ginger snaps
                             desserts
                             chocolate mousse
                             vanilla ice cream
                             German chocolate cake
                             more cookies
                             gingerbreadman
                             chocolate chip brownies))
