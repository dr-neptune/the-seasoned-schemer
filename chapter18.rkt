#lang racket
(require racket)

#| We Change, Therefore We Are The Same! |#

(define (lots n)
  (if (zero? n)
      '()
      (kons 'egg (lots (sub1 n)))))

(lots 12)

(define (lenkth l)
  (cond [(null? l) 0]
        [else (add1 (lenkth (kdr l)))]))

(define (add-at-end l)
  (cond [(null? (kdr l))
         (konsC (kar l) (kons 'egg '()))]
        [else (konsC (kar l) (add-at-end (kdr l)))]))

(define (kons kar kdr)
  (λ (selector)
    (selector kar kdr)))

(define (kar c) (c (λ (a d) a)))
(define (kdr c) (c (λ (a d) d)))

(define (bons kar)
  (let ([kdr '()])
    (λ (selector)
      (selector
       (λ (x) (set! kdr x))
       kar
       kdr))))

(define (kar c) (c (λ (s a d) a)))
(define (kdr c) (c (λ (s a d) d)))

(kar (bons 'egg))

(define (set-kdr c x)
  ((c (λ (s a d) s)) x))

(define (kons a d)
  (let ((c (bons a)))
    (set-kdr c d)
    c))

(define dozen (lots 12))

(define bakers-dozen (add-at-end dozen))

(define eklist?
  (λ (ls1 ls2)
    (cond [(null? ls1) (null? ls2)]
          [(null? ls2) #f]
          [else
           (and (eq? (kar ls1) (kar ls2))
                (eklist? (kdr ls1) (kdr ls2)))])))

(define (same? c1 c2)
  (let ([t1 (kdr c1)]
        [t2 (kdr c2)])
    (set-kdr c1 1)
    (set-kdr c2 2)
    (let ([v (= (kdr c1) (kdr c2))])
      (set-kdr c1 t1)
      (set-kdr c2 t2)
      v)))

(define (last-kons ls)
  (cond [(null? (kdr ls)) ls]
        [else (last-kons (kdr ls))]))
