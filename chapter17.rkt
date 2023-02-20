#lang racket
(require racket)

#| We Change, Therefore We Are! |#

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (deep m)
  (if (zero? m)
      'pizza
      (cons (deep (sub1 m)) '())))

(define (find n Ns Rs)
  (letrec ([F (λ (Ns Rs)
                (cond [(null? Ns) #f]
                      [(eq? (car Ns) n) (car Rs)]
                      [else
                       (F (cdr Ns) (cdr Rs))]))])
    (F Ns Rs)))

(define deepM
  (let ([Rs '()]
        [Ns '()])
    (letrec ([D (λ (m)
                  (if (zero? m)
                      'pizza
                      (cons (D (sub1 m)) '())))]
             [find
              (λ (n Ns Rs)
                (letrec ([F (λ (Ns Rs)
                              (cond [(null? Ns) #f]
                                    [(eq? (car Ns) n) (car Rs)]
                                    [else
                                     (F (cdr Ns) (cdr Rs))]))])
                  (F Ns Rs)))])
      (λ (n)
        (let ([exists (find n Ns Rs)])
          (if (atom? exists)
              (let ([result (D n)])
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)
              exists))))))

(deepM 5)

(define deepM
  (let ([Rs '()]
        [Ns '()])
    (λ (n)
      (let ([exists (find n Ns Rs)])
        (if (atom? exists)
            (let ([result (if (zero? n)
                              'pizza
                              (cons (deepM (sub1 n)) '()))])
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(deepM 1000)

(define consC
  (let ([N 0])
    (λ (x y)
      (set! N (add1 N))
      (cons x y))))

(define (deep m)
  (if (zero? m)
      'pizza
      (consC (deep (sub1 m)) '())))

(define counter 0)

(define consC
  (λ (x y)
    (set! counter (add1 counter))
    (cons x y)))

(define (deep m)
  (if (zero? m)
      'pizza
      (consC (deep (sub1 m)) '())))


(deep 5)
counter
(deep 100)
counter

(define (supercounter f)
  (letrec ([S (λ (n)
                (if (zero? n)
                    (f n)
                    (let ()
                      (f n)
                      (S (sub1 n)))))])
    (S 1000)
    counter))

(supercounter deep)

(define (rember1* a l)
  (letrec ([R (λ (l oh)
                (cond [(null? l) (oh 'no)]
                      [(atom? (car l))
                       (if (eq? (car l) a)
                           (cdr l)
                           (consC (car l) (R (cdr l) oh)))]
                      [else
                       (let ([new-car (let/cc oh
                                        (R (car l) oh))])
                         (if (atom? new-car)
                             (consC (car l) (R (cdr l) oh))
                             (consC new-car (cdr l))))]))])
    (let ([new-l (let/cc oh (R l oh))])
      (if (atom? new-l) l new-l))))


(rember1* 'noodles '((food) more (food)))
counter

(define (rember1* a l)
  (letrec ([R (λ (l)
                (cond [(null? l) '()]
                      [(atom? (car l))
                       (if (eq? (car l) a)
                           (cdr l)
                           (consC (car l) (R (cdr l))))]
                      [else
                       (let ([av (R (car l))])
                         (if (equal? (car l) av)
                             (consC (car l) (R (cdr l)))
                             (consC av (cdr l))))]))])
    (R l)))

(define counter 0)
(rember1* 'noodles '((food) more (food)))
counter
