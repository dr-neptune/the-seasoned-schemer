#lang racket
(require racket)

#| Ready, Set, Bang! |#

(define (sweet-tooth food)
  (cons food (cons 'cake '())))

(define last 'angelfood)

(sweet-tooth 'chocolate)

(define (sweet-toothL food)
  (set! last food)
  (cons food (cons 'cake '())))

(sweet-toothL 'chocolate)
last

(define ingredients '())
(define (sweet-toothR food)
  (set! ingredients (cons food ingredients))
  (cons food (cons 'cake '())))

(sweet-toothR 'chocolate)
(sweet-toothR 'cheese)
ingredients

(define (deep m)
  (cond [(zero? m) 'pizza]
        [else
         (cons (deep (sub1 m)) '())]))

(deep 10)

(define Ns '())
(define (deepR m)
  (cond [(zero? m) 'pizza]
        [else
         (set! Ns (cons m Ns))
         (cons (deepR (sub1 m)) '())]))

(deepR 4)
Ns

;; book version
(define Ns '())
(define (deepR m)
  (set! Ns (cons m Ns))
  (deep m))

(deepR 5)
Ns

(define Rs '())
(define Ns '())
(define (deepR n)
  (set! Ns (cons n Ns))
  (let ([res (deep n)])
    (set! Rs (cons res Rs))
    res))


(deepR 4)
Ns
Rs

(deepR 10)
Ns
Rs

(define (find n Ns Rs)
  (cond [(null? Ns) 'none]
        [(eq? (car Ns) n)
         (car Rs)]
        [else
         (find n (cdr Ns) (cdr Rs))]))

(define (find n Ns Rs)
  (letrec ([F (λ (Ns Rs)
                (cond [(null? Ns) #f]
                      [(eq? (car Ns) n) (car Rs)]
                      [else
                       (F (cdr Ns) (cdr Rs))]))])
    (F Ns Rs)))

(find 10 Ns Rs)
(find 3 Ns Rs)
(find 4 Ns Rs)

(define (deepM m)
  (let ([mem (find m Ns Rs)])
    (cond [(not (null? mem)) mem]
          [else (deepR m)])))

(deepM 20)


;; book
(define (member? a lat)
  (cond [(null? lat) #f]
        [(eq? (car lat) a) #t]
        [else (member? a (cdr lat))]))

(define (deepM n)
  (if (member? n Ns)
      (find n Ns Rs)
      (deepR n)))

(deepM 3)

(define (deepM n)
  (if (member? n Ns)
      (find n Ns Rs)
      (let ([result (deep n)])
        (set! Rs (cons result Rs))
        (set! Ns (cons n Ns))
        result)))

(deepM 18)

(define (deep m)
  (cond [(zero? m) 'pizza]
        [else
         (cons (deepM (sub1 m)) '())]))

(deepM 9)
Ns

(define (deepM n)
  (let ([Rs '()]
        [Ns '()])
    (if (member? n Ns)
        (find n Ns Rs)
        (let ([result (deep n)])
          (set! Rs (cons result Rs))
          (set! Ns (cons n Ns))
          result))))

(deepM 16)

(define Ns (inclusive-range 0 16))

(let rc ([Ns (reverse Ns)]
         [spaces 0])
  (if (null? (cdr Ns))
      (deepM (car Ns))
      (begin
        (displayln (deepM (car Ns)))
        (rc (cdr Ns) (add1 spaces)))))

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (deepM n)
  (let ([Rs '()]
        [Ns '()])
    (if (atom? (find n Ns Rs))
        (let ([result (deep n)])
          (set! Rs (cons result Rs))
          (set! Ns (cons n Ns))
          result)
        (find n Ns Rs))))

(define (length l)
  (cond [(null? l) 0]
        [else (add1 (length (cdr l)))]))

(define length
  (λ (l) 0))

(set! length (λ (l) (cond [(null? l) 0]
                          [else (add1 (length (cdr l)))])))

(define length
  (let ([h (λ (l) 0)])
    (set! h
          (λ (l)
            (cond [(null? l) 0]
                  [else (add1 (h (cdr l)))])))
    h))

(define length
  (let ([h (λ (l) 0)])
    (set! h ...)
    h))

(define L
  (λ (length)
    (λ (l)
      (cond [(null? l) 0]
            [else (add1 (length (cdr l)))]))))

(define length
  (let ([h (λ (l) 0)])
    (set! h (L (λ (arg) (h arg))))
    h))

(length '(alksjdf lkajsdf lkajsdf hasdfhasdfh hasdfhasdf))

(define Y!
  (λ (L)
    (let ([h (λ (l) '())])
      (set! h (L (λ (arg) (h arg))))
      h)))

(define (Y-bang f)
  (letrec ([h (f (λ (arg) (h arg)))])
    h))

(define length (Y! L))

(length '(a 2 d b dshlaks asj e jf 38))

(define D
  (λ (depth*)
    (λ (s)
      (cond [(null? s) 1]
            [(atom? (car s)) (depth* (cdr s))]
            [else
             (max (add1 (depth* (car s)))
                  (depth* (cdr s)))]))))

(define depth* (Y! D))

(depth* '((((a)) b) c))

(define bizarre
  (let ([x 0])
    (λ (f)
      (set! x (add1 x))
      (λ (a)
        (if (= a x)
            0
            (f a))))))

((Y! bizarre) 5) ; infinite loop
