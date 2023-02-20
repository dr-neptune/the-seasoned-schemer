#lang racket
(require racket)

#| The Difference Between Men and Boys ... |#

(define x (cons 'chicago (cons 'pizza '())))

(set! x 'gone)
(set! x 'skins)

(define (gourmet food)
  (cons food (cons x '())))

(gourmet 'onion)

(set! x 'rings)

(define (gourmand food)
  (set! x food)
  (cons food (cons x '())))

(gourmand 'onion)
(gourmand 'rings)

(define (diner food)
  (cons 'milkshake (cons food '())))

(define (dinerR food)
  (set! x food)
  (cons 'milkshake (cons food '())))

(dinerR 'onion)
(dinerR 'pecanpie)

(define (herbivore food)
  (let ([x 'minestrone])
    (set! x food)
    (cons food (cons x '()))))

(herbivore 'banana-bread)

(define (gobbler food)
  (let ([x 'minestrone])
    (set! x food)
    (cons food (cons x '()))))

(gobbler 'gumbo)

(define (nibbler food)
  (let ([x 'donut])
    (set! x food)
    (cons food (cons x '()))))

(define food '())
(define (glutton x)
  (set! food x)
  (cons 'more (cons x (cons 'more (cons x '())))))

(glutton 'garlic)

(define (chez-nous)
  (let ([plate '()])
    (set! plate x)
    (set! x food)
    (set! food plate)
    'done))

(displayln (format "food: ~a x: ~a" food x))
(chez-nous)
(displayln (format "food: ~a x: ~a" food x))
