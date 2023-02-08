#lang racket
(require racket)

(* (/ 24 (f 0)) 3)

(define k^ (lambda^ (v) (+ 3 v)))

(+ 3 (call/cc (λ (k^) ...)))


;; a very simple example of call/cc
(+ (call/cc
    (λ (k^)
      (/ (k^ 5)) 4))
   8)

;; first form:  k^ = (lambda^ (v) (+ v 8))
;; evaluate:    (/ (k^ 5) 4) when we know the value of k^
;; evaluate:    (k^ 5)
;; 13 is the result, the waiting division is forgotten since k^ escapes


;; continuations describe the rest of the computation
(* (+ (call/cc
       (λ (k^)
         (/ (k^ 5) 4)))
      8)
   3)

;; first form:   k^ = (lambda^ (v) (* (+ v 8) 3))
;; evaluate:     (/ (k^ 5) 4) when we know the value of k^
;; evaluate:     (k^ 5)
;; evaluate:     ((lambda^ (v) (* (+ v 8) 3)) 5)
;; result:       (* (+ 5 8) 3) => 39


(+ (call/cc
    (λ (k^)
      (/ (k^ 5)) 4))
   8)

;; first form: k^ = (lambda^ (v) (+ v 8))
;; evaluate:   (/ (k^ 5) 4) when we know k^
;; evaluate:   ((lambda^ (v) (+ v 8)) 5) => 13


(* (+ (let ([u (+ 3 2)])
        (call/cc
         (λ (j^)
           (/ (j^ u) 4))))
      8)
   3)

;; first form:  j^ = (lambda^ (v) (* (+ (let ([u (+ 3 2)]) v) 8) 3))
;; evaluate:    (/ (j^ u) 4) when we know j^
;; evaluate:    ((lambda^ (v) (* (+ v 8) 3)) 5)
;; evaluate:    (* (+ 5 8) 3) => 39


;; continuations are first-class objects
;; with call/cc we might save away the escape procedure, but not evoke it until later
(define +8^ '())

(+ (call/cc
    (λ (k^)
      (begin
        (set! +8^ k^)
        (display "inside body")
        5)))
   8)

;; first form:   k^ = (lambda^ (v) (+ v 8))
;; evaluate:     (begin (set! +8^ k^) (display "inside body") 5) when we know k^
;; evaluate:     set +8^ to k^, or (lambda^ (v) (+ v 8)) , display "inside body"
;; evaluate:     ((lambda^ (v) (+ v 8)) 5)  => 13
;; side effect:  we have stored k^ as +8^

(* (/ (+8^ 35) 0) 100)  ; => 43

;; first form: +8^ = (lambda^ (v) (+ v 8))
;; evaluate:   +8^ = (+ 35 8) => 43
;; this escapes the rest of the sequence, avoiding div by 0

;; II. Metaprogramming
