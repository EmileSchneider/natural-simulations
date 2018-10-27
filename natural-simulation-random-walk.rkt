#lang racket
(require 2htdp/image)

(define walker '(50 50))

(define (get-x-walker walker)
  (first walker))

(define (get-y-walker walker)
  (first (rest walker)))

(define (set-y-walker walker n)
  (cons (first walker) (cons n (rest (rest walker)))))

(define (move direction walker)
  '())

(define (move-north walker)
  (cons (add1 (first walker))
        (rest walker)))

(define (move-south walker)
  (cons (sub1 (first walker))
        (rest walker)))

(define (move-west walker)
  (cons (first walker)
        (cons (add1 (first (rest walker)))
              (rest (rest walker)))))

(define (move-east walker)
  (cons (first walker)
        (cons (add1 (first (rest walker)))
              (rest (rest walker)))))

(define (step walker)
  (let ([rnd (random 0 4)])
    (cond [(= rnd 0) (move-north walker)]
          [(= rnd 1) (move-south walker)]
          [(= rnd 2) (move-east walker)]
          [(= rnd 3) (move-west walker)])))

(define (run-simulation walker n)
  (cond [(= n 0) (draw-world walker)]
        [else (begin (draw-world walker)
                     (print walker)
                     (run-simulation (step walker) (sub1 n)))]))

(define (draw-world walker)
  (place-image (circle 5 "solid" "black") (get-x-walker walker) (get-y-walker walker) (empty-scene 200 200)))

;(send frame show #t))
(run-simulation walker 10)
