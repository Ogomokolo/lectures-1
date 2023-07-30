#lang racket

(require racket/trace) ; for dynamic function call traces


#|-----------------------------------------------------------------------------
;; Recursion (and Iteration)
-----------------------------------------------------------------------------|#

(define (println-times datum n)
  (when (> n 0)
    (println datum)
    (println-times datum (sub1 n))))

(trace println-times)

;; integer summation
(define (sum-to n)
  (if (<= n 0)
      0
      (+ n (sum-to (sub1 n)))))

(trace sum-to)

;; The following 3 optimizations to the above code, implements tail-call optimization (TCO)

(define (sum-to-acc n acc)
  (if (<= n 0)
      acc
      (sum-to-acc (sub1 n) (+ acc n))))

(trace sum-to-acc)

(define (sum-to-acc-2 n)
  (letrec ([sum (trace-lambda (i acc)
                  (if (<= i 0)
                      acc
                      (sum (sub1 i) (+ i acc))))])
    (sum n 0)))

(define (sum-to-acc-3 n)
  (define (sum i acc)
    (if (<= i 0)
        acc
        (sum (sub1 i) (+ i acc))))
  (trace sum)
  (sum n 0))

(define (sum-to-acc-4 n)
  (trace-let sum ([i n]
            [acc 0])
    (if (<= i 0)
        acc
        (sum (sub1 i) (+ i acc)))))

;; Fibonacci series: 0 1 1 2 3 5 8 13 21 34 55 ...
(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

; (trace fib)

;; The following optimization to the above code, implements tail-call optimization (TCO)

(define (fib-tail n)
  (trace-let rec ([m 0]
            [f0 0]
            [f1 1])
    (if (= m n)
        f0
        (rec (add1 m) f1 (+ f0 f1)))))



#|-----------------------------------------------------------------------------
;; On lists

- recursion over lists is an example of "structural recursion"
-----------------------------------------------------------------------------|#

(define (length lst)
  (if (empty? lst)
      0
      (add1 (length (rest lst)))))

(trace length)

;; The following optimization to the above code, implements tail-call optimization (TCO)

(define (length-tail lst)
  (trace-let rec ([l lst]
            [acc 0])
    (if (empty? l)
        acc
        (rec (rest l) (add1 acc)))))

#|-----------------------------------|#

(define (repeat n x)
  (if (= n 0)
      '()
      (cons x (repeat (sub1 n) x))))

(trace repeat)

;; The following optimization to the above code, implements tail-call optimization (TCO)

(define (repeat-tail n x)
  (trace-let rec ([num n]
            [acc '()])
    (if (= num 0)
        acc
        (rec (sub1 num) (cons x acc)))))

#|-----------------------------------|#

(define (reverse lst)
  (trace-let rec ([l lst]
            [acc '()])
  (if (empty? l)
      acc
      (rec (rest l) (cons (first l) acc)))))

#|-----------------------------------|#

(define (concat lst lst2)
  (trace-let rec ([l lst]
            [acc lst2])
  (if (empty? l)
      acc
      (rec (rest l) (cons (first l) acc)))))

#|-----------------------------------|#

(define (range n)
  (void))